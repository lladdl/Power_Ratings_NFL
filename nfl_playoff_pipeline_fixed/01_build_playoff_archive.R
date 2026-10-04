#!/usr/bin/env Rscript

# ============================================================
# 01 - BUILD PLAYOFF GAME ARCHIVE, OUTCOMES, AND BRACKET SEEDS
# 1970-2025
# ============================================================

source("00_config.R")
source(file.path("R", "playoff_helpers.R"))

if (!requireNamespace("nflreadr", quietly = TRUE)) {
  stop("Install nflreadr first: install.packages('nflreadr')")
}

if (!file.exists(historical_games_file)) {
  stop(
    paste0(
      "Historical game file not found:\n",
      historical_games_file,
      "\n\nRun the existing 1970-1998 historical rebuild first, ",
      "or change historical_games_file in 00_config.R."
    )
  )
}

master <- read_csv_base(master_ratings_file)
master$team <- normalize_team_code(master$team)
master$season <- as.integer(master$season)

# ------------------------------------------------------------
# 1970-1998
# ------------------------------------------------------------

legacy <- read_csv_base(historical_games_file)
legacy$team_dummy <- NA_character_

if (!"game_type" %in% names(legacy)) {
  stop("Historical game file must contain game_type.")
}

legacy_post <- legacy[
  legacy$season >= 1970 &
    legacy$season <= 1998 &
    legacy$game_type == "POST",
  ,
  drop = FALSE
]

# Historical postseason labels are inconsistent across the source data.
# The rebuild already identified the 269 postseason games correctly, so
# assign WC/DIV/CONF/SB from chronological position within each season
# and that season's actual playoff format.
legacy_post <- assign_legacy_playoff_rounds(
  legacy_post
)

legacy_post <- canonical_game_table(
  legacy_post,
  round_source = legacy_post$derived_playoff_round
)

legacy_seasons_found <- sort(
  unique(
    legacy_post$season
  )
)

if (
  nrow(
    legacy_post
  ) !=
  269L ||
  !identical(
    legacy_seasons_found,
    1970:1998
  )
) {
  
  stop(
    paste0(
      "Historical playoff archive validation failed before combining with modern data.\n",
      "Expected 269 games covering every season 1970-1998.\n",
      "Found ",
      nrow(
        legacy_post
      ),
      " games covering ",
      paste(
        legacy_seasons_found,
        collapse = ", "
      ),
      "."
    )
  )
}

cat(
  "Historical postseason rows loaded: ",
  nrow(
    legacy_post
  ),
  " (1970-1998)\n",
  sep = ""
)

# ------------------------------------------------------------
# 1999-2025
# ------------------------------------------------------------

cat("Loading nflreadr schedules for 1999-2025...\n")

modern_raw <- nflreadr::load_schedules(
  seasons = 1999:analysis_end_season
)

modern_raw <- as.data.frame(
  modern_raw,
  stringsAsFactors = FALSE
)

modern_post <- modern_raw[
  modern_raw$season >= 1999 &
    modern_raw$season <= analysis_end_season &
    toupper(as.character(modern_raw$game_type)) %in%
    c("WC", "DIV", "CON", "CONF", "SB"),
  ,
  drop = FALSE
]

modern_post <- canonical_game_table(
  modern_post,
  round_source = modern_post$game_type
)

# ------------------------------------------------------------
# Combine + conference/division metadata
# ------------------------------------------------------------
#
# IMPORTANT:
# The historical source and nflreadr contain many different
# source-specific columns. Keep ONLY the canonical fields needed
# by the playoff pipeline before rbind(). This guarantees the two
# sources have the same schema and column order.
# ------------------------------------------------------------

canonical_playoff_columns <- c(
  "game_id",
  "season",
  "game_date",
  "round",
  "round_order",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "neutral_site",
  "completed",
  "home_margin",
  "winner",
  "loser",
  "home_win"
)

keep_canonical_playoff_columns <- function(data) {
  
  missing <- setdiff(
    canonical_playoff_columns,
    names(
      data
    )
  )
  
  if (
    length(
      missing
    ) >
    0
  ) {
    
    stop(
      paste0(
        "Canonical playoff table is missing required columns: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  out <- data[
    ,
    canonical_playoff_columns,
    drop = FALSE
  ]
  
  # Force matching classes across historical and modern sources.
  out$game_id <- as.character(
    out$game_id
  )
  
  out$season <- as.integer(
    out$season
  )
  
  out$game_date <- as.character(
    out$game_date
  )
  
  out$round <- as.character(
    out$round
  )
  
  out$round_order <- as.integer(
    out$round_order
  )
  
  out$home_team <- as.character(
    out$home_team
  )
  
  out$away_team <- as.character(
    out$away_team
  )
  
  out$home_score <- as.numeric(
    out$home_score
  )
  
  out$away_score <- as.numeric(
    out$away_score
  )
  
  out$neutral_site <- as.logical(
    out$neutral_site
  )
  
  out$completed <- as.logical(
    out$completed
  )
  
  out$home_margin <- as.numeric(
    out$home_margin
  )
  
  out$winner <- as.character(
    out$winner
  )
  
  out$loser <- as.character(
    out$loser
  )
  
  out$home_win <- as.integer(
    out$home_win
  )
  
  out
}

legacy_post <- keep_canonical_playoff_columns(
  legacy_post
)

modern_post <- keep_canonical_playoff_columns(
  modern_post
)

cat(
  "Historical canonical columns: ",
  ncol(
    legacy_post
  ),
  "\n",
  sep = ""
)

cat(
  "Modern canonical columns: ",
  ncol(
    modern_post
  ),
  "\n",
  sep = ""
)

stopifnot(
  identical(
    names(
      legacy_post
    ),
    names(
      modern_post
    )
  )
)

playoff_games <- rbind(
  legacy_post,
  modern_post
)

playoff_games <- playoff_games[
  playoff_games$season >= analysis_start_season &
    playoff_games$season <= analysis_end_season &
    playoff_games$completed &
    !is.na(playoff_games$round),
  ,
  drop = FALSE
]

playoff_games$home_conference <- mapply(
  team_conference,
  playoff_games$season,
  playoff_games$home_team,
  USE.NAMES = FALSE
)

playoff_games$away_conference <- mapply(
  team_conference,
  playoff_games$season,
  playoff_games$away_team,
  USE.NAMES = FALSE
)

playoff_games$conference <- ifelse(
  playoff_games$round == "SB",
  "SB",
  playoff_games$home_conference
)

playoff_games$home_division <- mapply(
  team_division,
  playoff_games$season,
  playoff_games$home_team,
  USE.NAMES = FALSE
)

playoff_games$away_division <- mapply(
  team_division,
  playoff_games$season,
  playoff_games$away_team,
  USE.NAMES = FALSE
)

playoff_games$playoff_format <- vapply(
  playoff_games$season,
  season_playoff_format,
  character(1)
)

playoff_games <- playoff_games[
  order(
    playoff_games$season,
    playoff_games$round_order,
    playoff_games$game_date,
    playoff_games$game_id
  ),
  ,
  drop = FALSE
]

rownames(playoff_games) <- NULL

expected_all_seasons <- analysis_start_season:analysis_end_season
actual_all_seasons <- sort(
  unique(
    playoff_games$season
  )
)

if (
  !identical(
    actual_all_seasons,
    expected_all_seasons
  )
) {
  
  stop(
    paste0(
      "Combined playoff archive is incomplete. Expected every season ",
      analysis_start_season,
      "-",
      analysis_end_season,
      "; found ",
      paste(
        actual_all_seasons,
        collapse = ", "
      ),
      "."
    )
  )
}

if (
  nrow(
    playoff_games
  ) !=
  578L
) {
  
  stop(
    paste0(
      "Combined playoff game count is ",
      nrow(
        playoff_games
      ),
      "; expected 578 (269 historical + 309 modern)."
    )
  )
}

# ------------------------------------------------------------
# Team outcomes
# ------------------------------------------------------------

team_outcomes <- build_team_outcomes(
  playoff_games,
  master
)

# ------------------------------------------------------------
# Infer playoff seeds where the format requires them.
# 1970-1977 does not need seeds because the actual opening
# bracket can be simulated directly.
# ------------------------------------------------------------

seed_storage <- list()
k <- 1L

for (season in analysis_start_season:analysis_end_season) {
  
  sg <- playoff_games[
    playoff_games$season == season,
    ,
    drop = FALSE
  ]
  
  if (nrow(sg) == 0) next
  
  season_master <- master[
    master$season == season,
    ,
    drop = FALSE
  ]
  
  for (conf in c("AFC", "NFC")) {
    
    inferred <- infer_conference_seeds(
      season = season,
      conference = conf,
      games = playoff_games,
      master_rows = season_master
    )
    
    seed_storage[[k]] <- inferred
    k <- k + 1L
  }
}

# Different historical playoff formats return different seed
# diagnostics. For example, 1970-1977 does not require numeric
# seeds, so those rows do not naturally contain the later-era
# bracket diagnostic columns.
#
# Normalize every seed result to a common schema before rbind().

seed_columns <- unique(
  unlist(
    lapply(
      seed_storage,
      names
    ),
    use.names = FALSE
  )
)

normalize_seed_columns <- function(data, target_columns) {
  
  missing <- setdiff(
    target_columns,
    names(
      data
    )
  )
  
  for (
    column_name in
    missing
  ) {
    
    data[[column_name]] <- NA
  }
  
  data[
    ,
    target_columns,
    drop = FALSE
  ]
}

seed_storage <- lapply(
  seed_storage,
  normalize_seed_columns,
  target_columns = seed_columns
)

seed_table <- do.call(
  rbind,
  seed_storage
)

rownames(
  seed_table
) <- NULL

seed_table$season <- as.integer(
  seed_table$season
)

seed_table$conference <- as.character(
  seed_table$conference
)

seed_table$team <- as.character(
  seed_table$team
)

seed_table$seed <- suppressWarnings(
  as.integer(
    seed_table$seed
  )
)

seed_table$seed_inference_status <- as.character(
  seed_table$seed_inference_status
)

if (
  "bracket_match_score" %in%
  names(
    seed_table
  )
) {
  
  seed_table$bracket_match_score <- as.numeric(
    seed_table$bracket_match_score
  )
}

if (
  "record_order_penalty" %in%
  names(
    seed_table
  )
) {
  
  seed_table$record_order_penalty <- as.numeric(
    seed_table$record_order_penalty
  )
}

if (
  "candidate_ties_at_best_score" %in%
  names(
    seed_table
  )
) {
  
  seed_table$candidate_ties_at_best_score <- as.integer(
    seed_table$candidate_ties_at_best_score
  )
}

cat(
  "Seed inference rows combined: ",
  nrow(
    seed_table
  ),
  "\n",
  sep = ""
)

cat(
  "Seed inference columns: ",
  ncol(
    seed_table
  ),
  "\n",
  sep = ""
)

seed_table <- apply_seed_overrides(
  seed_table,
  seed_override_file
)

seed_table$division <- mapply(
  team_division,
  seed_table$season,
  seed_table$team,
  USE.NAMES = FALSE
)

seed_table$playoff_format <- vapply(
  seed_table$season,
  season_playoff_format,
  character(1)
)

# ------------------------------------------------------------
# Create an override template if one does not exist.
# ------------------------------------------------------------

if (!file.exists(seed_override_file)) {
  
  override_template <- data.frame(
    season = integer(0),
    conference = character(0),
    team = character(0),
    seed = integer(0),
    note = character(0),
    stringsAsFactors = FALSE
  )
  
  write_csv_base(
    override_template,
    seed_override_file
  )
}

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

validation_storage <- list()
v <- 1L

for (season in analysis_start_season:analysis_end_season) {
  
  sg <- playoff_games[
    playoff_games$season == season,
    ,
    drop = FALSE
  ]
  
  if (nrow(sg) == 0) next
  
  sb <- sg[
    sg$round == "SB",
    ,
    drop = FALSE
  ]
  
  seeds <- seed_table[
    seed_table$season == season,
    ,
    drop = FALSE
  ]
  
  validation_storage[[v]] <- data.frame(
    season = season,
    playoff_format = season_playoff_format(season),
    playoff_games = nrow(sg),
    wc_games = sum(sg$round == "WC"),
    div_games = sum(sg$round == "DIV"),
    conf_games = sum(sg$round == "CONF"),
    super_bowls = sum(sg$round == "SB"),
    champion = if (nrow(sb) == 1) sb$winner[1] else NA_character_,
    teams = length(unique(c(sg$home_team, sg$away_team))),
    missing_rounds = sum(is.na(sg$round)),
    missing_conferences = sum(
      is.na(sg$home_conference) |
        is.na(sg$away_conference)
    ),
    ambiguous_seed_rows = sum(
      grepl(
        "ambiguous",
        seeds$seed_inference_status %||% character(0)
      )
    ),
    stringsAsFactors = FALSE
  )
  
  v <- v + 1L
}

validation <- do.call(
  rbind,
  validation_storage
)

# ------------------------------------------------------------
# Outputs
# ------------------------------------------------------------

write_csv_base(
  playoff_games,
  file.path(
    output_directory,
    "01_playoff_games_1970_2025.csv"
  )
)

write_csv_base(
  team_outcomes,
  file.path(
    output_directory,
    "02_playoff_team_outcomes_1970_2025.csv"
  )
)

write_csv_base(
  seed_table,
  file.path(
    output_directory,
    "03_playoff_seeds_and_brackets_1970_2025.csv"
  )
)

write_csv_base(
  validation,
  file.path(
    output_directory,
    "01_playoff_archive_validation.csv"
  )
)

cat("\nPlayoff archive created.\n")
cat("Output directory:", output_directory, "\n")
cat("Games:", nrow(playoff_games), "\n")
cat("Team-season playoff rows:", nrow(team_outcomes), "\n")
cat("Seed rows:", nrow(seed_table), "\n")
cat("\nReview 01_playoff_archive_validation.csv before continuing.\n")
cat("If a historical seed needs correction, add it to playoff_seed_overrides.csv and rerun this script.\n")

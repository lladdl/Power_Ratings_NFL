#!/usr/bin/env Rscript

# ============================================================
# HISTORICAL NFL 1970-1998
# COMPLETE REBUILD FROM ACTUAL PFR-DERIVED RAW SCHEMA
# ============================================================
#
# This version matches the raw columns actually downloaded:
#
#   status
#   season
#   week
#   week_day
#   event_date
#   tm_market
#   tm_name
#   tm_alias
#   tm_alt_market
#   tm_alt_alias
#   opp_market
#   opp_name
#   opp_alias
#   opp_alt_market
#   opp_alt_alias
#   tm_location
#   opp_location
#   tm_score
#   opp_score
#   boxscore_stats_link
#
# The file has exactly one row per game.
#
# Expected 1970-1998 counts:
#
#   Total games:           6409
#   Regular-season games:  6140
#   Postseason games:       269
#   Team-season rows:        808
#
# IMPORTANT:
# Regular/postseason classification is based on the historically
# correct number of regular-season games in each season and game
# date ordering. This avoids depending on inconsistent historical
# postseason week labels.
#
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "tidyr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (
  length(
    missing_packages
  ) >
  0
) {
  
  stop(
    paste0(
      "Install these packages first: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})


# ============================================================
# 1. SETTINGS
# ============================================================

data_directory <- file.path(
  path.expand(
    "~/Downloads"
  ),
  "historical_nfl_1970_1998"
)

raw_file <- file.path(
  data_directory,
  "historical_nfl_1970_1998_pfr_game_logs_raw.csv"
)

if (
  !file.exists(
    raw_file
  )
) {
  
  stop(
    paste0(
      "Could not find:\n",
      raw_file,
      "\n\n",
      "Change data_directory near the top of this script if needed."
    )
  )
}


# ============================================================
# 2. HISTORICALLY EXPECTED REGULAR-SEASON GAME COUNTS
# ============================================================

expected_regular_games <- data.frame(
  season =
    1970:1998,
  expected_regular_games =
    c(
      rep(182, 6),  # 1970-1975: 26 teams x 14 games / 2
      rep(196, 2),  # 1976-1977: 28 teams x 14 games / 2
      rep(224, 4),  # 1978-1981: 28 teams x 16 games / 2
      126,          # 1982 strike season: 28 teams x 9 games / 2
      rep(224, 4),  # 1983-1986
      210,          # 1987 strike season: 28 teams x 15 games / 2
      rep(224, 7),  # 1988-1994
      rep(240, 4)   # 1995-1998: 30 teams x 16 games / 2
    ),
  stringsAsFactors =
    FALSE
)

stopifnot(
  sum(
    expected_regular_games$expected_regular_games
  ) ==
    6140
)


# ============================================================
# 3. HELPERS
# ============================================================

clean_names_simple <- function(
    x
) {
  
  x <- tolower(
    x
  )
  
  x <- gsub(
    "[^a-z0-9]+",
    "_",
    x
  )
  
  x <- gsub(
    "^_+|_+$",
    "",
    x
  )
  
  make.unique(
    x,
    sep = "_"
  )
}


normalize_team_code <- function(
    market,
    name,
    alias,
    alt_market = NA_character_,
    alt_alias = NA_character_
) {
  
  market <- as.character(
    market
  )
  
  name <- as.character(
    name
  )
  
  alias <- as.character(
    alias
  )
  
  alt_market <- as.character(
    alt_market
  )
  
  alt_alias <- as.character(
    alt_alias
  )
  
  full_name <- trimws(
    paste(
      market,
      name
    )
  )
  
  full_name_upper <- toupper(
    full_name
  )
  
  alias_upper <- toupper(
    trimws(
      alias
    )
  )
  
  alt_alias_upper <- toupper(
    trimws(
      alt_alias
    )
  )
  
  full_name_map <- c(
    "ATLANTA FALCONS" = "ATL",
    "BALTIMORE COLTS" = "IND",
    "BALTIMORE RAVENS" = "BAL",
    "BUFFALO BILLS" = "BUF",
    "CAROLINA PANTHERS" = "CAR",
    "CHICAGO BEARS" = "CHI",
    "CINCINNATI BENGALS" = "CIN",
    "CLEVELAND BROWNS" = "CLE",
    "DALLAS COWBOYS" = "DAL",
    "DENVER BRONCOS" = "DEN",
    "DETROIT LIONS" = "DET",
    "GREEN BAY PACKERS" = "GB",
    "HOUSTON OILERS" = "TEN",
    "INDIANAPOLIS COLTS" = "IND",
    "JACKSONVILLE JAGUARS" = "JAX",
    "KANSAS CITY CHIEFS" = "KC",
    "LOS ANGELES RAIDERS" = "LV",
    "LOS ANGELES RAMS" = "LA",
    "MIAMI DOLPHINS" = "MIA",
    "MINNESOTA VIKINGS" = "MIN",
    "NEW ENGLAND PATRIOTS" = "NE",
    "NEW ORLEANS SAINTS" = "NO",
    "NEW YORK GIANTS" = "NYG",
    "NEW YORK JETS" = "NYJ",
    "OAKLAND RAIDERS" = "LV",
    "PHILADELPHIA EAGLES" = "PHI",
    "PHOENIX CARDINALS" = "ARI",
    "PITTSBURGH STEELERS" = "PIT",
    "SAN DIEGO CHARGERS" = "LAC",
    "SAN FRANCISCO 49ERS" = "SF",
    "SEATTLE SEAHAWKS" = "SEA",
    "ST. LOUIS CARDINALS" = "ARI",
    "ST LOUIS CARDINALS" = "ARI",
    "ST. LOUIS RAMS" = "LA",
    "ST LOUIS RAMS" = "LA",
    "TAMPA BAY BUCCANEERS" = "TB",
    "TENNESSEE OILERS" = "TEN",
    "WASHINGTON REDSKINS" = "WAS"
  )
  
  alias_map <- c(
    "ARI" = "ARI",
    "ATL" = "ATL",
    "BAL" = "BAL",
    "BLT" = "IND",
    "BUF" = "BUF",
    "CAR" = "CAR",
    "CHI" = "CHI",
    "CIN" = "CIN",
    "CLE" = "CLE",
    "DAL" = "DAL",
    "DEN" = "DEN",
    "DET" = "DET",
    "GB" = "GB",
    "GNB" = "GB",
    "HOU" = "TEN",
    "IND" = "IND",
    "JAC" = "JAX",
    "JAX" = "JAX",
    "KAN" = "KC",
    "KC" = "KC",
    "LA" = "LA",
    "LAR" = "LA",
    "LV" = "LV",
    "LVR" = "LV",
    "MIA" = "MIA",
    "MIN" = "MIN",
    "NE" = "NE",
    "NWE" = "NE",
    "NO" = "NO",
    "NOR" = "NO",
    "NYG" = "NYG",
    "NYJ" = "NYJ",
    "OAK" = "LV",
    "PHI" = "PHI",
    "PHO" = "ARI",
    "PIT" = "PIT",
    "SD" = "LAC",
    "SDG" = "LAC",
    "SEA" = "SEA",
    "SF" = "SF",
    "SFO" = "SF",
    "STL" = "LA",
    "TB" = "TB",
    "TAM" = "TB",
    "TEN" = "TEN",
    "WAS" = "WAS",
    "WSH" = "WAS"
  )
  
  output <- rep(
    NA_character_,
    length(
      full_name_upper
    )
  )
  
  full_match <- match(
    full_name_upper,
    names(
      full_name_map
    )
  )
  
  full_ok <- !is.na(
    full_match
  )
  
  output[
    full_ok
  ] <- unname(
    full_name_map[
      full_match[
        full_ok
      ]
    ]
  )
  
  use_alias <- is.na(
    output
  )
  
  alias_match <- match(
    alias_upper[
      use_alias
    ],
    names(
      alias_map
    )
  )
  
  alias_ok <- !is.na(
    alias_match
  )
  
  alias_indices <- which(
    use_alias
  )
  
  output[
    alias_indices[
      alias_ok
    ]
  ] <- unname(
    alias_map[
      alias_match[
        alias_ok
      ]
    ]
  )
  
  use_alt_alias <- is.na(
    output
  )
  
  alt_match <- match(
    alt_alias_upper[
      use_alt_alias
    ],
    names(
      alias_map
    )
  )
  
  alt_ok <- !is.na(
    alt_match
  )
  
  alt_indices <- which(
    use_alt_alias
  )
  
  output[
    alt_indices[
      alt_ok
    ]
  ] <- unname(
    alias_map[
      alt_match[
        alt_ok
      ]
    ]
  )
  
  still_missing <- is.na(
    output
  )
  
  output[
    still_missing
  ] <- alias[
    still_missing
  ]
  
  output
}


normalize_location <- function(
    x
) {
  
  x <- toupper(
    trimws(
      as.character(
        x
      )
    )
  )
  
  case_when(
    x %in%
      c(
        "HOME",
        "H"
      ) ~
      "HOME",
    x %in%
      c(
        "AWAY",
        "A",
        "@",
        "ROAD"
      ) ~
      "AWAY",
    x %in%
      c(
        "NEUTRAL",
        "N"
      ) ~
      "NEUTRAL",
    TRUE ~
      x
  )
}


calculate_srs <- function(
    team_games
) {
  
  teams <- sort(
    unique(
      team_games$team
    )
  )
  
  n_teams <- length(
    teams
  )
  
  team_index <- setNames(
    seq_along(
      teams
    ),
    teams
  )
  
  A <- diag(
    n_teams
  )
  
  b <- rep(
    0,
    n_teams
  )
  
  mov <- team_games |>
    group_by(
      team
    ) |>
    summarise(
      mov =
        mean(
          margin
        ),
      .groups =
        "drop"
    )
  
  for (
    i in
    seq_along(
      teams
    )
  ) {
    
    team <- teams[i]
    
    tg <- team_games[
      team_games$team ==
        team,
    ]
    
    b[i] <- mean(
      tg$margin
    )
    
    opponent_counts <- table(
      tg$opponent
    )
    
    for (
      opponent in
      names(
        opponent_counts
      )
    ) {
      
      j <- team_index[
        opponent
      ]
      
      if (
        !is.na(
          j
        )
      ) {
        
        A[
          i,
          j
        ] <- A[
          i,
          j
        ] -
          as.numeric(
            opponent_counts[
              opponent
            ]
          ) /
          nrow(
            tg
          )
      }
    }
  }
  
  A_aug <- rbind(
    A,
    rep(
      1,
      n_teams
    )
  )
  
  b_aug <- c(
    b,
    0
  )
  
  srs <- as.numeric(
    qr.solve(
      A_aug,
      b_aug
    )
  )
  
  srs <- srs -
    mean(
      srs
    )
  
  output <- data.frame(
    team =
      teams,
    derived_SRS =
      srs,
    stringsAsFactors =
      FALSE
  )
  
  output <- output |>
    left_join(
      mov,
      by =
        "team"
    ) |>
    mutate(
      derived_SOS =
        derived_SRS -
        mov
    ) |>
    select(
      team,
      derived_SRS,
      derived_SOS
    )
  
  output
}


# ============================================================
# 4. READ RAW GAME LOGS
# ============================================================

raw <- suppressMessages(
  readr::read_csv(
    raw_file,
    show_col_types = FALSE,
    progress = FALSE
  )
)

names(
  raw
) <- clean_names_simple(
  names(
    raw
  )
)

cat(
  "\nRaw rows loaded: ",
  nrow(
    raw
  ),
  "\n",
  sep = ""
)

required_columns <- c(
  "season",
  "week",
  "week_day",
  "event_date",
  "tm_market",
  "tm_name",
  "tm_alias",
  "opp_market",
  "opp_name",
  "opp_alias",
  "tm_location",
  "opp_location",
  "tm_score",
  "opp_score",
  "boxscore_stats_link"
)

missing_columns <- setdiff(
  required_columns,
  names(
    raw
  )
)

if (
  length(
    missing_columns
  ) >
  0
) {
  
  stop(
    paste0(
      "Missing expected raw columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 5. PARSE DATES + ORIGINAL TEAM/OPP FIELDS
# ============================================================

raw$game_date <- suppressWarnings(
  as.Date(
    raw$event_date
  )
)

if (
  any(
    is.na(
      raw$game_date
    )
  )
) {
  
  raw$game_date <- suppressWarnings(
    as.Date(
      raw$event_date,
      format = "%m/%d/%Y"
    )
  )
}

if (
  any(
    is.na(
      raw$game_date
    )
  )
) {
  
  stop(
    "Some event_date values could not be parsed as dates."
  )
}


raw$tm_full_name <- trimws(
  paste(
    raw$tm_market,
    raw$tm_name
  )
)

raw$opp_full_name <- trimws(
  paste(
    raw$opp_market,
    raw$opp_name
  )
)

raw$tm_code <- normalize_team_code(
  raw$tm_market,
  raw$tm_name,
  raw$tm_alias,
  raw$tm_alt_market,
  raw$tm_alt_alias
)

raw$opp_code <- normalize_team_code(
  raw$opp_market,
  raw$opp_name,
  raw$opp_alias,
  raw$opp_alt_market,
  raw$opp_alt_alias
)

raw$tm_location_clean <- normalize_location(
  raw$tm_location
)

raw$opp_location_clean <- normalize_location(
  raw$opp_location
)


cat(
  "\nObserved tm_location values:\n"
)

print(
  sort(
    unique(
      raw$tm_location_clean
    )
  )
)

cat(
  "\nObserved opp_location values:\n"
)

print(
  sort(
    unique(
      raw$opp_location_clean
    )
  )
)


# ============================================================
# 6. RECONSTRUCT HOME / AWAY
# ============================================================

tm_is_home <- (
  raw$tm_location_clean ==
    "HOME"
)

tm_is_away <- (
  raw$tm_location_clean ==
    "AWAY"
)

tm_is_neutral <- (
  raw$tm_location_clean ==
    "NEUTRAL" |
    raw$opp_location_clean ==
    "NEUTRAL"
)

# Fallback if tm_location is blank but opponent location is usable.
tm_is_home[
  !tm_is_home &
    !tm_is_away &
    !tm_is_neutral &
    raw$opp_location_clean ==
    "AWAY"
] <- TRUE

tm_is_away[
  !tm_is_home &
    !tm_is_away &
    !tm_is_neutral &
    raw$opp_location_clean ==
    "HOME"
] <- TRUE


unresolved_location <- (
  !tm_is_home &
    !tm_is_away &
    !tm_is_neutral
)

if (
  any(
    unresolved_location
  )
) {
  
  unresolved_values <- unique(
    data.frame(
      tm_location =
        raw$tm_location[
          unresolved_location
        ],
      opp_location =
        raw$opp_location[
          unresolved_location
        ],
      stringsAsFactors =
        FALSE
    )
  )
  
  cat(
    "\nUnresolved location combinations:\n"
  )
  
  print(
    unresolved_values
  )
  
  stop(
    paste0(
      "Could not determine home/away for ",
      sum(
        unresolved_location
      ),
      " games."
    )
  )
}


# For neutral-site games, tm is retained as the nominal home side.
# HFA must be set to zero later for neutral_site == TRUE.

home_team_name <- ifelse(
  tm_is_away,
  raw$opp_full_name,
  raw$tm_full_name
)

away_team_name <- ifelse(
  tm_is_away,
  raw$tm_full_name,
  raw$opp_full_name
)

home_team <- ifelse(
  tm_is_away,
  raw$opp_code,
  raw$tm_code
)

away_team <- ifelse(
  tm_is_away,
  raw$tm_code,
  raw$opp_code
)

home_score <- ifelse(
  tm_is_away,
  raw$opp_score,
  raw$tm_score
)

away_score <- ifelse(
  tm_is_away,
  raw$tm_score,
  raw$opp_score
)


# ============================================================
# 7. BUILD BASE GAME TABLE
# ============================================================

games <- data.frame(
  season =
    as.integer(
      raw$season
    ),
  raw_week =
    as.character(
      raw$week
    ),
  week_day =
    as.character(
      raw$week_day
    ),
  game_date =
    raw$game_date,
  status =
    as.character(
      raw$status
    ),
  home_team_name =
    home_team_name,
  away_team_name =
    away_team_name,
  home_team =
    home_team,
  away_team =
    away_team,
  home_score =
    as.numeric(
      home_score
    ),
  away_score =
    as.numeric(
      away_score
    ),
  neutral_site =
    tm_is_neutral,
  tm_location_original =
    as.character(
      raw$tm_location
    ),
  opp_location_original =
    as.character(
      raw$opp_location
    ),
  tm_alias_original =
    as.character(
      raw$tm_alias
    ),
  opp_alias_original =
    as.character(
      raw$opp_alias
    ),
  boxscore_stats_link =
    as.character(
      raw$boxscore_stats_link
    ),
  source_dataset =
    as.character(
      raw$source_dataset
    ),
  source_url =
    as.character(
      raw$source_url
    ),
  stringsAsFactors =
    FALSE
)


# ============================================================
# 8. CLASSIFY REGULAR SEASON / POSTSEASON
#
# Each season is sorted chronologically.
# The first historically expected N games are regular season;
# remaining games are postseason.
# ============================================================

games <- games |>
  arrange(
    season,
    game_date,
    raw_week,
    home_team,
    away_team
  ) |>
  left_join(
    expected_regular_games,
    by =
      "season"
  ) |>
  group_by(
    season
  ) |>
  mutate(
    season_game_number =
      row_number(),
    game_type =
      ifelse(
        season_game_number <=
          expected_regular_games,
        "REG",
        "POST"
      )
  ) |>
  ungroup()


games$week_numeric <- suppressWarnings(
  as.integer(
    games$raw_week
  )
)

games$week <- ifelse(
  games$game_type ==
    "REG",
  games$week_numeric,
  NA_integer_
)

games$postseason_round <- ifelse(
  games$game_type ==
    "POST",
  games$raw_week,
  NA_character_
)


games$home_margin <- (
  games$home_score -
    games$away_score
)

games$total_points <- (
  games$home_score +
    games$away_score
)

games$winner <- ifelse(
  games$home_score >
    games$away_score,
  games$home_team,
  ifelse(
    games$away_score >
      games$home_score,
    games$away_team,
    "TIE"
  )
)

games$completed <- (
  !is.na(
    games$home_score
  ) &
    !is.na(
      games$away_score
    )
)


# ============================================================
# 9. VALIDATE GAME COUNTS
# ============================================================

actual_total_games <- nrow(
  games
)

actual_regular_games <- sum(
  games$game_type ==
    "REG"
)

actual_postseason_games <- sum(
  games$game_type ==
    "POST"
)

cat(
  "\n========================================\n"
)

cat(
  "GAME COUNT VALIDATION\n"
)

cat(
  "========================================\n"
)

cat(
  "Total games:          ",
  actual_total_games,
  " / 6409\n",
  sep = ""
)

cat(
  "Regular-season games: ",
  actual_regular_games,
  " / 6140\n",
  sep = ""
)

cat(
  "Postseason games:     ",
  actual_postseason_games,
  " / 269\n",
  sep = ""
)


# ============================================================
# 10. PER-SEASON VALIDATION
# ============================================================

season_game_counts <- games |>
  group_by(
    season
  ) |>
  summarise(
    total_games =
      n(),
    regular_games =
      sum(
        game_type ==
          "REG"
      ),
    postseason_games =
      sum(
        game_type ==
          "POST"
      ),
    .groups =
      "drop"
  ) |>
  left_join(
    expected_regular_games,
    by =
      "season"
  ) |>
  mutate(
    regular_matches_expected =
      regular_games ==
      expected_regular_games
  )


cat(
  "\nPer-season game counts:\n"
)

print(
  season_game_counts,
  n =
    nrow(
      season_game_counts
    )
)


# ============================================================
# 11. CREATE REGULAR-SEASON TEAM-GAME LONG TABLE
# ============================================================

regular_games <- games |>
  filter(
    game_type ==
      "REG",
    completed
  )

home_long <- regular_games |>
  transmute(
    season =
      season,
    week =
      week,
    game_date =
      game_date,
    team =
      home_team,
    opponent =
      away_team,
    site =
      ifelse(
        neutral_site,
        "N",
        "H"
      ),
    points_for =
      home_score,
    points_against =
      away_score,
    margin =
      home_score -
      away_score
  )

away_long <- regular_games |>
  transmute(
    season =
      season,
    week =
      week,
    game_date =
      game_date,
    team =
      away_team,
    opponent =
      home_team,
    site =
      ifelse(
        neutral_site,
        "N",
        "A"
      ),
    points_for =
      away_score,
    points_against =
      home_score,
    margin =
      away_score -
      home_score
  )

team_games <- bind_rows(
  home_long,
  away_long
)


# ============================================================
# 12. TEAM-SEASON SUMMARIES
# ============================================================

season_values <- sort(
  unique(
    team_games$season
  )
)

team_season_storage <- list()
team_season_position <- 1

for (
  season_value in
  season_values
) {
  
  tg <- team_games[
    team_games$season ==
      season_value,
  ]
  
  summary <- tg |>
    group_by(
      team
    ) |>
    summarise(
      games =
        n(),
      wins =
        sum(
          margin >
            0
        ),
      losses =
        sum(
          margin <
            0
        ),
      ties =
        sum(
          margin ==
            0
        ),
      win_pct =
        (
          wins +
            0.5 *
            ties
        ) /
        games,
      points_for =
        sum(
          points_for
        ),
      points_against =
        sum(
          points_against
        ),
      point_differential =
        points_for -
        points_against,
      mov =
        mean(
          margin
        ),
      points_for_per_game =
        mean(
          points_for
        ),
      points_against_per_game =
        mean(
          points_against
        ),
      .groups =
        "drop"
    )
  
  srs <- calculate_srs(
    tg
  )
  
  summary <- summary |>
    left_join(
      srs,
      by =
        "team"
    ) |>
    mutate(
      season =
        season_value,
      record =
        ifelse(
          ties >
            0,
          paste0(
            wins,
            "-",
            losses,
            "-",
            ties
          ),
          paste0(
            wins,
            "-",
            losses
          )
        )
    ) |>
    select(
      season,
      team,
      record,
      games,
      wins,
      losses,
      ties,
      win_pct,
      points_for,
      points_against,
      point_differential,
      mov,
      derived_SOS,
      derived_SRS,
      points_for_per_game,
      points_against_per_game
    ) |>
    arrange(
      desc(
        derived_SRS
      )
    )
  
  team_season_storage[[team_season_position]] <-
    summary
  
  team_season_position <-
    team_season_position +
    1
}

team_seasons <- bind_rows(
  team_season_storage
)


# ============================================================
# 13. EXPECTED TEAM-SEASON COUNT
# ============================================================

actual_team_season_rows <- nrow(
  team_seasons
)

cat(
  "\nTeam-season rows: ",
  actual_team_season_rows,
  " / 808\n",
  sep = ""
)


# ============================================================
# 14. TEAM NAME / ALIAS CROSSWALK
# ============================================================

team_crosswalk <- bind_rows(
  raw |>
    transmute(
      season =
        as.integer(
          season
        ),
      market =
        tm_market,
      name =
        tm_name,
      alias =
        tm_alias,
      alt_market =
        tm_alt_market,
      alt_alias =
        tm_alt_alias,
      canonical_team =
        tm_code
    ),
  raw |>
    transmute(
      season =
        as.integer(
          season
        ),
      market =
        opp_market,
      name =
        opp_name,
      alias =
        opp_alias,
      alt_market =
        opp_alt_market,
      alt_alias =
        opp_alt_alias,
      canonical_team =
        opp_code
    )
) |>
  distinct() |>
  arrange(
    season,
    canonical_team,
    market,
    name
  )


# ============================================================
# 15. VALIDATION TABLE
# ============================================================

validation <- data.frame(
  item =
    c(
      "total_games",
      "regular_season_games",
      "postseason_games",
      "team_season_rows",
      "first_season",
      "last_season",
      "all_season_regular_counts_match"
    ),
  actual =
    c(
      actual_total_games,
      actual_regular_games,
      actual_postseason_games,
      actual_team_season_rows,
      min(
        games$season
      ),
      max(
        games$season
      ),
      all(
        season_game_counts$regular_matches_expected
      )
    ),
  expected =
    c(
      6409,
      6140,
      269,
      808,
      1970,
      1998,
      TRUE
    ),
  stringsAsFactors =
    FALSE
)

validation$match <- (
  as.character(
    validation$actual
  ) ==
    as.character(
      validation$expected
    )
)


# ============================================================
# 16. SAVE OUTPUTS
# ============================================================

games_output <- file.path(
  data_directory,
  "historical_nfl_1970_1998_games_COMPLETE.csv"
)

team_output <- file.path(
  data_directory,
  "historical_nfl_1970_1998_team_seasons_COMPLETE.csv"
)

counts_output <- file.path(
  data_directory,
  "historical_nfl_1970_1998_season_game_counts_COMPLETE.csv"
)

crosswalk_output <- file.path(
  data_directory,
  "historical_nfl_1970_1998_team_crosswalk.csv"
)

validation_output <- file.path(
  data_directory,
  "historical_nfl_1970_1998_validation_COMPLETE.csv"
)

readr::write_csv(
  games,
  games_output
)

readr::write_csv(
  team_seasons,
  team_output
)

readr::write_csv(
  season_game_counts,
  counts_output
)

readr::write_csv(
  team_crosswalk,
  crosswalk_output
)

readr::write_csv(
  validation,
  validation_output
)


# ============================================================
# 17. FINAL REPORT
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "COMPLETE 1970-1998 DATASET BUILT\n"
)

cat(
  "========================================\n\n"
)

print(
  validation
)

cat(
  "\nFiles written:\n"
)

cat(
  "  ",
  games_output,
  "\n",
  sep = ""
)

cat(
  "  ",
  team_output,
  "\n",
  sep = ""
)

cat(
  "  ",
  counts_output,
  "\n",
  sep = ""
)

cat(
  "  ",
  crosswalk_output,
  "\n",
  sep = ""
)

cat(
  "  ",
  validation_output,
  "\n",
  sep = ""
)

cat(
  "\nIf every validation row is TRUE, these are the files to use going forward.\n"
)

#!/usr/bin/env Rscript

# ============================================================
# 03 - RE-FIT RATINGS WITH POSTSEASON GAMES INCLUDED
# Produces pre-vs-post ratings and round-by-round trajectories.
# ============================================================

source("00_config.R")
source(file.path("R", "playoff_helpers.R"))
source(file.path("R", "rating_core.R"))

if (!requireNamespace("nflreadr", quietly = TRUE)) {
  stop("Install nflreadr first: install.packages('nflreadr')")
}

playoff_file <- file.path(
  output_directory,
  "01_playoff_games_1970_2025.csv"
)

if (!file.exists(playoff_file)) {
  stop("Run 01_build_playoff_archive.R first.")
}

playoff_games <- read_csv_base(playoff_file)
master <- read_csv_base(master_ratings_file)

playoff_games$home_team <- normalize_team_code(playoff_games$home_team)
playoff_games$away_team <- normalize_team_code(playoff_games$away_team)
master$team <- normalize_team_code(master$team)

# ------------------------------------------------------------
# Load regular-season games
# ------------------------------------------------------------

if (!file.exists(historical_games_file)) {
  stop("Historical game file not found: ", historical_games_file)
}

legacy <- read_csv_base(historical_games_file)

legacy_reg <- legacy[
  legacy$season >= 1970 &
    legacy$season <= 1998 &
    legacy$game_type == "REG",
  ,
  drop = FALSE
]


if (
  nrow(
    legacy_reg
  ) !=
    6140L
) {

  stop(
    paste0(
      "Historical regular-season archive contains ",
      nrow(
        legacy_reg
      ),
      " REG games; expected 6140."
    )
  )
}

playoff_seasons_present <- sort(
  unique(
    playoff_games$season
  )
)

if (
  !identical(
    playoff_seasons_present,
    analysis_start_season:analysis_end_season
  )
) {

  stop(
    paste0(
      "Playoff archive is incomplete before postseason rating refit. Found seasons: ",
      paste(
        playoff_seasons_present,
        collapse = ", "
      )
    )
  )
}

legacy_reg$home_team <- normalize_team_code(legacy_reg$home_team)
legacy_reg$away_team <- normalize_team_code(legacy_reg$away_team)
legacy_reg$season <- as.integer(legacy_reg$season)
legacy_reg$week <- as.integer(legacy_reg$week)
legacy_reg$home_score <- as.numeric(legacy_reg$home_score)
legacy_reg$away_score <- as.numeric(legacy_reg$away_score)

if (!"neutral_site" %in% names(legacy_reg)) {
  legacy_reg$neutral_site <- FALSE
}
if (!is.logical(legacy_reg$neutral_site)) {
  legacy_reg$neutral_site <- toupper(as.character(legacy_reg$neutral_site)) %in%
    c("TRUE", "T", "1", "YES", "NEUTRAL")
}

cat("Loading modern regular-season schedules...\n")

modern <- nflreadr::load_schedules(
  seasons = 1999:analysis_end_season
)

modern <- as.data.frame(
  modern,
  stringsAsFactors = FALSE
)

modern_reg <- modern[
  modern$season >= 1999 &
    modern$season <= analysis_end_season &
    toupper(as.character(modern$game_type)) == "REG" &
    !is.na(modern$home_score) &
    !is.na(modern$away_score),
  ,
  drop = FALSE
]

modern_reg$home_team <- normalize_team_code(modern_reg$home_team)
modern_reg$away_team <- normalize_team_code(modern_reg$away_team)
modern_reg$season <- as.integer(modern_reg$season)
modern_reg$week <- as.integer(modern_reg$week)
modern_reg$home_score <- as.numeric(modern_reg$home_score)
modern_reg$away_score <- as.numeric(modern_reg$away_score)

if ("location" %in% names(modern_reg)) {
  modern_reg$neutral_site <- (
    tolower(as.character(modern_reg$location)) ==
      "neutral"
  )
  modern_reg$neutral_site[is.na(modern_reg$neutral_site)] <- FALSE
} else {
  modern_reg$neutral_site <- FALSE
}

rating_cols <- c(
  "season",
  "week",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "neutral_site"
)

regular_games <- rbind(
  legacy_reg[, rating_cols, drop = FALSE],
  modern_reg[, rating_cols, drop = FALSE]
)

regular_games <- regular_games[
  !is.na(regular_games$home_score) &
    !is.na(regular_games$away_score),
  ,
  drop = FALSE
]

# ------------------------------------------------------------
# Refit season by season
# ------------------------------------------------------------

trajectory_storage <- list()
validation_storage <- list()

k <- 1L
v <- 1L

for (season in analysis_start_season:analysis_end_season) {

  cat("Postseason-inclusive ratings:", season, "\n")

  reg <- regular_games[
    regular_games$season == season,
    ,
    drop = FALSE
  ]

  post <- playoff_games[
    playoff_games$season == season,
    ,
    drop = FALSE
  ]

  if (nrow(reg) == 0 || nrow(post) == 0) next

  league_teams <- sort(
    unique(
      c(
        reg$home_team,
        reg$away_team
      )
    )
  )

  playoff_teams <- sort(
    unique(
      c(
        post$home_team,
        post$away_team
      )
    )
  )

  master_season <- master[
    master$season == season,
    ,
    drop = FALSE
  ]

  pre_model <- fit_power_model(
    training_games = reg,
    league_teams = league_teams
  )

  for (team in playoff_teams) {

    master_idx <- which(
      master_season$team ==
        team
    )

    master_rating <- if (
      length(master_idx) ==
        1
    ) {
      master_season$rating[master_idx]
    } else {
      NA_real_
    }

    trajectory_storage[[k]] <- data.frame(
      season = season,
      team = team,
      stage = "REG_FINAL",
      stage_order = 0L,
      through_playoff_round = "REG_FINAL",
      rating = as.numeric(pre_model$ratings[team]),
      offense_rating = as.numeric(pre_model$offense_rating[team]),
      defense_rating = as.numeric(pre_model$defense_rating[team]),
      master_pre_playoff_rating = master_rating,
      rating_change_vs_master_pre = (
        as.numeric(pre_model$ratings[team]) -
          master_rating
      ),
      stringsAsFactors = FALSE
    )

    k <- k + 1L
  }

  base_week <- max(
    reg$week,
    na.rm = TRUE
  )

  rounds_present <- c(
    "WC",
    "DIV",
    "CONF",
    "SB"
  )

  active_rounds <- rounds_present[
    rounds_present %in%
      unique(
        post$round
      )
  ]

  cumulative_post <- post[0, , drop = FALSE]

  for (round_name in active_rounds) {

    current_round <- post[
      post$round ==
        round_name,
      ,
      drop = FALSE
    ]

    if (nrow(current_round) == 0) next

    current_round$week <- (
      base_week +
        match(
          round_name,
          active_rounds
        )
    )

    current_round$neutral_site <- as.logical(
      current_round$neutral_site
    )

    cumulative_post <- rbind(
      cumulative_post,
      current_round
    )

    post_rating_rows <- cumulative_post[
      ,
      c(
        "season",
        "week",
        "home_team",
        "away_team",
        "home_score",
        "away_score",
        "neutral_site"
      ),
      drop = FALSE
    ]

    training <- rbind(
      reg[
        ,
        c(
          "season",
          "week",
          "home_team",
          "away_team",
          "home_score",
          "away_score",
          "neutral_site"
        ),
        drop = FALSE
      ],
      post_rating_rows
    )

    model <- fit_power_model(
      training_games = training,
      league_teams = league_teams
    )

    for (team in playoff_teams) {

      master_idx <- which(
        master_season$team ==
          team
      )

      master_rating <- if (
        length(master_idx) ==
          1
      ) {
        master_season$rating[master_idx]
      } else {
        NA_real_
      }

      trajectory_storage[[k]] <- data.frame(
        season = season,
        team = team,
        stage = paste0("AFTER_", round_name),
        stage_order = round_order_from_name(round_name),
        through_playoff_round = round_name,
        rating = as.numeric(model$ratings[team]),
        offense_rating = as.numeric(model$offense_rating[team]),
        defense_rating = as.numeric(model$defense_rating[team]),
        master_pre_playoff_rating = master_rating,
        rating_change_vs_master_pre = (
          as.numeric(model$ratings[team]) -
            master_rating
        ),
        stringsAsFactors = FALSE
      )

      k <- k + 1L
    }
  }

  pre_rows <- trajectory_storage[
    vapply(
      trajectory_storage,
      function(x) {
        nrow(x) == 1 &&
          x$season[1] == season &&
          x$stage[1] == "REG_FINAL"
      },
      logical(1)
    )
  ]

  if (length(pre_rows) > 0) {
    pre_check <- do.call(rbind, pre_rows)
    validation_storage[[v]] <- data.frame(
      season = season,
      max_abs_regular_refit_minus_master = max(
        abs(
          pre_check$rating_change_vs_master_pre
        ),
        na.rm = TRUE
      ),
      mean_abs_regular_refit_minus_master = mean(
        abs(
          pre_check$rating_change_vs_master_pre
        ),
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
    v <- v + 1L
  }
}

trajectory <- do.call(
  rbind,
  trajectory_storage
)

trajectory <- trajectory[
  order(
    trajectory$season,
    trajectory$team,
    trajectory$stage_order
  ),
  ,
  drop = FALSE
]

# Final postseason-inclusive rating = latest fitted playoff stage.
final_rows <- do.call(
  rbind,
  lapply(
    split(
      trajectory,
      interaction(
        trajectory$season,
        trajectory$team,
        drop = TRUE
      )
    ),
    function(x) {
      x[
        which.max(
          x$stage_order
        ),
        ,
        drop = FALSE
      ]
    }
  )
)

pre_rows <- trajectory[
  trajectory$stage ==
    "REG_FINAL",
  ,
  drop = FALSE
]

pre_keep <- pre_rows[
  ,
  c(
    "season",
    "team",
    "rating",
    "offense_rating",
    "defense_rating",
    "master_pre_playoff_rating"
  ),
  drop = FALSE
]

names(pre_keep) <- c(
  "season",
  "team",
  "refit_regular_rating",
  "refit_regular_offense",
  "refit_regular_defense",
  "pre_playoff_rating"
)

final_keep <- final_rows[
  ,
  c(
    "season",
    "team",
    "stage",
    "rating",
    "offense_rating",
    "defense_rating"
  ),
  drop = FALSE
]

names(final_keep) <- c(
  "season",
  "team",
  "final_postseason_stage",
  "post_playoff_rating",
  "post_playoff_offense",
  "post_playoff_defense"
)

pre_post <- merge(
  pre_keep,
  final_keep,
  by = c("season", "team"),
  all = TRUE
)

pre_post$playoff_rating_change <- (
  pre_post$post_playoff_rating -
    pre_post$pre_playoff_rating
)

pre_post$playoff_offense_change <- (
  pre_post$post_playoff_offense -
    pre_post$refit_regular_offense
)

pre_post$playoff_defense_change <- (
  pre_post$post_playoff_defense -
    pre_post$refit_regular_defense
)

validation <- do.call(
  rbind,
  validation_storage
)

write_csv_base(
  pre_post,
  file.path(
    output_directory,
    "06_pre_vs_post_playoff_ratings.csv"
  )
)

write_csv_base(
  trajectory,
  file.path(
    output_directory,
    "07_playoff_rating_trajectories.csv"
  )
)

write_csv_base(
  validation,
  file.path(
    output_directory,
    "03_post_playoff_rating_validation.csv"
  )
)

cat("\nPostseason-inclusive ratings complete.\n")
cat("Rows in trajectory:", nrow(trajectory), "\n")
cat(
  "IMPORTANT: the original regular-season rating is preserved separately; ",
  "post-playoff ratings are a second postseason-inclusive view.\n",
  sep = ""
)

#!/usr/bin/env Rscript

# ============================================================
# 02 - FIT PLAYOFF WIN MODEL, PREDICT GAMES, RANK UPSETS
# ============================================================

source("00_config.R")
source(file.path("R", "playoff_helpers.R"))

games_file <- file.path(
  output_directory,
  "01_playoff_games_1970_2025.csv"
)

if (!file.exists(games_file)) {
  stop("Run 01_build_playoff_archive.R first.")
}

playoff_games <- read_csv_base(games_file)
master <- read_csv_base(master_ratings_file)

playoff_games$home_team <- normalize_team_code(playoff_games$home_team)
playoff_games$away_team <- normalize_team_code(playoff_games$away_team)
master$team <- normalize_team_code(master$team)

home_rating <- master[
  ,
  c(
    "season",
    "team",
    "rating",
    "offense_rating",
    "defense_rating",
    "estimated_HFA",
    "predictive_margin_sd"
  ),
  drop = FALSE
]

away_rating <- home_rating

names(home_rating) <- c(
  "season",
  "home_team",
  "home_pre_rating",
  "home_pre_offense",
  "home_pre_defense",
  "season_hfa",
  "season_predictive_margin_sd"
)

names(away_rating) <- c(
  "season",
  "away_team",
  "away_pre_rating",
  "away_pre_offense",
  "away_pre_defense",
  "away_season_hfa",
  "away_predictive_margin_sd"
)

games <- merge(
  playoff_games,
  home_rating,
  by = c("season", "home_team"),
  all.x = TRUE
)

games <- merge(
  games,
  away_rating,
  by = c("season", "away_team"),
  all.x = TRUE
)

games$rating_diff <- (
  games$home_pre_rating -
    games$away_pre_rating
)

games$home_field <- ifelse(
  games$neutral_site,
  0,
  1
)

games$expected_home_margin <- (
  games$rating_diff +
    ifelse(
      games$neutral_site,
      0,
      games$season_hfa
    )
)

games$margin_residual <- (
  games$home_margin -
    games$expected_home_margin
)

model_data <- games[
  games$completed &
    is.finite(games$expected_home_margin) &
    !is.na(games$home_win),
  ,
  drop = FALSE
]

if (nrow(model_data) < 50) {
  stop("Too few completed playoff games with pre-playoff ratings.")
}

# ------------------------------------------------------------
# Retrospective calibration:
# learn from all 1970-2025 postseason results.
# ------------------------------------------------------------

# Calibrate the regular-season model's expected point margin directly.
#
# IMPORTANT:
# There is intentionally NO intercept.
#
# expected_home_margin =
#   home regular-season rating
#   - away regular-season rating
#   + regular-season estimated HFA for non-neutral games
#
# Therefore:
#   expected_home_margin = 0  ->  P(home win) = 0.50
#
# This is especially important for Super Bowls, where neutral_site = TRUE
# and the nominal "home" designation should not create an artificial edge.
playoff_glm <- glm(
  home_win ~ 0 + expected_home_margin,
  data = model_data,
  family = binomial()
)

playoff_margin_slope <- unname(
  coef(
    playoff_glm
  )[["expected_home_margin"]]
)

if (
  !is.finite(
    playoff_margin_slope
  ) ||
  playoff_margin_slope <=
  0
) {
  
  stop(
    paste0(
      "Playoff calibration produced a non-positive expected-margin slope: ",
      playoff_margin_slope,
      ". Review the data before using playoff probabilities."
    )
  )
}

games$home_win_probability_retrospective <- as.numeric(
  predict(
    playoff_glm,
    newdata = games,
    type = "response"
  )
)

games$away_win_probability_retrospective <- (
  1 -
    games$home_win_probability_retrospective
)

# ------------------------------------------------------------
# Walk-forward calibration:
# season Y uses playoff results only from seasons < Y.
# ------------------------------------------------------------

games$home_win_probability_walkforward <- NA_real_

for (season in sort(unique(games$season))) {
  
  train <- model_data[
    model_data$season < season,
    ,
    drop = FALSE
  ]
  
  test_index <- which(
    games$season == season &
      is.finite(games$expected_home_margin)
  )
  
  if (
    nrow(train) <
    walkforward_min_games ||
    length(test_index) ==
    0
  ) {
    next
  }
  
  wf_model <- glm(
    home_win ~ 0 + expected_home_margin,
    data = train,
    family = binomial()
  )
  
  games$home_win_probability_walkforward[
    test_index
  ] <- as.numeric(
    predict(
      wf_model,
      newdata = games[test_index, , drop = FALSE],
      type = "response"
    )
  )
}

games$away_win_probability_walkforward <- (
  1 -
    games$home_win_probability_walkforward
)

# ------------------------------------------------------------
# Winner probability + upset ranking
# ------------------------------------------------------------

games$winner_probability_retrospective <- ifelse(
  games$winner ==
    games$home_team,
  games$home_win_probability_retrospective,
  games$away_win_probability_retrospective
)

games$winner_probability_walkforward <- ifelse(
  games$winner ==
    games$home_team,
  games$home_win_probability_walkforward,
  games$away_win_probability_walkforward
)

games$upset_retrospective <- (
  games$winner_probability_retrospective <
    0.5
)

games$upset_walkforward <- ifelse(
  is.na(games$winner_probability_walkforward),
  NA,
  games$winner_probability_walkforward <
    0.5
)

games$surprise_score_retrospective <- (
  -log(
    pmax(
      games$winner_probability_retrospective,
      1e-12
    )
  )
)

games$winner_pre_rating <- ifelse(
  games$winner ==
    games$home_team,
  games$home_pre_rating,
  games$away_pre_rating
)

games$loser_pre_rating <- ifelse(
  games$winner ==
    games$home_team,
  games$away_pre_rating,
  games$home_pre_rating
)

games$winner_rating_edge <- (
  games$winner_pre_rating -
    games$loser_pre_rating
)

games$winner_margin_vs_expectation <- ifelse(
  games$winner ==
    games$home_team,
  games$margin_residual,
  -games$margin_residual
)

upsets <- games[
  games$upset_retrospective,
  ,
  drop = FALSE
]

upsets <- upsets[
  order(
    upsets$winner_probability_retrospective,
    -upsets$surprise_score_retrospective
  ),
  ,
  drop = FALSE
]

upsets$historical_upset_rank <- seq_len(
  nrow(upsets)
)

# ------------------------------------------------------------
# Team postseason over/underperformance
# ------------------------------------------------------------

team_rows <- list()
k <- 1L

for (i in seq_len(nrow(games))) {
  
  g <- games[i, , drop = FALSE]
  
  team_rows[[k]] <- data.frame(
    season = g$season,
    game_id = g$game_id,
    round = g$round,
    team = g$home_team,
    opponent = g$away_team,
    site = ifelse(g$neutral_site, "Neutral", "Home"),
    pre_rating = g$home_pre_rating,
    expected_margin = g$expected_home_margin,
    actual_margin = g$home_margin,
    margin_vs_expectation = g$margin_residual,
    win_probability = g$home_win_probability_retrospective,
    won = as.integer(g$winner == g$home_team),
    stringsAsFactors = FALSE
  )
  k <- k + 1L
  
  team_rows[[k]] <- data.frame(
    season = g$season,
    game_id = g$game_id,
    round = g$round,
    team = g$away_team,
    opponent = g$home_team,
    site = ifelse(g$neutral_site, "Neutral", "Away"),
    pre_rating = g$away_pre_rating,
    expected_margin = -g$expected_home_margin,
    actual_margin = -g$home_margin,
    margin_vs_expectation = -g$margin_residual,
    win_probability = g$away_win_probability_retrospective,
    won = as.integer(g$winner == g$away_team),
    stringsAsFactors = FALSE
  )
  k <- k + 1L
}

team_game_performance <- do.call(
  rbind,
  team_rows
)

keys <- unique(
  team_game_performance[
    ,
    c(
      "season",
      "team"
    ),
    drop = FALSE
  ]
)

performance_rows <- list()

for (i in seq_len(nrow(keys))) {
  
  season <- keys$season[i]
  team <- keys$team[i]
  
  x <- team_game_performance[
    team_game_performance$season == season &
      team_game_performance$team == team,
    ,
    drop = FALSE
  ]
  
  performance_rows[[i]] <- data.frame(
    season = season,
    team = team,
    playoff_games = nrow(x),
    playoff_wins = sum(x$won),
    total_margin_vs_expectation = sum(
      x$margin_vs_expectation,
      na.rm = TRUE
    ),
    average_margin_vs_expectation = mean(
      x$margin_vs_expectation,
      na.rm = TRUE
    ),
    average_pregame_win_probability = mean(
      x$win_probability,
      na.rm = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

playoff_overperformance <- do.call(
  rbind,
  performance_rows
)

playoff_overperformance <- playoff_overperformance[
  order(
    -playoff_overperformance$total_margin_vs_expectation
  ),
  ,
  drop = FALSE
]

playoff_overperformance$overperformance_rank <- seq_len(
  nrow(
    playoff_overperformance
  )
)

# ------------------------------------------------------------
# Model coefficients + basic validation
# ------------------------------------------------------------

coef_table <- data.frame(
  term = names(coef(playoff_glm)),
  estimate = as.numeric(coef(playoff_glm)),
  stringsAsFactors = FALSE
)

games$predicted_home_win <- (
  games$home_win_probability_retrospective >=
    0.5
)

validation <- data.frame(
  metric = c(
    "postseason_games",
    "home_win_rate",
    "playoff_expected_margin_slope",
    "equal_team_neutral_probability",
    "retrospective_brier",
    "retrospective_log_loss",
    "retrospective_accuracy"
  ),
  value = c(
    nrow(model_data),
    mean(model_data$home_win),
    playoff_margin_slope,
    plogis(0),
    mean(
      (
        games$home_win_probability_retrospective -
          games$home_win
      )^2,
      na.rm = TRUE
    ),
    -mean(
      games$home_win *
        log(
          pmax(
            games$home_win_probability_retrospective,
            1e-12
          )
        ) +
        (
          1 -
            games$home_win
        ) *
        log(
          pmax(
            1 -
              games$home_win_probability_retrospective,
            1e-12
          )
        ),
      na.rm = TRUE
    ),
    mean(
      games$predicted_home_win ==
        as.logical(
          games$home_win
        ),
      na.rm = TRUE
    )
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Outputs
# ------------------------------------------------------------

write_csv_base(
  games,
  file.path(
    output_directory,
    "04_playoff_games_with_predictions.csv"
  )
)

write_csv_base(
  upsets,
  file.path(
    output_directory,
    "05_playoff_upsets_ranked.csv"
  )
)

write_csv_base(
  team_game_performance,
  file.path(
    output_directory,
    "10_playoff_team_game_performance.csv"
  )
)

write_csv_base(
  playoff_overperformance,
  file.path(
    output_directory,
    "10_playoff_overperformance.csv"
  )
)

write_csv_base(
  coef_table,
  file.path(
    output_directory,
    "playoff_probability_model_coefficients.csv"
  )
)

write_csv_base(
  validation,
  file.path(
    output_directory,
    "09_playoff_probability_model_validation.csv"
  )
)

saveRDS(
  playoff_glm,
  file.path(
    output_directory,
    "playoff_probability_model.rds"
  )
)

cat("\nPlayoff probability model complete.\n")
cat(
  "Calibration: home_win ~ 0 + expected_home_margin\n"
)
cat(
  "Equal-strength neutral-site probability: ",
  sprintf("%.3f", plogis(0)),
  "\n",
  sep = ""
)
print(summary(playoff_glm))
cat("\nTop 10 retrospective upsets:\n")
print(
  head(
    upsets[
      ,
      c(
        "season",
        "round",
        "winner",
        "loser",
        "winner_probability_retrospective",
        "winner_rating_edge"
      ),
      drop = FALSE
    ],
    10
  )
)

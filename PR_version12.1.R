# ============================================================
# FOOTBALL POWER RATINGS - VERSION 12.1
# NFL PRODUCTION POWER RATINGS + FORECASTS + SOS + SEASON CSV ARCHIVE
# ============================================================
#
# PURPOSE
# -------
# Version 12 is the production NFL system built from the research
# completed in Versions 9A-H through 11B.
#
# It separates three concepts:
#
#   1. PUBLISHED POWER RATING
#      Raw Score Offense/Defense Huber + HL12 estimate.
#
#   2. POINT FORECAST
#      Raw rating margin is shrunk toward a generic HFA-only
#      baseline using REAL-NFL historical weights learned from
#      prior seasons.
#
#   3. WIN PROBABILITY
#      Uses the fixed early-season blend plus an NFL-only logistic
#      calibrator learned from prior seasons.
#
# The published power ratings themselves are NEVER shrunk.
#
# ------------------------------------------------------------
# FINALIZED POWER-RATING ENGINE
# ------------------------------------------------------------
#
#   Score Offense/Defense Huber
#   component lambda = 2
#   Huber k = 1.5
#   recency half-life = 12 weeks
#   HFA prior center = 2.5 points
#
# Overall Rating = Offense Rating + Defense Rating
#
# Interpretation:
#   neutral-field points above/below league average.
#
# ------------------------------------------------------------
# FORECAST SHRINKAGE
# ------------------------------------------------------------
#
# Historical OOS predictions from calibration seasons are used
# to learn one rating-forecast weight for each games-played bucket:
#
#   1 game/team
#   2 games/team
#   3 games/team
#   4 games/team
#   5+ games/team
#
# Forecast Margin =
#
#   Baseline Margin
#   + learned_weight *
#       (Raw Rating Margin - Baseline Margin)
#
# where:
#
#   Baseline Margin = 2.5 for a normal home game
#   Baseline Margin = 0 for a neutral-site game
#
# ------------------------------------------------------------
# WIN PROBABILITY
# ------------------------------------------------------------
#
# Version 11B found that the best probability forecast used:
#
#   fixed blend weight = min(1, games_per_team / 4)
#
# followed by an NFL-only logistic calibration.
#
# Probability Margin =
#
#   Baseline Margin
#   + fixed_weight *
#       (Raw Rating Margin - Baseline Margin)
#
# z = Probability Margin / Predictive Margin SD
#
# P(Home Win) = logistic(intercept + slope * z)
#
# Intercept and slope are fit ONLY on completed PRIOR seasons.
# Current-season outcomes never calibrate current-season forecasts.
#
# ------------------------------------------------------------
# STRENGTH OF SCHEDULE
# ------------------------------------------------------------
#
# Version 12 restores:
#
#   played_SOS
#   remaining_SOS
#   full_SOS
#
# and location-adjusted:
#
#   played_location_difficulty
#   remaining_location_difficulty
#   full_location_difficulty
#
# SOS is a REPORTING METRIC only.
# It is NOT fed back into the rating equation.
#
# Location-adjusted difficulty for team i vs opponent j:
#
#   home:    opponent rating - estimated HFA
#   away:    opponent rating + estimated HFA
#   neutral: opponent rating
#
# ------------------------------------------------------------
# DATA
# ------------------------------------------------------------
#
# Uses:
#
#   nflreadr::load_schedules()
#
# Full regular-season schedule is retained, including future games.
#
# Default:
#
#   historical calibration: 2005 through prior season
#   production season:       2026
#
# ------------------------------------------------------------
# OUTPUTS
# ------------------------------------------------------------
#
# version12_current_power_rankings.csv
# version12_current_weekly_power_history.csv
# version12_next_week_predictions.csv
# version12_all_remaining_predictions.csv
# version12_current_season_oos_predictions.csv
# version12_current_season_oos_summary.csv
# version12_forecast_shrinkage_weights.csv
# version12_probability_calibration_coefficients.csv
# version12_historical_calibration_summary.csv
# version12_historical_oos_predictions.csv
# version12_1_model_snapshot.csv
#
# NEW IN VERSION 12.1:
#
# version12_1_season_team_summaries/
#   version12_1_1999_team_summary.csv
#   version12_1_2000_team_summary.csv
#   ...
#   version12_1_2025_team_summary.csv
#   version12_1_2026_team_summary_through_week_<N>.csv
#
# version12_1_all_season_team_summaries.csv
#
# plus PNG plots.
#
# ============================================================


# ============================================================
# 1. PACKAGES + USER SETTINGS
# ============================================================

if (
  !requireNamespace(
    "nflreadr",
    quietly = TRUE
  )
) {
  
  stop(
    "Package 'nflreadr' is required. Install with install.packages('nflreadr')."
  )
}

if (
  !requireNamespace(
    "ggplot2",
    quietly = TRUE
  )
) {
  
  stop(
    "Package 'ggplot2' is required. Install with install.packages('ggplot2')."
  )
}

library(nflreadr)
library(ggplot2)

# ------------------------------------------------------------
# Production season
# ------------------------------------------------------------

production_season <- 2026

# First season used to learn real-NFL forecast calibration.
calibration_start_season <- 1999

# Prior seasons only.
calibration_end_season <- production_season - 1

# NULL:
#   automatically use the latest FULLY completed regular-season week.
#
# Numeric:
#   historical replay through that week.
#
# Example:
#   as_of_week <- 8
#
as_of_week <- NULL

output_prefix <- "version12_1"

# ------------------------------------------------------------
# Finalized power-rating constants
# ------------------------------------------------------------

component_lambda <- 2
huber_k <- 1.5
recency_half_life <- 12

hfa_prior <- 2.5

max_huber_iterations <- 40
huber_tolerance <- 1e-6

# ------------------------------------------------------------
# Predictive margin uncertainty
# ------------------------------------------------------------

generic_margin_noise_prior_sd <- 8.5
margin_noise_prior_weight <- 20

# ------------------------------------------------------------
# Generic results-free forecast baseline
# ------------------------------------------------------------

generic_hfa_baseline <- 2.5

probability_epsilon <- 1e-6

# Synthetic calibration constants remain defined only because some
# inherited helper functions reference them. Version 12 production
# probabilities do NOT use these coefficients.
win_probability_intercept <- 0.0192183
win_probability_slope <- 1.440608



# 2. HOME-FIELD SHRINKAGE
# ============================================================

get_hfa_lambda <- function(
    data,
    league_teams
) {
  
  average_games_played <- (
    2 *
      nrow(
        data
      )
  ) /
    length(
      league_teams
    )
  
  if (
    average_games_played <=
    2
  ) {
    
    return(10)
    
  } else if (
    average_games_played <=
    4
  ) {
    
    return(8)
    
  } else if (
    average_games_played <=
    6
  ) {
    
    return(5)
    
  } else {
    
    return(2)
  }
}


# ============================================================
# 3. RECENCY WEIGHTS
# ============================================================

make_recency_weights <- function(
    training_games,
    half_life =
      recency_half_life
) {
  
  if (
    nrow(
      training_games
    ) ==
    0
  ) {
    
    return(
      numeric(0)
    )
  }
  
  if (
    is.infinite(
      half_life
    )
  ) {
    
    return(
      rep(
        1,
        nrow(
          training_games
        )
      )
    )
  }
  
  reference_week <- max(
    training_games$week
  )
  
  weeks_old <- (
    reference_week -
      training_games$week
  )
  
  raw_weights <- 0.5^(
    weeks_old /
      half_life
  )
  
  raw_weights / mean(
    raw_weights
  )
}


# ============================================================
# 4. BASE SCORE DESIGN (OVERRIDDEN BELOW FOR NEUTRAL SITES)
# ============================================================

build_score_design <- function(
    data,
    league_teams
) {
  
  n_games <- nrow(
    data
  )
  
  n_teams <- length(
    league_teams
  )
  
  offense_columns <- paste0(
    "O__",
    league_teams
  )
  
  defense_columns <- paste0(
    "D__",
    league_teams
  )
  
  X <- matrix(
    0,
    nrow =
      2 *
      n_games,
    ncol =
      2 +
      2 *
      n_teams
  )
  
  colnames(X) <- c(
    "Intercept",
    "HFA",
    offense_columns,
    defense_columns
  )
  
  y <- numeric(
    2 *
      n_games
  )
  
  for (
    i in
    seq_len(
      n_games
    )
  ) {
    
    home_row <- 2 * i - 1
    away_row <- 2 * i
    
    home <- data$home_team[i]
    away <- data$away_team[i]
    
    X[home_row, "Intercept"] <- 1
    X[home_row, "HFA"] <- 0.5
    X[home_row, paste0("O__", home)] <- 1
    X[home_row, paste0("D__", away)] <- -1
    
    X[away_row, "Intercept"] <- 1
    X[away_row, "HFA"] <- -0.5
    X[away_row, paste0("O__", away)] <- 1
    X[away_row, paste0("D__", home)] <- -1
    
    y[home_row] <- data$home_score[i]
    y[away_row] <- data$away_score[i]
  }
  
  list(
    X = X,
    y = y,
    offense_columns =
      offense_columns,
    defense_columns =
      defense_columns
  )
}


# ============================================================
# 5. WEIGHTED RIDGE SOLVER
# ============================================================

solve_weighted_ridge <- function(
    X,
    y,
    observation_weights,
    penalty,
    prior_target
) {
  
  sqrt_weights <- sqrt(
    observation_weights
  )
  
  X_weighted <- sweep(
    X,
    MARGIN = 1,
    STATS =
      sqrt_weights,
    FUN = "*"
  )
  
  y_weighted <- (
    y *
      sqrt_weights
  )
  
  beta_matrix <- solve(
    crossprod(
      X_weighted
    ) +
      penalty,
    crossprod(
      X_weighted,
      y_weighted
    ) +
      penalty %*%
      prior_target
  )
  
  beta <- as.numeric(
    beta_matrix
  )
  
  names(beta) <- colnames(
    X
  )
  
  beta
}


# ============================================================
# 6. HUBER OFFENSE / DEFENSE MODEL
# ============================================================

fit_score_offdef_huber <- function(
    data,
    league_teams,
    component_lambda,
    huber_k,
    hfa_lambda,
    hfa_prior,
    game_weights,
    max_iterations =
      max_huber_iterations,
    tolerance =
      huber_tolerance
) {
  
  design <- build_score_design(
    data =
      data,
    league_teams =
      league_teams
  )
  
  X <- design$X
  y <- design$y
  
  n_teams <- length(
    league_teams
  )
  
  base_weights <- rep(
    game_weights,
    each = 2
  )
  
  penalty <- diag(
    c(
      0,
      hfa_lambda,
      rep(
        component_lambda,
        2 *
          n_teams
      )
    )
  )
  
  prior_target <- c(
    0,
    hfa_prior,
    rep(
      0,
      2 *
        n_teams
    )
  )
  
  beta <- solve_weighted_ridge(
    X =
      X,
    y =
      y,
    observation_weights =
      base_weights,
    penalty =
      penalty,
    prior_target =
      prior_target
  )
  
  converged <- FALSE
  iterations_used <- 0
  
  for (
    iteration in
    seq_len(
      max_iterations
    )
  ) {
    
    iterations_used <- iteration
    
    residuals <- as.numeric(
      y -
        X %*%
        beta
    )
    
    residual_median <- median(
      residuals
    )
    
    robust_scale <- (
      1.4826 *
        median(
          abs(
            residuals -
              residual_median
          )
        )
    )
    
    if (
      !is.finite(
        robust_scale
      ) ||
      robust_scale <
      1e-8
    ) {
      
      robust_scale <- sd(
        residuals
      )
    }
    
    if (
      !is.finite(
        robust_scale
      ) ||
      robust_scale <
      1e-8
    ) {
      
      robust_scale <- 1
    }
    
    standardized_absolute_residual <- (
      abs(
        residuals
      ) /
        robust_scale
    )
    
    huber_weights <- rep(
      1,
      length(
        residuals
      )
    )
    
    large_residual <- (
      standardized_absolute_residual >
        huber_k
    )
    
    huber_weights[
      large_residual
    ] <- (
      huber_k /
        standardized_absolute_residual[
          large_residual
        ]
    )
    
    combined_weights <- (
      base_weights *
        huber_weights
    )
    
    new_beta <- solve_weighted_ridge(
      X =
        X,
      y =
        y,
      observation_weights =
        combined_weights,
      penalty =
        penalty,
      prior_target =
        prior_target
    )
    
    maximum_change <- max(
      abs(
        new_beta -
          beta
      )
    )
    
    beta <- new_beta
    
    if (
      maximum_change <
      tolerance
    ) {
      
      converged <- TRUE
      break
    }
  }
  
  offense <- beta[
    design$offense_columns
  ]
  
  defense <- beta[
    design$defense_columns
  ]
  
  names(
    offense
  ) <- league_teams
  
  names(
    defense
  ) <- league_teams
  
  list(
    offense =
      offense,
    defense =
      defense,
    hfa =
      as.numeric(
        beta["HFA"]
      ),
    league_mean =
      as.numeric(
        beta["Intercept"]
      ),
    converged =
      converged,
    iterations =
      iterations_used
  )
}


# ============================================================
# 7. CENTER COMPONENTS
# ============================================================

center_rating_components <- function(
    fit,
    league_teams
) {
  
  offense_rating <- fit$offense[
    league_teams
  ]
  
  defense_rating <- fit$defense[
    league_teams
  ]
  
  offense_mean <- mean(
    offense_rating
  )
  
  defense_mean <- mean(
    defense_rating
  )
  
  offense_rating <- (
    offense_rating -
      offense_mean
  )
  
  defense_rating <- (
    defense_rating -
      defense_mean
  )
  
  rating <- (
    offense_rating +
      defense_rating
  )
  
  rating <- (
    rating -
      mean(
        rating
      )
  )
  
  adjusted_league_mean_points <- (
    fit$league_mean +
      offense_mean -
      defense_mean
  )
  
  names(
    offense_rating
  ) <- league_teams
  
  names(
    defense_rating
  ) <- league_teams
  
  names(
    rating
  ) <- league_teams
  
  list(
    offense_rating =
      offense_rating,
    defense_rating =
      defense_rating,
    rating =
      rating,
    league_mean_points =
      adjusted_league_mean_points
  )
}


# ============================================================
# 8. FIT FINAL POWER MODEL
# ============================================================

fit_power_model <- function(
    training_games,
    league_teams
) {
  
  if (
    nrow(
      training_games
    ) ==
    0
  ) {
    
    stop(
      "Cannot fit the finalized team model with zero completed games."
    )
  }
  
  hfa_lambda <- get_hfa_lambda(
    data =
      training_games,
    league_teams =
      league_teams
  )
  
  game_weights <- make_recency_weights(
    training_games =
      training_games,
    half_life =
      recency_half_life
  )
  
  raw_fit <- fit_score_offdef_huber(
    data =
      training_games,
    league_teams =
      league_teams,
    component_lambda =
      component_lambda,
    huber_k =
      huber_k,
    hfa_lambda =
      hfa_lambda,
    hfa_prior =
      hfa_prior,
    game_weights =
      game_weights
  )
  
  centered <- center_rating_components(
    fit =
      raw_fit,
    league_teams =
      league_teams
  )
  
  list(
    ratings =
      centered$rating,
    offense_rating =
      centered$offense_rating,
    defense_rating =
      centered$defense_rating,
    hfa =
      raw_fit$hfa,
    league_mean_points =
      centered$league_mean_points,
    hfa_lambda =
      hfa_lambda,
    component_lambda =
      component_lambda,
    huber_k =
      huber_k,
    recency_half_life =
      recency_half_life,
    converged =
      raw_fit$converged,
    iterations =
      raw_fit$iterations
  )
}


# ============================================================

# 10. BASE GAME PREDICTOR (OVERRIDDEN BELOW FOR NEUTRAL SITES)
# ============================================================

predict_games_from_model <- function(
    model,
    target_schedule
) {
  
  if (
    nrow(
      target_schedule
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  home_team <- target_schedule$home_team
  away_team <- target_schedule$away_team
  
  expected_home_score <- (
    model$league_mean_points +
      model$offense_rating[
        home_team
      ] -
      model$defense_rating[
        away_team
      ] +
      model$hfa /
      2
  )
  
  expected_away_score <- (
    model$league_mean_points +
      model$offense_rating[
        away_team
      ] -
      model$defense_rating[
        home_team
      ] -
      model$hfa /
      2
  )
  
  expected_margin <- (
    expected_home_score -
      expected_away_score
  )
  
  rating_identity_margin <- (
    model$ratings[
      home_team
    ] -
      model$ratings[
        away_team
      ] +
      model$hfa
  )
  
  if (
    max(
      abs(
        expected_margin -
        rating_identity_margin
      )
    ) >
    1e-7
  ) {
    
    stop(
      "Expected-score and rating-margin identities do not agree."
    )
  }
  
  output <- target_schedule
  
  output$predicted_home_score <-
    as.numeric(
      expected_home_score
    )
  
  output$predicted_away_score <-
    as.numeric(
      expected_away_score
    )
  
  output$predicted_margin <-
    as.numeric(
      expected_margin
    )
  
  output$predicted_total <- (
    output$predicted_home_score +
      output$predicted_away_score
  )
  
  output$pregame_home_rating <-
    as.numeric(
      model$ratings[
        home_team
      ]
    )
  
  output$pregame_away_rating <-
    as.numeric(
      model$ratings[
        away_team
      ]
    )
  
  output$pregame_home_offense <-
    as.numeric(
      model$offense_rating[
        home_team
      ]
    )
  
  output$pregame_away_offense <-
    as.numeric(
      model$offense_rating[
        away_team
      ]
    )
  
  output$pregame_home_defense <-
    as.numeric(
      model$defense_rating[
        home_team
      ]
    )
  
  output$pregame_away_defense <-
    as.numeric(
      model$defense_rating[
        away_team
      ]
    )
  
  output$pregame_estimated_HFA <-
    model$hfa
  
  output$pregame_league_mean_points <-
    model$league_mean_points
  
  output
}


# ============================================================
# 11. PREDICTIVE MARGIN SD
# ============================================================

calculate_training_margin_residuals <- function(
    model,
    training_games
) {
  
  fitted_games <- predict_games_from_model(
    model =
      model,
    target_schedule =
      training_games
  )
  
  (
    training_games$actual_margin -
      fitted_games$predicted_margin
  )
}


estimate_predictive_margin_sd <- function(
    model,
    training_games
) {
  
  residuals <- calculate_training_margin_residuals(
    model =
      model,
    training_games =
      training_games
  )
  
  n_residuals <- length(
    residuals
  )
  
  if (
    n_residuals <=
    1
  ) {
    
    return(
      generic_margin_noise_prior_sd
    )
  }
  
  empirical_variance <- mean(
    residuals^2
  )
  
  shrunk_variance <- (
    margin_noise_prior_weight *
      generic_margin_noise_prior_sd^2 +
      n_residuals *
      empirical_variance
  ) /
    (
      margin_noise_prior_weight +
        n_residuals
    )
  
  sqrt(
    shrunk_variance
  )
}


# ============================================================
# 12. EMPIRICAL-LOGISTIC WIN PROBABILITY
# ============================================================

clip_probability <- function(
    probability
) {
  
  pmin(
    pmax(
      probability,
      probability_epsilon
    ),
    1 -
      probability_epsilon
  )
}


margin_to_home_win_probability <- function(
    predicted_margin,
    predictive_margin_sd
) {
  
  standardized_margin <- (
    predicted_margin /
      predictive_margin_sd
  )
  
  linear_predictor <- (
    win_probability_intercept +
      win_probability_slope *
      standardized_margin
  )
  
  clip_probability(
    plogis(
      linear_predictor
    )
  )
}


# ============================================================
# 13. ADD WIN PROBABILITIES
# ============================================================

add_probability_forecasts <- function(
    predictions,
    predictive_margin_sd
) {
  
  if (
    nrow(
      predictions
    ) ==
    0
  ) {
    
    return(
      predictions
    )
  }
  
  predictions$predictive_margin_sd <-
    predictive_margin_sd
  
  predictions$standardized_predicted_margin <- (
    predictions$predicted_margin /
      predictive_margin_sd
  )
  
  predictions$home_win_probability <-
    margin_to_home_win_probability(
      predicted_margin =
        predictions$predicted_margin,
      predictive_margin_sd =
        predictive_margin_sd
    )
  
  predictions$away_win_probability <- (
    1 -
      predictions$home_win_probability
  )
  
  predictions$predicted_winner <- ifelse(
    predictions$home_win_probability >=
      0.5,
    predictions$home_team,
    predictions$away_team
  )
  
  predictions$predicted_winner_probability <- pmax(
    predictions$home_win_probability,
    predictions$away_win_probability
  )
  
  predictions$probability_calibration_intercept <-
    win_probability_intercept
  
  predictions$probability_calibration_slope <-
    win_probability_slope
  
  predictions
}


# ============================================================


# ============================================================
# 9. NEUTRAL-SITE SCORE DESIGN OVERRIDE
# ============================================================

build_score_design <- function(data, league_teams) {
  n_games <- nrow(data)
  n_teams <- length(league_teams)
  offense_columns <- paste0("O__", league_teams)
  defense_columns <- paste0("D__", league_teams)
  X <- matrix(0, nrow = 2 * n_games, ncol = 2 + 2 * n_teams)
  colnames(X) <- c("Intercept", "HFA", offense_columns, defense_columns)
  y <- numeric(2 * n_games)
  
  for (i in seq_len(n_games)) {
    home_row <- 2 * i - 1
    away_row <- 2 * i
    home <- data$home_team[i]
    away <- data$away_team[i]
    hfa_half <- if (isTRUE(data$neutral_site[i])) 0 else 0.5
    
    X[home_row, "Intercept"] <- 1
    X[home_row, "HFA"] <- hfa_half
    X[home_row, paste0("O__", home)] <- 1
    X[home_row, paste0("D__", away)] <- -1
    
    X[away_row, "Intercept"] <- 1
    X[away_row, "HFA"] <- -hfa_half
    X[away_row, paste0("O__", away)] <- 1
    X[away_row, paste0("D__", home)] <- -1
    
    y[home_row] <- data$home_score[i]
    y[away_row] <- data$away_score[i]
  }
  
  list(X = X, y = y, offense_columns = offense_columns,
       defense_columns = defense_columns)
}

# ============================================================
# 14. NEUTRAL-SITE GAME PREDICTOR OVERRIDE
# ============================================================

predict_games_from_model <- function(model, target_schedule) {
  if (nrow(target_schedule) == 0) return(data.frame())
  
  home_team <- target_schedule$home_team
  away_team <- target_schedule$away_team
  hfa_effect <- ifelse(target_schedule$neutral_site, 0, model$hfa)
  
  expected_home_score <- model$league_mean_points +
    model$offense_rating[home_team] - model$defense_rating[away_team] +
    hfa_effect / 2
  
  expected_away_score <- model$league_mean_points +
    model$offense_rating[away_team] - model$defense_rating[home_team] -
    hfa_effect / 2
  
  expected_margin <- expected_home_score - expected_away_score
  rating_margin <- model$ratings[home_team] - model$ratings[away_team] + hfa_effect
  if (max(abs(expected_margin - rating_margin)) > 1e-7) {
    stop("Expected-score margin and rating-margin identity disagree.")
  }
  
  output <- target_schedule
  output$predicted_home_score <- as.numeric(expected_home_score)
  output$predicted_away_score <- as.numeric(expected_away_score)
  output$predicted_margin <- as.numeric(expected_margin)
  output$predicted_total <- output$predicted_home_score + output$predicted_away_score
  output$pregame_home_rating <- as.numeric(model$ratings[home_team])
  output$pregame_away_rating <- as.numeric(model$ratings[away_team])
  output$pregame_home_offense <- as.numeric(model$offense_rating[home_team])
  output$pregame_away_offense <- as.numeric(model$offense_rating[away_team])
  output$pregame_home_defense <- as.numeric(model$defense_rating[home_team])
  output$pregame_away_defense <- as.numeric(model$defense_rating[away_team])
  output$pregame_estimated_HFA <- model$hfa
  output$pregame_HFA_effect <- hfa_effect
  output$pregame_league_mean_points <- model$league_mean_points
  output
}

# ============================================================


# ============================================================
# 15. GENERAL HELPERS
# ============================================================

clip_probability <- function(
    probability
) {
  
  pmin(
    pmax(
      probability,
      probability_epsilon
    ),
    1 -
      probability_epsilon
  )
}


prepare_schedule_rows <- function(
    schedule_data
) {
  
  output <- as.data.frame(
    schedule_data,
    stringsAsFactors = FALSE
  )
  
  required_columns <- c(
    "game_id",
    "season",
    "game_type",
    "week",
    "home_team",
    "away_team",
    "home_score",
    "away_score"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(
      output
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
        "load_schedules() output is missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
  }
  
  output$season <- as.integer(
    output$season
  )
  
  output$week <- as.integer(
    output$week
  )
  
  output$home_team <- as.character(
    output$home_team
  )
  
  output$away_team <- as.character(
    output$away_team
  )
  
  if (
    "location" %in%
    names(
      output
    )
  ) {
    
    output$neutral_site <- (
      tolower(
        as.character(
          output$location
        )
      ) ==
        "neutral"
    )
    
    output$neutral_site[
      is.na(
        output$neutral_site
      )
    ] <- FALSE
    
  } else {
    
    warning(
      "No location column found. All games will be treated as non-neutral."
    )
    
    output$neutral_site <- FALSE
  }
  
  optional_numeric <- c(
    "spread_line",
    "total_line",
    "home_moneyline",
    "away_moneyline"
  )
  
  for (
    column_name in
    optional_numeric
  ) {
    
    if (
      !column_name %in%
      names(
        output
      )
    ) {
      
      output[[column_name]] <- NA_real_
    }
  }
  
  output$completed <- (
    !is.na(
      output$home_score
    ) &
      !is.na(
        output$away_score
      )
  )
  
  output$actual_margin <- ifelse(
    output$completed,
    output$home_score -
      output$away_score,
    NA_real_
  )
  
  output$actual_total <- ifelse(
    output$completed,
    output$home_score +
      output$away_score,
    NA_real_
  )
  
  output <- output[
    order(
      output$season,
      output$week,
      output$game_id
    ),
  ]
  
  rownames(
    output
  ) <- NULL
  
  output
}


get_games_played_bucket <- function(
    average_games_per_team
) {
  
  bucket <- round(
    average_games_per_team
  )
  
  bucket <- pmax(
    bucket,
    1
  )
  
  bucket <- pmin(
    bucket,
    5
  )
  
  as.integer(
    bucket
  )
}


get_fixed_blend_weight <- function(
    average_games_per_team
) {
  
  pmin(
    1,
    pmax(
      0,
      average_games_per_team /
        4
    )
  )
}


# ============================================================
# 16. LOAD FULL NFL SCHEDULES
# ============================================================

cat("\n========================================\n")
cat("VERSION 12 - LOAD NFL SCHEDULES\n")
cat("========================================\n\n")

seasons_to_load <- calibration_start_season:production_season

cat(
  "Loading",
  min(
    seasons_to_load
  ),
  "through",
  max(
    seasons_to_load
  ),
  "...\n"
)

raw_schedule <- nflreadr::load_schedules(
  seasons =
    seasons_to_load
)

full_schedule <- prepare_schedule_rows(
  raw_schedule
)

full_schedule <- full_schedule[
  full_schedule$game_type ==
    "REG",
]

rownames(
  full_schedule
) <- NULL

historical_completed_games <- full_schedule[
  full_schedule$season >=
    calibration_start_season &
    full_schedule$season <=
    calibration_end_season &
    full_schedule$completed,
]

production_schedule <- full_schedule[
  full_schedule$season ==
    production_season,
]

if (
  nrow(
    production_schedule
  ) ==
  0
) {
  
  stop(
    paste0(
      "No regular-season schedule rows found for production season ",
      production_season,
      "."
    )
  )
}

cat(
  "Historical completed calibration games:",
  nrow(
    historical_completed_games
  ),
  "\n"
)

cat(
  "Production-season scheduled games:",
  nrow(
    production_schedule
  ),
  "\n\n"
)


# ============================================================
# 17. HISTORICAL OOS BACKTEST FOR CALIBRATION
# ============================================================

generate_historical_oos_predictions <- function(
    historical_games
) {
  
  seasons <- sort(
    unique(
      historical_games$season
    )
  )
  
  prediction_storage <- list()
  prediction_position <- 1
  
  for (
    current_season in
    seasons
  ) {
    
    cat(
      "Historical calibration backtest:",
      current_season,
      "\n"
    )
    
    season_games <- historical_games[
      historical_games$season ==
        current_season,
    ]
    
    league_teams <- sort(
      unique(
        c(
          season_games$home_team,
          season_games$away_team
        )
      )
    )
    
    active_weeks <- sort(
      unique(
        season_games$week
      )
    )
    
    for (
      target_week in
      active_weeks
    ) {
      
      training_games <- season_games[
        season_games$week <
          target_week,
      ]
      
      if (
        nrow(
          training_games
        ) ==
        0
      ) {
        
        next
      }
      
      target_games <- season_games[
        season_games$week ==
          target_week,
      ]
      
      if (
        nrow(
          target_games
        ) ==
        0
      ) {
        
        next
      }
      
      model <- fit_power_model(
        training_games =
          training_games,
        league_teams =
          league_teams
      )
      
      predictive_margin_sd <-
        estimate_predictive_margin_sd(
          model =
            model,
          training_games =
            training_games
        )
      
      predictions <- predict_games_from_model(
        model =
          model,
        target_schedule =
          target_games
      )
      
      average_games_per_team_before_week <- (
        2 *
          nrow(
            training_games
          )
      ) /
        length(
          league_teams
        )
      
      predictions$predictive_margin_sd <-
        predictive_margin_sd
      
      predictions$average_games_per_team_before_week <-
        average_games_per_team_before_week
      
      predictions$games_played_bucket <-
        get_games_played_bucket(
          average_games_per_team_before_week
        )
      
      predictions$baseline_margin <- ifelse(
        predictions$neutral_site,
        0,
        generic_hfa_baseline
      )
      
      predictions$actual_home_win_soft <- ifelse(
        predictions$actual_margin >
          0,
        1,
        ifelse(
          predictions$actual_margin <
            0,
          0,
          0.5
        )
      )
      
      predictions$actual_home_win_non_tie <- ifelse(
        predictions$actual_margin >
          0,
        1,
        ifelse(
          predictions$actual_margin <
            0,
          0,
          NA_real_
        )
      )
      
      prediction_storage[[prediction_position]] <-
        predictions[
          ,
          c(
            "season",
            "week",
            "game_id",
            "home_team",
            "away_team",
            "neutral_site",
            "actual_margin",
            "actual_total",
            "predicted_home_score",
            "predicted_away_score",
            "predicted_margin",
            "predicted_total",
            "predictive_margin_sd",
            "average_games_per_team_before_week",
            "games_played_bucket",
            "baseline_margin",
            "actual_home_win_soft",
            "actual_home_win_non_tie",
            "spread_line",
            "home_moneyline",
            "away_moneyline"
          )
        ]
      
      prediction_position <-
        prediction_position +
        1
    }
  }
  
  output <- do.call(
    rbind,
    prediction_storage
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


historical_oos_predictions <-
  generate_historical_oos_predictions(
    historical_games =
      historical_completed_games
  )


# ============================================================
# 18. LEARN REAL-NFL FORECAST SHRINKAGE
# ============================================================

learn_forecast_shrinkage_weights <- function(
    calibration_data
) {
  
  rows <- list()
  
  for (
    bucket in
    1:5
  ) {
    
    current <- calibration_data[
      calibration_data$games_played_bucket ==
        bucket,
    ]
    
    fallback_weight <- get_fixed_blend_weight(
      if (
        bucket <
        5
      ) {
        bucket
      } else {
        5
      }
    )
    
    if (
      nrow(
        current
      ) ==
      0
    ) {
      
      learned_weight <- fallback_weight
      
    } else {
      
      delta <- (
        current$predicted_margin -
          current$baseline_margin
      )
      
      target <- (
        current$actual_margin -
          current$baseline_margin
      )
      
      denominator <- sum(
        delta^2
      )
      
      if (
        !is.finite(
          denominator
        ) ||
        denominator <=
        1e-12
      ) {
        
        learned_weight <- fallback_weight
        
      } else {
        
        learned_weight <- (
          sum(
            delta *
              target
          ) /
            denominator
        )
        
        learned_weight <- pmin(
          1,
          pmax(
            0,
            learned_weight
          )
        )
      }
    }
    
    rows[[bucket]] <- data.frame(
      games_played_bucket =
        bucket,
      calibration_games =
        nrow(
          current
        ),
      learned_forecast_weight =
        learned_weight,
      fixed_probability_weight =
        get_fixed_blend_weight(
          if (
            bucket <
            5
          ) {
            bucket
          } else {
            5
          }
        ),
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


forecast_shrinkage_weights <-
  learn_forecast_shrinkage_weights(
    historical_oos_predictions
  )


# ============================================================
# 19. FIT REAL-NFL WIN-PROBABILITY CALIBRATOR
# ============================================================
#
# Version 11B selected:
#
#   fixed blend -> NFL expanding logistic
#
# for Brier / log loss.
#
# For production season 2026, all 2005-2025 OOS games are valid
# prior-season calibration data.
#
# ============================================================

historical_probability_calibration <-
  historical_oos_predictions[
    !is.na(
      historical_oos_predictions$actual_home_win_non_tie
    ),
  ]

historical_probability_calibration$fixed_blend_weight <-
  get_fixed_blend_weight(
    historical_probability_calibration$average_games_per_team_before_week
  )

historical_probability_calibration$probability_margin <- (
  historical_probability_calibration$baseline_margin +
    historical_probability_calibration$fixed_blend_weight *
    (
      historical_probability_calibration$predicted_margin -
        historical_probability_calibration$baseline_margin
    )
)

historical_probability_calibration$z_probability_margin <- (
  historical_probability_calibration$probability_margin /
    historical_probability_calibration$predictive_margin_sd
)

nfl_probability_model <- glm(
  actual_home_win_non_tie ~
    z_probability_margin,
  family =
    binomial(),
  data =
    historical_probability_calibration
)

nfl_probability_coefficients <- coef(
  nfl_probability_model
)

nfl_probability_intercept <- as.numeric(
  nfl_probability_coefficients[1]
)

nfl_probability_slope <- as.numeric(
  nfl_probability_coefficients[2]
)

probability_calibration_coefficients <- data.frame(
  calibration_start_season =
    calibration_start_season,
  calibration_end_season =
    calibration_end_season,
  calibration_games =
    nrow(
      historical_probability_calibration
    ),
  intercept =
    nfl_probability_intercept,
  slope =
    nfl_probability_slope,
  stringsAsFactors =
    FALSE
)


# ============================================================
# 20. HISTORICAL CALIBRATION SUMMARY
# ============================================================

historical_oos_predictions$learned_forecast_weight <-
  forecast_shrinkage_weights$learned_forecast_weight[
    match(
      historical_oos_predictions$games_played_bucket,
      forecast_shrinkage_weights$games_played_bucket
    )
  ]

historical_oos_predictions$forecast_margin <- (
  historical_oos_predictions$baseline_margin +
    historical_oos_predictions$learned_forecast_weight *
    (
      historical_oos_predictions$predicted_margin -
        historical_oos_predictions$baseline_margin
    )
)

historical_oos_predictions$fixed_probability_weight <-
  get_fixed_blend_weight(
    historical_oos_predictions$average_games_per_team_before_week
  )

historical_oos_predictions$probability_margin <- (
  historical_oos_predictions$baseline_margin +
    historical_oos_predictions$fixed_probability_weight *
    (
      historical_oos_predictions$predicted_margin -
        historical_oos_predictions$baseline_margin
    )
)

historical_oos_predictions$home_win_probability <-
  clip_probability(
    plogis(
      nfl_probability_intercept +
        nfl_probability_slope *
        (
          historical_oos_predictions$probability_margin /
            historical_oos_predictions$predictive_margin_sd
        )
    )
  )

historical_oos_predictions$raw_margin_error <- (
  historical_oos_predictions$predicted_margin -
    historical_oos_predictions$actual_margin
)

historical_oos_predictions$forecast_margin_error <- (
  historical_oos_predictions$forecast_margin -
    historical_oos_predictions$actual_margin
)

historical_oos_predictions$brier_score <- (
  historical_oos_predictions$home_win_probability -
    historical_oos_predictions$actual_home_win_soft
)^2

historical_non_tie <- historical_oos_predictions[
  !is.na(
    historical_oos_predictions$actual_home_win_non_tie
  ),
]

historical_non_tie$log_loss <- -(
  historical_non_tie$actual_home_win_non_tie *
    log(
      historical_non_tie$home_win_probability
    ) +
    (
      1 -
        historical_non_tie$actual_home_win_non_tie
    ) *
    log(
      1 -
        historical_non_tie$home_win_probability
    )
)

historical_calibration_summary <- data.frame(
  seasons =
    paste0(
      calibration_start_season,
      "-",
      calibration_end_season
    ),
  games =
    nrow(
      historical_oos_predictions
    ),
  raw_margin_MAE =
    mean(
      abs(
        historical_oos_predictions$raw_margin_error
      )
    ),
  raw_margin_RMSE =
    sqrt(
      mean(
        historical_oos_predictions$raw_margin_error^2
      )
    ),
  forecast_margin_MAE =
    mean(
      abs(
        historical_oos_predictions$forecast_margin_error
      )
    ),
  forecast_margin_RMSE =
    sqrt(
      mean(
        historical_oos_predictions$forecast_margin_error^2
      )
    ),
  brier_score =
    mean(
      historical_oos_predictions$brier_score
    ),
  log_loss =
    mean(
      historical_non_tie$log_loss
    ),
  stringsAsFactors =
    FALSE
)


# ============================================================
# 21. DETERMINE PRODUCTION PUBLICATION WEEK
# ============================================================

scheduled_weeks <- sort(
  unique(
    production_schedule$week
  )
)

completed_weeks <- integer(0)

for (
  current_week in
  scheduled_weeks
) {
  
  week_rows <- production_schedule[
    production_schedule$week ==
      current_week,
  ]
  
  if (
    nrow(
      week_rows
    ) >
    0 &&
    all(
      week_rows$completed
    )
  ) {
    
    completed_weeks <- c(
      completed_weeks,
      current_week
    )
  }
}

if (
  is.null(
    as_of_week
  )
) {
  
  if (
    length(
      completed_weeks
    ) ==
    0
  ) {
    
    stop(
      paste0(
        "No fully completed regular-season week exists yet for ",
        production_season,
        "."
      )
    )
  }
  
  publication_week <- max(
    completed_weeks
  )
  
} else {
  
  publication_week <- as.integer(
    as_of_week
  )
}

completed_production_games <- production_schedule[
  production_schedule$completed &
    production_schedule$week <=
    publication_week,
]

if (
  nrow(
    completed_production_games
  ) ==
  0
) {
  
  stop(
    "No completed production-season games are available for the selected publication week."
  )
}

production_teams <- sort(
  unique(
    c(
      production_schedule$home_team,
      production_schedule$away_team
    )
  )
)

average_games_per_team <- (
  2 *
    nrow(
      completed_production_games
    )
) /
  length(
    production_teams
  )


# ============================================================
# 22. CURRENT POWER MODEL
# ============================================================

current_model <- fit_power_model(
  training_games =
    completed_production_games,
  league_teams =
    production_teams
)

current_predictive_margin_sd <-
  estimate_predictive_margin_sd(
    model =
      current_model,
    training_games =
      completed_production_games
  )


# ============================================================
# 23. TEAM RECORDS
# ============================================================

calculate_records <- function(
    completed_games,
    league_teams
) {
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    team <- league_teams[i]
    
    team_games <- completed_games[
      completed_games$home_team ==
        team |
        completed_games$away_team ==
        team,
    ]
    
    wins <- 0
    losses <- 0
    ties <- 0
    
    if (
      nrow(
        team_games
      ) >
      0
    ) {
      
      home_win <- (
        team_games$home_team ==
          team &
          team_games$actual_margin >
          0
      )
      
      away_win <- (
        team_games$away_team ==
          team &
          team_games$actual_margin <
          0
      )
      
      ties_vector <- (
        team_games$actual_margin ==
          0
      )
      
      wins <- sum(
        home_win |
          away_win
      )
      
      ties <- sum(
        ties_vector
      )
      
      losses <- (
        nrow(
          team_games
        ) -
          wins -
          ties
      )
    }
    
    games_played <- (
      wins +
        losses +
        ties
    )
    
    record <- if (
      ties >
      0
    ) {
      
      paste0(
        wins,
        "-",
        losses,
        "-",
        ties
      )
      
    } else {
      
      paste0(
        wins,
        "-",
        losses
      )
    }
    
    rows[[i]] <- data.frame(
      team =
        team,
      wins =
        wins,
      losses =
        losses,
      ties =
        ties,
      games_played =
        games_played,
      record =
        record,
      win_pct =
        if (
          games_played >
          0
        ) {
          (
            wins +
              0.5 *
              ties
          ) /
            games_played
        } else {
          NA_real_
        },
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}




rank_desc_keep_na <- function(
    values
) {
  
  output <- rep(
    NA_integer_,
    length(
      values
    )
  )
  
  valid <- !is.na(
    values
  )
  
  if (
    any(
      valid
    )
  ) {
    
    output[valid] <- as.integer(
      rank(
        -values[valid],
        ties.method =
          "min"
      )
    )
  }
  
  output
}


# ============================================================
# 24. STRENGTH OF SCHEDULE + LOCATION DIFFICULTY
# ============================================================

calculate_sos_metrics <- function(
    season_schedule,
    completed_games,
    current_ratings,
    estimated_hfa,
    league_teams
) {
  
  played_ids <- completed_games$game_id
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    team <- league_teams[i]
    
    team_schedule <- season_schedule[
      season_schedule$home_team ==
        team |
        season_schedule$away_team ==
        team,
    ]
    
    team_schedule$opponent <- ifelse(
      team_schedule$home_team ==
        team,
      team_schedule$away_team,
      team_schedule$home_team
    )
    
    team_schedule$is_home <- (
      team_schedule$home_team ==
        team
    )
    
    team_schedule$is_played <- (
      team_schedule$game_id %in%
        played_ids
    )
    
    played <- team_schedule[
      team_schedule$is_played,
    ]
    
    remaining <- team_schedule[
      !team_schedule$is_played,
    ]
    
    get_raw_sos <- function(
    games_subset
    ) {
      
      if (
        nrow(
          games_subset
        ) ==
        0
      ) {
        
        return(
          NA_real_
        )
      }
      
      mean(
        current_ratings[
          games_subset$opponent
        ]
      )
    }
    
    get_location_difficulty <- function(
    games_subset
    ) {
      
      if (
        nrow(
          games_subset
        ) ==
        0
      ) {
        
        return(
          NA_real_
        )
      }
      
      opponent_rating <- current_ratings[
        games_subset$opponent
      ]
      
      location_adjustment <- ifelse(
        games_subset$neutral_site,
        0,
        ifelse(
          games_subset$is_home,
          -estimated_hfa,
          estimated_hfa
        )
      )
      
      mean(
        opponent_rating +
          location_adjustment
      )
    }
    
    rows[[i]] <- data.frame(
      team =
        team,
      played_SOS =
        get_raw_sos(
          played
        ),
      remaining_SOS =
        get_raw_sos(
          remaining
        ),
      full_SOS =
        get_raw_sos(
          team_schedule
        ),
      played_location_difficulty =
        get_location_difficulty(
          played
        ),
      remaining_location_difficulty =
        get_location_difficulty(
          remaining
        ),
      full_location_difficulty =
        get_location_difficulty(
          team_schedule
        ),
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  output$played_SOS_rank <- rank_desc_keep_na(
    output$played_SOS
  )
  
  output$remaining_SOS_rank <- rank_desc_keep_na(
    output$remaining_SOS
  )
  
  output$full_SOS_rank <- rank_desc_keep_na(
    output$full_SOS
  )
  
  output$played_location_difficulty_rank <- rank_desc_keep_na(
    output$played_location_difficulty
  )
  
  output$remaining_location_difficulty_rank <- rank_desc_keep_na(
    output$remaining_location_difficulty
  )
  
  output$full_location_difficulty_rank <- rank_desc_keep_na(
    output$full_location_difficulty
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


# ============================================================
# 25. BUILD CURRENT POWER TABLE
# ============================================================

build_current_power_table <- function(
    model,
    completed_games,
    season_schedule,
    publication_week,
    league_teams,
    previous_table = NULL
) {
  
  records <- calculate_records(
    completed_games =
      completed_games,
    league_teams =
      league_teams
  )
  
  sos <- calculate_sos_metrics(
    season_schedule =
      season_schedule,
    completed_games =
      completed_games,
    current_ratings =
      model$ratings,
    estimated_hfa =
      model$hfa,
    league_teams =
      league_teams
  )
  
  output <- data.frame(
    season =
      production_season,
    week =
      publication_week,
    team =
      league_teams,
    rating =
      as.numeric(
        model$ratings[
          league_teams
        ]
      ),
    offense_rating =
      as.numeric(
        model$offense_rating[
          league_teams
        ]
      ),
    defense_rating =
      as.numeric(
        model$defense_rating[
          league_teams
        ]
      ),
    stringsAsFactors =
      FALSE
  )
  
  output <- merge(
    output,
    records,
    by =
      "team",
    all.x =
      TRUE
  )
  
  output <- merge(
    output,
    sos,
    by =
      "team",
    all.x =
      TRUE
  )
  
  output$rank <- as.integer(
    rank(
      -output$rating,
      ties.method =
        "min"
    )
  )
  
  output$offense_rank <- as.integer(
    rank(
      -output$offense_rating,
      ties.method =
        "min"
    )
  )
  
  output$defense_rank <- as.integer(
    rank(
      -output$defense_rating,
      ties.method =
        "min"
    )
  )
  
  output$previous_rank <- NA_integer_
  output$rank_change <- NA_integer_
  output$previous_rating <- NA_real_
  output$rating_change <- NA_real_
  
  if (
    !is.null(
      previous_table
    )
  ) {
    
    previous_lookup <- previous_table[
      ,
      c(
        "team",
        "rank",
        "rating"
      )
    ]
    
    names(
      previous_lookup
    ) <- c(
      "team",
      "previous_rank_lookup",
      "previous_rating_lookup"
    )
    
    output <- merge(
      output,
      previous_lookup,
      by =
        "team",
      all.x =
        TRUE
    )
    
    output$previous_rank <-
      output$previous_rank_lookup
    
    output$rank_change <- (
      output$previous_rank -
        output$rank
    )
    
    output$previous_rating <-
      output$previous_rating_lookup
    
    output$rating_change <- (
      output$rating -
        output$previous_rating
    )
    
    output$previous_rank_lookup <- NULL
    output$previous_rating_lookup <- NULL
  }
  
  output$estimated_HFA <-
    model$hfa
  
  output$league_mean_points <-
    model$league_mean_points
  
  output$average_games_per_team <- (
    2 *
      nrow(
        completed_games
      )
  ) /
    length(
      league_teams
    )
  
  output$publication_stage <- if (
    output$average_games_per_team[1] <=
    2
  ) {
    "Provisional"
  } else if (
    output$average_games_per_team[1] <=
    4
  ) {
    "Early Season"
  } else {
    "Standard"
  }
  
  output <- output[
    order(
      output$rank,
      output$team
    ),
  ]
  
  rownames(
    output
  ) <- NULL
  
  output
}


# ============================================================
# 26. WEEKLY POWER HISTORY WITH SOS
# ============================================================

build_weekly_power_history <- function(
    completed_games,
    season_schedule,
    through_week,
    league_teams
) {
  
  history_storage <- list()
  history_position <- 1
  
  previous_table <- NULL
  
  for (
    current_week in
    sort(
      unique(
        completed_games$week[
          completed_games$week <=
          through_week
        ]
      )
    )
  ) {
    
    current_completed <- completed_games[
      completed_games$week <=
        current_week,
    ]
    
    model <- fit_power_model(
      training_games =
        current_completed,
      league_teams =
        league_teams
    )
    
    current_table <- build_current_power_table(
      model =
        model,
      completed_games =
        current_completed,
      season_schedule =
        season_schedule,
      publication_week =
        current_week,
      league_teams =
        league_teams,
      previous_table =
        previous_table
    )
    
    history_storage[[history_position]] <-
      current_table
    
    history_position <-
      history_position +
      1
    
    previous_table <- current_table
  }
  
  output <- do.call(
    rbind,
    history_storage
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


current_weekly_power_history <-
  build_weekly_power_history(
    completed_games =
      completed_production_games,
    season_schedule =
      production_schedule,
    through_week =
      publication_week,
    league_teams =
      production_teams
  )

current_power_rankings <-
  current_weekly_power_history[
    current_weekly_power_history$week ==
      publication_week,
  ]

current_power_rankings <- current_power_rankings[
  order(
    current_power_rankings$rank
  ),
]


# ============================================================
# 27. APPLY VERSION 12 FORECAST LAYER
# ============================================================

apply_version12_forecast_layer <- function(
    raw_predictions,
    training_games,
    league_teams
) {
  
  if (
    nrow(
      raw_predictions
    ) ==
    0
  ) {
    
    return(
      raw_predictions
    )
  }
  
  average_games <- (
    2 *
      nrow(
        training_games
      )
  ) /
    length(
      league_teams
    )
  
  games_bucket <- get_games_played_bucket(
    average_games
  )
  
  learned_weight <-
    forecast_shrinkage_weights$learned_forecast_weight[
      match(
        games_bucket,
        forecast_shrinkage_weights$games_played_bucket
      )
    ]
  
  fixed_probability_weight <-
    get_fixed_blend_weight(
      average_games
    )
  
  predictive_margin_sd <-
    estimate_predictive_margin_sd(
      model =
        fit_power_model(
          training_games =
            training_games,
          league_teams =
            league_teams
        ),
      training_games =
        training_games
    )
  
  output <- raw_predictions
  
  output$raw_predicted_home_score <-
    output$predicted_home_score
  
  output$raw_predicted_away_score <-
    output$predicted_away_score
  
  output$raw_rating_margin <-
    output$predicted_margin
  
  output$raw_predicted_total <-
    output$predicted_total
  
  output$baseline_margin <- ifelse(
    output$neutral_site,
    0,
    generic_hfa_baseline
  )
  
  output$average_games_per_team_before_game <-
    average_games
  
  output$games_played_bucket <-
    games_bucket
  
  output$learned_forecast_weight <-
    learned_weight
  
  output$fixed_probability_weight <-
    fixed_probability_weight
  
  output$forecast_margin <- (
    output$baseline_margin +
      learned_weight *
      (
        output$raw_rating_margin -
          output$baseline_margin
      )
  )
  
  output$forecast_total <-
    output$raw_predicted_total
  
  output$forecast_home_score <- (
    output$forecast_total +
      output$forecast_margin
  ) /
    2
  
  output$forecast_away_score <- (
    output$forecast_total -
      output$forecast_margin
  ) /
    2
  
  output$probability_margin <- (
    output$baseline_margin +
      fixed_probability_weight *
      (
        output$raw_rating_margin -
          output$baseline_margin
      )
  )
  
  output$predictive_margin_sd <-
    predictive_margin_sd
  
  output$standardized_probability_margin <- (
    output$probability_margin /
      predictive_margin_sd
  )
  
  output$home_win_probability <-
    clip_probability(
      plogis(
        nfl_probability_intercept +
          nfl_probability_slope *
          output$standardized_probability_margin
      )
    )
  
  output$away_win_probability <- (
    1 -
      output$home_win_probability
  )
  
  output$predicted_winner <- ifelse(
    output$home_win_probability >=
      0.5,
    output$home_team,
    output$away_team
  )
  
  output$predicted_winner_probability <- pmax(
    output$home_win_probability,
    output$away_win_probability
  )
  
  output$probability_intercept <-
    nfl_probability_intercept
  
  output$probability_slope <-
    nfl_probability_slope
  
  output
}


# ============================================================
# 28. NEXT WEEK + ALL REMAINING FORECASTS
# ============================================================

remaining_schedule <- production_schedule[
  !production_schedule$game_id %in%
    completed_production_games$game_id,
]

remaining_schedule <- remaining_schedule[
  order(
    remaining_schedule$week,
    remaining_schedule$game_id
  ),
]

if (
  nrow(
    remaining_schedule
  ) >
  0
) {
  
  raw_remaining_predictions <-
    predict_games_from_model(
      model =
        current_model,
      target_schedule =
        remaining_schedule
    )
  
  all_remaining_predictions <-
    apply_version12_forecast_layer(
      raw_predictions =
        raw_remaining_predictions,
      training_games =
        completed_production_games,
      league_teams =
        production_teams
    )
  
  next_week <- min(
    all_remaining_predictions$week
  )
  
  next_week_predictions <-
    all_remaining_predictions[
      all_remaining_predictions$week ==
        next_week,
    ]
  
} else {
  
  next_week <- NA_integer_
  next_week_predictions <- data.frame()
  all_remaining_predictions <- data.frame()
}


# ============================================================
# 29. CURRENT-SEASON WALK-FORWARD OOS
# ============================================================

build_current_season_oos <- function(
    completed_games,
    league_teams
) {
  
  weeks <- sort(
    unique(
      completed_games$week
    )
  )
  
  prediction_storage <- list()
  prediction_position <- 1
  
  for (
    target_week in
    weeks
  ) {
    
    training_games <- completed_games[
      completed_games$week <
        target_week,
    ]
    
    if (
      nrow(
        training_games
      ) ==
      0
    ) {
      
      next
    }
    
    target_games <- completed_games[
      completed_games$week ==
        target_week,
    ]
    
    model <- fit_power_model(
      training_games =
        training_games,
      league_teams =
        league_teams
    )
    
    raw_predictions <- predict_games_from_model(
      model =
        model,
      target_schedule =
        target_games
    )
    
    predictions <-
      apply_version12_forecast_layer(
        raw_predictions =
          raw_predictions,
        training_games =
          training_games,
        league_teams =
          league_teams
      )
    
    predictions$actual_margin <-
      target_games$actual_margin
    
    predictions$margin_error <- (
      predictions$forecast_margin -
        predictions$actual_margin
    )
    
    predictions$absolute_margin_error <- abs(
      predictions$margin_error
    )
    
    predictions$squared_margin_error <- (
      predictions$margin_error^2
    )
    
    predictions$actual_home_win_soft <- ifelse(
      predictions$actual_margin >
        0,
      1,
      ifelse(
        predictions$actual_margin <
          0,
        0,
        0.5
      )
    )
    
    predictions$actual_home_win_non_tie <- ifelse(
      predictions$actual_margin >
        0,
      1,
      ifelse(
        predictions$actual_margin <
          0,
        0,
        NA_real_
      )
    )
    
    predictions$winner_correct <- ifelse(
      is.na(
        predictions$actual_home_win_non_tie
      ),
      NA_real_,
      as.numeric(
        (
          predictions$home_win_probability >=
            0.5
        ) ==
          (
            predictions$actual_home_win_non_tie ==
              1
          )
      )
    )
    
    predictions$brier_score <- (
      predictions$home_win_probability -
        predictions$actual_home_win_soft
    )^2
    
    predictions$log_loss <- ifelse(
      is.na(
        predictions$actual_home_win_non_tie
      ),
      NA_real_,
      -(
        predictions$actual_home_win_non_tie *
          log(
            predictions$home_win_probability
          ) +
          (
            1 -
              predictions$actual_home_win_non_tie
          ) *
          log(
            1 -
              predictions$home_win_probability
          )
      )
    )
    
    prediction_storage[[prediction_position]] <-
      predictions
    
    prediction_position <-
      prediction_position +
      1
  }
  
  if (
    length(
      prediction_storage
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  output <- do.call(
    rbind,
    prediction_storage
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


current_season_oos_predictions <-
  build_current_season_oos(
    completed_games =
      completed_production_games,
    league_teams =
      production_teams
  )

if (
  nrow(
    current_season_oos_predictions
  ) >
  0
) {
  
  non_tie <- current_season_oos_predictions[
    !is.na(
      current_season_oos_predictions$winner_correct
    ),
  ]
  
  current_season_oos_summary <- data.frame(
    season =
      production_season,
    through_week =
      publication_week,
    games =
      nrow(
        current_season_oos_predictions
      ),
    margin_MAE =
      mean(
        current_season_oos_predictions$absolute_margin_error
      ),
    margin_RMSE =
      sqrt(
        mean(
          current_season_oos_predictions$squared_margin_error
        )
      ),
    margin_bias =
      mean(
        current_season_oos_predictions$margin_error
      ),
    winner_accuracy =
      if (
        nrow(
          non_tie
        ) >
        0
      ) {
        mean(
          non_tie$winner_correct
        )
      } else {
        NA_real_
      },
    brier_score =
      mean(
        current_season_oos_predictions$brier_score
      ),
    log_loss =
      mean(
        current_season_oos_predictions$log_loss,
        na.rm =
          TRUE
      ),
    stringsAsFactors =
      FALSE
  )
  
} else {
  
  current_season_oos_summary <- data.frame()
}


# ============================================================
# 30. MODEL SNAPSHOT
# ============================================================

current_games_bucket <- get_games_played_bucket(
  average_games_per_team
)

current_learned_weight <-
  forecast_shrinkage_weights$learned_forecast_weight[
    match(
      current_games_bucket,
      forecast_shrinkage_weights$games_played_bucket
    )
  ]

current_fixed_probability_weight <-
  get_fixed_blend_weight(
    average_games_per_team
  )

model_snapshot <- data.frame(
  production_season =
    production_season,
  publication_week =
    publication_week,
  completed_games =
    nrow(
      completed_production_games
    ),
  teams =
    length(
      production_teams
    ),
  average_games_per_team =
    average_games_per_team,
  games_played_bucket =
    current_games_bucket,
  estimated_HFA =
    current_model$hfa,
  league_mean_points =
    current_model$league_mean_points,
  predictive_margin_sd =
    current_predictive_margin_sd,
  learned_forecast_weight =
    current_learned_weight,
  fixed_probability_weight =
    current_fixed_probability_weight,
  probability_intercept =
    nfl_probability_intercept,
  probability_slope =
    nfl_probability_slope,
  component_lambda =
    component_lambda,
  huber_k =
    huber_k,
  recency_half_life =
    recency_half_life,
  hfa_prior =
    hfa_prior,
  calibration_start_season =
    calibration_start_season,
  calibration_end_season =
    calibration_end_season,
  stringsAsFactors =
    FALSE
)


# ============================================================
# 31. PLOTS
# ============================================================

team_colors <- setNames(
  grDevices::hcl.colors(
    length(
      production_teams
    ),
    palette =
      "Dark 3"
  ),
  production_teams
)

current_plot_data <- current_power_rankings

current_plot_data$team_factor <- factor(
  current_plot_data$team,
  levels =
    rev(
      current_plot_data$team[
        order(
          current_plot_data$rank
        )
      ]
    )
)

power_plot <- ggplot(
  current_plot_data,
  aes(
    x =
      team_factor,
    y =
      rating,
    fill =
      team
  )
) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept =
      0,
    linetype =
      "dashed"
  ) +
  scale_fill_manual(
    values =
      team_colors
  ) +
  guides(
    fill =
      "none"
  ) +
  labs(
    title =
      paste0(
        "Version 12 NFL Power Ratings - ",
        production_season,
        " Through Week ",
        publication_week
      ),
    subtitle =
      "Raw opponent-adjusted Huber Off/Def rating; no forecast shrinkage applied",
    x =
      "Team",
    y =
      "Neutral-Field Rating"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_current_power_ratings.png"
  ),
  plot =
    power_plot,
  width =
    10,
  height =
    10,
  dpi =
    170
)


sos_plot_data <- current_power_rankings[
  !is.na(
    current_power_rankings$remaining_SOS
  ),
]

if (
  nrow(
    sos_plot_data
  ) >
  0
) {
  
  sos_plot_data$team_factor <- factor(
    sos_plot_data$team,
    levels =
      sos_plot_data$team[
        order(
          sos_plot_data$remaining_SOS
        )
      ]
  )
  
  sos_plot <- ggplot(
    sos_plot_data,
    aes(
      x =
        team_factor,
      y =
        remaining_SOS
    )
  ) +
    geom_col() +
    coord_flip() +
    geom_hline(
      yintercept =
        0,
      linetype =
        "dashed"
    ) +
    labs(
      title =
        paste0(
          "Version 12 Remaining Strength of Schedule - Week ",
          publication_week
        ),
      subtitle =
        "Average current rating of remaining opponents; higher = harder",
      x =
        "Team",
      y =
        "Remaining SOS"
    ) +
    theme_minimal()
  
  ggsave(
    paste0(
      output_prefix,
      "_remaining_SOS.png"
    ),
    plot =
      sos_plot,
    width =
      10,
    height =
      10,
    dpi =
      170
  )
}


if (
  nrow(
    next_week_predictions
  ) >
  0
) {
  
  next_plot <- next_week_predictions
  
  next_plot$matchup <- paste0(
    next_plot$away_team,
    " @ ",
    next_plot$home_team
  )
  
  next_plot$matchup <- factor(
    next_plot$matchup,
    levels =
      next_plot$matchup[
        order(
          next_plot$home_win_probability
        )
      ]
  )
  
  probability_plot <- ggplot(
    next_plot,
    aes(
      x =
        matchup,
      y =
        home_win_probability
    )
  ) +
    geom_col() +
    coord_flip() +
    geom_hline(
      yintercept =
        0.5,
      linetype =
        "dashed"
    ) +
    scale_y_continuous(
      limits =
        c(
          0,
          1
        )
    ) +
    labs(
      title =
        paste0(
          "Version 12 Week ",
          next_week,
          " Win Probabilities"
        ),
      subtitle =
        "NFL-calibrated probability layer",
      x =
        "Matchup",
      y =
        "Home Win Probability"
    ) +
    theme_minimal()
  
  ggsave(
    paste0(
      output_prefix,
      "_next_week_win_probabilities.png"
    ),
    plot =
      probability_plot,
    width =
      11,
    height =
      8,
    dpi =
      170
  )
}



# ============================================================
# 32. SEASON-BY-SEASON TEAM SUMMARY ARCHIVE
# ============================================================
#
# One CSV is written for every historical calibration season.
#
# Historical seasons:
#   final regular-season snapshot using all completed REG games.
#
# Production season:
#   current snapshot through publication_week.
#
# These summaries are intentionally richer than the minimum
# requested fields so they can serve as a reusable historical
# team-level modeling / analysis table.
#
# Included:
#
#   season
#   through_week
#   team
#   rating / rank
#   offense_rating / offense_rank
#   defense_rating / defense_rank
#   W-L-T record
#   wins / losses / ties / win_pct
#   points_for / points_against
#   point_differential
#   per-game scoring statistics
#   scheduled / played / remaining games
#   played / remaining / full SOS + ranks
#   played / remaining / full location difficulty + ranks
#   estimated HFA
#   league mean points
#   predictive margin SD
#
# SOS remains a reporting statistic only.
# It does not enter the rating fit.
#
# ============================================================


calculate_team_scoring_summary <- function(
    completed_games,
    season_schedule,
    league_teams
) {
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    team <- league_teams[i]
    
    team_completed <- completed_games[
      completed_games$home_team ==
        team |
        completed_games$away_team ==
        team,
    ]
    
    team_schedule <- season_schedule[
      season_schedule$home_team ==
        team |
        season_schedule$away_team ==
        team,
    ]
    
    games_played <- nrow(
      team_completed
    )
    
    scheduled_games <- nrow(
      team_schedule
    )
    
    remaining_games <- max(
      0,
      scheduled_games -
        games_played
    )
    
    if (
      games_played >
      0
    ) {
      
      points_for <- sum(
        ifelse(
          team_completed$home_team ==
            team,
          team_completed$home_score,
          team_completed$away_score
        )
      )
      
      points_against <- sum(
        ifelse(
          team_completed$home_team ==
            team,
          team_completed$away_score,
          team_completed$home_score
        )
      )
      
    } else {
      
      points_for <- 0
      points_against <- 0
    }
    
    point_differential <- (
      points_for -
        points_against
    )
    
    rows[[i]] <- data.frame(
      team =
        team,
      scheduled_games =
        scheduled_games,
      games_played_scoring =
        games_played,
      remaining_games =
        remaining_games,
      points_for =
        points_for,
      points_against =
        points_against,
      point_differential =
        point_differential,
      points_for_per_game =
        if (
          games_played >
          0
        ) {
          points_for /
            games_played
        } else {
          NA_real_
        },
      points_against_per_game =
        if (
          games_played >
          0
        ) {
          points_against /
            games_played
        } else {
          NA_real_
        },
      point_differential_per_game =
        if (
          games_played >
          0
        ) {
          point_differential /
            games_played
        } else {
          NA_real_
        },
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


build_season_team_summary <- function(
    season_value,
    through_week,
    completed_games,
    season_schedule,
    model,
    league_teams
) {
  
  records <- calculate_records(
    completed_games =
      completed_games,
    league_teams =
      league_teams
  )
  
  scoring <- calculate_team_scoring_summary(
    completed_games =
      completed_games,
    season_schedule =
      season_schedule,
    league_teams =
      league_teams
  )
  
  sos <- calculate_sos_metrics(
    season_schedule =
      season_schedule,
    completed_games =
      completed_games,
    current_ratings =
      model$ratings,
    estimated_hfa =
      model$hfa,
    league_teams =
      league_teams
  )
  
  predictive_margin_sd <-
    estimate_predictive_margin_sd(
      model =
        model,
      training_games =
        completed_games
    )
  
  output <- data.frame(
    season =
      season_value,
    through_week =
      through_week,
    team =
      league_teams,
    rating =
      as.numeric(
        model$ratings[
          league_teams
        ]
      ),
    offense_rating =
      as.numeric(
        model$offense_rating[
          league_teams
        ]
      ),
    defense_rating =
      as.numeric(
        model$defense_rating[
          league_teams
        ]
      ),
    stringsAsFactors =
      FALSE
  )
  
  output <- merge(
    output,
    records,
    by =
      "team",
    all.x =
      TRUE
  )
  
  output <- merge(
    output,
    scoring,
    by =
      "team",
    all.x =
      TRUE
  )
  
  output <- merge(
    output,
    sos,
    by =
      "team",
    all.x =
      TRUE
  )
  
  output$rank <- as.integer(
    rank(
      -output$rating,
      ties.method =
        "min"
    )
  )
  
  output$offense_rank <- as.integer(
    rank(
      -output$offense_rating,
      ties.method =
        "min"
    )
  )
  
  output$defense_rank <- as.integer(
    rank(
      -output$defense_rating,
      ties.method =
        "min"
    )
  )
  
  output$rating_minus_point_diff_per_game <- (
    output$rating -
      output$point_differential_per_game
  )
  
  output$estimated_HFA <-
    model$hfa
  
  output$league_mean_points <-
    model$league_mean_points
  
  output$predictive_margin_sd <-
    predictive_margin_sd
  
  output$average_games_per_team <- (
    2 *
      nrow(
        completed_games
      )
  ) /
    length(
      league_teams
    )
  
  output <- output[
    ,
    c(
      "season",
      "through_week",
      "team",
      "rank",
      "rating",
      "offense_rank",
      "offense_rating",
      "defense_rank",
      "defense_rating",
      "record",
      "wins",
      "losses",
      "ties",
      "win_pct",
      "scheduled_games",
      "games_played",
      "remaining_games",
      "points_for",
      "points_against",
      "point_differential",
      "points_for_per_game",
      "points_against_per_game",
      "point_differential_per_game",
      "rating_minus_point_diff_per_game",
      "played_SOS",
      "played_SOS_rank",
      "remaining_SOS",
      "remaining_SOS_rank",
      "full_SOS",
      "full_SOS_rank",
      "played_location_difficulty",
      "played_location_difficulty_rank",
      "remaining_location_difficulty",
      "remaining_location_difficulty_rank",
      "full_location_difficulty",
      "full_location_difficulty_rank",
      "estimated_HFA",
      "league_mean_points",
      "predictive_margin_sd",
      "average_games_per_team"
    )
  ]
  
  output <- output[
    order(
      output$rank,
      output$team
    ),
  ]
  
  rownames(
    output
  ) <- NULL
  
  output
}


season_summary_directory <- paste0(
  output_prefix,
  "_season_team_summaries"
)

if (
  !dir.exists(
    season_summary_directory
  )
) {
  
  dir.create(
    season_summary_directory,
    recursive =
      TRUE
  )
}


season_team_summary_storage <- list()
season_team_summary_position <- 1

historical_summary_seasons <- sort(
  unique(
    historical_completed_games$season
  )
)

cat("\n========================================\n")
cat("BUILDING SEASON TEAM SUMMARY CSVs\n")
cat("========================================\n\n")


for (
  season_value in
  historical_summary_seasons
) {
  
  season_schedule <- full_schedule[
    full_schedule$season ==
      season_value &
      full_schedule$game_type ==
      "REG",
  ]
  
  season_completed <- season_schedule[
    season_schedule$completed,
  ]
  
  if (
    nrow(
      season_completed
    ) ==
    0
  ) {
    
    next
  }
  
  league_teams <- sort(
    unique(
      c(
        season_schedule$home_team,
        season_schedule$away_team
      )
    )
  )
  
  final_week <- max(
    season_completed$week
  )
  
  final_model <- fit_power_model(
    training_games =
      season_completed,
    league_teams =
      league_teams
  )
  
  season_summary <- build_season_team_summary(
    season_value =
      season_value,
    through_week =
      final_week,
    completed_games =
      season_completed,
    season_schedule =
      season_schedule,
    model =
      final_model,
    league_teams =
      league_teams
  )
  
  season_team_summary_storage[[season_team_summary_position]] <-
    season_summary
  
  season_team_summary_position <-
    season_team_summary_position +
    1
  
  historical_filename <- file.path(
    season_summary_directory,
    paste0(
      output_prefix,
      "_",
      season_value,
      "_team_summary.csv"
    )
  )
  
  write.csv(
    season_summary,
    historical_filename,
    row.names =
      FALSE
  )
  
  cat(
    "Wrote",
    historical_filename,
    "\n"
  )
}


current_season_team_summary <- build_season_team_summary(
  season_value =
    production_season,
  through_week =
    publication_week,
  completed_games =
    completed_production_games,
  season_schedule =
    production_schedule,
  model =
    current_model,
  league_teams =
    production_teams
)

season_team_summary_storage[[season_team_summary_position]] <-
  current_season_team_summary

current_summary_filename <- file.path(
  season_summary_directory,
  paste0(
    output_prefix,
    "_",
    production_season,
    "_team_summary_through_week_",
    publication_week,
    ".csv"
  )
)

write.csv(
  current_season_team_summary,
  current_summary_filename,
  row.names =
    FALSE
)

cat(
  "Wrote",
  current_summary_filename,
  "\n"
)


all_season_team_summaries <- do.call(
  rbind,
  season_team_summary_storage
)

rownames(
  all_season_team_summaries
) <- NULL

write.csv(
  all_season_team_summaries,
  paste0(
    output_prefix,
    "_all_season_team_summaries.csv"
  ),
  row.names =
    FALSE
)



# ============================================================
# 33. SAVE OUTPUTS
# ============================================================

write.csv(
  current_power_rankings,
  paste0(
    output_prefix,
    "_current_power_rankings.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  current_weekly_power_history,
  paste0(
    output_prefix,
    "_current_weekly_power_history.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  next_week_predictions,
  paste0(
    output_prefix,
    "_next_week_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  all_remaining_predictions,
  paste0(
    output_prefix,
    "_all_remaining_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  current_season_oos_predictions,
  paste0(
    output_prefix,
    "_current_season_oos_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  current_season_oos_summary,
  paste0(
    output_prefix,
    "_current_season_oos_summary.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  forecast_shrinkage_weights,
  paste0(
    output_prefix,
    "_forecast_shrinkage_weights.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  probability_calibration_coefficients,
  paste0(
    output_prefix,
    "_probability_calibration_coefficients.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  historical_calibration_summary,
  paste0(
    output_prefix,
    "_historical_calibration_summary.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  historical_oos_predictions,
  paste0(
    output_prefix,
    "_historical_oos_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  model_snapshot,
  paste0(
    output_prefix,
    "_model_snapshot.csv"
  ),
  row.names =
    FALSE
)


# ============================================================
# 34. CONSOLE REPORT
# ============================================================

cat("\n========================================\n")
cat("VERSION 12 NFL PRODUCTION SYSTEM\n")
cat("========================================\n\n")

cat(
  "Production season:",
  production_season,
  "\n"
)

cat(
  "Publication week:",
  publication_week,
  "\n"
)

cat(
  "Completed production games:",
  nrow(
    completed_production_games
  ),
  "\n"
)

cat(
  "Average games/team:",
  round(
    average_games_per_team,
    3
  ),
  "\n"
)

cat(
  "Estimated HFA:",
  round(
    current_model$hfa,
    3
  ),
  "\n"
)

cat(
  "League mean points/team:",
  round(
    current_model$league_mean_points,
    3
  ),
  "\n"
)

cat(
  "Predictive margin SD:",
  round(
    current_predictive_margin_sd,
    3
  ),
  "\n\n"
)

cat("REAL-NFL FORECAST CALIBRATION:\n")

print(
  forecast_shrinkage_weights,
  row.names =
    FALSE
)

cat("\nNFL WIN-PROBABILITY CALIBRATION:\n")

print(
  probability_calibration_coefficients,
  row.names =
    FALSE
)

cat("\nHISTORICAL CALIBRATION PERFORMANCE:\n")

print(
  historical_calibration_summary,
  row.names =
    FALSE
)

cat("\nCURRENT POWER RANKINGS + SOS:\n\n")

print(
  current_power_rankings[
    ,
    c(
      "rank",
      "team",
      "rating",
      "offense_rating",
      "offense_rank",
      "defense_rating",
      "defense_rank",
      "record",
      "rank_change",
      "rating_change",
      "played_SOS",
      "played_SOS_rank",
      "remaining_SOS",
      "remaining_SOS_rank",
      "full_SOS",
      "full_SOS_rank",
      "remaining_location_difficulty",
      "remaining_location_difficulty_rank"
    )
  ],
  row.names =
    FALSE
)

if (
  nrow(
    next_week_predictions
  ) >
  0
) {
  
  cat("\n========================================\n")
  cat(
    "NEXT WEEK FORECASTS - WEEK ",
    next_week,
    "\n",
    sep = ""
  )
  cat("========================================\n\n")
  
  print(
    next_week_predictions[
      ,
      c(
        "game_id",
        "away_team",
        "home_team",
        "raw_rating_margin",
        "learned_forecast_weight",
        "forecast_away_score",
        "forecast_home_score",
        "forecast_margin",
        "probability_margin",
        "home_win_probability",
        "away_win_probability",
        "predicted_winner",
        "predicted_winner_probability"
      )
    ],
    row.names =
      FALSE
  )
}

if (
  nrow(
    current_season_oos_summary
  ) >
  0
) {
  
  cat("\nCURRENT-SEASON WALK-FORWARD OOS:\n\n")
  
  print(
    current_season_oos_summary,
    row.names =
      FALSE
  )
}


cat("\nSEASON TEAM SUMMARY ARCHIVE:\n")
cat(
  "  Folder: ",
  season_summary_directory,
  "\n",
  sep = ""
)
cat(
  "  Historical season CSVs: ",
  length(
    historical_summary_seasons
  ),
  "\n",
  sep = ""
)
cat(
  "  Current season summary: ",
  current_summary_filename,
  "\n",
  sep = ""
)
cat(
  "  Combined archive: ",
  paste0(
    output_prefix,
    "_all_season_team_summaries.csv"
  ),
  "\n"
)

cat("\nMODEL INTERPRETATION:\n")
cat(
  "  Raw rating = published team strength estimate.\n"
)
cat(
  "  Forecast margin = learned real-NFL shrinkage applied to raw rating margin.\n"
)
cat(
  "  Probability margin = fixed blend used only for NFL-calibrated win probability.\n"
)
cat(
  "  SOS is derived from current opponent ratings and is never an input to rating fitting.\n"
)
cat(
  "  Neutral-site games receive zero HFA.\n\n"
)

cat(
  "Outputs written with prefix: ",
  output_prefix,
  "\n",
  sep = ""
)

# ============================================================
# END VERSION 12.1
# ============================================================

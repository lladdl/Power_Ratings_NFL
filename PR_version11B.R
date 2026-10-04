# ============================================================
# FOOTBALL POWER RATINGS - VERSION 11B
# EXPANDING-WINDOW NFL FORECAST CALIBRATION EXPERIMENT
# ============================================================
#
# Default: 2005-2025 NFL regular seasons.
# Every season resets all teams to equal. Week 1 is excluded from
# primary forecasting because there are no same-season results yet.
#
# Locked model:
#   Score Offense/Defense Huber
#   component lambda = 2
#   Huber k = 1.5
#   recency half-life = 12 weeks
#   HFA prior = 2.5
#   win probability = logit^-1(0.0192183 + 1.440608*z)
#
# Neutral-site games receive zero HFA contribution.
# nflverse spread_line / moneyline are evaluation benchmarks only.
# They NEVER enter model fitting.
# ============================================================

if (!requireNamespace("nflreadr", quietly = TRUE)) {
  stop("Package 'nflreadr' is required. Install with install.packages('nflreadr').")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Install with install.packages('ggplot2').")
}
library(nflreadr)
library(ggplot2)

seasons_to_test <- 2005:2025
quick_test <- FALSE
if (quick_test) seasons_to_test <- 2022:2025
output_prefix <- "version11B"

component_lambda <- 2
huber_k <- 1.5
recency_half_life <- 12
hfa_prior <- 2.5
max_huber_iterations <- 40
huber_tolerance <- 1e-6

generic_margin_noise_prior_sd <- 8.5
margin_noise_prior_weight <- 20
win_probability_intercept <- 0.0192183
win_probability_slope <- 1.440608
probability_epsilon <- 1e-6
generic_hfa_baseline <- 2.5
probability_bin_breaks <- seq(0, 1, by = 0.10)


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
# 15. LOAD REAL NFL SCHEDULES / RESULTS
# ============================================================

cat("\n========================================\n")
cat("VERSION 11 - LOAD REAL NFL DATA\n")
cat("========================================\n\n")
cat("Loading seasons", min(seasons_to_test), "through", max(seasons_to_test), "...\n")

raw_schedule <- nflreadr::load_schedules(seasons = seasons_to_test)
required <- c("game_id", "season", "game_type", "week", "home_team",
              "away_team", "home_score", "away_score")
missing <- setdiff(required, names(raw_schedule))
if (length(missing) > 0) stop(paste("Missing schedule columns:", paste(missing, collapse = ", ")))

games <- as.data.frame(raw_schedule, stringsAsFactors = FALSE)
games <- games[games$game_type == "REG", ]
games <- games[!is.na(games$home_score) & !is.na(games$away_score), ]
games$season <- as.integer(games$season)
games$week <- as.integer(games$week)
games$home_team <- as.character(games$home_team)
games$away_team <- as.character(games$away_team)
games$actual_margin <- games$home_score - games$away_score
games$actual_total <- games$home_score + games$away_score

if ("location" %in% names(games)) {
  games$neutral_site <- tolower(as.character(games$location)) == "neutral"
  games$neutral_site[is.na(games$neutral_site)] <- FALSE
} else {
  warning("No location column found; all games treated as non-neutral.")
  games$neutral_site <- FALSE
}

for (nm in c("spread_line", "total_line", "home_moneyline", "away_moneyline")) {
  if (!nm %in% names(games)) games[[nm]] <- NA_real_
}

games <- games[order(games$season, games$week, games$game_id), ]
rownames(games) <- NULL
write.csv(games, paste0(output_prefix, "_nfl_schedule_input.csv"), row.names = FALSE)

cat("Completed REG games:", nrow(games), "\n")
cat("Seasons:", length(unique(games$season)), "\n")
cat("Neutral-site games:", sum(games$neutral_site), "\n\n")

# ============================================================
# 16. MARKET-PROBABILITY HELPERS
# ============================================================

american_odds_to_implied_probability <- function(odds) {
  output <- rep(NA_real_, length(odds))
  pos <- !is.na(odds) & odds > 0
  neg <- !is.na(odds) & odds < 0
  output[pos] <- 100 / (odds[pos] + 100)
  output[neg] <- -odds[neg] / (-odds[neg] + 100)
  output
}

calculate_no_vig_home_probability <- function(home_moneyline, away_moneyline) {
  h <- american_odds_to_implied_probability(home_moneyline)
  a <- american_odds_to_implied_probability(away_moneyline)
  out <- h / (h + a)
  out[!is.finite(out)] <- NA_real_
  out
}

# ============================================================
# 17. SCORE OOS FORECASTS
# ============================================================

score_predictions <- function(predictions) {
  predictions$margin_error <- predictions$predicted_margin - predictions$actual_margin
  predictions$absolute_margin_error <- abs(predictions$margin_error)
  predictions$squared_margin_error <- predictions$margin_error^2
  predictions$home_score_error <- predictions$predicted_home_score - predictions$home_score
  predictions$away_score_error <- predictions$predicted_away_score - predictions$away_score
  predictions$total_score_error <- predictions$predicted_total - predictions$actual_total
  
  predictions$actual_home_win_soft <- ifelse(
    predictions$actual_margin > 0, 1,
    ifelse(predictions$actual_margin < 0, 0, 0.5)
  )
  predictions$actual_home_win_non_tie <- ifelse(
    predictions$actual_margin > 0, 1,
    ifelse(predictions$actual_margin < 0, 0, NA_real_)
  )
  predictions$winner_correct <- ifelse(
    is.na(predictions$actual_home_win_non_tie), NA_real_,
    as.numeric((predictions$home_win_probability >= 0.5) ==
                 (predictions$actual_home_win_non_tie == 1))
  )
  predictions$brier_score <-
    (predictions$home_win_probability - predictions$actual_home_win_soft)^2
  predictions$log_loss <- ifelse(
    is.na(predictions$actual_home_win_non_tie), NA_real_,
    -(predictions$actual_home_win_non_tie * log(predictions$home_win_probability) +
        (1 - predictions$actual_home_win_non_tie) *
        log(1 - predictions$home_win_probability))
  )
  
  # Generic HFA-only results-free benchmark.
  predictions$baseline_margin <- ifelse(predictions$neutral_site, 0, generic_hfa_baseline)
  predictions$baseline_margin_error <- predictions$baseline_margin - predictions$actual_margin
  predictions$baseline_absolute_margin_error <- abs(predictions$baseline_margin_error)
  predictions$baseline_squared_margin_error <- predictions$baseline_margin_error^2
  
  # nflverse closing spread benchmark: positive = home team favored.
  predictions$market_margin_error <- predictions$spread_line - predictions$actual_margin
  predictions$market_absolute_margin_error <- abs(predictions$market_margin_error)
  predictions$market_squared_margin_error <- predictions$market_margin_error^2
  
  predictions$market_home_win_probability <- calculate_no_vig_home_probability(
    predictions$home_moneyline, predictions$away_moneyline
  )
  predictions$market_brier_score <-
    (predictions$market_home_win_probability - predictions$actual_home_win_soft)^2
  p <- clip_probability(predictions$market_home_win_probability)
  predictions$market_log_loss <- ifelse(
    is.na(p) | is.na(predictions$actual_home_win_non_tie), NA_real_,
    -(predictions$actual_home_win_non_tie * log(p) +
        (1 - predictions$actual_home_win_non_tie) * log(1 - p))
  )
  predictions
}

# ============================================================
# 18. ONE WEEKLY RATING SNAPSHOT
# ============================================================

build_rating_snapshot <- function(season, week, completed_games, league_teams) {
  model <- fit_power_model(completed_games, league_teams)
  out <- data.frame(
    season = season,
    week = week,
    team = league_teams,
    rating = as.numeric(model$ratings[league_teams]),
    offense_rating = as.numeric(model$offense_rating[league_teams]),
    defense_rating = as.numeric(model$defense_rating[league_teams]),
    stringsAsFactors = FALSE
  )
  out$rank <- as.integer(rank(-out$rating, ties.method = "min"))
  out$offense_rank <- as.integer(rank(-out$offense_rating, ties.method = "min"))
  out$defense_rank <- as.integer(rank(-out$defense_rating, ties.method = "min"))
  out$estimated_HFA <- model$hfa
  out$league_mean_points <- model$league_mean_points
  out <- out[order(out$rank, out$team), ]
  rownames(out) <- NULL
  list(table = out, model = model)
}

# ============================================================
# 19. BACKTEST ONE SEASON
# ============================================================

backtest_one_season <- function(season_games) {
  current_season <- unique(season_games$season)
  if (length(current_season) != 1) stop("Expected exactly one season.")
  league_teams <- sort(unique(c(season_games$home_team, season_games$away_team)))
  active_weeks <- sort(unique(season_games$week))
  
  prediction_storage <- list()
  rating_storage <- list()
  hfa_storage <- list()
  prediction_position <- 1
  rating_position <- 1
  hfa_position <- 1
  
  for (target_week in active_weeks) {
    training_games <- season_games[season_games$week < target_week, ]
    completed_through_week <- season_games[season_games$week <= target_week, ]
    
    # End-of-week ratings after this week's games.
    if (nrow(completed_through_week) > 0) {
      snap <- build_rating_snapshot(
        current_season, target_week, completed_through_week, league_teams
      )
      rating_storage[[rating_position]] <- snap$table
      rating_position <- rating_position + 1
    }
    
    # Week 1 / any no-history week is not part of primary OOS evaluation.
    if (nrow(training_games) == 0) next
    target_games <- season_games[season_games$week == target_week, ]
    if (nrow(target_games) == 0) next
    
    model <- fit_power_model(training_games, league_teams)
    predictive_sd <- estimate_predictive_margin_sd(model, training_games)
    pred <- predict_games_from_model(model, target_games)
    pred$predictive_margin_sd <- predictive_sd
    pred$standardized_predicted_margin <- pred$predicted_margin / predictive_sd
    pred$home_win_probability <- margin_to_home_win_probability(
      pred$predicted_margin, predictive_sd
    )
    pred$away_win_probability <- 1 - pred$home_win_probability
    pred$predicted_winner <- ifelse(
      pred$home_win_probability >= 0.5, pred$home_team, pred$away_team
    )
    pred$predicted_winner_probability <- pmax(
      pred$home_win_probability, pred$away_win_probability
    )
    pred <- score_predictions(pred)
    pred$training_games <- nrow(training_games)
    pred$average_games_per_team_before_week <-
      2 * nrow(training_games) / length(league_teams)
    
    prediction_storage[[prediction_position]] <- pred
    prediction_position <- prediction_position + 1
    
    hfa_storage[[hfa_position]] <- data.frame(
      season = current_season,
      target_week = target_week,
      training_games = nrow(training_games),
      average_games_per_team_before_week =
        2 * nrow(training_games) / length(league_teams),
      estimated_HFA = model$hfa,
      league_mean_points = model$league_mean_points,
      predictive_margin_sd = predictive_sd,
      stringsAsFactors = FALSE
    )
    hfa_position <- hfa_position + 1
  }
  
  list(
    predictions = if (length(prediction_storage)) do.call(rbind, prediction_storage) else data.frame(),
    ratings = if (length(rating_storage)) do.call(rbind, rating_storage) else data.frame(),
    hfa = if (length(hfa_storage)) do.call(rbind, hfa_storage) else data.frame()
  )
}

# ============================================================
# 20. RUN MULTI-SEASON BACKTEST
# ============================================================

cat("========================================\n")
cat("RUNNING WALK-FORWARD NFL BACKTEST\n")
cat("========================================\n\n")

prediction_storage <- list()
rating_storage <- list()
hfa_storage <- list()
prediction_position <- 1
rating_position <- 1
hfa_position <- 1
available_seasons <- sort(unique(games$season))

for (current_season in available_seasons) {
  cat("Backtesting", current_season, "...\n")
  season_result <- backtest_one_season(games[games$season == current_season, ])
  if (nrow(season_result$predictions)) {
    prediction_storage[[prediction_position]] <- season_result$predictions
    prediction_position <- prediction_position + 1
  }
  if (nrow(season_result$ratings)) {
    rating_storage[[rating_position]] <- season_result$ratings
    rating_position <- rating_position + 1
  }
  if (nrow(season_result$hfa)) {
    hfa_storage[[hfa_position]] <- season_result$hfa
    hfa_position <- hfa_position + 1
  }
}

all_oos_predictions <- do.call(rbind, prediction_storage)
weekly_power_ratings <- do.call(rbind, rating_storage)
hfa_by_week <- do.call(rbind, hfa_storage)
rownames(all_oos_predictions) <- NULL
rownames(weekly_power_ratings) <- NULL
rownames(hfa_by_week) <- NULL

# ============================================================



# ============================================================
# 21. VERSION 11B EXPANDING-WINDOW SETTINGS
# ============================================================
#
# The base rating engine above remains COMPLETELY FIXED.
#
# Version 11B tests only:
#
#   A. probability recalibration
#   B. early-season forecast-margin blending
#
# All calibration for test season S uses ONLY seasons < S.
#
# Example:
#
#   2005-2009 calibrate -> evaluate 2010
#   2005-2010 calibrate -> evaluate 2011
#   ...
#   2005-2024 calibrate -> evaluate 2025
#
# No test-season outcomes are used to determine:
#
#   - logistic intercept/slope
#   - learned blend weights
#
# ============================================================

minimum_calibration_seasons <- 5

first_test_season <- (
  min(
    available_seasons
  ) +
    minimum_calibration_seasons
)

test_seasons <- available_seasons[
  available_seasons >=
    first_test_season
]

if (
  length(
    test_seasons
  ) ==
  0
) {
  
  stop(
    "Not enough seasons are available for Version 11B expanding validation."
  )
}

candidate_names <- c(
  "Frozen Synthetic | No Blend",
  "NFL Expanding | No Blend",
  "Frozen Synthetic | Fixed Blend",
  "NFL Expanding | Fixed Blend",
  "Frozen Synthetic | Learned Blend",
  "NFL Expanding | Learned Blend"
)

candidate_colors <- setNames(
  grDevices::hcl.colors(
    length(
      candidate_names
    ),
    palette =
      "Dark 3"
  ),
  candidate_names
)

reference_candidate <- "Frozen Synthetic | No Blend"


# ============================================================
# 22. GAMES-PLAYED BUCKETS
# ============================================================
#
# Bucket 1 = approximately one completed game/team
# Bucket 2 = approximately two
# Bucket 3 = approximately three
# Bucket 4 = approximately four
# Bucket 5 = five or more
#
# ============================================================

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


all_oos_predictions$games_played_bucket <-
  get_games_played_bucket(
    all_oos_predictions$average_games_per_team_before_week
  )


# ============================================================
# 23. FIXED EARLY-SEASON BLEND
# ============================================================
#
# Transparent non-learned challenger:
#
#   w = min(1, games_played_per_team / 4)
#
# Forecast margin:
#
#   blended =
#       baseline_margin
#       + w * (rating_margin - baseline_margin)
#
# Approximate weights:
#
#   1 game  -> 0.25
#   2 games -> 0.50
#   3 games -> 0.75
#   4+      -> 1.00
#
# The power rating itself is NOT blended.
# Only the game forecast is blended.
#
# ============================================================

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
# 24. LEARNED BLEND WEIGHTS
# ============================================================
#
# For each games-played bucket, choose w in [0,1] minimizing
# squared margin error on PRIOR SEASONS ONLY:
#
#   forecast =
#       baseline
#       + w * (model - baseline)
#
# Closed-form least-squares solution:
#
#   w =
#     sum(delta * target) / sum(delta^2)
#
# where:
#
#   delta  = model_margin - baseline_margin
#   target = actual_margin - baseline_margin
#
# ============================================================

learn_blend_weights <- function(
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
    
    representative_games <- if (
      bucket <
      5
    ) {
      bucket
    } else {
      5
    }
    
    fallback_weight <- get_fixed_blend_weight(
      representative_games
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
      learned_weight =
        learned_weight,
      fixed_weight_reference =
        fallback_weight,
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


apply_learned_blend_weight <- function(
    data,
    weight_table
) {
  
  weights <- weight_table$learned_weight[
    match(
      data$games_played_bucket,
      weight_table$games_played_bucket
    )
  ]
  
  if (
    any(
      is.na(
        weights
      )
    )
  ) {
    
    stop(
      "Missing learned blend weight for at least one games-played bucket."
    )
  }
  
  weights
}


# ============================================================
# 25. ADD CANDIDATE MARGIN FORECASTS
# ============================================================

add_candidate_margin_columns <- function(
    data,
    learned_weight_table
) {
  
  output <- data
  
  output$no_blend_weight <- 1
  
  output$fixed_blend_weight <-
    get_fixed_blend_weight(
      output$average_games_per_team_before_week
    )
  
  output$learned_blend_weight <-
    apply_learned_blend_weight(
      data =
        output,
      weight_table =
        learned_weight_table
    )
  
  output$margin_no_blend <-
    output$predicted_margin
  
  output$margin_fixed_blend <- (
    output$baseline_margin +
      output$fixed_blend_weight *
      (
        output$predicted_margin -
          output$baseline_margin
      )
  )
  
  output$margin_learned_blend <- (
    output$baseline_margin +
      output$learned_blend_weight *
      (
        output$predicted_margin -
          output$baseline_margin
      )
  )
  
  # Preserve the original predicted total while altering only
  # the expected margin. This gives internally consistent
  # blended expected scores for diagnostics.
  output$home_score_fixed_blend <- (
    output$predicted_total +
      output$margin_fixed_blend
  ) /
    2
  
  output$away_score_fixed_blend <- (
    output$predicted_total -
      output$margin_fixed_blend
  ) /
    2
  
  output$home_score_learned_blend <- (
    output$predicted_total +
      output$margin_learned_blend
  ) /
    2
  
  output$away_score_learned_blend <- (
    output$predicted_total -
      output$margin_learned_blend
  ) /
    2
  
  output
}


# ============================================================
# 26. NFL EXPANDING LOGISTIC CALIBRATOR
# ============================================================

fit_expanding_logistic_calibrator <- function(
    calibration_data,
    margin_column
) {
  
  non_tie <- calibration_data[
    !is.na(
      calibration_data$actual_home_win_non_tie
    ),
  ]
  
  non_tie$z_margin <- (
    non_tie[[margin_column]] /
      non_tie$predictive_margin_sd
  )
  
  fit <- glm(
    actual_home_win_non_tie ~
      z_margin,
    family =
      binomial(),
    data =
      non_tie
  )
  
  fit
}


apply_frozen_synthetic_probability <- function(
    margin,
    predictive_margin_sd
) {
  
  z_margin <- (
    margin /
      predictive_margin_sd
  )
  
  clip_probability(
    plogis(
      win_probability_intercept +
        win_probability_slope *
        z_margin
    )
  )
}


apply_nfl_expanding_probability <- function(
    margin,
    predictive_margin_sd,
    calibration_model
) {
  
  new_data <- data.frame(
    z_margin = (
      margin /
        predictive_margin_sd
    )
  )
  
  probability <- predict(
    calibration_model,
    newdata =
      new_data,
    type =
      "response"
  )
  
  clip_probability(
    as.numeric(
      probability
    )
  )
}


# ============================================================
# 27. SCORE ONE VERSION 11B CANDIDATE
# ============================================================

score_candidate <- function(
    test_data,
    candidate_name,
    margin,
    home_win_probability,
    blend_type,
    probability_type,
    test_season
) {
  
  probability <- clip_probability(
    home_win_probability
  )
  
  actual_soft <-
    test_data$actual_home_win_soft
  
  actual_non_tie <-
    test_data$actual_home_win_non_tie
  
  margin_error <- (
    margin -
      test_data$actual_margin
  )
  
  predicted_home_win <- (
    probability >=
      0.5
  )
  
  winner_correct <- ifelse(
    is.na(
      actual_non_tie
    ),
    NA_real_,
    as.numeric(
      predicted_home_win ==
        (
          actual_non_tie ==
            1
        )
    )
  )
  
  brier <- (
    probability -
      actual_soft
  )^2
  
  log_loss <- ifelse(
    is.na(
      actual_non_tie
    ),
    NA_real_,
    -(
      actual_non_tie *
        log(
          probability
        ) +
        (
          1 -
            actual_non_tie
        ) *
        log(
          1 -
            probability
        )
    )
  )
  
  output <- data.frame(
    season =
      test_season,
    week =
      test_data$week,
    game_id =
      test_data$game_id,
    home_team =
      test_data$home_team,
    away_team =
      test_data$away_team,
    neutral_site =
      test_data$neutral_site,
    average_games_per_team_before_week =
      test_data$average_games_per_team_before_week,
    games_played_bucket =
      test_data$games_played_bucket,
    candidate =
      candidate_name,
    blend_type =
      blend_type,
    probability_type =
      probability_type,
    original_rating_margin =
      test_data$predicted_margin,
    baseline_margin =
      test_data$baseline_margin,
    candidate_margin =
      margin,
    predictive_margin_sd =
      test_data$predictive_margin_sd,
    home_win_probability =
      probability,
    away_win_probability =
      1 -
      probability,
    actual_margin =
      test_data$actual_margin,
    actual_home_win_soft =
      actual_soft,
    actual_home_win_non_tie =
      actual_non_tie,
    margin_error =
      margin_error,
    absolute_margin_error =
      abs(
        margin_error
      ),
    squared_margin_error =
      margin_error^2,
    winner_correct =
      winner_correct,
    brier_score =
      brier,
    log_loss =
      log_loss,
    spread_line =
      test_data$spread_line,
    market_absolute_margin_error =
      test_data$market_absolute_margin_error,
    market_squared_margin_error =
      test_data$market_squared_margin_error,
    market_home_win_probability =
      test_data$market_home_win_probability,
    market_brier_score =
      test_data$market_brier_score,
    market_log_loss =
      test_data$market_log_loss,
    stringsAsFactors =
      FALSE
  )
  
  output
}


# ============================================================
# 28. RUN STRICT EXPANDING-WINDOW EXPERIMENT
# ============================================================

cat("\n========================================\n")
cat("VERSION 11B EXPANDING FORECAST CALIBRATION\n")
cat("========================================\n\n")

cat(
  "Calibration begins with",
  minimum_calibration_seasons,
  "full prior seasons.\n"
)

cat(
  "First test season:",
  min(
    test_seasons
  ),
  "\n"
)

cat(
  "Final test season:",
  max(
    test_seasons
  ),
  "\n\n"
)

candidate_prediction_storage <- list()
coefficient_storage <- list()
blend_weight_storage <- list()

candidate_prediction_position <- 1
coefficient_position <- 1
blend_weight_position <- 1

for (
  test_season in
  test_seasons
) {
  
  cat(
    "Calibrating on seasons <",
    test_season,
    "and testing",
    test_season,
    "...\n"
  )
  
  calibration_data <- all_oos_predictions[
    all_oos_predictions$season <
      test_season,
  ]
  
  test_data <- all_oos_predictions[
    all_oos_predictions$season ==
      test_season,
  ]
  
  if (
    nrow(
      calibration_data
    ) ==
    0 ||
    nrow(
      test_data
    ) ==
    0
  ) {
    
    next
  }
  
  # ----------------------------------------------------------
  # Learn early-season blend weights from PRIOR SEASONS ONLY.
  # ----------------------------------------------------------
  
  learned_weights <-
    learn_blend_weights(
      calibration_data =
        calibration_data
    )
  
  learned_weights$test_season <-
    test_season
  
  learned_weights$calibration_start_season <-
    min(
      calibration_data$season
    )
  
  learned_weights$calibration_end_season <-
    max(
      calibration_data$season
    )
  
  blend_weight_storage[[blend_weight_position]] <-
    learned_weights
  
  blend_weight_position <-
    blend_weight_position +
    1
  
  calibration_augmented <-
    add_candidate_margin_columns(
      data =
        calibration_data,
      learned_weight_table =
        learned_weights
    )
  
  test_augmented <-
    add_candidate_margin_columns(
      data =
        test_data,
      learned_weight_table =
        learned_weights
    )
  
  # ----------------------------------------------------------
  # Fit separate expanding logistic calibrators for each
  # margin formulation, again using PRIOR SEASONS ONLY.
  # ----------------------------------------------------------
  
  logistic_no_blend <-
    fit_expanding_logistic_calibrator(
      calibration_data =
        calibration_augmented,
      margin_column =
        "margin_no_blend"
    )
  
  logistic_fixed_blend <-
    fit_expanding_logistic_calibrator(
      calibration_data =
        calibration_augmented,
      margin_column =
        "margin_fixed_blend"
    )
  
  logistic_learned_blend <-
    fit_expanding_logistic_calibrator(
      calibration_data =
        calibration_augmented,
      margin_column =
        "margin_learned_blend"
    )
  
  logistic_models <- list(
    "No Blend" =
      logistic_no_blend,
    "Fixed Blend" =
      logistic_fixed_blend,
    "Learned Blend" =
      logistic_learned_blend
  )
  
  for (
    blend_name in
    names(
      logistic_models
    )
  ) {
    
    current_model <-
      logistic_models[[blend_name]]
    
    current_coefficients <-
      coef(
        current_model
      )
    
    coefficient_storage[[coefficient_position]] <-
      data.frame(
        test_season =
          test_season,
        calibration_start_season =
          min(
            calibration_data$season
          ),
        calibration_end_season =
          max(
            calibration_data$season
          ),
        blend_type =
          blend_name,
        intercept =
          as.numeric(
            current_coefficients[1]
          ),
        slope =
          as.numeric(
            current_coefficients[2]
          ),
        calibration_games =
          nrow(
            calibration_augmented
          ),
        calibration_non_tied_games =
          sum(
            !is.na(
              calibration_augmented$actual_home_win_non_tie
            )
          ),
        stringsAsFactors =
          FALSE
      )
    
    coefficient_position <-
      coefficient_position +
      1
  }
  
  # ----------------------------------------------------------
  # Candidate 1:
  # Frozen synthetic probability + no margin blend
  # ----------------------------------------------------------
  
  probability_frozen_no <-
    apply_frozen_synthetic_probability(
      margin =
        test_augmented$margin_no_blend,
      predictive_margin_sd =
        test_augmented$predictive_margin_sd
    )
  
  candidate_prediction_storage[[candidate_prediction_position]] <-
    score_candidate(
      test_data =
        test_augmented,
      candidate_name =
        "Frozen Synthetic | No Blend",
      margin =
        test_augmented$margin_no_blend,
      home_win_probability =
        probability_frozen_no,
      blend_type =
        "No Blend",
      probability_type =
        "Frozen Synthetic",
      test_season =
        test_season
    )
  
  candidate_prediction_position <-
    candidate_prediction_position +
    1
  
  # ----------------------------------------------------------
  # Candidate 2:
  # Expanding NFL probability + no margin blend
  # ----------------------------------------------------------
  
  probability_nfl_no <-
    apply_nfl_expanding_probability(
      margin =
        test_augmented$margin_no_blend,
      predictive_margin_sd =
        test_augmented$predictive_margin_sd,
      calibration_model =
        logistic_no_blend
    )
  
  candidate_prediction_storage[[candidate_prediction_position]] <-
    score_candidate(
      test_data =
        test_augmented,
      candidate_name =
        "NFL Expanding | No Blend",
      margin =
        test_augmented$margin_no_blend,
      home_win_probability =
        probability_nfl_no,
      blend_type =
        "No Blend",
      probability_type =
        "NFL Expanding",
      test_season =
        test_season
    )
  
  candidate_prediction_position <-
    candidate_prediction_position +
    1
  
  # ----------------------------------------------------------
  # Candidate 3:
  # Frozen synthetic probability + fixed blend
  # ----------------------------------------------------------
  
  probability_frozen_fixed <-
    apply_frozen_synthetic_probability(
      margin =
        test_augmented$margin_fixed_blend,
      predictive_margin_sd =
        test_augmented$predictive_margin_sd
    )
  
  candidate_prediction_storage[[candidate_prediction_position]] <-
    score_candidate(
      test_data =
        test_augmented,
      candidate_name =
        "Frozen Synthetic | Fixed Blend",
      margin =
        test_augmented$margin_fixed_blend,
      home_win_probability =
        probability_frozen_fixed,
      blend_type =
        "Fixed Blend",
      probability_type =
        "Frozen Synthetic",
      test_season =
        test_season
    )
  
  candidate_prediction_position <-
    candidate_prediction_position +
    1
  
  # ----------------------------------------------------------
  # Candidate 4:
  # Expanding NFL probability + fixed blend
  # ----------------------------------------------------------
  
  probability_nfl_fixed <-
    apply_nfl_expanding_probability(
      margin =
        test_augmented$margin_fixed_blend,
      predictive_margin_sd =
        test_augmented$predictive_margin_sd,
      calibration_model =
        logistic_fixed_blend
    )
  
  candidate_prediction_storage[[candidate_prediction_position]] <-
    score_candidate(
      test_data =
        test_augmented,
      candidate_name =
        "NFL Expanding | Fixed Blend",
      margin =
        test_augmented$margin_fixed_blend,
      home_win_probability =
        probability_nfl_fixed,
      blend_type =
        "Fixed Blend",
      probability_type =
        "NFL Expanding",
      test_season =
        test_season
    )
  
  candidate_prediction_position <-
    candidate_prediction_position +
    1
  
  # ----------------------------------------------------------
  # Candidate 5:
  # Frozen synthetic probability + learned blend
  # ----------------------------------------------------------
  
  probability_frozen_learned <-
    apply_frozen_synthetic_probability(
      margin =
        test_augmented$margin_learned_blend,
      predictive_margin_sd =
        test_augmented$predictive_margin_sd
    )
  
  candidate_prediction_storage[[candidate_prediction_position]] <-
    score_candidate(
      test_data =
        test_augmented,
      candidate_name =
        "Frozen Synthetic | Learned Blend",
      margin =
        test_augmented$margin_learned_blend,
      home_win_probability =
        probability_frozen_learned,
      blend_type =
        "Learned Blend",
      probability_type =
        "Frozen Synthetic",
      test_season =
        test_season
    )
  
  candidate_prediction_position <-
    candidate_prediction_position +
    1
  
  # ----------------------------------------------------------
  # Candidate 6:
  # Expanding NFL probability + learned blend
  # ----------------------------------------------------------
  
  probability_nfl_learned <-
    apply_nfl_expanding_probability(
      margin =
        test_augmented$margin_learned_blend,
      predictive_margin_sd =
        test_augmented$predictive_margin_sd,
      calibration_model =
        logistic_learned_blend
    )
  
  candidate_prediction_storage[[candidate_prediction_position]] <-
    score_candidate(
      test_data =
        test_augmented,
      candidate_name =
        "NFL Expanding | Learned Blend",
      margin =
        test_augmented$margin_learned_blend,
      home_win_probability =
        probability_nfl_learned,
      blend_type =
        "Learned Blend",
      probability_type =
        "NFL Expanding",
      test_season =
        test_season
    )
  
  candidate_prediction_position <-
    candidate_prediction_position +
    1
}

candidate_predictions <- do.call(
  rbind,
  candidate_prediction_storage
)

logistic_coefficients_by_test_season <- do.call(
  rbind,
  coefficient_storage
)

learned_blend_weights_by_test_season <- do.call(
  rbind,
  blend_weight_storage
)

rownames(
  candidate_predictions
) <- NULL

rownames(
  logistic_coefficients_by_test_season
) <- NULL

rownames(
  learned_blend_weights_by_test_season
) <- NULL


# ============================================================
# 29. CALIBRATION METRICS
# ============================================================

calculate_ece <- function(
    probability,
    outcome_soft,
    bin_breaks =
      probability_bin_breaks
) {
  
  bins <- cut(
    probability,
    breaks =
      bin_breaks,
    include.lowest =
      TRUE,
    right =
      TRUE
  )
  
  weighted_error <- 0
  maximum_error <- 0
  
  for (
    current_bin in
    levels(
      bins
    )
  ) {
    
    index <- (
      bins ==
        current_bin
    )
    
    if (
      !any(
        index
      )
    ) {
      
      next
    }
    
    predicted <- mean(
      probability[
        index
      ]
    )
    
    observed <- mean(
      outcome_soft[
        index
      ]
    )
    
    error <- abs(
      observed -
        predicted
    )
    
    weight <- sum(
      index
    ) /
      length(
        probability
      )
    
    weighted_error <- (
      weighted_error +
        weight *
        error
    )
    
    maximum_error <- max(
      maximum_error,
      error
    )
  }
  
  list(
    ECE =
      weighted_error,
    max_calibration_error =
      maximum_error
  )
}


# ============================================================
# 30. SUMMARY ONE CANDIDATE SLICE
# ============================================================

summarize_candidate_predictions <- function(
    data
) {
  
  non_tie <- data[
    !is.na(
      data$winner_correct
    ),
  ]
  
  calibration <- calculate_ece(
    probability =
      data$home_win_probability,
    outcome_soft =
      data$actual_home_win_soft
  )
  
  market_available <- (
    !is.na(
      data$spread_line
    )
  )
  
  moneyline_available <- (
    !is.na(
      data$market_home_win_probability
    )
  )
  
  data.frame(
    games =
      nrow(
        data
      ),
    non_tied_games =
      nrow(
        non_tie
      ),
    margin_MAE =
      mean(
        data$absolute_margin_error
      ),
    margin_RMSE =
      sqrt(
        mean(
          data$squared_margin_error
        )
      ),
    margin_bias =
      mean(
        data$margin_error
      ),
    margin_correlation =
      if (
        sd(
          data$candidate_margin
        ) ==
        0 ||
        sd(
          data$actual_margin
        ) ==
        0
      ) {
        NA_real_
      } else {
        cor(
          data$candidate_margin,
          data$actual_margin
        )
      },
    winner_accuracy =
      if (
        nrow(
          non_tie
        ) ==
        0
      ) {
        NA_real_
      } else {
        mean(
          non_tie$winner_correct
        )
      },
    brier_score =
      mean(
        data$brier_score
      ),
    log_loss =
      mean(
        data$log_loss,
        na.rm =
          TRUE
      ),
    ECE =
      calibration$ECE,
    max_calibration_error =
      calibration$max_calibration_error,
    mean_home_win_probability =
      mean(
        data$home_win_probability
      ),
    actual_home_win_rate_soft =
      mean(
        data$actual_home_win_soft
      ),
    market_games =
      sum(
        market_available
      ),
    market_margin_MAE =
      if (
        any(
          market_available
        )
      ) {
        mean(
          data$market_absolute_margin_error[
            market_available
          ]
        )
      } else {
        NA_real_
      },
    model_MAE_gap_vs_market =
      if (
        any(
          market_available
        )
      ) {
        mean(
          data$absolute_margin_error[
            market_available
          ]
        ) -
          mean(
            data$market_absolute_margin_error[
              market_available
            ]
          )
      } else {
        NA_real_
      },
    moneyline_games =
      sum(
        moneyline_available
      ),
    market_brier_score =
      if (
        any(
          moneyline_available
        )
      ) {
        mean(
          data$market_brier_score[
            moneyline_available
          ],
          na.rm =
            TRUE
        )
      } else {
        NA_real_
      },
    market_log_loss =
      if (
        any(
          moneyline_available
        )
      ) {
        mean(
          data$market_log_loss[
            moneyline_available
          ],
          na.rm =
            TRUE
        )
      } else {
        NA_real_
      },
    stringsAsFactors =
      FALSE
  )
}


# ============================================================
# 31. OVERALL CANDIDATE SUMMARY
# ============================================================

overall_summary_rows <- list()

for (
  i in
  seq_along(
    candidate_names
  )
) {
  
  candidate_name <- candidate_names[i]
  
  current <- candidate_predictions[
    candidate_predictions$candidate ==
      candidate_name,
  ]
  
  summary_row <-
    summarize_candidate_predictions(
      current
    )
  
  summary_row$candidate <-
    candidate_name
  
  overall_summary_rows[[i]] <-
    summary_row
}

overall_candidate_summary <- do.call(
  rbind,
  overall_summary_rows
)

overall_candidate_summary <- overall_candidate_summary[
  ,
  c(
    "candidate",
    setdiff(
      names(
        overall_candidate_summary
      ),
      "candidate"
    )
  )
]

overall_candidate_summary <- overall_candidate_summary[
  order(
    overall_candidate_summary$brier_score,
    overall_candidate_summary$margin_MAE
  ),
]

rownames(
  overall_candidate_summary
) <- NULL


# ============================================================
# 32. SUMMARY BY TEST SEASON
# ============================================================

season_summary_rows <- list()
season_summary_position <- 1

for (
  test_season in
  test_seasons
) {
  
  for (
    candidate_name in
    candidate_names
  ) {
    
    current <- candidate_predictions[
      candidate_predictions$season ==
        test_season &
        candidate_predictions$candidate ==
        candidate_name,
    ]
    
    if (
      nrow(
        current
      ) ==
      0
    ) {
      
      next
    }
    
    summary_row <-
      summarize_candidate_predictions(
        current
      )
    
    summary_row$season <-
      test_season
    
    summary_row$candidate <-
      candidate_name
    
    season_summary_rows[[season_summary_position]] <-
      summary_row
    
    season_summary_position <-
      season_summary_position +
      1
  }
}

summary_by_test_season <- do.call(
  rbind,
  season_summary_rows
)

rownames(
  summary_by_test_season
) <- NULL


# ============================================================
# 33. SUMMARY BY TARGET WEEK
# ============================================================

test_weeks <- sort(
  unique(
    candidate_predictions$week
  )
)

week_summary_rows <- list()
week_summary_position <- 1

for (
  current_week in
  test_weeks
) {
  
  for (
    candidate_name in
    candidate_names
  ) {
    
    current <- candidate_predictions[
      candidate_predictions$week ==
        current_week &
        candidate_predictions$candidate ==
        candidate_name,
    ]
    
    if (
      nrow(
        current
      ) ==
      0
    ) {
      
      next
    }
    
    summary_row <-
      summarize_candidate_predictions(
        current
      )
    
    summary_row$week <-
      current_week
    
    summary_row$candidate <-
      candidate_name
    
    week_summary_rows[[week_summary_position]] <-
      summary_row
    
    week_summary_position <-
      week_summary_position +
      1
  }
}

summary_by_week <- do.call(
  rbind,
  week_summary_rows
)

rownames(
  summary_by_week
) <- NULL


# ============================================================
# 34. SUMMARY BY GAMES-PLAYED BUCKET
# ============================================================

bucket_summary_rows <- list()
bucket_summary_position <- 1

for (
  bucket in
  1:5
) {
  
  for (
    candidate_name in
    candidate_names
  ) {
    
    current <- candidate_predictions[
      candidate_predictions$games_played_bucket ==
        bucket &
        candidate_predictions$candidate ==
        candidate_name,
    ]
    
    if (
      nrow(
        current
      ) ==
      0
    ) {
      
      next
    }
    
    summary_row <-
      summarize_candidate_predictions(
        current
      )
    
    summary_row$games_played_bucket <-
      bucket
    
    summary_row$candidate <-
      candidate_name
    
    bucket_summary_rows[[bucket_summary_position]] <-
      summary_row
    
    bucket_summary_position <-
      bucket_summary_position +
      1
  }
}

summary_by_games_bucket <- do.call(
  rbind,
  bucket_summary_rows
)

rownames(
  summary_by_games_bucket
) <- NULL


# ============================================================
# 35. PAIRED GAME-LEVEL DIFFERENCES VS VERSION 11 REFERENCE
# ============================================================

reference_data <- candidate_predictions[
  candidate_predictions$candidate ==
    reference_candidate,
  c(
    "season",
    "week",
    "game_id",
    "absolute_margin_error",
    "squared_margin_error",
    "brier_score",
    "log_loss"
  )
]

names(
  reference_data
)[4:7] <- c(
  "reference_absolute_margin_error",
  "reference_squared_margin_error",
  "reference_brier_score",
  "reference_log_loss"
)

paired_rows <- list()

for (
  i in
  seq_along(
    candidate_names
  )
) {
  
  candidate_name <- candidate_names[i]
  
  candidate_data <- candidate_predictions[
    candidate_predictions$candidate ==
      candidate_name,
    c(
      "season",
      "week",
      "game_id",
      "absolute_margin_error",
      "squared_margin_error",
      "brier_score",
      "log_loss"
    )
  ]
  
  names(
    candidate_data
  )[4:7] <- c(
    "candidate_absolute_margin_error",
    "candidate_squared_margin_error",
    "candidate_brier_score",
    "candidate_log_loss"
  )
  
  paired <- merge(
    reference_data,
    candidate_data,
    by = c(
      "season",
      "week",
      "game_id"
    ),
    sort =
      FALSE
  )
  
  mae_difference <- (
    paired$candidate_absolute_margin_error -
      paired$reference_absolute_margin_error
  )
  
  rmse_difference <- (
    paired$candidate_squared_margin_error -
      paired$reference_squared_margin_error
  )
  
  brier_difference <- (
    paired$candidate_brier_score -
      paired$reference_brier_score
  )
  
  log_loss_difference <- (
    paired$candidate_log_loss -
      paired$reference_log_loss
  )
  
  calculate_ci <- function(
    difference
  ) {
    
    finite <- difference[
      is.finite(
        difference
      )
    ]
    
    standard_error <- (
      sd(
        finite
      ) /
        sqrt(
          length(
            finite
          )
        )
    )
    
    c(
      mean =
        mean(
          finite
        ),
      lower =
        mean(
          finite
        ) -
        1.96 *
        standard_error,
      upper =
        mean(
          finite
        ) +
        1.96 *
        standard_error,
      probability_better =
        mean(
          finite <
            0
        )
    )
  }
  
  mae_ci <- calculate_ci(
    mae_difference
  )
  
  mse_ci <- calculate_ci(
    rmse_difference
  )
  
  brier_ci <- calculate_ci(
    brier_difference
  )
  
  logloss_ci <- calculate_ci(
    log_loss_difference
  )
  
  paired_rows[[i]] <- data.frame(
    candidate =
      candidate_name,
    mean_absolute_error_difference =
      mae_ci["mean"],
    MAE_CI_lower =
      mae_ci["lower"],
    MAE_CI_upper =
      mae_ci["upper"],
    probability_lower_absolute_error =
      mae_ci["probability_better"],
    mean_squared_error_difference =
      mse_ci["mean"],
    MSE_CI_lower =
      mse_ci["lower"],
    MSE_CI_upper =
      mse_ci["upper"],
    probability_lower_squared_error =
      mse_ci["probability_better"],
    mean_brier_difference =
      brier_ci["mean"],
    Brier_CI_lower =
      brier_ci["lower"],
    Brier_CI_upper =
      brier_ci["upper"],
    probability_lower_brier =
      brier_ci["probability_better"],
    mean_logloss_difference =
      logloss_ci["mean"],
    LogLoss_CI_lower =
      logloss_ci["lower"],
    LogLoss_CI_upper =
      logloss_ci["upper"],
    probability_lower_logloss =
      logloss_ci["probability_better"],
    stringsAsFactors =
      FALSE
  )
}

paired_vs_reference <- do.call(
  rbind,
  paired_rows
)

rownames(
  paired_vs_reference
) <- NULL


# ============================================================
# 36. POOLED PROBABILITY RELIABILITY BY CANDIDATE
# ============================================================

reliability_rows <- list()
reliability_position <- 1

for (
  candidate_name in
  candidate_names
) {
  
  current_candidate <- candidate_predictions[
    candidate_predictions$candidate ==
      candidate_name,
  ]
  
  current_candidate$probability_bin <- cut(
    current_candidate$home_win_probability,
    breaks =
      probability_bin_breaks,
    include.lowest =
      TRUE,
    right =
      TRUE
  )
  
  for (
    current_bin in
    levels(
      current_candidate$probability_bin
    )
  ) {
    
    current <- current_candidate[
      current_candidate$probability_bin ==
        current_bin,
    ]
    
    if (
      nrow(
        current
      ) ==
      0
    ) {
      
      next
    }
    
    reliability_rows[[reliability_position]] <-
      data.frame(
        candidate =
          candidate_name,
        probability_bin =
          current_bin,
        games =
          nrow(
            current
          ),
        mean_predicted_home_win_probability =
          mean(
            current$home_win_probability
          ),
        actual_home_win_rate_soft =
          mean(
            current$actual_home_win_soft
          ),
        calibration_difference =
          mean(
            current$actual_home_win_soft
          ) -
          mean(
            current$home_win_probability
          ),
        stringsAsFactors =
          FALSE
      )
    
    reliability_position <-
      reliability_position +
      1
  }
}

probability_reliability <- do.call(
  rbind,
  reliability_rows
)

rownames(
  probability_reliability
) <- NULL


# ============================================================
# 37. MARKET COMPARISON FOR TEST PERIOD
# ============================================================

market_rows <- list()

for (
  test_season in
  test_seasons
) {
  
  # Use the best-at-this-point candidate only once after ranking
  # below. For now retain market reference by season from the
  # unblended Version 11 base prediction.
  current <- all_oos_predictions[
    all_oos_predictions$season ==
      test_season &
      !is.na(
        all_oos_predictions$spread_line
      ),
  ]
  
  if (
    nrow(
      current
    ) ==
    0
  ) {
    
    next
  }
  
  market_rows[[length(market_rows) + 1]] <-
    data.frame(
      season =
        test_season,
      games =
        nrow(
          current
        ),
      original_model_MAE =
        mean(
          current$absolute_margin_error
        ),
      market_MAE =
        mean(
          current$market_absolute_margin_error
        ),
      original_model_minus_market_MAE =
        mean(
          current$absolute_margin_error
        ) -
        mean(
          current$market_absolute_margin_error
        ),
      stringsAsFactors =
        FALSE
    )
}

market_comparison <- if (
  length(
    market_rows
  ) >
  0
) {
  
  do.call(
    rbind,
    market_rows
  )
  
} else {
  
  data.frame()
}


# ============================================================
# 38. SELECT BEST CANDIDATES BY PRIMARY METRIC
# ============================================================

best_margin_candidate <- overall_candidate_summary$candidate[
  which.min(
    overall_candidate_summary$margin_MAE
  )
]

best_brier_candidate <- overall_candidate_summary$candidate[
  which.min(
    overall_candidate_summary$brier_score
  )
]

best_logloss_candidate <- overall_candidate_summary$candidate[
  which.min(
    overall_candidate_summary$log_loss
  )
]


# ============================================================
# 39. PLOTS
# ============================================================

plot_candidates <- candidate_names

week_plot_data <- summary_by_week[
  summary_by_week$candidate %in%
    plot_candidates,
]

week_margin_plot <- ggplot(
  week_plot_data,
  aes(
    x =
      week,
    y =
      margin_MAE,
    color =
      candidate,
    group =
      candidate
  )
) +
  geom_line(
    linewidth =
      0.9
  ) +
  geom_point(
    size =
      1.5
  ) +
  scale_color_manual(
    values =
      candidate_colors
  ) +
  labs(
    title =
      "Version 11B: Margin MAE by Target Week",
    subtitle =
      "Expanding-window NFL calibration; lower is better",
    x =
      "Target Week",
    y =
      "Margin MAE",
    color =
      "Candidate"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_margin_MAE_by_week.png"
  ),
  plot =
    week_margin_plot,
  width =
    14,
  height =
    8,
  dpi =
    170
)


season_plot_data <- summary_by_test_season

season_brier_plot <- ggplot(
  season_plot_data,
  aes(
    x =
      season,
    y =
      brier_score,
    color =
      candidate,
    group =
      candidate
  )
) +
  geom_line(
    linewidth =
      0.8
  ) +
  geom_point(
    size =
      1.3
  ) +
  scale_color_manual(
    values =
      candidate_colors
  ) +
  labs(
    title =
      "Version 11B: Brier Score by Test Season",
    subtitle =
      "Each season uses only earlier seasons for calibration",
    x =
      "Test Season",
    y =
      "Brier Score",
    color =
      "Candidate"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_brier_by_test_season.png"
  ),
  plot =
    season_brier_plot,
  width =
    14,
  height =
    8,
  dpi =
    170
)


reliability_plot <- ggplot(
  probability_reliability,
  aes(
    x =
      mean_predicted_home_win_probability,
    y =
      actual_home_win_rate_soft,
    color =
      candidate,
    group =
      candidate
  )
) +
  geom_abline(
    intercept =
      0,
    slope =
      1,
    linetype =
      "dashed"
  ) +
  geom_line(
    linewidth =
      0.8
  ) +
  geom_point(
    aes(
      size =
        games
    )
  ) +
  coord_equal(
    xlim =
      c(
        0,
        1
      ),
    ylim =
      c(
        0,
        1
      )
  ) +
  scale_color_manual(
    values =
      candidate_colors
  ) +
  labs(
    title =
      "Version 11B: Real-NFL Probability Reliability",
    subtitle =
      "Strict expanding-season evaluation",
    x =
      "Predicted Home Win Probability",
    y =
      "Observed Home Win Rate",
    color =
      "Candidate",
    size =
      "Games"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_probability_reliability.png"
  ),
  plot =
    reliability_plot,
  width =
    10,
  height =
    9,
  dpi =
    170
)


blend_weight_plot <- ggplot(
  learned_blend_weights_by_test_season,
  aes(
    x =
      test_season,
    y =
      learned_weight,
    color =
      factor(
        games_played_bucket
      ),
    group =
      games_played_bucket
  )
) +
  geom_line(
    linewidth =
      0.9
  ) +
  geom_point(
    size =
      1.6
  ) +
  labs(
    title =
      "Version 11B: Expanding Learned Margin-Blend Weights",
    subtitle =
      "Weight 0 = HFA baseline only; weight 1 = full rating forecast",
    x =
      "Test Season",
    y =
      "Learned Rating-Forecast Weight",
    color =
      "Games-Played Bucket"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_learned_blend_weights.png"
  ),
  plot =
    blend_weight_plot,
  width =
    12,
  height =
    7,
  dpi =
    170
)


logistic_slope_plot <- ggplot(
  logistic_coefficients_by_test_season,
  aes(
    x =
      test_season,
    y =
      slope,
    color =
      blend_type,
    group =
      blend_type
  )
) +
  geom_line(
    linewidth =
      0.9
  ) +
  geom_point(
    size =
      1.6
  ) +
  geom_hline(
    yintercept =
      win_probability_slope,
    linetype =
      "dashed"
  ) +
  labs(
    title =
      "Version 11B: Expanding NFL Logistic Slopes",
    subtitle =
      "Dashed line = frozen synthetic slope 1.440608",
    x =
      "Test Season",
    y =
      "NFL-Calibrated Logistic Slope",
    color =
      "Margin Forecast"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_logistic_slope_history.png"
  ),
  plot =
    logistic_slope_plot,
  width =
    12,
  height =
    7,
  dpi =
    170
)


# ============================================================
# 40. SAVE VERSION 11B OUTPUTS
# ============================================================

write.csv(
  candidate_predictions,
  paste0(
    output_prefix,
    "_candidate_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  overall_candidate_summary,
  paste0(
    output_prefix,
    "_overall_candidate_summary.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  summary_by_test_season,
  paste0(
    output_prefix,
    "_summary_by_test_season.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  summary_by_week,
  paste0(
    output_prefix,
    "_summary_by_week.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  summary_by_games_bucket,
  paste0(
    output_prefix,
    "_summary_by_games_bucket.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  paired_vs_reference,
  paste0(
    output_prefix,
    "_paired_vs_reference.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  logistic_coefficients_by_test_season,
  paste0(
    output_prefix,
    "_logistic_coefficients_by_test_season.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  learned_blend_weights_by_test_season,
  paste0(
    output_prefix,
    "_learned_blend_weights_by_test_season.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  probability_reliability,
  paste0(
    output_prefix,
    "_probability_reliability.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  market_comparison,
  paste0(
    output_prefix,
    "_market_comparison.csv"
  ),
  row.names =
    FALSE
)


# ============================================================
# 41. CONSOLE REPORT
# ============================================================

cat("\n========================================\n")
cat("VERSION 11B COMPLETE\n")
cat("========================================\n\n")

cat("RATING ENGINE REMAINED FIXED:\n")
cat(
  "  Score Offense/Defense Huber + HL12\n"
)
cat(
  "  component lambda = ",
  component_lambda,
  "\n",
  sep = ""
)
cat(
  "  Huber k = ",
  huber_k,
  "\n\n",
  sep = ""
)

cat("STRICT EXPANDING TEST:\n")
cat(
  "  Calibration seasons begin:",
  min(
    available_seasons
  ),
  "\n"
)
cat(
  "  First unseen test season:",
  min(
    test_seasons
  ),
  "\n"
)
cat(
  "  Last unseen test season:",
  max(
    test_seasons
  ),
  "\n"
)
cat(
  "  Test seasons:",
  length(
    test_seasons
  ),
  "\n"
)
cat(
  "  Unique test games:",
  nrow(
    candidate_predictions[
      candidate_predictions$candidate ==
        reference_candidate,
    ]
  ),
  "\n\n"
)

cat("OVERALL CANDIDATE RESULTS:\n\n")

print(
  overall_candidate_summary[
    ,
    c(
      "candidate",
      "games",
      "margin_MAE",
      "margin_RMSE",
      "margin_bias",
      "winner_accuracy",
      "brier_score",
      "log_loss",
      "ECE",
      "max_calibration_error",
      "market_margin_MAE",
      "model_MAE_gap_vs_market"
    )
  ],
  row.names =
    FALSE
)

cat("\nBEST CANDIDATES:\n")
cat(
  "  Lowest margin MAE:",
  best_margin_candidate,
  "\n"
)
cat(
  "  Lowest Brier:",
  best_brier_candidate,
  "\n"
)
cat(
  "  Lowest log loss:",
  best_logloss_candidate,
  "\n\n"
)

cat(
  "PAIRED DIFFERENCES VS CURRENT VERSION 11 FORECAST\n",
  "(negative differences are improvements):\n\n",
  sep = ""
)

print(
  paired_vs_reference,
  row.names =
    FALSE
)

cat("\nLATEST EXPANDING NFL LOGISTIC COEFFICIENTS:\n\n")

latest_test_season <- max(
  logistic_coefficients_by_test_season$test_season
)

print(
  logistic_coefficients_by_test_season[
    logistic_coefficients_by_test_season$test_season ==
      latest_test_season,
  ],
  row.names =
    FALSE
)

cat("\nLATEST LEARNED BLEND WEIGHTS:\n\n")

print(
  learned_blend_weights_by_test_season[
    learned_blend_weights_by_test_season$test_season ==
      latest_test_season,
  ],
  row.names =
    FALSE
)

cat("\nEARLY-SEASON PERFORMANCE BY GAMES-PLAYED BUCKET:\n\n")

print(
  summary_by_games_bucket[
    ,
    c(
      "games_played_bucket",
      "candidate",
      "games",
      "margin_MAE",
      "margin_RMSE",
      "winner_accuracy",
      "brier_score",
      "log_loss"
    )
  ],
  row.names =
    FALSE
)

cat("\nINTERPRETATION:\n")
cat(
  "  1. The power-rating engine was never retuned.\n"
)
cat(
  "  2. Every test season was completely unseen when calibration parameters were estimated.\n"
)
cat(
  "  3. Learned blend weights use prior seasons only and are constrained to [0,1].\n"
)
cat(
  "  4. Expanding NFL logistic coefficients use prior seasons only.\n"
)
cat(
  "  5. Market data remain evaluation-only benchmarks.\n"
)

cat(
  "\nCSV outputs and PNG plots written with prefix: ",
  output_prefix,
  "\n",
  sep = ""
)

# ============================================================
# END VERSION 11B
# ============================================================

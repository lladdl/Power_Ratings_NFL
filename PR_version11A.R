# ============================================================
# FOOTBALL POWER RATINGS - VERSION 11
# REAL NFL MULTI-SEASON BACKTEST USING nflreadr::load_schedules()
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
output_prefix <- "version11"

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
# 21. SUMMARY FUNCTION
# ============================================================

summarize_predictions <- function(predictions) {
  non_tie <- predictions[!is.na(predictions$winner_correct), ]
  score_errors <- c(predictions$home_score_error, predictions$away_score_error)
  mkt <- !is.na(predictions$spread_line)
  ml <- !is.na(predictions$market_home_win_probability)
  
  data.frame(
    games = nrow(predictions),
    non_tied_games = nrow(non_tie),
    margin_MAE = mean(predictions$absolute_margin_error),
    margin_RMSE = sqrt(mean(predictions$squared_margin_error)),
    margin_bias = mean(predictions$margin_error),
    margin_correlation = cor(predictions$predicted_margin, predictions$actual_margin),
    all_score_MAE = mean(abs(score_errors)),
    all_score_RMSE = sqrt(mean(score_errors^2)),
    total_score_MAE = mean(abs(predictions$total_score_error)),
    total_score_RMSE = sqrt(mean(predictions$total_score_error^2)),
    winner_accuracy = mean(non_tie$winner_correct),
    brier_score = mean(predictions$brier_score),
    log_loss = mean(predictions$log_loss, na.rm = TRUE),
    mean_predicted_home_win_probability = mean(predictions$home_win_probability),
    actual_home_win_rate_soft = mean(predictions$actual_home_win_soft),
    baseline_margin_MAE = mean(predictions$baseline_absolute_margin_error),
    baseline_margin_RMSE = sqrt(mean(predictions$baseline_squared_margin_error)),
    MAE_improvement_vs_HFA_baseline =
      mean(predictions$baseline_absolute_margin_error) - mean(predictions$absolute_margin_error),
    RMSE_improvement_vs_HFA_baseline =
      sqrt(mean(predictions$baseline_squared_margin_error)) -
      sqrt(mean(predictions$squared_margin_error)),
    market_games = sum(mkt),
    market_margin_MAE = if (any(mkt)) mean(predictions$market_absolute_margin_error[mkt]) else NA_real_,
    market_margin_RMSE = if (any(mkt)) sqrt(mean(predictions$market_squared_margin_error[mkt])) else NA_real_,
    model_MAE_gap_vs_market = if (any(mkt))
      mean(predictions$absolute_margin_error[mkt]) -
      mean(predictions$market_absolute_margin_error[mkt]) else NA_real_,
    moneyline_games = sum(ml),
    market_brier_score = if (any(ml)) mean(predictions$market_brier_score[ml], na.rm = TRUE) else NA_real_,
    market_log_loss = if (any(ml)) mean(predictions$market_log_loss[ml], na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 22. AGGREGATE EVALUATION TABLES
# ============================================================

overall_summary <- summarize_predictions(all_oos_predictions)

season_rows <- list()
for (i in seq_along(available_seasons)) {
  s <- available_seasons[i]
  x <- summarize_predictions(all_oos_predictions[all_oos_predictions$season == s, ])
  x$season <- s
  season_rows[[i]] <- x
}
summary_by_season <- do.call(rbind, season_rows)
summary_by_season <- summary_by_season[, c("season", setdiff(names(summary_by_season), "season"))]

available_weeks <- sort(unique(all_oos_predictions$week))
week_rows <- list()
for (i in seq_along(available_weeks)) {
  w <- available_weeks[i]
  x <- summarize_predictions(all_oos_predictions[all_oos_predictions$week == w, ])
  x$week <- w
  week_rows[[i]] <- x
}
summary_by_week <- do.call(rbind, week_rows)
summary_by_week <- summary_by_week[, c("week", setdiff(names(summary_by_week), "week"))]

# Era groups are purely reporting summaries, not model inputs.
all_oos_predictions$era <- cut(
  all_oos_predictions$season,
  breaks = c(2004, 2009, 2014, 2019, 2025),
  labels = c("2005-2009", "2010-2014", "2015-2019", "2020-2025"),
  include.lowest = TRUE
)
era_rows <- list()
for (i in seq_along(levels(all_oos_predictions$era))) {
  e <- levels(all_oos_predictions$era)[i]
  x <- summarize_predictions(all_oos_predictions[all_oos_predictions$era == e, ])
  x$era <- e
  era_rows[[i]] <- x
}
summary_by_era <- do.call(rbind, era_rows)
summary_by_era <- summary_by_era[, c("era", setdiff(names(summary_by_era), "era"))]

# Final power table from each season.
final_rows <- list()
for (i in seq_along(available_seasons)) {
  s <- available_seasons[i]
  x <- weekly_power_ratings[weekly_power_ratings$season == s, ]
  final_rows[[i]] <- x[x$week == max(x$week), ]
}
final_ratings_by_season <- do.call(rbind, final_rows)
rownames(final_ratings_by_season) <- NULL

# ============================================================
# 23. PROBABILITY RELIABILITY TABLE
# ============================================================

calibration_data <- all_oos_predictions
calibration_data$probability_bin <- cut(
  calibration_data$home_win_probability,
  breaks = probability_bin_breaks,
  include.lowest = TRUE,
  right = TRUE
)
calibration_rows <- list()
pos <- 1
for (bin in levels(calibration_data$probability_bin)) {
  x <- calibration_data[calibration_data$probability_bin == bin, ]
  if (!nrow(x)) next
  calibration_rows[[pos]] <- data.frame(
    probability_bin = bin,
    games = nrow(x),
    mean_predicted_home_win_probability = mean(x$home_win_probability),
    actual_home_win_rate_soft = mean(x$actual_home_win_soft),
    calibration_difference =
      mean(x$actual_home_win_soft) - mean(x$home_win_probability),
    stringsAsFactors = FALSE
  )
  pos <- pos + 1
}
probability_calibration <- do.call(rbind, calibration_rows)

# ============================================================
# 24. MODEL VS MARKET BY SEASON
# ============================================================

market_rows <- list()
pos <- 1
for (s in available_seasons) {
  x <- all_oos_predictions[
    all_oos_predictions$season == s & !is.na(all_oos_predictions$spread_line),
  ]
  if (!nrow(x)) next
  market_rows[[pos]] <- data.frame(
    season = s,
    games = nrow(x),
    model_margin_MAE = mean(x$absolute_margin_error),
    market_margin_MAE = mean(x$market_absolute_margin_error),
    model_minus_market_MAE =
      mean(x$absolute_margin_error) - mean(x$market_absolute_margin_error),
    model_margin_RMSE = sqrt(mean(x$squared_margin_error)),
    market_margin_RMSE = sqrt(mean(x$market_squared_margin_error)),
    stringsAsFactors = FALSE
  )
  pos <- pos + 1
}
market_comparison <- if (length(market_rows)) do.call(rbind, market_rows) else data.frame()

# ============================================================
# 25. SAVE CSV OUTPUTS
# ============================================================

write.csv(all_oos_predictions, paste0(output_prefix, "_all_oos_predictions.csv"), row.names = FALSE)
write.csv(overall_summary, paste0(output_prefix, "_overall_summary.csv"), row.names = FALSE)
write.csv(summary_by_season, paste0(output_prefix, "_summary_by_season.csv"), row.names = FALSE)
write.csv(summary_by_week, paste0(output_prefix, "_summary_by_week.csv"), row.names = FALSE)
write.csv(summary_by_era, paste0(output_prefix, "_summary_by_era.csv"), row.names = FALSE)
write.csv(weekly_power_ratings, paste0(output_prefix, "_weekly_power_ratings.csv"), row.names = FALSE)
write.csv(final_ratings_by_season, paste0(output_prefix, "_final_ratings_by_season.csv"), row.names = FALSE)
write.csv(hfa_by_week, paste0(output_prefix, "_hfa_by_week.csv"), row.names = FALSE)
write.csv(probability_calibration, paste0(output_prefix, "_probability_calibration.csv"), row.names = FALSE)
write.csv(market_comparison, paste0(output_prefix, "_market_comparison.csv"), row.names = FALSE)

# ============================================================
# 26. PLOTS
# ============================================================

p1 <- ggplot(summary_by_season, aes(x = season, y = margin_MAE)) +
  geom_line() + geom_point() +
  geom_line(aes(y = baseline_margin_MAE), linetype = "dashed") +
  labs(title = "Version 11: NFL Margin MAE by Season",
       subtitle = "Solid = power-rating model; dashed = generic HFA-only baseline",
       x = "Season", y = "Margin MAE") + theme_minimal()
ggsave(paste0(output_prefix, "_margin_MAE_by_season.png"), p1, width = 11, height = 7, dpi = 170)

p2 <- ggplot(summary_by_week, aes(x = week, y = margin_MAE)) +
  geom_line() + geom_point() +
  geom_line(aes(y = baseline_margin_MAE), linetype = "dashed") +
  labs(title = "Version 11: NFL Forecast Error by Week",
       subtitle = "Tests whether results-only ratings improve as information accumulates",
       x = "Target Week", y = "Margin MAE") + theme_minimal()
ggsave(paste0(output_prefix, "_margin_MAE_by_week.png"), p2, width = 10, height = 7, dpi = 170)

p3 <- ggplot(summary_by_season, aes(x = season, y = brier_score)) +
  geom_line() + geom_point() + geom_hline(yintercept = 0.25, linetype = "dashed") +
  labs(title = "Version 11: Brier Score by NFL Season",
       subtitle = "Dashed = uninformed 50/50 benchmark",
       x = "Season", y = "Brier Score") + theme_minimal()
ggsave(paste0(output_prefix, "_brier_by_season.png"), p3, width = 11, height = 7, dpi = 170)

p4 <- ggplot(probability_calibration,
             aes(x = mean_predicted_home_win_probability,
                 y = actual_home_win_rate_soft)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_line() + geom_point(aes(size = games)) +
  coord_equal(xlim = c(0,1), ylim = c(0,1)) +
  labs(title = "Version 11: Real NFL Win-Probability Calibration",
       subtitle = "Probability coefficients remain frozen from synthetic 9C-MC",
       x = "Predicted Home Win Probability", y = "Observed Home Win Rate",
       size = "Games") + theme_minimal()
ggsave(paste0(output_prefix, "_probability_calibration.png"), p4, width = 8, height = 8, dpi = 170)

p5 <- ggplot(hfa_by_week, aes(x = target_week, y = estimated_HFA, group = season)) +
  geom_line(alpha = 0.25) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2) +
  geom_hline(yintercept = hfa_prior, linetype = "dashed") +
  labs(title = "Version 11: Estimated NFL Home-Field Advantage",
       subtitle = "Thin = individual seasons; thick = cross-season mean",
       x = "Target Week", y = "Estimated HFA") + theme_minimal()
ggsave(paste0(output_prefix, "_HFA_by_week.png"), p5, width = 10, height = 7, dpi = 170)

if (nrow(market_comparison)) {
  p6 <- ggplot(market_comparison, aes(x = season)) +
    geom_line(aes(y = model_margin_MAE)) +
    geom_point(aes(y = model_margin_MAE)) +
    geom_line(aes(y = market_margin_MAE), linetype = "dashed") +
    geom_point(aes(y = market_margin_MAE), shape = 1) +
    labs(title = "Version 11: Model vs Closing Spread",
         subtitle = "Solid = power rating; dashed = nflverse closing spread",
         x = "Season", y = "Margin MAE") + theme_minimal()
  ggsave(paste0(output_prefix, "_model_vs_market.png"), p6, width = 11, height = 7, dpi = 170)
}

# ============================================================
# 27. CONSOLE REPORT
# ============================================================

cat("\n========================================\n")
cat("VERSION 11 NFL BACKTEST COMPLETE\n")
cat("========================================\n\n")
cat("Locked model: Score Offense/Defense Huber + HL12\n")
cat("component lambda =", component_lambda, "\n")
cat("Huber k =", huber_k, "\n")
cat("HFA prior =", hfa_prior, "\n")
cat("Probability = logit^-1(", win_probability_intercept, " + ",
    win_probability_slope, " * standardized margin)\n\n", sep = "")
cat("Seasons:", min(available_seasons), "through", max(available_seasons), "\n")
cat("Total REG games loaded:", nrow(games), "\n")
cat("Total Week 2+ OOS forecasts:", nrow(all_oos_predictions), "\n")
cat("Neutral-site OOS games:", sum(all_oos_predictions$neutral_site), "\n\n")

cat("OVERALL OOS RESULTS:\n\n")
print(overall_summary, row.names = FALSE)

cat("\nRESULTS BY SEASON:\n\n")
print(summary_by_season[, c(
  "season", "games", "margin_MAE", "margin_RMSE", "margin_bias",
  "winner_accuracy", "brier_score", "log_loss", "baseline_margin_MAE",
  "MAE_improvement_vs_HFA_baseline", "market_margin_MAE",
  "model_MAE_gap_vs_market"
)], row.names = FALSE)

cat("\nRESULTS BY WEEK:\n\n")
print(summary_by_week[, c(
  "week", "games", "margin_MAE", "margin_RMSE", "winner_accuracy",
  "brier_score", "log_loss", "baseline_margin_MAE"
)], row.names = FALSE)

cat("\nPROBABILITY CALIBRATION:\n\n")
print(probability_calibration, row.names = FALSE)

cat("\nNOTES:\n")
cat("1. Week 1 is excluded from the primary model forecast test.\n")
cat("2. Every season resets team strength to equal.\n")
cat("3. Closing spreads/moneylines are benchmarks only, never features.\n")
cat("4. Neutral-site games receive zero HFA.\n")
cat("5. Hyperparameters and probability coefficients are frozen from the synthetic research phase.\n")

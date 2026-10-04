# ============================================================
# FOOTBALL POWER RATINGS - VERSION 6D
# SELECTIVE SMOOTH ADAPTATION
# ============================================================
#
# MODELS:
#
# 1. Fixed
#       cap = 21
#       team lambda = 2
#
# 2. Estimated Always Smooth
#       estimated parity controls smooth cap + lambda
#
# 3. Estimated Selective Smooth
#       estimated SD < 4.0:
#           adaptive low-SD model
#
#       estimated SD 4.0 through 6.5:
#           fixed cap = 21
#           fixed lambda = 2
#
#       estimated SD > 6.5:
#           adaptive high-SD model
#
# 4. Oracle Always Smooth
#       same as Always Smooth, but knows true hidden SD
#
# 5. Oracle Selective Smooth
#       same as Selective Smooth, but knows true hidden SD
#
# ============================================================


library(ggplot2)



# ============================================================
# 1. LOAD SCHEDULE
# ============================================================

base_games <- read.csv(
  "football_power_ratings_games.csv",
  stringsAsFactors = FALSE
)


schedule <- base_games[
  ,
  c(
    "game_id",
    "week",
    "home_team",
    "away_team"
  )
]


teams <- sort(
  unique(
    c(
      schedule$home_team,
      schedule$away_team
    )
  )
)


n_teams <- length(teams)

max_week <- max(schedule$week)


cat("\nTeams:", n_teams, "\n")
cat("Games per season:", nrow(schedule), "\n")
cat("Weeks:", max_week, "\n")



# ============================================================
# 2. SIMULATION SETTINGS
# ============================================================

true_hfa <- 2.7

noise_sd <- 8.5

hfa_prior <- 2.5


true_sd_values <- c(
  2,
  3,
  4,
  5,
  6,
  7,
  8
)



# ============================================================
# 3. VERSION 6D SETTINGS
# ============================================================

# Week 5 prediction uses results through Week 4.

adaptive_start_week <- 5


# Lambda crossover discovered previously

lambda_parity_threshold <- 4.5


# Selective-adaptation region

selective_low_cutoff <- 4.0

selective_high_cutoff <- 6.5


# Monte Carlo sizes

n_calibration_per_sd <- 500

n_evaluation_per_sd <- 1000



# ============================================================
# 4. GENERATE TRUE RATINGS WITH EXACT SD
# ============================================================

generate_true_ratings <- function(
    league_teams,
    target_sd
) {
  
  ratings <- rnorm(
    length(league_teams),
    mean = 0,
    sd = 1
  )
  
  
  # Center ratings
  
  ratings <- ratings - mean(ratings)
  
  
  # Scale to SD = 1
  
  ratings <- ratings / sd(ratings)
  
  
  # Scale to desired SD
  
  ratings <- ratings * target_sd
  
  
  names(ratings) <- league_teams
  
  
  return(ratings)
}



# ============================================================
# 5. SIMULATE ONE SEASON
# ============================================================

simulate_season <- function(
    schedule,
    true_ratings,
    true_hfa = 2.7,
    noise_sd = 8.5
) {
  
  sim_games <- schedule
  
  
  # ----------------------------------------------------------
  # TRUE EXPECTED MARGIN
  # ----------------------------------------------------------
  
  sim_games$true_expected_margin <- (
    true_ratings[sim_games$home_team] -
      true_ratings[sim_games$away_team] +
      true_hfa
  )
  
  
  sim_games$true_expected_margin <- as.numeric(
    sim_games$true_expected_margin
  )
  
  
  # ----------------------------------------------------------
  # ACTUAL MARGIN
  # ----------------------------------------------------------
  
  sim_games$actual_margin <- round(
    sim_games$true_expected_margin +
      rnorm(
        nrow(sim_games),
        mean = 0,
        sd = noise_sd
      )
  )
  
  
  # ----------------------------------------------------------
  # REMOVE TIES
  # ----------------------------------------------------------
  
  zero_games <- which(
    sim_games$actual_margin == 0
  )
  
  
  if (length(zero_games) > 0) {
    
    sim_games$actual_margin[zero_games] <- sample(
      c(-1, 1),
      size = length(zero_games),
      replace = TRUE
    )
  }
  
  
  return(sim_games)
}



# ============================================================
# 6. HFA SHRINKAGE
# ============================================================

get_hfa_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <- (
    2 * nrow(data)
  ) / length(league_teams)
  
  
  if (avg_games_played <= 2) {
    
    return(10)
    
  } else if (avg_games_played <= 4) {
    
    return(8)
    
  } else if (avg_games_played <= 6) {
    
    return(5)
    
  } else {
    
    return(2)
  }
}



# ============================================================
# 7. DYNAMIC TEAM LAMBDA
# ============================================================

get_dynamic_team_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <- (
    2 * nrow(data)
  ) / length(league_teams)
  
  
  if (avg_games_played <= 2) {
    
    return(5)
    
  } else if (avg_games_played <= 4) {
    
    return(4)
    
  } else if (avg_games_played <= 6) {
    
    return(3)
    
  } else {
    
    return(2)
  }
}



# ============================================================
# 8. PARITY-BASED TEAM LAMBDA
# ============================================================

choose_team_lambda <- function(
    parity_sd,
    training_games,
    league_teams
) {
  
  if (parity_sd < lambda_parity_threshold) {
    
    return(
      get_dynamic_team_lambda(
        data = training_games,
        league_teams = league_teams
      )
    )
    
  } else {
    
    return(2)
  }
}



# ============================================================
# 9. SMOOTH MARGIN CAP FUNCTION
# ============================================================
#
# cap = 14 + 5.25 * (SD - 4)
#
# Lower bound = 14
# Upper bound = 35
#
# Examples:
#
# SD 4.0 -> 14.00
# SD 4.5 -> 16.63
# SD 5.0 -> 19.25
# SD 5.5 -> 21.88
# SD 6.0 -> 24.50
# SD 6.5 -> 27.13
# SD 7.0 -> 29.75
# SD 7.5 -> 32.38
# SD 8.0 -> 35.00
#
# ============================================================

choose_cap_smooth <- function(
    parity_sd
) {
  
  cap <- 14 +
    5.25 * (parity_sd - 4)
  
  
  # Lower bound
  
  cap <- max(
    14,
    cap
  )
  
  
  # Upper bound
  
  cap <- min(
    35,
    cap
  )
  
  
  return(cap)
}



# ============================================================
# 10. SELECTIVE SMOOTH SETTINGS
# ============================================================
#
# SD < 4:
#
#   adapt
#
#
# SD 4 through 6.5:
#
#   fixed lambda = 2
#   fixed cap = 21
#
#
# SD > 6.5:
#
#   adaptive smooth cap
#   lambda = 2
#
# ============================================================

choose_selective_settings <- function(
    parity_sd,
    training_games,
    league_teams
) {
  
  # ----------------------------------------------------------
  # LOW-SD / HIGH-PARITY REGIME
  # ----------------------------------------------------------
  
  if (parity_sd < selective_low_cutoff) {
    
    team_lambda <- get_dynamic_team_lambda(
      data = training_games,
      league_teams = league_teams
    )
    
    
    margin_cap <- choose_cap_smooth(
      parity_sd
    )
    
    
    regime <- "Low SD Adaptive"
    
    
    # ----------------------------------------------------------
    # MIDDLE REGIME
    # ----------------------------------------------------------
    
  } else if (parity_sd <= selective_high_cutoff) {
    
    team_lambda <- 2
    
    margin_cap <- 21
    
    regime <- "Fixed Middle"
    
    
    # ----------------------------------------------------------
    # HIGH-SD / LOW-PARITY REGIME
    # ----------------------------------------------------------
    
  } else {
    
    team_lambda <- 2
    
    
    margin_cap <- choose_cap_smooth(
      parity_sd
    )
    
    
    regime <- "High SD Adaptive"
  }
  
  
  return(
    list(
      team_lambda = team_lambda,
      margin_cap = margin_cap,
      regime = regime
    )
  )
}



# ============================================================
# 11. POWER RATING MODEL
# ============================================================

fit_power_ratings <- function(
    data,
    league_teams,
    margin_cap,
    team_lambda,
    hfa_lambda,
    hfa_prior = 2.5
) {
  
  n_games <- nrow(data)
  
  n_teams <- length(league_teams)
  
  
  # ----------------------------------------------------------
  # RESPONSE
  # ----------------------------------------------------------
  
  if (is.infinite(margin_cap)) {
    
    y <- data$actual_margin
    
  } else {
    
    y <- pmax(
      pmin(
        data$actual_margin,
        margin_cap
      ),
      -margin_cap
    )
  }
  
  
  # ----------------------------------------------------------
  # DESIGN MATRIX
  # ----------------------------------------------------------
  
  X <- matrix(
    0,
    nrow = n_games,
    ncol = n_teams + 1
  )
  
  
  colnames(X) <- c(
    "HFA",
    league_teams
  )
  
  
  X[, "HFA"] <- 1
  
  
  for (i in seq_len(n_games)) {
    
    home <- data$home_team[i]
    
    away <- data$away_team[i]
    
    
    X[i, home] <- 1
    
    X[i, away] <- -1
  }
  
  
  # ----------------------------------------------------------
  # PENALTY MATRIX
  # ----------------------------------------------------------
  
  penalty <- diag(
    c(
      hfa_lambda,
      rep(
        team_lambda,
        n_teams
      )
    )
  )
  
  
  # ----------------------------------------------------------
  # PRIOR TARGET
  # ----------------------------------------------------------
  
  prior_target <- c(
    hfa_prior,
    rep(
      0,
      n_teams
    )
  )
  
  
  # ----------------------------------------------------------
  # RIDGE SOLUTION
  # ----------------------------------------------------------
  
  beta_matrix <- solve(
    crossprod(X) + penalty,
    crossprod(X, y) +
      penalty %*% prior_target
  )
  
  
  beta <- as.numeric(
    beta_matrix
  )
  
  
  names(beta) <- colnames(X)
  
  
  estimated_hfa <- beta["HFA"]
  
  
  ratings <- beta[league_teams]
  
  
  # Center ratings around zero
  
  ratings <- ratings - mean(ratings)
  
  
  rating_table <- data.frame(
    team = league_teams,
    rating = as.numeric(ratings),
    stringsAsFactors = FALSE
  )
  
  
  return(
    list(
      ratings = rating_table,
      home_field = as.numeric(estimated_hfa)
    )
  )
}



# ============================================================
# 12. RAW-MARGIN PARITY ESTIMATOR
# ============================================================

estimate_observed_parity_sd <- function(
    training_games,
    league_teams,
    hfa_prior = 2.5
) {
  
  current_hfa_lambda <- get_hfa_lambda(
    data = training_games,
    league_teams = league_teams
  )
  
  
  parity_model <- fit_power_ratings(
    data = training_games,
    league_teams = league_teams,
    margin_cap = Inf,
    team_lambda = 2,
    hfa_lambda = current_hfa_lambda,
    hfa_prior = hfa_prior
  )
  
  
  observed_sd <- sd(
    parity_model$ratings$rating
  )
  
  
  return(observed_sd)
}



# ============================================================
# 13. BUILD PARITY CALIBRATION DATA
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "BUILDING PARITY CALIBRATION DATA\n"
)

cat(
  "========================================\n\n"
)


set.seed(6001)


calibration_rows <- list()

calibration_index <- 1


for (current_true_sd in true_sd_values) {
  
  cat(
    "Calibration SD:",
    current_true_sd,
    "\n"
  )
  
  
  for (s in 1:n_calibration_per_sd) {
    
    true_ratings <- generate_true_ratings(
      league_teams = teams,
      target_sd = current_true_sd
    )
    
    
    games <- simulate_season(
      schedule = schedule,
      true_ratings = true_ratings,
      true_hfa = true_hfa,
      noise_sd = noise_sd
    )
    
    
    for (completed_week in 2:(max_week - 1)) {
      
      training_games <- games[
        games$week <= completed_week,
      ]
      
      
      observed_sd <- estimate_observed_parity_sd(
        training_games = training_games,
        league_teams = teams,
        hfa_prior = hfa_prior
      )
      
      
      calibration_rows[[calibration_index]] <- data.frame(
        true_rating_SD = current_true_sd,
        week = completed_week,
        observed_rating_SD = observed_sd
      )
      
      
      calibration_index <- calibration_index + 1
    }
  }
}



calibration_data <- do.call(
  rbind,
  calibration_rows
)


rownames(calibration_data) <- NULL



# ============================================================
# 14. WEEK-SPECIFIC CALIBRATION MODELS
# ============================================================

calibration_coefficients <- data.frame(
  week = 2:(max_week - 1),
  intercept = NA_real_,
  slope = NA_real_,
  r_squared = NA_real_
)



for (i in seq_len(nrow(calibration_coefficients))) {
  
  current_week <- calibration_coefficients$week[i]
  
  
  current_data <- calibration_data[
    calibration_data$week == current_week,
  ]
  
  
  calibration_model <- lm(
    true_rating_SD ~ observed_rating_SD,
    data = current_data
  )
  
  
  calibration_coefficients$intercept[i] <-
    coef(calibration_model)[1]
  
  
  calibration_coefficients$slope[i] <-
    coef(calibration_model)[2]
  
  
  calibration_coefficients$r_squared[i] <-
    summary(calibration_model)$r.squared
}



cat(
  "\n========================================\n"
)

cat(
  "PARITY CALIBRATION COEFFICIENTS\n"
)

cat(
  "========================================\n\n"
)


print(
  calibration_coefficients
)



# ============================================================
# 15. APPLY PARITY CALIBRATION
# ============================================================

calibrate_parity_sd <- function(
    observed_sd,
    completed_week,
    calibration_coefficients
) {
  
  current_row <- calibration_coefficients[
    calibration_coefficients$week == completed_week,
  ]
  
  
  estimated_sd <- (
    current_row$intercept +
      current_row$slope * observed_sd
  )
  
  
  estimated_sd <- max(
    0,
    estimated_sd
  )
  
  
  return(
    as.numeric(
      estimated_sd
    )
  )
}



# ============================================================
# 16. SAFE CORRELATION
# ============================================================

safe_cor <- function(
    x,
    y
) {
  
  if (
    sd(x) == 0 ||
    sd(y) == 0
  ) {
    
    return(NA_real_)
  }
  
  
  return(
    cor(
      x,
      y
    )
  )
}



# ============================================================
# 17. FIT AND PREDICT ONE WEEK
# ============================================================

fit_predict_week <- function(
    training_games,
    test_games,
    league_teams,
    margin_cap,
    team_lambda,
    hfa_lambda,
    hfa_prior,
    true_ratings
) {
  
  model <- fit_power_ratings(
    data = training_games,
    league_teams = league_teams,
    margin_cap = margin_cap,
    team_lambda = team_lambda,
    hfa_lambda = hfa_lambda,
    hfa_prior = hfa_prior
  )
  
  
  rating_lookup <- model$ratings$rating
  
  
  names(rating_lookup) <-
    model$ratings$team
  
  
  predicted_margin <- (
    rating_lookup[test_games$home_team] -
      rating_lookup[test_games$away_team] +
      model$home_field
  )
  
  
  rating_rmse <- sqrt(
    mean(
      (
        rating_lookup[league_teams] -
          true_ratings[league_teams]
      )^2
    )
  )
  
  
  return(
    list(
      prediction = as.numeric(predicted_margin),
      rating_RMSE = rating_rmse
    )
  )
}



# ============================================================
# 18. EVALUATE ALL FIVE MODELS
# ============================================================

evaluate_models <- function(
    games,
    true_ratings,
    true_rating_sd,
    league_teams,
    calibration_coefficients,
    adaptive_start_week = 5
) {
  
  model_names <- c(
    "fixed",
    "estimated_always_smooth",
    "estimated_selective",
    "oracle_always_smooth",
    "oracle_selective"
  )
  
  
  # ----------------------------------------------------------
  # STORAGE
  # ----------------------------------------------------------
  
  prediction_storage <- list()
  
  rating_rmse_storage <- list()
  
  
  for (model_name in model_names) {
    
    prediction_storage[[model_name]] <- list()
    
    rating_rmse_storage[[model_name]] <- numeric()
  }
  
  
  diagnostic_rows <- list()
  
  diagnostic_index <- 1
  
  
  
  # ==========================================================
  # WALK-FORWARD LOOP
  # ==========================================================
  
  for (target_week in 2:max_week) {
    
    training_games <- games[
      games$week < target_week,
    ]
    
    
    test_games <- games[
      games$week == target_week,
    ]
    
    
    completed_week <- target_week - 1
    
    
    current_hfa_lambda <- get_hfa_lambda(
      data = training_games,
      league_teams = league_teams
    )
    
    
    # ========================================================
    # PARITY ESTIMATION
    # ========================================================
    
    observed_sd <- NA_real_
    
    estimated_sd <- NA_real_
    
    
    if (completed_week >= 2) {
      
      observed_sd <- estimate_observed_parity_sd(
        training_games = training_games,
        league_teams = league_teams,
        hfa_prior = hfa_prior
      )
      
      
      estimated_sd <- calibrate_parity_sd(
        observed_sd = observed_sd,
        completed_week = completed_week,
        calibration_coefficients = calibration_coefficients
      )
    }
    
    
    # ========================================================
    # SETTINGS BEFORE ADAPTATION BEGINS
    # ========================================================
    
    if (target_week < adaptive_start_week) {
      
      # Fixed
      
      fixed_cap <- 21
      fixed_lambda <- 2
      
      
      # Estimated Always Smooth
      
      est_always_cap <- 21
      est_always_lambda <- 2
      
      
      # Estimated Selective
      
      est_selective_cap <- 21
      est_selective_lambda <- 2
      est_selective_regime <- "Pre-Adaptive"
      
      
      # Oracle Always Smooth
      
      oracle_always_cap <- 21
      oracle_always_lambda <- 2
      
      
      # Oracle Selective
      
      oracle_selective_cap <- 21
      oracle_selective_lambda <- 2
      oracle_selective_regime <- "Pre-Adaptive"
      
      
      # ========================================================
      # ACTIVE ADAPTIVE PERIOD
      # ========================================================
      
    } else {
      
      # ------------------------------------------------------
      # FIXED
      # ------------------------------------------------------
      
      fixed_cap <- 21
      
      fixed_lambda <- 2
      
      
      # ------------------------------------------------------
      # ESTIMATED ALWAYS SMOOTH
      # ------------------------------------------------------
      
      est_always_cap <- choose_cap_smooth(
        estimated_sd
      )
      
      
      est_always_lambda <- choose_team_lambda(
        parity_sd = estimated_sd,
        training_games = training_games,
        league_teams = league_teams
      )
      
      
      # ------------------------------------------------------
      # ESTIMATED SELECTIVE
      # ------------------------------------------------------
      
      estimated_selective_settings <-
        choose_selective_settings(
          parity_sd = estimated_sd,
          training_games = training_games,
          league_teams = league_teams
        )
      
      
      est_selective_cap <-
        estimated_selective_settings$margin_cap
      
      
      est_selective_lambda <-
        estimated_selective_settings$team_lambda
      
      
      est_selective_regime <-
        estimated_selective_settings$regime
      
      
      # ------------------------------------------------------
      # ORACLE ALWAYS SMOOTH
      # ------------------------------------------------------
      
      oracle_always_cap <- choose_cap_smooth(
        true_rating_sd
      )
      
      
      oracle_always_lambda <- choose_team_lambda(
        parity_sd = true_rating_sd,
        training_games = training_games,
        league_teams = league_teams
      )
      
      
      # ------------------------------------------------------
      # ORACLE SELECTIVE
      # ------------------------------------------------------
      
      oracle_selective_settings <-
        choose_selective_settings(
          parity_sd = true_rating_sd,
          training_games = training_games,
          league_teams = league_teams
        )
      
      
      oracle_selective_cap <-
        oracle_selective_settings$margin_cap
      
      
      oracle_selective_lambda <-
        oracle_selective_settings$team_lambda
      
      
      oracle_selective_regime <-
        oracle_selective_settings$regime
    }
    
    
    # ========================================================
    # MODEL CONFIGURATIONS
    # ========================================================
    
    configs <- list(
      fixed = list(
        cap = fixed_cap,
        lambda = fixed_lambda
      ),
      
      estimated_always_smooth = list(
        cap = est_always_cap,
        lambda = est_always_lambda
      ),
      
      estimated_selective = list(
        cap = est_selective_cap,
        lambda = est_selective_lambda
      ),
      
      oracle_always_smooth = list(
        cap = oracle_always_cap,
        lambda = oracle_always_lambda
      ),
      
      oracle_selective = list(
        cap = oracle_selective_cap,
        lambda = oracle_selective_lambda
      )
    )
    
    
    # ========================================================
    # FIT EACH MODEL
    # ========================================================
    
    for (model_name in model_names) {
      
      result <- fit_predict_week(
        training_games = training_games,
        test_games = test_games,
        league_teams = league_teams,
        margin_cap = configs[[model_name]]$cap,
        team_lambda = configs[[model_name]]$lambda,
        hfa_lambda = current_hfa_lambda,
        hfa_prior = hfa_prior,
        true_ratings = true_ratings
      )
      
      
      current_predictions <- data.frame(
        week = target_week,
        actual_margin = test_games$actual_margin,
        predicted_margin = result$prediction
      )
      
      
      current_predictions$error <- (
        current_predictions$actual_margin -
          current_predictions$predicted_margin
      )
      
      
      current_predictions$correct_winner <- (
        current_predictions$actual_margin > 0
      ) == (
        current_predictions$predicted_margin > 0
      )
      
      
      next_position <- (
        length(
          prediction_storage[[model_name]]
        ) + 1
      )
      
      
      prediction_storage[[model_name]][[next_position]] <-
        current_predictions
      
      
      rating_rmse_storage[[model_name]] <- c(
        rating_rmse_storage[[model_name]],
        result$rating_RMSE
      )
    }
    
    
    # ========================================================
    # WEEKLY DIAGNOSTICS
    # ========================================================
    
    diagnostic_rows[[diagnostic_index]] <- data.frame(
      target_week = target_week,
      true_rating_SD = true_rating_sd,
      estimated_rating_SD = estimated_sd,
      
      parity_absolute_error = abs(
        estimated_sd -
          true_rating_sd
      ),
      
      estimated_always_cap = est_always_cap,
      
      estimated_selective_cap = est_selective_cap,
      
      oracle_always_cap = oracle_always_cap,
      
      oracle_selective_cap = oracle_selective_cap,
      
      estimated_selective_regime =
        est_selective_regime,
      
      oracle_selective_regime =
        oracle_selective_regime,
      
      selective_regime_correct = (
        est_selective_regime ==
          oracle_selective_regime
      ),
      
      estimated_selective_lambda =
        est_selective_lambda,
      
      oracle_selective_lambda =
        oracle_selective_lambda,
      
      selective_lambda_correct = (
        est_selective_lambda ==
          oracle_selective_lambda
      ),
      
      stringsAsFactors = FALSE
    )
    
    
    diagnostic_index <- diagnostic_index + 1
  }
  
  
  # ==========================================================
  # SUMMARIZE EACH MODEL
  # ==========================================================
  
  model_results <- list()
  
  result_index <- 1
  
  
  for (model_name in model_names) {
    
    predictions <- do.call(
      rbind,
      prediction_storage[[model_name]]
    )
    
    
    model_results[[result_index]] <- data.frame(
      model = model_name,
      
      MAE = mean(
        abs(
          predictions$error
        )
      ),
      
      RMSE = sqrt(
        mean(
          predictions$error^2
        )
      ),
      
      Accuracy = mean(
        predictions$correct_winner
      ),
      
      Correlation = safe_cor(
        predictions$predicted_margin,
        predictions$actual_margin
      ),
      
      Bias = mean(
        predictions$error
      ),
      
      Rating_RMSE = mean(
        rating_rmse_storage[[model_name]]
      ),
      
      stringsAsFactors = FALSE
    )
    
    
    result_index <- result_index + 1
  }
  
  
  return(
    list(
      metrics = do.call(
        rbind,
        model_results
      ),
      
      diagnostics = do.call(
        rbind,
        diagnostic_rows
      )
    )
  )
}



# ============================================================
# 19. FINAL EVALUATION MONTE CARLO
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "STARTING VERSION 6D EVALUATION\n"
)

cat(
  "========================================\n\n"
)


set.seed(6004)


season_result_rows <- list()

diagnostic_result_rows <- list()


result_index <- 1

diagnostic_index <- 1



for (current_true_sd in true_sd_values) {
  
  cat(
    "\n========================================\n"
  )
  
  cat(
    "TRUE RATING SD:",
    current_true_sd,
    "\n"
  )
  
  cat(
    "========================================\n"
  )
  
  
  for (s in 1:n_evaluation_per_sd) {
    
    # --------------------------------------------------------
    # HIDDEN TRUE RATINGS
    # --------------------------------------------------------
    
    true_ratings <- generate_true_ratings(
      league_teams = teams,
      target_sd = current_true_sd
    )
    
    
    # --------------------------------------------------------
    # SIMULATE SEASON
    # --------------------------------------------------------
    
    games <- simulate_season(
      schedule = schedule,
      true_ratings = true_ratings,
      true_hfa = true_hfa,
      noise_sd = noise_sd
    )
    
    
    # --------------------------------------------------------
    # EVALUATE FIVE MODELS
    # --------------------------------------------------------
    
    current_result <- evaluate_models(
      games = games,
      true_ratings = true_ratings,
      true_rating_sd = current_true_sd,
      league_teams = teams,
      calibration_coefficients = calibration_coefficients,
      adaptive_start_week = adaptive_start_week
    )
    
    
    # --------------------------------------------------------
    # SAVE MODEL METRICS
    # --------------------------------------------------------
    
    current_metrics <- current_result$metrics
    
    
    current_metrics$true_rating_SD <-
      current_true_sd
    
    
    current_metrics$simulation <- s
    
    
    season_result_rows[[result_index]] <-
      current_metrics
    
    
    result_index <- result_index + 1
    
    
    # --------------------------------------------------------
    # SAVE WEEKLY DIAGNOSTICS
    # --------------------------------------------------------
    
    current_diagnostics <-
      current_result$diagnostics
    
    
    current_diagnostics$simulation <- s
    
    
    diagnostic_result_rows[[diagnostic_index]] <-
      current_diagnostics
    
    
    diagnostic_index <- diagnostic_index + 1
    
    
    # --------------------------------------------------------
    # PROGRESS
    # --------------------------------------------------------
    
    if (s %% 100 == 0) {
      
      cat(
        "SD",
        current_true_sd,
        "- completed",
        s,
        "of",
        n_evaluation_per_sd,
        "\n"
      )
    }
  }
}



# ============================================================
# 20. COMBINE RESULTS
# ============================================================

season_results <- do.call(
  rbind,
  season_result_rows
)


rownames(season_results) <- NULL



adaptive_diagnostics <- do.call(
  rbind,
  diagnostic_result_rows
)


rownames(adaptive_diagnostics) <- NULL



# ============================================================
# 21. MODEL LABELS
# ============================================================

model_labels <- c(
  fixed = "Fixed",
  
  estimated_always_smooth =
    "Estimated Always Smooth",
  
  estimated_selective =
    "Estimated Selective Smooth",
  
  oracle_always_smooth =
    "Oracle Always Smooth",
  
  oracle_selective =
    "Oracle Selective Smooth"
)


season_results$model_label <- model_labels[
  season_results$model
]



# ============================================================
# 22. OVERALL SUMMARY
# ============================================================

overall_summary <- aggregate(
  cbind(
    MAE,
    RMSE,
    Accuracy,
    Correlation,
    Rating_RMSE
  ) ~
    model +
    model_label,
  data = season_results,
  FUN = mean
)


overall_summary <- overall_summary[
  order(
    overall_summary$MAE
  ),
]


rownames(overall_summary) <- NULL



cat(
  "\n\n========================================\n"
)

cat(
  "OVERALL MODEL COMPARISON\n"
)

cat(
  "========================================\n\n"
)


print(
  overall_summary
)



# ============================================================
# 23. RESULTS BY TRUE SD
# ============================================================

summary_by_sd <- aggregate(
  cbind(
    MAE,
    RMSE,
    Accuracy,
    Rating_RMSE
  ) ~
    true_rating_SD +
    model +
    model_label,
  data = season_results,
  FUN = mean
)


summary_by_sd <- summary_by_sd[
  order(
    summary_by_sd$true_rating_SD,
    summary_by_sd$MAE
  ),
]


rownames(summary_by_sd) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "RESULTS BY TRUE RATING SD\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_by_sd
)



# ============================================================
# 24. CREATE WIDE MAE TABLE
# ============================================================

mae_data <- season_results[
  ,
  c(
    "true_rating_SD",
    "simulation",
    "model",
    "MAE"
  )
]


mae_wide <- reshape(
  mae_data,
  idvar = c(
    "true_rating_SD",
    "simulation"
  ),
  timevar = "model",
  direction = "wide"
)



# ============================================================
# 25. PAIRED DIFFERENCES
# ============================================================

mae_wide$always_vs_fixed <- (
  mae_wide$MAE.estimated_always_smooth -
    mae_wide$MAE.fixed
)


mae_wide$selective_vs_fixed <- (
  mae_wide$MAE.estimated_selective -
    mae_wide$MAE.fixed
)


mae_wide$selective_vs_always <- (
  mae_wide$MAE.estimated_selective -
    mae_wide$MAE.estimated_always_smooth
)


mae_wide$oracle_always_vs_fixed <- (
  mae_wide$MAE.oracle_always_smooth -
    mae_wide$MAE.fixed
)


mae_wide$oracle_selective_vs_fixed <- (
  mae_wide$MAE.oracle_selective -
    mae_wide$MAE.fixed
)


mae_wide$oracle_selective_vs_oracle_always <- (
  mae_wide$MAE.oracle_selective -
    mae_wide$MAE.oracle_always_smooth
)



paired_summary <- data.frame(
  comparison = c(
    "Estimated Always Smooth - Fixed",
    "Estimated Selective - Fixed",
    "Estimated Selective - Always Smooth",
    "Oracle Always Smooth - Fixed",
    "Oracle Selective - Fixed",
    "Oracle Selective - Oracle Always"
  ),
  
  mean_difference = c(
    mean(
      mae_wide$always_vs_fixed
    ),
    
    mean(
      mae_wide$selective_vs_fixed
    ),
    
    mean(
      mae_wide$selective_vs_always
    ),
    
    mean(
      mae_wide$oracle_always_vs_fixed
    ),
    
    mean(
      mae_wide$oracle_selective_vs_fixed
    ),
    
    mean(
      mae_wide$oracle_selective_vs_oracle_always
    )
  ),
  
  stringsAsFactors = FALSE
)



# ============================================================
# 26. MONTE CARLO INTERVALS
# ============================================================

difference_columns <- list(
  mae_wide$always_vs_fixed,
  mae_wide$selective_vs_fixed,
  mae_wide$selective_vs_always,
  mae_wide$oracle_always_vs_fixed,
  mae_wide$oracle_selective_vs_fixed,
  mae_wide$oracle_selective_vs_oracle_always
)


paired_summary$MC_SE <- NA_real_

paired_summary$lower_95 <- NA_real_

paired_summary$upper_95 <- NA_real_



for (i in seq_along(difference_columns)) {
  
  current_difference <- difference_columns[[i]]
  
  
  current_mean <- mean(
    current_difference
  )
  
  
  current_se <- sd(
    current_difference
  ) / sqrt(
    length(
      current_difference
    )
  )
  
  
  paired_summary$MC_SE[i] <-
    current_se
  
  
  paired_summary$lower_95[i] <- (
    current_mean -
      1.96 * current_se
  )
  
  
  paired_summary$upper_95[i] <- (
    current_mean +
      1.96 * current_se
  )
}



cat(
  "\n========================================\n"
)

cat(
  "PAIRED MAE DIFFERENCES\n"
)

cat(
  "========================================\n\n"
)


print(
  paired_summary
)



# ============================================================
# 27. WIN RATES
# ============================================================

win_rates <- data.frame(
  comparison = c(
    "Always Smooth beats Fixed",
    "Selective Smooth beats Fixed",
    "Selective beats Always Smooth",
    "Oracle Always beats Fixed",
    "Oracle Selective beats Fixed",
    "Oracle Selective beats Oracle Always"
  ),
  
  win_rate = c(
    mean(
      mae_wide$MAE.estimated_always_smooth <
        mae_wide$MAE.fixed
    ),
    
    mean(
      mae_wide$MAE.estimated_selective <
        mae_wide$MAE.fixed
    ),
    
    mean(
      mae_wide$MAE.estimated_selective <
        mae_wide$MAE.estimated_always_smooth
    ),
    
    mean(
      mae_wide$MAE.oracle_always_smooth <
        mae_wide$MAE.fixed
    ),
    
    mean(
      mae_wide$MAE.oracle_selective <
        mae_wide$MAE.fixed
    ),
    
    mean(
      mae_wide$MAE.oracle_selective <
        mae_wide$MAE.oracle_always_smooth
    )
  ),
  
  stringsAsFactors = FALSE
)


win_rates$win_rate <- (
  win_rates$win_rate * 100
)



cat(
  "\n========================================\n"
)

cat(
  "MODEL WIN RATES\n"
)

cat(
  "========================================\n\n"
)


print(
  win_rates
)



# ============================================================
# 28. ORACLE GAIN CAPTURED
# ============================================================

model_means <- tapply(
  season_results$MAE,
  season_results$model,
  mean
)


fixed_mean <- model_means[
  "fixed"
]


always_estimated_mean <- model_means[
  "estimated_always_smooth"
]


selective_estimated_mean <- model_means[
  "estimated_selective"
]


always_oracle_mean <- model_means[
  "oracle_always_smooth"
]


selective_oracle_mean <- model_means[
  "oracle_selective"
]



always_estimated_gain <- (
  fixed_mean -
    always_estimated_mean
)


always_oracle_gain <- (
  fixed_mean -
    always_oracle_mean
)


selective_estimated_gain <- (
  fixed_mean -
    selective_estimated_mean
)


selective_oracle_gain <- (
  fixed_mean -
    selective_oracle_mean
)



oracle_capture <- data.frame(
  strategy = c(
    "Always Smooth",
    "Selective Smooth"
  ),
  
  fixed_MAE = c(
    fixed_mean,
    fixed_mean
  ),
  
  estimated_MAE = c(
    always_estimated_mean,
    selective_estimated_mean
  ),
  
  oracle_MAE = c(
    always_oracle_mean,
    selective_oracle_mean
  ),
  
  estimated_gain = c(
    always_estimated_gain,
    selective_estimated_gain
  ),
  
  oracle_gain = c(
    always_oracle_gain,
    selective_oracle_gain
  ),
  
  stringsAsFactors = FALSE
)



oracle_capture$percent_oracle_gain_captured <- (
  100 *
    oracle_capture$estimated_gain /
    oracle_capture$oracle_gain
)



cat(
  "\n========================================\n"
)

cat(
  "ORACLE GAIN CAPTURED\n"
)

cat(
  "========================================\n\n"
)


print(
  oracle_capture
)



# ============================================================
# 29. ACTIVE ADAPTIVE WEEKS
# ============================================================

active_diagnostics <- adaptive_diagnostics[
  adaptive_diagnostics$target_week >=
    adaptive_start_week,
]



# ============================================================
# 30. SELECTIVE DECISION QUALITY
# ============================================================

decision_quality <- data.frame(
  parity_MAE = mean(
    active_diagnostics$parity_absolute_error,
    na.rm = TRUE
  ),
  
  selective_regime_accuracy = mean(
    active_diagnostics$selective_regime_correct,
    na.rm = TRUE
  ),
  
  selective_lambda_accuracy = mean(
    active_diagnostics$selective_lambda_correct,
    na.rm = TRUE
  )
)



cat(
  "\n========================================\n"
)

cat(
  "SELECTIVE DECISION QUALITY\n"
)

cat(
  "========================================\n\n"
)


print(
  decision_quality
)



# ============================================================
# 31. ESTIMATED REGIME USAGE
# ============================================================

estimated_regime_table <- as.data.frame(
  prop.table(
    table(
      active_diagnostics$
        estimated_selective_regime
    )
  )
)


names(estimated_regime_table) <- c(
  "regime",
  "proportion"
)


estimated_regime_table$percent <- (
  estimated_regime_table$proportion *
    100
)



cat(
  "\n========================================\n"
)

cat(
  "ESTIMATED SELECTIVE REGIME USAGE\n"
)

cat(
  "========================================\n\n"
)


print(
  estimated_regime_table
)



# ============================================================
# 32. SELECTIVE DECISION QUALITY BY WEEK
# ============================================================

week_groups <- split(
  active_diagnostics,
  active_diagnostics$target_week
)


decision_by_week_rows <- list()

week_index <- 1



for (week_name in names(week_groups)) {
  
  current_data <- week_groups[[week_name]]
  
  
  decision_by_week_rows[[week_index]] <- data.frame(
    target_week =
      unique(
        current_data$target_week
      ),
    
    parity_MAE =
      mean(
        current_data$parity_absolute_error,
        na.rm = TRUE
      ),
    
    regime_accuracy =
      mean(
        current_data$selective_regime_correct,
        na.rm = TRUE
      ),
    
    lambda_accuracy =
      mean(
        current_data$selective_lambda_correct,
        na.rm = TRUE
      )
  )
  
  
  week_index <- week_index + 1
}



decision_by_week <- do.call(
  rbind,
  decision_by_week_rows
)


rownames(decision_by_week) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "SELECTIVE DECISIONS BY WEEK\n"
)

cat(
  "========================================\n\n"
)


print(
  decision_by_week
)



# ============================================================
# 33. ADAPTATION RATE BY TRUE SD
# ============================================================

active_diagnostics$estimated_is_adaptive <- (
  active_diagnostics$
    estimated_selective_regime !=
    "Fixed Middle"
)


sd_groups <- split(
  active_diagnostics,
  active_diagnostics$true_rating_SD
)


adaptation_rate_rows <- list()

adaptation_index <- 1



for (sd_name in names(sd_groups)) {
  
  current_data <- sd_groups[[sd_name]]
  
  
  adaptation_rate_rows[[adaptation_index]] <- data.frame(
    true_rating_SD =
      unique(
        current_data$true_rating_SD
      ),
    
    adaptive_rate =
      100 *
      mean(
        current_data$estimated_is_adaptive
      )
  )
  
  
  adaptation_index <- adaptation_index + 1
}



adaptation_rate <- do.call(
  rbind,
  adaptation_rate_rows
)


rownames(adaptation_rate) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "SELECTIVE ADAPTATION RATE BY TRUE SD\n"
)

cat(
  "========================================\n\n"
)


print(
  adaptation_rate
)



# ============================================================
# 34. PLOT MAE BY TRUE SD
# ============================================================

estimated_models <- summary_by_sd[
  summary_by_sd$model %in%
    c(
      "fixed",
      "estimated_always_smooth",
      "estimated_selective"
    ),
]


mae_plot <- ggplot(
  estimated_models,
  aes(
    x = true_rating_SD,
    y = MAE,
    group = model_label,
    linetype = model_label
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_continuous(
    breaks = true_sd_values
  ) +
  labs(
    title =
      "Version 6D: Selective vs Always Adaptive",
    
    subtitle =
      "Selective model stays fixed at moderate estimated parity",
    
    x =
      "True Team Rating SD",
    
    y =
      "Out-of-Sample Raw MAE",
    
    linetype =
      "Model"
  ) +
  theme_minimal()


print(
  mae_plot
)



# ============================================================
# 35. IMPROVEMENT OVER FIXED
# ============================================================

fixed_by_sd <- summary_by_sd[
  summary_by_sd$model == "fixed",
  c(
    "true_rating_SD",
    "MAE"
  )
]


names(fixed_by_sd)[2] <-
  "fixed_MAE"



estimated_by_sd <- summary_by_sd[
  summary_by_sd$model %in%
    c(
      "estimated_always_smooth",
      "estimated_selective"
    ),
]


estimated_by_sd <- merge(
  estimated_by_sd,
  fixed_by_sd,
  by = "true_rating_SD"
)


estimated_by_sd$improvement <- (
  estimated_by_sd$fixed_MAE -
    estimated_by_sd$MAE
)



improvement_plot <- ggplot(
  estimated_by_sd,
  aes(
    x = true_rating_SD,
    y = improvement,
    group = model_label,
    linetype = model_label
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_continuous(
    breaks = true_sd_values
  ) +
  labs(
    title =
      "Improvement Over Fixed Model",
    
    subtitle =
      "Positive values mean adaptive model has lower MAE",
    
    x =
      "True Team Rating SD",
    
    y =
      "Fixed MAE - Adaptive MAE",
    
    linetype =
      "Model"
  ) +
  theme_minimal()


print(
  improvement_plot
)



# ============================================================
# 36. ADAPTATION RATE PLOT
# ============================================================

adaptation_plot <- ggplot(
  adaptation_rate,
  aes(
    x = true_rating_SD,
    y = adaptive_rate
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_continuous(
    breaks = true_sd_values
  ) +
  scale_y_continuous(
    limits = c(
      0,
      100
    )
  ) +
  labs(
    title =
      "How Often the Selective Model Adapts",
    
    x =
      "True Team Rating SD",
    
    y =
      "Adaptive Weeks (%)"
  ) +
  theme_minimal()


print(
  adaptation_plot
)



# ============================================================
# 37. SAVE RESULTS
# ============================================================

write.csv(
  calibration_coefficients,
  "version6D_calibration_coefficients.csv",
  row.names = FALSE
)


write.csv(
  season_results,
  "version6D_all_model_results.csv",
  row.names = FALSE
)


write.csv(
  summary_by_sd,
  "version6D_results_by_SD.csv",
  row.names = FALSE
)


write.csv(
  overall_summary,
  "version6D_overall_summary.csv",
  row.names = FALSE
)


write.csv(
  paired_summary,
  "version6D_paired_comparisons.csv",
  row.names = FALSE
)


write.csv(
  oracle_capture,
  "version6D_oracle_gain_capture.csv",
  row.names = FALSE
)


write.csv(
  decision_by_week,
  "version6D_decisions_by_week.csv",
  row.names = FALSE
)


write.csv(
  adaptation_rate,
  "version6D_adaptation_rate_by_SD.csv",
  row.names = FALSE
)



# ============================================================
# 38. FINAL OUTPUT
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 6D COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Selective Low Cutoff:",
  selective_low_cutoff,
  "\n"
)


cat(
  "Selective High Cutoff:",
  selective_high_cutoff,
  "\n"
)


cat(
  "Adaptive Start Week:",
  adaptive_start_week,
  "\n"
)


cat(
  "Evaluation Seasons:",
  n_evaluation_per_sd *
    length(true_sd_values),
  "\n\n"
)



cat(
  "OVERALL MODEL COMPARISON:\n\n"
)


print(
  overall_summary
)



cat(
  "\nRESULTS BY TRUE SD:\n\n"
)


print(
  summary_by_sd
)



cat(
  "\nPAIRED MAE COMPARISONS:\n\n"
)


print(
  paired_summary
)



cat(
  "\nMODEL WIN RATES:\n\n"
)


print(
  win_rates
)



cat(
  "\nORACLE GAIN CAPTURED:\n\n"
)


print(
  oracle_capture
)



cat(
  "\nSELECTIVE DECISION QUALITY:\n\n"
)


print(
  decision_quality
)



cat(
  "\nSELECTIVE ADAPTATION RATE BY TRUE SD:\n\n"
)


print(
  adaptation_rate
)


# ============================================================
# END VERSION 6D
# ============================================================
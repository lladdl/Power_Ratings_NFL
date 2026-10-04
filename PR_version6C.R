# ============================================================
# FOOTBALL POWER RATINGS - VERSION 6C
# THREE-BAND vs SMOOTH ADAPTIVE MARGIN CAPS
# ============================================================
#
# MODELS
#
# 1. Fixed
#       lambda = 2
#       cap = 21
#
# 2. Estimated 3-Band
#       estimated parity determines:
#           team lambda
#           3-band margin cap
#
# 3. Estimated Smooth
#       estimated parity determines:
#           team lambda
#           smooth continuous margin cap
#
# 4. Oracle 3-Band
#       hidden true SD determines settings
#
# 5. Oracle Smooth
#       hidden true SD determines settings
#
#
# PRIMARY QUESTION
#
# Can a simpler or smoother margin-cap rule preserve
# the gains at low/high parity while reducing the losses
# around SD = 4-6?
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


# Adaptation begins when predicting Week 5.
#
# Thus Week 5 settings use Weeks 1-4 only.

adaptive_start_week <- 5


# Version 5C lambda crossover

lambda_parity_threshold <- 4.5


# Separate calibration simulations

n_calibration_per_sd <- 500


# Final evaluation simulations

n_evaluation_per_sd <- 1000



# ============================================================
# 3. GENERATE TRUE RATINGS WITH EXACT SD
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
  
  ratings <- ratings - mean(ratings)
  
  ratings <- ratings / sd(ratings)
  
  ratings <- ratings * target_sd
  
  names(ratings) <- league_teams
  
  return(ratings)
}



# ============================================================
# 4. SIMULATE SEASON
# ============================================================

simulate_season <- function(
    schedule,
    true_ratings,
    true_hfa = 2.7,
    noise_sd = 8.5
) {
  
  sim_games <- schedule
  
  
  sim_games$true_expected_margin <-
    
    true_ratings[
      sim_games$home_team
    ] -
    
    true_ratings[
      sim_games$away_team
    ] +
    
    true_hfa
  
  
  sim_games$true_expected_margin <-
    as.numeric(
      sim_games$true_expected_margin
    )
  
  
  sim_games$actual_margin <-
    
    round(
      
      sim_games$true_expected_margin +
        
        rnorm(
          nrow(sim_games),
          mean = 0,
          sd = noise_sd
        )
      
    )
  
  
  # Avoid ties
  
  zero_games <- which(
    sim_games$actual_margin == 0
  )
  
  
  if (
    length(zero_games) > 0
  ) {
    
    sim_games$actual_margin[
      zero_games
    ] <- sample(
      c(-1, 1),
      size = length(zero_games),
      replace = TRUE
    )
  }
  
  
  return(sim_games)
}



# ============================================================
# 5. HFA SHRINKAGE
# ============================================================

get_hfa_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <-
    
    (2 * nrow(data)) /
    length(league_teams)
  
  
  if (
    avg_games_played <= 2
  ) {
    
    return(10)
    
  } else if (
    avg_games_played <= 4
  ) {
    
    return(8)
    
  } else if (
    avg_games_played <= 6
  ) {
    
    return(5)
    
  } else {
    
    return(2)
  }
}



# ============================================================
# 6. DYNAMIC TEAM LAMBDA
# ============================================================

get_dynamic_team_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <-
    
    (2 * nrow(data)) /
    length(league_teams)
  
  
  if (
    avg_games_played <= 2
  ) {
    
    return(5)
    
  } else if (
    avg_games_played <= 4
  ) {
    
    return(4)
    
  } else if (
    avg_games_played <= 6
  ) {
    
    return(3)
    
  } else {
    
    return(2)
  }
}



# ============================================================
# 7. ADAPTIVE TEAM LAMBDA
# ============================================================

choose_team_lambda <- function(
    parity_sd,
    training_games,
    league_teams
) {
  
  if (
    parity_sd <
    lambda_parity_threshold
  ) {
    
    return(
      
      get_dynamic_team_lambda(
        
        data =
          training_games,
        
        league_teams =
          league_teams
      )
      
    )
    
  } else {
    
    return(2)
  }
}



# ============================================================
# 8. THREE-BAND MARGIN CAP
# ============================================================
#
# HIGH PARITY:
#
#   estimated SD < 4.5
#       cap = 14
#
# MODERATE:
#
#   4.5 <= SD < 6.5
#       cap = 21
#
# LOW PARITY:
#
#   SD >= 6.5
#       cap = 35
#
# ============================================================

choose_cap_three_band <- function(
    parity_sd
) {
  
  if (
    parity_sd < 4.5
  ) {
    
    return(14)
    
  } else if (
    parity_sd < 6.5
  ) {
    
    return(21)
    
  } else {
    
    return(35)
  }
}



# ============================================================
# 9. SMOOTH MARGIN CAP
# ============================================================
#
# Continuous rule:
#
# cap = 14 + 5.25 * (SD - 4)
#
#
# Bounded:
#
# minimum = 14
# maximum = 35
#
#
# Approximate values:
#
# SD 4.0 -> 14
# SD 4.5 -> 16.6
# SD 5.0 -> 19.3
# SD 5.5 -> 21.9
# SD 6.0 -> 24.5
# SD 6.5 -> 27.1
# SD 7.0 -> 29.8
# SD 7.5 -> 32.4
# SD 8.0 -> 35
#
# ============================================================

choose_cap_smooth <- function(
    parity_sd
) {
  
  cap <- 14 +
    5.25 *
    (parity_sd - 4)
  
  
  # Lower boundary
  
  cap <- max(
    14,
    cap
  )
  
  
  # Upper boundary
  
  cap <- min(
    35,
    cap
  )
  
  
  return(cap)
}



# ============================================================
# 10. POWER RATING MODEL
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
  # RESPONSE VARIABLE
  # ----------------------------------------------------------
  
  if (
    is.infinite(margin_cap)
  ) {
    
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
  
  
  
  for (
    i in seq_len(n_games)
  ) {
    
    home <-
      data$home_team[i]
    
    
    away <-
      data$away_team[i]
    
    
    X[i, home] <- 1
    
    X[i, away] <- -1
  }
  
  
  
  # ----------------------------------------------------------
  # PENALTY
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
  # PRIOR
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
    
    crossprod(X) +
      penalty,
    
    crossprod(
      X,
      y
    ) +
      
      penalty %*%
      prior_target
    
  )
  
  
  beta <- as.numeric(
    beta_matrix
  )
  
  
  names(beta) <-
    colnames(X)
  
  
  
  estimated_hfa <-
    beta["HFA"]
  
  
  ratings <-
    beta[
      league_teams
    ]
  
  
  ratings <-
    ratings -
    mean(ratings)
  
  
  
  rating_table <- data.frame(
    
    team =
      league_teams,
    
    rating =
      as.numeric(
        ratings
      ),
    
    stringsAsFactors =
      FALSE
  )
  
  
  
  return(
    
    list(
      
      ratings =
        rating_table,
      
      home_field =
        as.numeric(
          estimated_hfa
        )
      
    )
    
  )
}



# ============================================================
# 11. RAW PARITY ESTIMATOR
# ============================================================

estimate_observed_parity_sd <- function(
    training_games,
    league_teams,
    hfa_prior = 2.5
) {
  
  hfa_lambda <- get_hfa_lambda(
    
    data =
      training_games,
    
    league_teams =
      league_teams
  )
  
  
  
  parity_model <- fit_power_ratings(
    
    data =
      training_games,
    
    league_teams =
      league_teams,
    
    margin_cap =
      Inf,
    
    team_lambda =
      2,
    
    hfa_lambda =
      hfa_lambda,
    
    hfa_prior =
      hfa_prior
  )
  
  
  
  observed_sd <- sd(
    parity_model$ratings$rating
  )
  
  
  return(observed_sd)
}



# ============================================================
# 12. BUILD PARITY CALIBRATION SAMPLE
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



for (
  current_true_sd in true_sd_values
) {
  
  cat(
    "Calibration SD:",
    current_true_sd,
    "\n"
  )
  
  
  for (
    s in 1:n_calibration_per_sd
  ) {
    
    true_ratings <- generate_true_ratings(
      
      league_teams =
        teams,
      
      target_sd =
        current_true_sd
    )
    
    
    
    games <- simulate_season(
      
      schedule =
        schedule,
      
      true_ratings =
        true_ratings,
      
      true_hfa =
        true_hfa,
      
      noise_sd =
        noise_sd
    )
    
    
    
    for (
      completed_week in 2:(max_week - 1)
    ) {
      
      training_games <- games[
        
        games$week <=
          completed_week,
        
      ]
      
      
      observed_sd <-
        
        estimate_observed_parity_sd(
          
          training_games =
            training_games,
          
          league_teams =
            teams,
          
          hfa_prior =
            hfa_prior
        )
      
      
      
      calibration_rows[[calibration_index]] <- data.frame(
        
        true_rating_SD =
          current_true_sd,
        
        week =
          completed_week,
        
        observed_rating_SD =
          observed_sd
      )
      
      
      calibration_index <-
        calibration_index + 1
    }
  }
}



calibration_data <- do.call(
  rbind,
  calibration_rows
)



# ============================================================
# 13. FIT WEEK-SPECIFIC CALIBRATION EQUATIONS
# ============================================================

calibration_coefficients <- data.frame(
  
  week =
    2:(max_week - 1),
  
  intercept =
    NA_real_,
  
  slope =
    NA_real_,
  
  r_squared =
    NA_real_
)



for (
  i in seq_len(
    nrow(
      calibration_coefficients
    )
  )
) {
  
  current_week <-
    calibration_coefficients$week[i]
  
  
  current_data <- calibration_data[
    
    calibration_data$week ==
      current_week,
    
  ]
  
  
  model <- lm(
    
    true_rating_SD ~
      
      observed_rating_SD,
    
    data =
      current_data
  )
  
  
  calibration_coefficients$intercept[i] <-
    
    coef(model)[1]
  
  
  calibration_coefficients$slope[i] <-
    
    coef(model)[2]
  
  
  calibration_coefficients$r_squared[i] <-
    
    summary(model)$r.squared
}



print(
  calibration_coefficients
)



# ============================================================
# 14. APPLY CALIBRATION
# ============================================================

calibrate_parity_sd <- function(
    observed_sd,
    completed_week,
    calibration_coefficients
) {
  
  row <- calibration_coefficients[
    
    calibration_coefficients$week ==
      completed_week,
    
  ]
  
  
  estimated_sd <-
    
    row$intercept +
    
    row$slope *
    observed_sd
  
  
  estimated_sd <- max(
    0,
    estimated_sd
  )
  
  
  return(estimated_sd)
}



# ============================================================
# 15. SAFE CORRELATION
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
    cor(x, y)
  )
}



# ============================================================
# 16. SINGLE MODEL FIT + PREDICTION
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
    
    data =
      training_games,
    
    league_teams =
      league_teams,
    
    margin_cap =
      margin_cap,
    
    team_lambda =
      team_lambda,
    
    hfa_lambda =
      hfa_lambda,
    
    hfa_prior =
      hfa_prior
  )
  
  
  
  lookup <-
    model$ratings$rating
  
  
  names(lookup) <-
    model$ratings$team
  
  
  
  predicted_margin <-
    
    lookup[
      test_games$home_team
    ] -
    
    lookup[
      test_games$away_team
    ] +
    
    model$home_field
  
  
  
  rating_rmse <- sqrt(
    
    mean(
      
      (
        
        lookup[
          league_teams
        ] -
          
          true_ratings[
            league_teams
          ]
        
      )^2
      
    )
    
  )
  
  
  
  return(
    
    list(
      
      prediction =
        as.numeric(
          predicted_margin
        ),
      
      rating_RMSE =
        rating_rmse
      
    )
    
  )
}



# ============================================================
# 17. EVALUATE ALL FIVE MODELS
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
    
    "estimated_3band",
    
    "estimated_smooth",
    
    "oracle_3band",
    
    "oracle_smooth"
  )
  
  
  prediction_storage <-
    
    setNames(
      vector(
        "list",
        length(model_names)
      ),
      model_names
    )
  
  
  rating_rmse_storage <-
    
    setNames(
      vector(
        "list",
        length(model_names)
      ),
      model_names
    )
  
  
  diagnostics <- list()
  
  diagnostic_index <- 1
  
  
  
  # ==========================================================
  # WEEKLY WALK-FORWARD
  # ==========================================================
  
  for (
    target_week in 2:max_week
  ) {
    
    training_games <- games[
      
      games$week <
        target_week,
      
    ]
    
    
    test_games <- games[
      
      games$week ==
        target_week,
      
    ]
    
    
    completed_week <-
      target_week - 1
    
    
    
    hfa_lambda <- get_hfa_lambda(
      
      data =
        training_games,
      
      league_teams =
        league_teams
    )
    
    
    
    # ========================================================
    # ESTIMATE PARITY
    # ========================================================
    
    observed_sd <- NA_real_
    
    estimated_sd <- NA_real_
    
    
    
    if (
      completed_week >= 2
    ) {
      
      observed_sd <-
        
        estimate_observed_parity_sd(
          
          training_games =
            training_games,
          
          league_teams =
            league_teams,
          
          hfa_prior =
            hfa_prior
        )
      
      
      estimated_sd <-
        
        calibrate_parity_sd(
          
          observed_sd =
            observed_sd,
          
          completed_week =
            completed_week,
          
          calibration_coefficients =
            calibration_coefficients
        )
    }
    
    
    
    # ========================================================
    # DEFAULT SETTINGS BEFORE ADAPTATION
    # ========================================================
    
    if (
      target_week <
      adaptive_start_week
    ) {
      
      fixed_cap <- 21
      
      est_3_cap <- 21
      
      est_smooth_cap <- 21
      
      oracle_3_cap <- 21
      
      oracle_smooth_cap <- 21
      
      
      fixed_lambda <- 2
      
      est_lambda <- 2
      
      oracle_lambda <- 2
      
      
    } else {
      
      
      # Fixed baseline
      
      fixed_cap <- 21
      
      fixed_lambda <- 2
      
      
      
      # ----------------------------------------------
      # ESTIMATED PARITY SETTINGS
      # ----------------------------------------------
      
      est_3_cap <-
        
        choose_cap_three_band(
          estimated_sd
        )
      
      
      est_smooth_cap <-
        
        choose_cap_smooth(
          estimated_sd
        )
      
      
      est_lambda <-
        
        choose_team_lambda(
          
          parity_sd =
            estimated_sd,
          
          training_games =
            training_games,
          
          league_teams =
            league_teams
        )
      
      
      
      # ----------------------------------------------
      # ORACLE SETTINGS
      # ----------------------------------------------
      
      oracle_3_cap <-
        
        choose_cap_three_band(
          true_rating_sd
        )
      
      
      oracle_smooth_cap <-
        
        choose_cap_smooth(
          true_rating_sd
        )
      
      
      oracle_lambda <-
        
        choose_team_lambda(
          
          parity_sd =
            true_rating_sd,
          
          training_games =
            training_games,
          
          league_teams =
            league_teams
        )
    }
    
    
    
    # ========================================================
    # MODEL CONFIGURATIONS
    # ========================================================
    
    configs <- list(
      
      fixed =
        list(
          cap = fixed_cap,
          lambda = fixed_lambda
        ),
      
      estimated_3band =
        list(
          cap = est_3_cap,
          lambda = est_lambda
        ),
      
      estimated_smooth =
        list(
          cap = est_smooth_cap,
          lambda = est_lambda
        ),
      
      oracle_3band =
        list(
          cap = oracle_3_cap,
          lambda = oracle_lambda
        ),
      
      oracle_smooth =
        list(
          cap = oracle_smooth_cap,
          lambda = oracle_lambda
        )
      
    )
    
    
    
    # ========================================================
    # FIT EACH MODEL
    # ========================================================
    
    for (
      model_name in model_names
    ) {
      
      result <- fit_predict_week(
        
        training_games =
          training_games,
        
        test_games =
          test_games,
        
        league_teams =
          league_teams,
        
        margin_cap =
          configs[[model_name]]$cap,
        
        team_lambda =
          configs[[model_name]]$lambda,
        
        hfa_lambda =
          hfa_lambda,
        
        hfa_prior =
          hfa_prior,
        
        true_ratings =
          true_ratings
      )
      
      
      
      current_predictions <- data.frame(
        
        week =
          target_week,
        
        actual_margin =
          test_games$actual_margin,
        
        predicted_margin =
          result$prediction
      )
      
      
      current_predictions$error <-
        
        current_predictions$actual_margin -
        
        current_predictions$predicted_margin
      
      
      current_predictions$correct_winner <-
        
        (
          current_predictions$actual_margin >
            0
        ) ==
        
        (
          current_predictions$predicted_margin >
            0
        )
      
      
      
      prediction_storage[[model_name]][[length(prediction_storage[[model_name]]) + 1]] <- current_predictions
      
      
      
      rating_rmse_storage[[model_name]] <- c(rating_rmse_storage[[model_name]],
        result$rating_RMSE
      )
    }
    
    
    
    # ========================================================
    # DIAGNOSTICS
    # ========================================================
    
    diagnostics[[diagnostic_index]] <- data.frame(
      
      target_week =
        target_week,
      
      true_rating_SD =
        true_rating_sd,
      
      estimated_rating_SD =
        estimated_sd,
      
      parity_absolute_error =
        abs(
          estimated_sd -
            true_rating_sd
        ),
      
      estimated_3band_cap =
        est_3_cap,
      
      oracle_3band_cap =
        oracle_3_cap,
      
      three_band_cap_correct =
        est_3_cap ==
        oracle_3_cap,
      
      estimated_smooth_cap =
        est_smooth_cap,
      
      oracle_smooth_cap =
        oracle_smooth_cap,
      
      smooth_cap_absolute_error =
        abs(
          est_smooth_cap -
            oracle_smooth_cap
        ),
      
      estimated_lambda =
        est_lambda,
      
      oracle_lambda =
        oracle_lambda,
      
      lambda_correct =
        est_lambda ==
        oracle_lambda
    )
    
    
    diagnostic_index <-
      diagnostic_index + 1
  }
  
  
  
  # ==========================================================
  # MODEL SUMMARY
  # ==========================================================
  
  model_results <- list()
  
  
  result_index <- 1
  
  
  
  for (
    model_name in model_names
  ) {
    
    predictions <- do.call(
      
      rbind,
      
      prediction_storage[[model_name]]
    )
    
    
    model_results[[result_index]] <- data.frame(
      
      model =
        model_name,
      
      MAE =
        mean(
          abs(
            predictions$error
          )
        ),
      
      RMSE =
        sqrt(
          mean(
            predictions$error^2
          )
        ),
      
      Accuracy =
        mean(
          predictions$correct_winner
        ),
      
      Correlation =
        safe_cor(
          
          predictions$predicted_margin,
          
          predictions$actual_margin
        ),
      
      Bias =
        mean(
          predictions$error
        ),
      
      Rating_RMSE =
        mean(
          rating_rmse_storage[[model_name]]
        )
    )
    
    
    result_index <-
      result_index + 1
  }
  
  
  
  return(
    
    list(
      
      metrics =
        do.call(
          rbind,
          model_results
        ),
      
      diagnostics =
        do.call(
          rbind,
          diagnostics
        )
      
    )
    
  )
}



# ============================================================
# 18. EVALUATION MONTE CARLO
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "STARTING VERSION 6C EVALUATION\n"
)

cat(
  "========================================\n"
)


set.seed(6003)


season_result_rows <- list()

diagnostic_rows <- list()


result_index <- 1

diagnostic_index <- 1



for (
  current_true_sd in true_sd_values
) {
  
  cat(
    "\nTRUE SD:",
    current_true_sd,
    "\n"
  )
  
  
  for (
    s in 1:n_evaluation_per_sd
  ) {
    
    true_ratings <- generate_true_ratings(
      
      league_teams =
        teams,
      
      target_sd =
        current_true_sd
    )
    
    
    
    games <- simulate_season(
      
      schedule =
        schedule,
      
      true_ratings =
        true_ratings,
      
      true_hfa =
        true_hfa,
      
      noise_sd =
        noise_sd
    )
    
    
    
    result <- evaluate_models(
      
      games =
        games,
      
      true_ratings =
        true_ratings,
      
      true_rating_sd =
        current_true_sd,
      
      league_teams =
        teams,
      
      calibration_coefficients =
        calibration_coefficients,
      
      adaptive_start_week =
        adaptive_start_week
    )
    
    
    
    current_metrics <-
      result$metrics
    
    
    current_metrics$true_rating_SD <-
      current_true_sd
    
    
    current_metrics$simulation <-
      s
    
    
    season_result_rows[[result_index]] <- current_metrics
    
    
    result_index <-
      result_index + 1
    
    
    
    current_diagnostics <-
      result$diagnostics
    
    
    current_diagnostics$simulation <-
      s
    
    
    diagnostic_rows[[diagnostic_index]] <- current_diagnostics
    
    
    diagnostic_index <-
      diagnostic_index + 1
    
    
    
    if (
      s %% 100 == 0
    ) {
      
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



season_results <- do.call(
  
  rbind,
  
  season_result_rows
)


rownames(season_results) <- NULL



adaptive_diagnostics <- do.call(
  
  rbind,
  
  diagnostic_rows
)


rownames(adaptive_diagnostics) <- NULL



# ============================================================
# 19. MODEL LABELS
# ============================================================

model_labels <- c(
  
  fixed =
    "Fixed",
  
  estimated_3band =
    "Estimated 3-Band",
  
  estimated_smooth =
    "Estimated Smooth",
  
  oracle_3band =
    "Oracle 3-Band",
  
  oracle_smooth =
    "Oracle Smooth"
)


season_results$model_label <-
  
  model_labels[season_results$model]



# ============================================================
# 20. OVERALL MODEL COMPARISON
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
  
  data =
    season_results,
  
  FUN =
    mean
)


overall_summary <-
  
  overall_summary[
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
# 21. RESULTS BY TRUE SD
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
  
  data =
    season_results,
  
  FUN =
    mean
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
# 22. CREATE WIDE MAE TABLE
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
  
  idvar =
    c(
      "true_rating_SD",
      "simulation"
    ),
  
  timevar =
    "model",
  
  direction =
    "wide"
)



# ============================================================
# 23. PAIRED DIFFERENCES
# ============================================================

mae_wide$three_band_vs_fixed <-
  
  mae_wide$MAE.estimated_3band -
  
  mae_wide$MAE.fixed


mae_wide$smooth_vs_fixed <-
  
  mae_wide$MAE.estimated_smooth -
  
  mae_wide$MAE.fixed


mae_wide$smooth_vs_three_band <-
  
  mae_wide$MAE.estimated_smooth -
  
  mae_wide$MAE.estimated_3band


mae_wide$oracle_three_vs_fixed <-
  
  mae_wide$MAE.oracle_3band -
  
  mae_wide$MAE.fixed


mae_wide$oracle_smooth_vs_fixed <-
  
  mae_wide$MAE.oracle_smooth -
  
  mae_wide$MAE.fixed



paired_summary <- data.frame(
  
  comparison = c(
    
    "Estimated 3-Band - Fixed",
    
    "Estimated Smooth - Fixed",
    
    "Smooth - 3-Band",
    
    "Oracle 3-Band - Fixed",
    
    "Oracle Smooth - Fixed"
    
  ),
  
  mean_difference = c(
    
    mean(
      mae_wide$three_band_vs_fixed
    ),
    
    mean(
      mae_wide$smooth_vs_fixed
    ),
    
    mean(
      mae_wide$smooth_vs_three_band
    ),
    
    mean(
      mae_wide$oracle_three_vs_fixed
    ),
    
    mean(
      mae_wide$oracle_smooth_vs_fixed
    )
    
  )
)



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
# 24. WIN RATES
# ============================================================

win_rates <- data.frame(
  
  comparison = c(
    
    "Estimated 3-Band beats Fixed",
    
    "Estimated Smooth beats Fixed",
    
    "Smooth beats 3-Band",
    
    "Oracle 3-Band beats Fixed",
    
    "Oracle Smooth beats Fixed"
    
  ),
  
  win_rate = c(
    
    mean(
      mae_wide$MAE.estimated_3band <
        mae_wide$MAE.fixed
    ),
    
    mean(
      mae_wide$MAE.estimated_smooth <
        mae_wide$MAE.fixed
    ),
    
    mean(
      mae_wide$MAE.estimated_smooth <
        mae_wide$MAE.estimated_3band
    ),
    
    mean(
      mae_wide$MAE.oracle_3band <
        mae_wide$MAE.fixed
    ),
    
    mean(
      mae_wide$MAE.oracle_smooth <
        mae_wide$MAE.fixed
    )
    
  )
)


win_rates$win_rate <-
  
  win_rates$win_rate *
  100



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
# 25. ORACLE GAIN CAPTURED
# ============================================================

model_means <- tapply(
  
  season_results$MAE,
  
  season_results$model,
  
  mean
)


fixed_mean <-
  model_means["fixed"]


estimated_three_mean <-
  model_means["estimated_3band"]


estimated_smooth_mean <-
  model_means["estimated_smooth"]


oracle_three_mean <-
  model_means["oracle_3band"]


oracle_smooth_mean <-
  model_means["oracle_smooth"]



three_oracle_gain <-
  
  fixed_mean -
  oracle_three_mean


three_estimated_gain <-
  
  fixed_mean -
  estimated_three_mean



smooth_oracle_gain <-
  
  fixed_mean -
  oracle_smooth_mean


smooth_estimated_gain <-
  
  fixed_mean -
  estimated_smooth_mean



three_percent_captured <-
  
  100 *
  three_estimated_gain /
  three_oracle_gain


smooth_percent_captured <-
  
  100 *
  smooth_estimated_gain /
  smooth_oracle_gain



oracle_capture_table <- data.frame(
  
  strategy =
    c(
      "3-Band",
      "Smooth"
    ),
  
  fixed_MAE =
    c(
      fixed_mean,
      fixed_mean
    ),
  
  estimated_MAE =
    c(
      estimated_three_mean,
      estimated_smooth_mean
    ),
  
  oracle_MAE =
    c(
      oracle_three_mean,
      oracle_smooth_mean
    ),
  
  estimated_gain =
    c(
      three_estimated_gain,
      smooth_estimated_gain
    ),
  
  oracle_gain =
    c(
      three_oracle_gain,
      smooth_oracle_gain
    ),
  
  percent_oracle_gain_captured =
    c(
      three_percent_captured,
      smooth_percent_captured
    )
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
  oracle_capture_table
)



# ============================================================
# 26. ACTIVE ADAPTIVE WEEKS ONLY
# ============================================================

active_diagnostics <- adaptive_diagnostics[
  
  adaptive_diagnostics$target_week >=
    adaptive_start_week,
  
]



# ============================================================
# 27. ADAPTIVE DECISION QUALITY
# ============================================================

decision_quality <- data.frame(
  
  parity_MAE = mean(
    
    active_diagnostics$parity_absolute_error,
    
    na.rm = TRUE
  ),
  
  three_band_cap_accuracy = mean(
    
    active_diagnostics$three_band_cap_correct,
    
    na.rm = TRUE
  ),
  
  smooth_cap_MAE = mean(
    
    active_diagnostics$smooth_cap_absolute_error,
    
    na.rm = TRUE
  ),
  
  smooth_cap_within_3_points = mean(
    
    active_diagnostics$smooth_cap_absolute_error <=
      3,
    
    na.rm = TRUE
  ),
  
  smooth_cap_within_5_points = mean(
    
    active_diagnostics$smooth_cap_absolute_error <=
      5,
    
    na.rm = TRUE
  ),
  
  lambda_accuracy = mean(
    
    active_diagnostics$lambda_correct,
    
    na.rm = TRUE
  )
)



cat(
  "\n========================================\n"
)

cat(
  "ADAPTIVE DECISION QUALITY\n"
)

cat(
  "========================================\n\n"
)


print(
  decision_quality
)



# ============================================================
# 28. DECISION QUALITY BY WEEK
# ============================================================

decision_week_list <- lapply(
  
  split(
    active_diagnostics,
    active_diagnostics$target_week
  ),
  
  function(x) {
    
    data.frame(
      
      target_week =
        unique(
          x$target_week
        ),
      
      parity_MAE =
        mean(
          x$parity_absolute_error,
          na.rm = TRUE
        ),
      
      three_band_accuracy =
        mean(
          x$three_band_cap_correct,
          na.rm = TRUE
        ),
      
      smooth_cap_MAE =
        mean(
          x$smooth_cap_absolute_error,
          na.rm = TRUE
        ),
      
      smooth_within_3 =
        mean(
          x$smooth_cap_absolute_error <=
            3,
          na.rm = TRUE
        ),
      
      lambda_accuracy =
        mean(
          x$lambda_correct,
          na.rm = TRUE
        )
      
    )
  }
)



decision_by_week <- do.call(
  
  rbind,
  
  decision_week_list
)


rownames(decision_by_week) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "ADAPTIVE DECISION QUALITY BY WEEK\n"
)

cat(
  "========================================\n\n"
)


print(
  decision_by_week
)



# ============================================================
# 29. PLOT MODEL MAE BY TRUE SD
# ============================================================

estimated_plot_data <- summary_by_sd[
  
  summary_by_sd$model %in%
    c(
      "fixed",
      "estimated_3band",
      "estimated_smooth"
    ),
  
]


mae_plot <- ggplot(
  
  estimated_plot_data,
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      MAE,
    
    group =
      model_label,
    
    linetype =
      model_label
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks =
      true_sd_values
  ) +
  
  labs(
    
    title =
      "Version 6C: Fixed vs Adaptive Margin-Cap Rules",
    
    subtitle =
      "Estimated parity uses prior games only",
    
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
# 30. IMPROVEMENT OVER FIXED BY SD
# ============================================================

fixed_by_sd <- summary_by_sd[
  
  summary_by_sd$model ==
    "fixed",
  
  c(
    "true_rating_SD",
    "MAE"
  )
]


names(fixed_by_sd)[2] <-
  "fixed_MAE"



adaptive_by_sd <- summary_by_sd[
  
  summary_by_sd$model %in%
    c(
      "estimated_3band",
      "estimated_smooth"
    ),
  
]


adaptive_by_sd <- merge(
  
  adaptive_by_sd,
  
  fixed_by_sd,
  
  by =
    "true_rating_SD"
)


adaptive_by_sd$improvement <-
  
  adaptive_by_sd$fixed_MAE -
  
  adaptive_by_sd$MAE



improvement_plot <- ggplot(
  
  adaptive_by_sd,
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      improvement,
    
    group =
      model_label,
    
    linetype =
      model_label
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
    breaks =
      true_sd_values
  ) +
  
  labs(
    
    title =
      "Adaptive Improvement Over Fixed Model",
    
    subtitle =
      "Positive values mean adaptive model has lower MAE",
    
    x =
      "True Team Rating SD",
    
    y =
      "Fixed MAE - Adaptive MAE",
    
    linetype =
      "Adaptive Rule"
  ) +
  
  theme_minimal()


print(
  improvement_plot
)



# ============================================================
# 31. SMOOTH CAP FUNCTION PLOT
# ============================================================

smooth_sd_plot <- seq(
  2,
  8,
  by = 0.05
)


smooth_cap_plot_data <- data.frame(
  
  parity_SD =
    smooth_sd_plot,
  
  margin_cap =
    sapply(
      smooth_sd_plot,
      choose_cap_smooth
    )
)



smooth_function_plot <- ggplot(
  
  smooth_cap_plot_data,
  
  aes(
    x = parity_SD,
    y = margin_cap
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    
    data =
      data.frame(
        parity_SD =
          true_sd_values,
        
        margin_cap =
          sapply(
            true_sd_values,
            choose_cap_smooth
          )
      ),
    
    size = 3
  ) +
  
  labs(
    
    title =
      "Smooth Adaptive Margin-Cap Function",
    
    x =
      "Estimated League Rating SD",
    
    y =
      "Margin Cap"
  ) +
  
  theme_minimal()


print(
  smooth_function_plot
)



# ============================================================
# 32. SAVE RESULTS
# ============================================================

write.csv(
  
  calibration_coefficients,
  
  "version6C_calibration_coefficients.csv",
  
  row.names = FALSE
)


write.csv(
  
  season_results,
  
  "version6C_all_model_results.csv",
  
  row.names = FALSE
)


write.csv(
  
  overall_summary,
  
  "version6C_overall_summary.csv",
  
  row.names = FALSE
)


write.csv(
  
  summary_by_sd,
  
  "version6C_results_by_SD.csv",
  
  row.names = FALSE
)


write.csv(
  
  paired_summary,
  
  "version6C_paired_comparisons.csv",
  
  row.names = FALSE
)


write.csv(
  
  oracle_capture_table,
  
  "version6C_oracle_gain_capture.csv",
  
  row.names = FALSE
)


write.csv(
  
  decision_by_week,
  
  "version6C_decision_quality_by_week.csv",
  
  row.names = FALSE
)



# ============================================================
# 33. FINAL OUTPUT
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 6C COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Calibration simulations per SD:",
  n_calibration_per_sd,
  "\n"
)


cat(
  "Evaluation simulations per SD:",
  n_evaluation_per_sd,
  "\n"
)


cat(
  "Total evaluation seasons:",
  n_evaluation_per_sd *
    length(true_sd_values),
  "\n"
)


cat(
  "Adaptive start week:",
  adaptive_start_week,
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
  "\nPAIRED COMPARISONS:\n\n"
)


print(
  paired_summary
)


cat(
  "\nWIN RATES:\n\n"
)


print(
  win_rates
)


cat(
  "\nORACLE GAIN CAPTURED:\n\n"
)


print(
  oracle_capture_table
)


cat(
  "\nADAPTIVE DECISION QUALITY:\n\n"
)


print(
  decision_quality
)


# ============================================================
# END VERSION 6C
# ============================================================
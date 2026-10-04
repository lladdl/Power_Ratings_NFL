# ============================================================
# FOOTBALL POWER RATINGS - VERSION 7A
# DYNAMIC TEAM STRENGTH SIMULATOR
# ============================================================
#
# PURPOSE
# -------
#
# Versions 1-6 assumed that each team's true hidden strength
# stayed constant for the entire season.
#
# Version 7A changes the DATA-GENERATING PROCESS:
#
#     True team strength can now change from week to week.
#
#
# IMPORTANT:
#
# We DO NOT add recency weighting yet.
#
# The rating model remains the Version 6 winner:
#
#     - Raw-margin parity estimator
#     - Week-specific parity calibration
#     - Smooth adaptive margin cap
#     - Parity-dependent team lambda
#     - HFA prior
#     - All previous games receive equal weight
#
#
# This isolates one question:
#
#     How badly does the current model degrade when
#     true team strength changes during the season?
#
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

max_week <- max(
  schedule$week
)


cat("\nTeams:", n_teams, "\n")
cat("Games per season:", nrow(schedule), "\n")
cat("Weeks:", max_week, "\n")



# ============================================================
# 2. GLOBAL SIMULATION SETTINGS
# ============================================================

true_hfa <- 2.7

noise_sd <- 8.5

hfa_prior <- 2.5


# ------------------------------------------------------------
# LEAGUE PARITY LEVELS
# ------------------------------------------------------------
#
# These remain exactly controlled each week.
#
# This means that a league with target SD = 5 continues
# to have hidden rating SD = 5 every week even though
# individual teams move.
#
# ------------------------------------------------------------

true_sd_values <- c(
  2,
  3,
  4,
  5,
  6,
  7,
  8
)



# ------------------------------------------------------------
# WEEKLY TEAM-STRENGTH DRIFT LEVELS
# ------------------------------------------------------------
#
# 0.00 = completely stable teams
#
# Increasing values create larger week-to-week changes.
#
# ------------------------------------------------------------

drift_sd_values <- c(
  0.00,
  0.25,
  0.50,
  0.75,
  1.00,
  1.50
)



# ------------------------------------------------------------
# ADAPTIVE MODEL SETTINGS
# ------------------------------------------------------------

adaptive_start_week <- 5

lambda_parity_threshold <- 4.5



# ------------------------------------------------------------
# MONTE CARLO SETTINGS
# ------------------------------------------------------------
#
# Calibration uses static seasons only.
#
# Evaluation then uses dynamic seasons.
#
# Start with 500 per SD x drift cell.
#
# This means:
#
# 7 parity values
# x
# 6 drift values
# x
# 500 seasons
#
# = 21,000 evaluation seasons.
#
# You can increase this to 1000 later.
#
# ------------------------------------------------------------

n_calibration_per_sd <- 500

n_evaluation_per_cell <- 500



# ============================================================
# 3. GENERATE INITIAL TRUE RATINGS
# ============================================================
#
# Ratings:
#
#     mean = 0
#     exact sample SD = target_sd
#
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
  
  
  ratings <- ratings -
    mean(ratings)
  
  
  ratings <- ratings /
    sd(ratings)
  
  
  ratings <- ratings *
    target_sd
  
  
  names(ratings) <-
    league_teams
  
  
  return(
    ratings
  )
}



# ============================================================
# 4. UPDATE TRUE RATINGS ONE WEEK
# ============================================================
#
# Each team's previous rating receives a random shock:
#
#     R_i,t* =
#
#     R_i,t-1 + Normal(0, drift_sd)
#
#
# Then ratings are:
#
#     1. re-centered at zero
#     2. rescaled back to target league SD
#
#
# WHY RESCALE?
#
# This isolates:
#
#     TEAM MOVEMENT
#
# from:
#
#     LEAGUE PARITY
#
#
# If target SD = 5, the league stays SD = 5,
# but teams can move up and down within that distribution.
#
# ============================================================

update_true_ratings <- function(
    previous_ratings,
    target_sd,
    drift_sd
) {
  
  # ----------------------------------------------------------
  # NO DRIFT
  # ----------------------------------------------------------
  
  if (drift_sd == 0) {
    
    return(
      previous_ratings
    )
  }
  
  
  # ----------------------------------------------------------
  # RANDOM TEAM SHOCKS
  # ----------------------------------------------------------
  
  new_ratings <- previous_ratings +
    rnorm(
      length(previous_ratings),
      mean = 0,
      sd = drift_sd
    )
  
  
  names(new_ratings) <-
    names(previous_ratings)
  
  
  # ----------------------------------------------------------
  # CENTER
  # ----------------------------------------------------------
  
  new_ratings <- new_ratings -
    mean(new_ratings)
  
  
  # ----------------------------------------------------------
  # PRESERVE TARGET LEAGUE SD
  # ----------------------------------------------------------
  
  current_sd <- sd(
    new_ratings
  )
  
  
  if (current_sd > 0) {
    
    new_ratings <- (
      new_ratings /
        current_sd
    ) * target_sd
  }
  
  
  return(
    new_ratings
  )
}



# ============================================================
# 5. SIMULATE DYNAMIC TRUE RATINGS FOR ENTIRE SEASON
# ============================================================

generate_dynamic_rating_history <- function(
    league_teams,
    target_sd,
    drift_sd,
    max_week
) {
  
  rating_matrix <- matrix(
    NA_real_,
    nrow = max_week,
    ncol = length(league_teams)
  )
  
  
  colnames(rating_matrix) <-
    league_teams
  
  
  rownames(rating_matrix) <-
    paste0(
      "Week_",
      seq_len(max_week)
    )
  
  
  # ----------------------------------------------------------
  # WEEK 1
  # ----------------------------------------------------------
  
  rating_matrix[1, ] <- generate_true_ratings(
    league_teams = league_teams,
    target_sd = target_sd
  )
  
  
  # ----------------------------------------------------------
  # WEEKS 2+
  # ----------------------------------------------------------
  
  if (max_week >= 2) {
    
    for (current_week in 2:max_week) {
      
      previous_ratings <-
        rating_matrix[
          current_week - 1,
        ]
      
      
      names(previous_ratings) <-
        league_teams
      
      
      new_ratings <- update_true_ratings(
        previous_ratings = previous_ratings,
        target_sd = target_sd,
        drift_sd = drift_sd
      )
      
      
      rating_matrix[
        current_week,
      ] <- new_ratings
    }
  }
  
  
  return(
    rating_matrix
  )
}



# ============================================================
# 6. SUMMARIZE TRUE RATING MOVEMENT
# ============================================================

summarize_true_rating_movement <- function(
    rating_matrix
) {
  
  n_weeks <- nrow(
    rating_matrix
  )
  
  
  if (n_weeks < 2) {
    
    return(
      list(
        mean_absolute_change = NA_real_,
        rms_change = NA_real_,
        mean_weekly_correlation = NA_real_
      )
    )
  }
  
  
  absolute_changes <- numeric()
  
  squared_changes <- numeric()
  
  weekly_correlations <- numeric()
  
  
  
  for (current_week in 2:n_weeks) {
    
    previous <- rating_matrix[
      current_week - 1,
    ]
    
    
    current <- rating_matrix[
      current_week,
    ]
    
    
    difference <- current -
      previous
    
    
    absolute_changes <- c(
      absolute_changes,
      abs(difference)
    )
    
    
    squared_changes <- c(
      squared_changes,
      difference^2
    )
    
    
    weekly_correlations <- c(
      weekly_correlations,
      cor(
        previous,
        current
      )
    )
  }
  
  
  return(
    list(
      mean_absolute_change = mean(
        absolute_changes
      ),
      
      rms_change = sqrt(
        mean(
          squared_changes
        )
      ),
      
      mean_weekly_correlation = mean(
        weekly_correlations
      )
    )
  )
}



# ============================================================
# 7. SIMULATE ONE DYNAMIC SEASON
# ============================================================

simulate_dynamic_season <- function(
    schedule,
    league_teams,
    target_sd,
    drift_sd,
    true_hfa = 2.7,
    noise_sd = 8.5
) {
  
  max_week <- max(
    schedule$week
  )
  
  
  # ----------------------------------------------------------
  # CREATE WEEK-BY-WEEK HIDDEN TEAM RATINGS
  # ----------------------------------------------------------
  
  rating_matrix <- generate_dynamic_rating_history(
    league_teams = league_teams,
    target_sd = target_sd,
    drift_sd = drift_sd,
    max_week = max_week
  )
  
  
  sim_games <- schedule
  
  
  sim_games$true_home_rating <- NA_real_
  
  sim_games$true_away_rating <- NA_real_
  
  
  
  # ----------------------------------------------------------
  # GET TRUE RATINGS FOR EACH GAME'S WEEK
  # ----------------------------------------------------------
  
  for (i in seq_len(nrow(sim_games))) {
    
    current_week <- sim_games$week[i]
    
    home <- sim_games$home_team[i]
    
    away <- sim_games$away_team[i]
    
    
    sim_games$true_home_rating[i] <-
      rating_matrix[
        current_week,
        home
      ]
    
    
    sim_games$true_away_rating[i] <-
      rating_matrix[
        current_week,
        away
      ]
  }
  
  
  
  # ----------------------------------------------------------
  # TRUE EXPECTED GAME MARGIN
  # ----------------------------------------------------------
  
  sim_games$true_expected_margin <- (
    sim_games$true_home_rating -
      sim_games$true_away_rating +
      true_hfa
  )
  
  
  
  # ----------------------------------------------------------
  # ACTUAL GAME MARGIN
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
  
  
  
  # ----------------------------------------------------------
  # TRUE MOVEMENT DIAGNOSTICS
  # ----------------------------------------------------------
  
  movement <- summarize_true_rating_movement(
    rating_matrix
  )
  
  
  
  return(
    list(
      games = sim_games,
      rating_matrix = rating_matrix,
      mean_absolute_change = movement$mean_absolute_change,
      rms_change = movement$rms_change,
      mean_weekly_correlation = movement$mean_weekly_correlation
    )
  )
}



# ============================================================
# 8. HFA LAMBDA
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
# 9. DYNAMIC TEAM LAMBDA FROM VERSION 6
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
# 10. PARITY-BASED TEAM LAMBDA
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
# 11. SMOOTH MARGIN CAP
# ============================================================
#
# Version 6 winner.
#
# cap = 14 + 5.25 * (estimated SD - 4)
#
# bounded:
#
#     14 <= cap <= 35
#
# ============================================================

choose_cap_smooth <- function(
    parity_sd
) {
  
  cap <- 14 +
    5.25 * (
      parity_sd - 4
    )
  
  
  cap <- max(
    14,
    cap
  )
  
  
  cap <- min(
    35,
    cap
  )
  
  
  return(
    cap
  )
}



# ============================================================
# 12. POWER RATING MODEL
# ============================================================
#
# IMPORTANT:
#
# There is STILL NO RECENCY WEIGHTING here.
#
# Every historical game receives equal weight.
#
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
    crossprod(X) + penalty,
    crossprod(X, y) +
      penalty %*% prior_target
  )
  
  
  beta <- as.numeric(
    beta_matrix
  )
  
  
  names(beta) <- colnames(X)
  
  
  estimated_hfa <- beta["HFA"]
  
  
  ratings <- beta[
    league_teams
  ]
  
  
  ratings <- ratings -
    mean(ratings)
  
  
  
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
# 13. RAW-MARGIN PARITY ESTIMATOR
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
  
  
  return(
    sd(
      parity_model$ratings$rating
    )
  )
}



# ============================================================
# 14. BUILD STATIC PARITY CALIBRATION DATA
# ============================================================
#
# IMPORTANT:
#
# We calibrate using drift = 0.
#
# This preserves the Version 6 calibration system.
#
# We are testing how the existing model handles a new
# dynamic environment rather than retraining it specifically
# for dynamic teams.
#
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "BUILDING STATIC PARITY CALIBRATION DATA\n"
)

cat(
  "========================================\n\n"
)


set.seed(7001)


calibration_rows <- list()

calibration_index <- 1



for (current_true_sd in true_sd_values) {
  
  cat(
    "Calibration SD:",
    current_true_sd,
    "\n"
  )
  
  
  for (s in 1:n_calibration_per_sd) {
    
    simulated <- simulate_dynamic_season(
      schedule = schedule,
      league_teams = teams,
      target_sd = current_true_sd,
      drift_sd = 0,
      true_hfa = true_hfa,
      noise_sd = noise_sd
    )
    
    
    games <- simulated$games
    
    
    
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
# 15. WEEK-SPECIFIC PARITY CALIBRATION
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
# 16. APPLY PARITY CALIBRATION
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
      current_row$slope *
      observed_sd
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
# 17. SAFE CORRELATION
# ============================================================

safe_cor <- function(
    x,
    y
) {
  
  if (
    sd(x) == 0 ||
    sd(y) == 0
  ) {
    
    return(
      NA_real_
    )
  }
  
  
  return(
    cor(
      x,
      y
    )
  )
}



# ============================================================
# 18. EVALUATE CURRENT VERSION 6 MODEL
#    ON ONE DYNAMIC SEASON
# ============================================================

evaluate_dynamic_season <- function(
    games,
    rating_matrix,
    target_sd,
    drift_sd,
    league_teams,
    calibration_coefficients,
    true_hfa,
    adaptive_start_week = 5,
    hfa_prior = 2.5
) {
  
  prediction_rows <- list()
  
  prediction_index <- 1
  
  
  rating_rmse_values <- numeric()
  
  parity_error_values <- numeric()
  
  cap_values <- numeric()
  
  lambda_values <- numeric()
  
  
  
  # ==========================================================
  # WALK FORWARD
  # ==========================================================
  
  for (target_week in 2:max_week) {
    
    training_games <- games[
      games$week < target_week,
    ]
    
    
    test_games <- games[
      games$week == target_week,
    ]
    
    
    completed_week <- target_week - 1
    
    
    
    # --------------------------------------------------------
    # HFA SHRINKAGE
    # --------------------------------------------------------
    
    current_hfa_lambda <- get_hfa_lambda(
      data = training_games,
      league_teams = league_teams
    )
    
    
    
    # --------------------------------------------------------
    # SETTINGS
    # --------------------------------------------------------
    
    if (target_week < adaptive_start_week) {
      
      estimated_parity_sd <- NA_real_
      
      margin_cap <- 21
      
      team_lambda <- 2
      
    } else {
      
      observed_parity_sd <- estimate_observed_parity_sd(
        training_games = training_games,
        league_teams = league_teams,
        hfa_prior = hfa_prior
      )
      
      
      estimated_parity_sd <- calibrate_parity_sd(
        observed_sd = observed_parity_sd,
        completed_week = completed_week,
        calibration_coefficients = calibration_coefficients
      )
      
      
      margin_cap <- choose_cap_smooth(
        estimated_parity_sd
      )
      
      
      team_lambda <- choose_team_lambda(
        parity_sd = estimated_parity_sd,
        training_games = training_games,
        league_teams = league_teams
      )
      
      
      parity_error_values <- c(
        parity_error_values,
        abs(
          estimated_parity_sd -
            target_sd
        )
      )
      
      
      cap_values <- c(
        cap_values,
        margin_cap
      )
      
      
      lambda_values <- c(
        lambda_values,
        team_lambda
      )
    }
    
    
    
    # --------------------------------------------------------
    # FIT CURRENT MODEL
    # --------------------------------------------------------
    
    model <- fit_power_ratings(
      data = training_games,
      league_teams = league_teams,
      margin_cap = margin_cap,
      team_lambda = team_lambda,
      hfa_lambda = current_hfa_lambda,
      hfa_prior = hfa_prior
    )
    
    
    
    rating_lookup <- model$ratings$rating
    
    
    names(rating_lookup) <-
      model$ratings$team
    
    
    
    # --------------------------------------------------------
    # CURRENT TRUE RATINGS
    #
    # These are the hidden strengths that are true during
    # the week we are trying to predict.
    # --------------------------------------------------------
    
    current_true_ratings <- rating_matrix[
      target_week,
    ]
    
    
    names(current_true_ratings) <-
      league_teams
    
    
    
    # --------------------------------------------------------
    # RATING RECOVERY ERROR
    # --------------------------------------------------------
    
    current_rating_rmse <- sqrt(
      mean(
        (
          rating_lookup[league_teams] -
            current_true_ratings[league_teams]
        )^2
      )
    )
    
    
    rating_rmse_values <- c(
      rating_rmse_values,
      current_rating_rmse
    )
    
    
    
    # --------------------------------------------------------
    # MODEL PREDICTIONS
    # --------------------------------------------------------
    
    predicted_margin <- (
      rating_lookup[
        test_games$home_team
      ] -
        rating_lookup[
          test_games$away_team
        ] +
        model$home_field
    )
    
    
    
    # --------------------------------------------------------
    # ORACLE PREDICTION
    #
    # Oracle knows current hidden ratings and true HFA.
    #
    # This gives us the irreducible simulation benchmark.
    # --------------------------------------------------------
    
    oracle_prediction <- (
      current_true_ratings[
        test_games$home_team
      ] -
        current_true_ratings[
          test_games$away_team
        ] +
        true_hfa
    )
    
    
    
    current_predictions <- data.frame(
      week = target_week,
      
      actual_margin =
        test_games$actual_margin,
      
      model_prediction =
        as.numeric(predicted_margin),
      
      oracle_prediction =
        as.numeric(oracle_prediction),
      
      stringsAsFactors = FALSE
    )
    
    
    
    current_predictions$model_error <- (
      current_predictions$actual_margin -
        current_predictions$model_prediction
    )
    
    
    current_predictions$oracle_error <- (
      current_predictions$actual_margin -
        current_predictions$oracle_prediction
    )
    
    
    current_predictions$model_correct <- (
      current_predictions$actual_margin > 0
    ) == (
      current_predictions$model_prediction > 0
    )
    
    
    current_predictions$oracle_correct <- (
      current_predictions$actual_margin > 0
    ) == (
      current_predictions$oracle_prediction > 0
    )
    
    
    
    prediction_rows[[prediction_index]] <-
      current_predictions
    
    
    prediction_index <- prediction_index + 1
  }
  
  
  
  predictions <- do.call(
    rbind,
    prediction_rows
  )
  
  
  
  # ==========================================================
  # RETURN SEASON METRICS
  # ==========================================================
  
  result <- data.frame(
    true_rating_SD = target_sd,
    
    drift_SD = drift_sd,
    
    model_MAE = mean(
      abs(
        predictions$model_error
      )
    ),
    
    model_RMSE = sqrt(
      mean(
        predictions$model_error^2
      )
    ),
    
    model_accuracy = mean(
      predictions$model_correct
    ),
    
    model_correlation = safe_cor(
      predictions$model_prediction,
      predictions$actual_margin
    ),
    
    model_bias = mean(
      predictions$model_error
    ),
    
    current_rating_RMSE = mean(
      rating_rmse_values
    ),
    
    oracle_MAE = mean(
      abs(
        predictions$oracle_error
      )
    ),
    
    oracle_RMSE = sqrt(
      mean(
        predictions$oracle_error^2
      )
    ),
    
    oracle_accuracy = mean(
      predictions$oracle_correct
    ),
    
    parity_MAE = if (
      length(parity_error_values) > 0
    ) {
      mean(parity_error_values)
    } else {
      NA_real_
    },
    
    mean_margin_cap = if (
      length(cap_values) > 0
    ) {
      mean(cap_values)
    } else {
      NA_real_
    },
    
    mean_team_lambda = if (
      length(lambda_values) > 0
    ) {
      mean(lambda_values)
    } else {
      NA_real_
    }
  )
  
  
  return(
    result
  )
}



# ============================================================
# 19. RUN VERSION 7A MONTE CARLO
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "STARTING VERSION 7A MONTE CARLO\n"
)

cat(
  "========================================\n\n"
)


set.seed(7002)


season_result_rows <- list()

result_index <- 1



for (current_true_sd in true_sd_values) {
  
  for (current_drift_sd in drift_sd_values) {
    
    cat(
      "\nTrue SD:",
      current_true_sd,
      "| Drift SD:",
      current_drift_sd,
      "\n"
    )
    
    
    for (s in 1:n_evaluation_per_cell) {
      
      # ------------------------------------------------------
      # SIMULATE DYNAMIC SEASON
      # ------------------------------------------------------
      
      simulated <- simulate_dynamic_season(
        schedule = schedule,
        league_teams = teams,
        target_sd = current_true_sd,
        drift_sd = current_drift_sd,
        true_hfa = true_hfa,
        noise_sd = noise_sd
      )
      
      
      # ------------------------------------------------------
      # EVALUATE CURRENT VERSION 6 MODEL
      # ------------------------------------------------------
      
      season_metrics <- evaluate_dynamic_season(
        games = simulated$games,
        rating_matrix = simulated$rating_matrix,
        target_sd = current_true_sd,
        drift_sd = current_drift_sd,
        league_teams = teams,
        calibration_coefficients = calibration_coefficients,
        true_hfa = true_hfa,
        adaptive_start_week = adaptive_start_week,
        hfa_prior = hfa_prior
      )
      
      
      # ------------------------------------------------------
      # ADD TRUE MOVEMENT DIAGNOSTICS
      # ------------------------------------------------------
      
      season_metrics$mean_absolute_true_change <-
        simulated$mean_absolute_change
      
      
      season_metrics$rms_true_change <-
        simulated$rms_change
      
      
      season_metrics$mean_weekly_true_correlation <-
        simulated$mean_weekly_correlation
      
      
      season_metrics$simulation <- s
      
      
      
      season_result_rows[[result_index]] <-
        season_metrics
      
      
      result_index <- result_index + 1
      
      
      
      # ------------------------------------------------------
      # PROGRESS
      # ------------------------------------------------------
      
      if (s %% 100 == 0) {
        
        cat(
          "Completed",
          s,
          "of",
          n_evaluation_per_cell,
          "\n"
        )
      }
    }
  }
}



season_results <- do.call(
  rbind,
  season_result_rows
)


rownames(season_results) <- NULL



# ============================================================
# 20. SUMMARY BY TRUE SD AND DRIFT SD
# ============================================================

summary_by_cell <- aggregate(
  cbind(
    model_MAE,
    model_RMSE,
    model_accuracy,
    model_correlation,
    current_rating_RMSE,
    oracle_MAE,
    oracle_RMSE,
    oracle_accuracy,
    parity_MAE,
    mean_margin_cap,
    mean_team_lambda,
    mean_absolute_true_change,
    rms_true_change,
    mean_weekly_true_correlation
  ) ~
    true_rating_SD +
    drift_SD,
  data = season_results,
  FUN = mean,
  na.rm = TRUE
)


summary_by_cell <- summary_by_cell[
  order(
    summary_by_cell$true_rating_SD,
    summary_by_cell$drift_SD
  ),
]


rownames(summary_by_cell) <- NULL



cat(
  "\n\n========================================\n"
)

cat(
  "RESULTS BY TRUE SD AND DRIFT SD\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_by_cell
)



# ============================================================
# 21. OVERALL SUMMARY BY DRIFT
# ============================================================

summary_by_drift <- aggregate(
  cbind(
    model_MAE,
    model_RMSE,
    model_accuracy,
    model_correlation,
    current_rating_RMSE,
    oracle_MAE,
    oracle_RMSE,
    oracle_accuracy,
    parity_MAE,
    mean_margin_cap,
    mean_team_lambda,
    mean_absolute_true_change,
    rms_true_change,
    mean_weekly_true_correlation
  ) ~
    drift_SD,
  data = season_results,
  FUN = mean,
  na.rm = TRUE
)


summary_by_drift <- summary_by_drift[
  order(
    summary_by_drift$drift_SD
  ),
]


rownames(summary_by_drift) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "OVERALL RESULTS BY DRIFT SD\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_by_drift
)



# ============================================================
# 22. COMPARE EACH DRIFT LEVEL TO STABLE-TEAM BASELINE
# ============================================================

stable_row <- summary_by_drift[
  summary_by_drift$drift_SD == 0,
]


drift_comparison <- summary_by_drift


drift_comparison$MAE_change_from_stable <- (
  drift_comparison$model_MAE -
    stable_row$model_MAE
)


drift_comparison$RMSE_change_from_stable <- (
  drift_comparison$model_RMSE -
    stable_row$model_RMSE
)


drift_comparison$rating_RMSE_change_from_stable <- (
  drift_comparison$current_rating_RMSE -
    stable_row$current_rating_RMSE
)


drift_comparison$oracle_MAE_change_from_stable <- (
  drift_comparison$oracle_MAE -
    stable_row$oracle_MAE
)



cat(
  "\n========================================\n"
)

cat(
  "CHANGE FROM STABLE-TEAM BASELINE\n"
)

cat(
  "========================================\n\n"
)


print(
  drift_comparison[
    ,
    c(
      "drift_SD",
      "model_MAE",
      "MAE_change_from_stable",
      "model_RMSE",
      "RMSE_change_from_stable",
      "current_rating_RMSE",
      "rating_RMSE_change_from_stable",
      "oracle_MAE",
      "oracle_MAE_change_from_stable"
    )
  ]
)



# ============================================================
# 23. MODEL-ORACLE GAP
# ============================================================
#
# As teams change more rapidly, the model may fall farther
# behind the oracle.
#
# ============================================================

summary_by_drift$model_oracle_MAE_gap <- (
  summary_by_drift$model_MAE -
    summary_by_drift$oracle_MAE
)


summary_by_drift$model_oracle_RMSE_gap <- (
  summary_by_drift$model_RMSE -
    summary_by_drift$oracle_RMSE
)



cat(
  "\n========================================\n"
)

cat(
  "MODEL VS ORACLE GAP BY DRIFT\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_by_drift[
    ,
    c(
      "drift_SD",
      "model_MAE",
      "oracle_MAE",
      "model_oracle_MAE_gap",
      "model_RMSE",
      "oracle_RMSE",
      "model_oracle_RMSE_gap"
    )
  ]
)



# ============================================================
# 24. VALIDATE THAT PARITY IS PRESERVED
# ============================================================
#
# We explicitly verify the hidden rating SD remains at
# its requested target despite team movement.
#
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "DYNAMIC SIMULATOR VALIDATION EXAMPLE\n"
)

cat(
  "========================================\n\n"
)


set.seed(7003)


example_simulation <- simulate_dynamic_season(
  schedule = schedule,
  league_teams = teams,
  target_sd = 5,
  drift_sd = 1,
  true_hfa = true_hfa,
  noise_sd = noise_sd
)


example_rating_matrix <-
  example_simulation$rating_matrix


example_weekly_sd <- apply(
  example_rating_matrix,
  1,
  sd
)


example_weekly_mean <- apply(
  example_rating_matrix,
  1,
  mean
)


example_validation <- data.frame(
  week = 1:max_week,
  rating_mean = example_weekly_mean,
  rating_SD = example_weekly_sd
)


print(
  example_validation
)



# ============================================================
# 25. EXAMPLE TRUE RATING HISTORY
# ============================================================

example_rating_history <- data.frame(
  week = 1:max_week,
  example_rating_matrix,
  check.names = FALSE
)



cat(
  "\n========================================\n"
)

cat(
  "EXAMPLE WEEKLY TRUE RATINGS\n"
)

cat(
  "========================================\n\n"
)


print(
  example_rating_history
)



# ============================================================
# 26. PLOT EXAMPLE TRUE TEAM MOVEMENT
# ============================================================

example_long_rows <- list()

example_long_index <- 1



for (team_name in teams) {
  
  example_long_rows[[example_long_index]] <- data.frame(
    week = 1:max_week,
    team = team_name,
    true_rating = example_rating_matrix[, team_name],
    stringsAsFactors = FALSE
  )
  
  
  example_long_index <- example_long_index + 1
}



example_long <- do.call(
  rbind,
  example_long_rows
)



true_rating_plot <- ggplot(
  example_long,
  aes(
    x = week,
    y = true_rating,
    group = team,
    linetype = team
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.5
  ) +
  scale_x_continuous(
    breaks = 1:max_week
  ) +
  labs(
    title = "Version 7A Example: Hidden Team Strength Changes",
    subtitle = "Target league rating SD = 5, weekly drift SD = 1",
    x = "Week",
    y = "Hidden True Rating",
    linetype = "Team"
  ) +
  theme_minimal()


print(
  true_rating_plot
)



# ============================================================
# 27. PLOT MODEL MAE BY DRIFT
# ============================================================

mae_drift_plot <- ggplot(
  summary_by_drift,
  aes(
    x = drift_SD,
    y = model_MAE
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Current Model Accuracy as Team Strength Becomes Dynamic",
    x = "Weekly True Rating Drift SD",
    y = "Out-of-Sample MAE"
  ) +
  theme_minimal()


print(
  mae_drift_plot
)



# ============================================================
# 28. PLOT RATING RECOVERY BY DRIFT
# ============================================================

rating_rmse_plot <- ggplot(
  summary_by_drift,
  aes(
    x = drift_SD,
    y = current_rating_RMSE
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Ability to Recover Current Team Strength",
    subtitle = "Ratings are compared with the hidden strength during the predicted week",
    x = "Weekly True Rating Drift SD",
    y = "Current-Team Rating RMSE"
  ) +
  theme_minimal()


print(
  rating_rmse_plot
)



# ============================================================
# 29. PLOT MODEL VS ORACLE
# ============================================================

oracle_plot_data <- rbind(
  data.frame(
    drift_SD = summary_by_drift$drift_SD,
    model = "Current Rating Model",
    MAE = summary_by_drift$model_MAE
  ),
  
  data.frame(
    drift_SD = summary_by_drift$drift_SD,
    model = "True-Rating Oracle",
    MAE = summary_by_drift$oracle_MAE
  )
)



oracle_plot <- ggplot(
  oracle_plot_data,
  aes(
    x = drift_SD,
    y = MAE,
    group = model,
    linetype = model
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Current Model vs True-Strength Oracle",
    x = "Weekly True Rating Drift SD",
    y = "Out-of-Sample MAE",
    linetype = "Prediction Model"
  ) +
  theme_minimal()


print(
  oracle_plot
)



# ============================================================
# 30. PLOT REALIZED TEAM MOVEMENT
# ============================================================

movement_plot <- ggplot(
  summary_by_drift,
  aes(
    x = drift_SD,
    y = mean_absolute_true_change
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Realized Week-to-Week Team Strength Movement",
    x = "Requested Weekly Drift SD",
    y = "Mean Absolute Weekly Rating Change"
  ) +
  theme_minimal()


print(
  movement_plot
)



# ============================================================
# 31. PLOT WEEKLY TRUE RATING CORRELATION
# ============================================================

correlation_plot <- ggplot(
  summary_by_drift,
  aes(
    x = drift_SD,
    y = mean_weekly_true_correlation
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Persistence of True Team Strength",
    x = "Weekly Drift SD",
    y = "Average Correlation Between Consecutive Weeks"
  ) +
  theme_minimal()


print(
  correlation_plot
)



# ============================================================
# 32. SAVE RESULTS
# ============================================================

write.csv(
  calibration_coefficients,
  "version7A_calibration_coefficients.csv",
  row.names = FALSE
)


write.csv(
  season_results,
  "version7A_all_season_results.csv",
  row.names = FALSE
)


write.csv(
  summary_by_cell,
  "version7A_results_by_parity_and_drift.csv",
  row.names = FALSE
)


write.csv(
  summary_by_drift,
  "version7A_results_by_drift.csv",
  row.names = FALSE
)


write.csv(
  drift_comparison,
  "version7A_change_from_stable_baseline.csv",
  row.names = FALSE
)


write.csv(
  example_rating_history,
  "version7A_example_dynamic_true_ratings.csv",
  row.names = FALSE
)



# ============================================================
# 33. FINAL OUTPUT
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 7A COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "True rating SD values:",
  paste(
    true_sd_values,
    collapse = ", "
  ),
  "\n"
)


cat(
  "Drift SD values:",
  paste(
    drift_sd_values,
    collapse = ", "
  ),
  "\n"
)


cat(
  "Simulations per parity x drift cell:",
  n_evaluation_per_cell,
  "\n"
)


cat(
  "Total dynamic evaluation seasons:",
  length(true_sd_values) *
    length(drift_sd_values) *
    n_evaluation_per_cell,
  "\n\n"
)



cat(
  "OVERALL RESULTS BY DRIFT:\n\n"
)


print(
  summary_by_drift
)



cat(
  "\nCHANGE FROM STABLE-TEAM BASELINE:\n\n"
)


print(
  drift_comparison[
    ,
    c(
      "drift_SD",
      "model_MAE",
      "MAE_change_from_stable",
      "model_RMSE",
      "RMSE_change_from_stable",
      "current_rating_RMSE",
      "rating_RMSE_change_from_stable",
      "oracle_MAE",
      "oracle_MAE_change_from_stable"
    )
  ]
)



cat(
  "\nMODEL VS ORACLE GAP:\n\n"
)


print(
  summary_by_drift[
    ,
    c(
      "drift_SD",
      "model_MAE",
      "oracle_MAE",
      "model_oracle_MAE_gap",
      "current_rating_RMSE",
      "mean_absolute_true_change",
      "mean_weekly_true_correlation"
    )
  ]
)


# ============================================================
# END VERSION 7A
# ============================================================
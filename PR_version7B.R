# ============================================================
# FOOTBALL POWER RATINGS - VERSION 7B
# RECENCY HALF-LIFE x TEAM-STRENGTH DRIFT
# ============================================================
#
# PURPOSE
# -------
#
# Version 7A showed that the current model becomes worse
# when hidden team strength changes during the season.
#
# Version 7B tests whether weighting recent games more heavily
# improves performance.
#
#
# IMPORTANT:
#
# The following remain unchanged:
#
#   - Raw-margin parity estimator
#   - Week-specific parity calibration
#   - Smooth adaptive margin cap
#   - Parity-based team lambda
#   - HFA prior
#   - Walk-forward evaluation
#
#
# ONLY NEW FEATURE:
#
#   Recency weighting in the final power-rating fit.
#
#
# HALF-LIFE INTERPRETATION
# ------------------------
#
# A 4-week half-life means:
#
#   A game 4 weeks older than the most recent available
#   game receives half as much relative weight.
#
#
# Raw recency weight:
#
#   weight = 0.5^(weeks_old / half_life)
#
#
# We normalize weights to mean 1 before fitting.
#
# This is important because ridge regression depends on the
# absolute scale of the data relative to the penalty.
#
# Normalization lets us isolate:
#
#   RECENCY EMPHASIS
#
# rather than:
#
#   recency + artificially stronger ridge shrinkage.
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
# 2. GLOBAL SETTINGS
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


drift_sd_values <- c(
  0.00,
  0.25,
  0.50,
  0.75,
  1.00,
  1.50
)



# ============================================================
# 3. HALF-LIVES TO TEST
# ============================================================
#
# Inf = no decay.
#
# ============================================================

half_life_values <- c(
  Inf,
  12,
  8,
  6,
  4,
  3,
  2
)


half_life_labels <- c(
  "No Decay",
  "HL 12",
  "HL 8",
  "HL 6",
  "HL 4",
  "HL 3",
  "HL 2"
)


names(half_life_labels) <- as.character(
  half_life_values
)



# ============================================================
# 4. MODEL SETTINGS
# ============================================================

adaptive_start_week <- 5

lambda_parity_threshold <- 4.5



# ============================================================
# 5. MONTE CARLO SETTINGS
# ============================================================
#
# 500 gives:
#
#   7 parity levels
# x 6 drift levels
# x 500 seasons
# = 21,000 seasons
#
# Each season evaluates all 7 half-lives.
#
# If runtime is too long during development, temporarily use:
#
# n_evaluation_per_cell <- 100
#
# Then return to 500 for the real run.
#
# ============================================================

n_calibration_per_sd <- 500

n_evaluation_per_cell <- 500



# ============================================================
# 6. GENERATE INITIAL TRUE RATINGS
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
  
  
  return(ratings)
}



# ============================================================
# 7. UPDATE TRUE RATINGS
# ============================================================

update_true_ratings <- function(
    previous_ratings,
    target_sd,
    drift_sd
) {
  
  if (drift_sd == 0) {
    
    return(
      previous_ratings
    )
  }
  
  
  new_ratings <- previous_ratings +
    rnorm(
      length(previous_ratings),
      mean = 0,
      sd = drift_sd
    )
  
  
  names(new_ratings) <-
    names(previous_ratings)
  
  
  # Center league around zero
  
  new_ratings <- new_ratings -
    mean(new_ratings)
  
  
  # Preserve exact league parity SD
  
  current_sd <- sd(
    new_ratings
  )
  
  
  if (current_sd > 0) {
    
    new_ratings <- (
      new_ratings /
        current_sd
    ) * target_sd
  }
  
  
  return(new_ratings)
}



# ============================================================
# 8. GENERATE DYNAMIC RATING HISTORY
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
  
  
  # Week 1
  
  rating_matrix[1, ] <- generate_true_ratings(
    league_teams = league_teams,
    target_sd = target_sd
  )
  
  
  # Weeks 2+
  
  if (max_week >= 2) {
    
    for (current_week in 2:max_week) {
      
      previous_ratings <- rating_matrix[
        current_week - 1,
      ]
      
      
      names(previous_ratings) <-
        league_teams
      
      
      rating_matrix[current_week, ] <- update_true_ratings(
        previous_ratings = previous_ratings,
        target_sd = target_sd,
        drift_sd = drift_sd
      )
    }
  }
  
  
  return(rating_matrix)
}



# ============================================================
# 9. TRUE-RATING MOVEMENT SUMMARY
# ============================================================

summarize_true_rating_movement <- function(
    rating_matrix
) {
  
  n_weeks <- nrow(
    rating_matrix
  )
  
  
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
# 10. SIMULATE DYNAMIC SEASON
# ============================================================

simulate_dynamic_season <- function(
    schedule,
    league_teams,
    target_sd,
    drift_sd,
    true_hfa = 2.7,
    noise_sd = 8.5
) {
  
  current_max_week <- max(
    schedule$week
  )
  
  
  rating_matrix <- generate_dynamic_rating_history(
    league_teams = league_teams,
    target_sd = target_sd,
    drift_sd = drift_sd,
    max_week = current_max_week
  )
  
  
  sim_games <- schedule
  
  
  sim_games$true_home_rating <- NA_real_
  
  sim_games$true_away_rating <- NA_real_
  
  
  
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
  
  
  # True expected margin
  
  sim_games$true_expected_margin <- (
    sim_games$true_home_rating -
      sim_games$true_away_rating +
      true_hfa
  )
  
  
  # Simulated actual margin
  
  sim_games$actual_margin <- round(
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
  
  
  if (length(zero_games) > 0) {
    
    sim_games$actual_margin[zero_games] <- sample(
      c(-1, 1),
      size = length(zero_games),
      replace = TRUE
    )
  }
  
  
  movement <- summarize_true_rating_movement(
    rating_matrix
  )
  
  
  return(
    list(
      games = sim_games,
      
      rating_matrix = rating_matrix,
      
      mean_absolute_change =
        movement$mean_absolute_change,
      
      rms_change =
        movement$rms_change,
      
      mean_weekly_correlation =
        movement$mean_weekly_correlation
    )
  )
}



# ============================================================
# 11. HFA LAMBDA
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
# 12. DYNAMIC TEAM LAMBDA
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
# 13. PARITY-BASED TEAM LAMBDA
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
# 14. SMOOTH MARGIN CAP
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
  
  
  return(cap)
}



# ============================================================
# 15. CREATE RECENCY WEIGHTS
# ============================================================
#
# Most recent available week gets raw weight = 1.
#
# If half-life = 4:
#
#   0 weeks older -> 1.000
#   1 week older  -> 0.841
#   2 weeks older -> 0.707
#   4 weeks older -> 0.500
#   8 weeks older -> 0.250
#
#
# Then weights are normalized so:
#
#   mean(weight) = 1
#
#
# The RELATIVE half-life remains exactly the same.
#
# ============================================================

make_recency_weights <- function(
    training_games,
    half_life
) {
  
  n_games <- nrow(
    training_games
  )
  
  
  # No decay
  
  if (is.infinite(half_life)) {
    
    return(
      rep(
        1,
        n_games
      )
    )
  }
  
  
  # Most recent completed week
  
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
  
  
  # ----------------------------------------------------------
  # NORMALIZE
  # ----------------------------------------------------------
  #
  # Keeps average weight = 1 so the total scale of
  # the likelihood remains comparable to the original model.
  #
  # ----------------------------------------------------------
  
  normalized_weights <- (
    raw_weights /
      mean(raw_weights)
  )
  
  
  return(
    normalized_weights
  )
}



# ============================================================
# 16. EFFECTIVE SAMPLE SIZE
# ============================================================
#
# Useful diagnostic.
#
# ESS = (sum w)^2 / sum(w^2)
#
# ============================================================

effective_sample_size <- function(
    weights
) {
  
  return(
    sum(weights)^2 /
      sum(weights^2)
  )
}



# ============================================================
# 17. WEIGHTED POWER-RATING MODEL
# ============================================================
#
# Weighted ridge:
#
# beta =
#
# (X' W X + P)^(-1)
# (X' W y + P * prior)
#
#
# Instead of building W:
#
#   Xw = sqrt(W) X
#   yw = sqrt(W) y
#
# ============================================================

fit_power_ratings <- function(
    data,
    league_teams,
    margin_cap,
    team_lambda,
    hfa_lambda,
    hfa_prior = 2.5,
    game_weights = NULL
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
  # GAME WEIGHTS
  # ----------------------------------------------------------
  
  if (is.null(game_weights)) {
    
    game_weights <- rep(
      1,
      n_games
    )
  }
  
  
  if (length(game_weights) != n_games) {
    
    stop(
      "Length of game_weights must equal number of games."
    )
  }
  
  
  if (any(game_weights <= 0)) {
    
    stop(
      "All game weights must be positive."
    )
  }
  
  
  
  sqrt_weights <- sqrt(
    game_weights
  )
  
  
  # Explicitly weight rows
  
  X_weighted <- sweep(
    X,
    MARGIN = 1,
    STATS = sqrt_weights,
    FUN = "*"
  )
  
  
  y_weighted <- (
    y *
      sqrt_weights
  )
  
  
  
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
  # WEIGHTED RIDGE SOLUTION
  # ----------------------------------------------------------
  
  beta_matrix <- solve(
    crossprod(X_weighted) +
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
  
  
  names(beta) <- colnames(X)
  
  
  estimated_hfa <- beta[
    "HFA"
  ]
  
  
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
# 18. RAW PARITY ESTIMATOR
# ============================================================
#
# IMPORTANT:
#
# Parity estimator remains UNWEIGHTED.
#
# Version 7B is isolating recency in the final team-rating fit.
#
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
    hfa_prior = hfa_prior,
    game_weights = rep(
      1,
      nrow(training_games)
    )
  )
  
  
  return(
    sd(
      parity_model$ratings$rating
    )
  )
}



# ============================================================
# 19. BUILD STATIC PARITY CALIBRATION DATA
# ============================================================
#
# Same approach as Version 7A.
#
# Calibration uses zero-drift seasons.
#
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
      
      
      calibration_index <- (
        calibration_index + 1
      )
    }
  }
}



calibration_data <- do.call(
  rbind,
  calibration_rows
)


rownames(calibration_data) <- NULL



# ============================================================
# 20. WEEK-SPECIFIC CALIBRATION EQUATIONS
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
# 21. APPLY PARITY CALIBRATION
# ============================================================

calibrate_parity_sd <- function(
    observed_sd,
    completed_week,
    calibration_coefficients
) {
  
  current_row <- calibration_coefficients[
    calibration_coefficients$week ==
      completed_week,
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
# 22. SAFE CORRELATION
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
# 23. EVALUATE ALL HALF-LIVES ON ONE SEASON
# ============================================================

evaluate_half_lives <- function(
    games,
    rating_matrix,
    target_sd,
    drift_sd,
    league_teams,
    calibration_coefficients,
    half_life_values,
    true_hfa,
    adaptive_start_week = 5,
    hfa_prior = 2.5
) {
  
  # ----------------------------------------------------------
  # STORAGE
  # ----------------------------------------------------------
  
  prediction_storage <- list()
  
  rating_rmse_storage <- list()
  
  ess_storage <- list()
  
  
  for (half_life in half_life_values) {
    
    current_key <- as.character(
      half_life
    )
    
    
    prediction_storage[[current_key]] <- list()
    
    rating_rmse_storage[[current_key]] <- numeric()
    
    ess_storage[[current_key]] <- numeric()
  }
  
  
  parity_error_values <- numeric()
  
  cap_values <- numeric()
  
  lambda_values <- numeric()
  
  
  oracle_prediction_rows <- list()
  
  oracle_index <- 1
  
  
  
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
    
    
    completed_week <- (
      target_week - 1
    )
    
    
    
    # --------------------------------------------------------
    # HFA SHRINKAGE
    # --------------------------------------------------------
    
    current_hfa_lambda <- get_hfa_lambda(
      data = training_games,
      league_teams = league_teams
    )
    
    
    
    # --------------------------------------------------------
    # PARITY / CAP / LAMBDA
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
    # CURRENT HIDDEN TRUE RATINGS
    # --------------------------------------------------------
    
    current_true_ratings <- rating_matrix[
      target_week,
    ]
    
    
    names(current_true_ratings) <-
      league_teams
    
    
    
    # --------------------------------------------------------
    # ORACLE
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
    
    
    oracle_current <- data.frame(
      week = target_week,
      
      actual_margin =
        test_games$actual_margin,
      
      oracle_prediction =
        as.numeric(
          oracle_prediction
        )
    )
    
    
    oracle_current$oracle_error <- (
      oracle_current$actual_margin -
        oracle_current$oracle_prediction
    )
    
    
    oracle_current$oracle_correct <- (
      oracle_current$actual_margin > 0
    ) == (
      oracle_current$oracle_prediction > 0
    )
    
    
    oracle_prediction_rows[[oracle_index]] <-
      oracle_current
    
    
    oracle_index <- (
      oracle_index + 1
    )
    
    
    
    # ========================================================
    # FIT EACH HALF-LIFE
    # ========================================================
    
    for (half_life in half_life_values) {
      
      current_key <- as.character(
        half_life
      )
      
      
      # ------------------------------------------------------
      # RECENCY WEIGHTS
      # ------------------------------------------------------
      
      game_weights <- make_recency_weights(
        training_games = training_games,
        half_life = half_life
      )
      
      
      current_ess <- effective_sample_size(
        game_weights
      )
      
      
      # ------------------------------------------------------
      # FIT MODEL
      # ------------------------------------------------------
      
      model <- fit_power_ratings(
        data = training_games,
        league_teams = league_teams,
        margin_cap = margin_cap,
        team_lambda = team_lambda,
        hfa_lambda = current_hfa_lambda,
        hfa_prior = hfa_prior,
        game_weights = game_weights
      )
      
      
      rating_lookup <- model$ratings$rating
      
      
      names(rating_lookup) <-
        model$ratings$team
      
      
      
      # ------------------------------------------------------
      # CURRENT RATING RECOVERY
      # ------------------------------------------------------
      
      current_rating_rmse <- sqrt(
        mean(
          (
            rating_lookup[league_teams] -
              current_true_ratings[league_teams]
          )^2
        )
      )
      
      
      
      # ------------------------------------------------------
      # PREDICT
      # ------------------------------------------------------
      
      predicted_margin <- (
        rating_lookup[
          test_games$home_team
        ] -
          rating_lookup[
            test_games$away_team
          ] +
          model$home_field
      )
      
      
      current_predictions <- data.frame(
        week = target_week,
        
        actual_margin =
          test_games$actual_margin,
        
        predicted_margin =
          as.numeric(
            predicted_margin
          )
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
      
      
      
      # ------------------------------------------------------
      # STORE
      # ------------------------------------------------------
      
      next_position <- (
        length(
          prediction_storage[[current_key]]
        ) + 1
      )
      
      
      prediction_storage[[current_key]][[next_position]] <-
        current_predictions
      
      
      rating_rmse_storage[[current_key]] <- c(
        rating_rmse_storage[[current_key]],
        current_rating_rmse
      )
      
      
      ess_storage[[current_key]] <- c(
        ess_storage[[current_key]],
        current_ess
      )
    }
  }
  
  
  
  # ==========================================================
  # ORACLE SUMMARY
  # ==========================================================
  
  oracle_predictions <- do.call(
    rbind,
    oracle_prediction_rows
  )
  
  
  oracle_MAE <- mean(
    abs(
      oracle_predictions$oracle_error
    )
  )
  
  
  oracle_RMSE <- sqrt(
    mean(
      oracle_predictions$oracle_error^2
    )
  )
  
  
  oracle_accuracy <- mean(
    oracle_predictions$oracle_correct
  )
  
  
  
  # ==========================================================
  # SUMMARIZE EACH HALF-LIFE
  # ==========================================================
  
  result_rows <- list()
  
  result_index <- 1
  
  
  
  for (half_life in half_life_values) {
    
    current_key <- as.character(
      half_life
    )
    
    
    predictions <- do.call(
      rbind,
      prediction_storage[[current_key]]
    )
    
    
    current_label <- if (
      is.infinite(half_life)
    ) {
      
      "No Decay"
      
    } else {
      
      paste0(
        "HL ",
        half_life
      )
    }
    
    
    result_rows[[result_index]] <- data.frame(
      true_rating_SD = target_sd,
      
      drift_SD = drift_sd,
      
      half_life = half_life,
      
      half_life_label = current_label,
      
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
      
      current_rating_RMSE = mean(
        rating_rmse_storage[[current_key]]
      ),
      
      mean_effective_sample_size = mean(
        ess_storage[[current_key]]
      ),
      
      oracle_MAE = oracle_MAE,
      
      oracle_RMSE = oracle_RMSE,
      
      oracle_accuracy = oracle_accuracy,
      
      parity_MAE = if (
        length(parity_error_values) > 0
      ) {
        mean(
          parity_error_values
        )
      } else {
        NA_real_
      },
      
      mean_margin_cap = if (
        length(cap_values) > 0
      ) {
        mean(
          cap_values
        )
      } else {
        NA_real_
      },
      
      mean_team_lambda = if (
        length(lambda_values) > 0
      ) {
        mean(
          lambda_values
        )
      } else {
        NA_real_
      },
      
      stringsAsFactors = FALSE
    )
    
    
    result_index <- (
      result_index + 1
    )
  }
  
  
  return(
    do.call(
      rbind,
      result_rows
    )
  )
}



# ============================================================
# 24. RUN VERSION 7B MONTE CARLO
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "STARTING VERSION 7B MONTE CARLO\n"
)

cat(
  "========================================\n\n"
)


set.seed(7004)


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
      # SIMULATE ONE SEASON
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
      # TEST EVERY HALF-LIFE ON THE SAME SEASON
      # ------------------------------------------------------
      
      season_metrics <- evaluate_half_lives(
        games = simulated$games,
        rating_matrix = simulated$rating_matrix,
        target_sd = current_true_sd,
        drift_sd = current_drift_sd,
        league_teams = teams,
        calibration_coefficients = calibration_coefficients,
        half_life_values = half_life_values,
        true_hfa = true_hfa,
        adaptive_start_week = adaptive_start_week,
        hfa_prior = hfa_prior
      )
      
      
      season_metrics$simulation <- s
      
      
      season_metrics$mean_absolute_true_change <-
        simulated$mean_absolute_change
      
      
      season_metrics$rms_true_change <-
        simulated$rms_change
      
      
      season_metrics$mean_weekly_true_correlation <-
        simulated$mean_weekly_correlation
      
      
      
      season_result_rows[[result_index]] <-
        season_metrics
      
      
      result_index <- (
        result_index + 1
      )
      
      
      
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
# 25. ORDER HALF-LIFE LABELS
# ============================================================

season_results$half_life_label <- factor(
  season_results$half_life_label,
  levels = c(
    "No Decay",
    "HL 12",
    "HL 8",
    "HL 6",
    "HL 4",
    "HL 3",
    "HL 2"
  )
)



# ============================================================
# 26. SUMMARY BY PARITY, DRIFT, HALF-LIFE
# ============================================================

summary_by_cell <- aggregate(
  cbind(
    MAE,
    RMSE,
    Accuracy,
    Correlation,
    current_rating_RMSE,
    mean_effective_sample_size,
    oracle_MAE,
    oracle_RMSE
  ) ~
    true_rating_SD +
    drift_SD +
    half_life +
    half_life_label,
  data = season_results,
  FUN = mean
)


summary_by_cell <- summary_by_cell[
  order(
    summary_by_cell$true_rating_SD,
    summary_by_cell$drift_SD,
    summary_by_cell$MAE
  ),
]


rownames(summary_by_cell) <- NULL



# ============================================================
# 27. OVERALL SUMMARY BY DRIFT AND HALF-LIFE
# ============================================================

summary_by_drift <- aggregate(
  cbind(
    MAE,
    RMSE,
    Accuracy,
    Correlation,
    current_rating_RMSE,
    mean_effective_sample_size,
    oracle_MAE,
    oracle_RMSE
  ) ~
    drift_SD +
    half_life +
    half_life_label,
  data = season_results,
  FUN = mean
)


summary_by_drift <- summary_by_drift[
  order(
    summary_by_drift$drift_SD,
    summary_by_drift$MAE
  ),
]


rownames(summary_by_drift) <- NULL



cat(
  "\n\n========================================\n"
)

cat(
  "RESULTS BY DRIFT AND HALF-LIFE\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_by_drift
)



# ============================================================
# 28. BEST HALF-LIFE AT EACH DRIFT LEVEL
# ============================================================

drift_groups <- split(
  summary_by_drift,
  summary_by_drift$drift_SD
)


best_half_life_rows <- list()

best_index <- 1



for (drift_name in names(drift_groups)) {
  
  current_data <- drift_groups[[drift_name]]
  
  
  best_row <- current_data[
    which.min(
      current_data$MAE
    ),
  ]
  
  
  best_half_life_rows[[best_index]] <- data.frame(
    drift_SD = best_row$drift_SD,
    
    best_half_life =
      best_row$half_life,
    
    best_half_life_label =
      as.character(
        best_row$half_life_label
      ),
    
    best_MAE =
      best_row$MAE,
    
    best_RMSE =
      best_row$RMSE,
    
    best_rating_RMSE =
      best_row$current_rating_RMSE,
    
    stringsAsFactors = FALSE
  )
  
  
  best_index <- (
    best_index + 1
  )
}



best_half_life_by_drift <- do.call(
  rbind,
  best_half_life_rows
)


rownames(best_half_life_by_drift) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "BEST HALF-LIFE BY DRIFT SD\n"
)

cat(
  "========================================\n\n"
)


print(
  best_half_life_by_drift
)



# ============================================================
# 29. CREATE NO-DECAY BASELINE
# ============================================================

no_decay_results <- season_results[
  season_results$half_life_label ==
    "No Decay",
  c(
    "true_rating_SD",
    "drift_SD",
    "simulation",
    "MAE",
    "RMSE",
    "current_rating_RMSE"
  )
]


names(no_decay_results)[
  names(no_decay_results) == "MAE"
] <- "baseline_MAE"


names(no_decay_results)[
  names(no_decay_results) == "RMSE"
] <- "baseline_RMSE"


names(no_decay_results)[
  names(no_decay_results) ==
    "current_rating_RMSE"
] <- "baseline_rating_RMSE"



# ============================================================
# 30. MERGE BASELINE WITH ALL HALF-LIVES
# ============================================================

paired_results <- merge(
  season_results,
  no_decay_results,
  by = c(
    "true_rating_SD",
    "drift_SD",
    "simulation"
  )
)



# ============================================================
# 31. CALCULATE PAIRED IMPROVEMENT
# ============================================================
#
# Positive improvement means recency weighting is better.
#
# ============================================================

paired_results$MAE_improvement <- (
  paired_results$baseline_MAE -
    paired_results$MAE
)


paired_results$RMSE_improvement <- (
  paired_results$baseline_RMSE -
    paired_results$RMSE
)


paired_results$rating_RMSE_improvement <- (
  paired_results$baseline_rating_RMSE -
    paired_results$current_rating_RMSE
)



# ============================================================
# 32. PAIRED SUMMARY BY DRIFT + HALF-LIFE
# ============================================================

paired_groups <- split(
  paired_results,
  interaction(
    paired_results$drift_SD,
    paired_results$half_life_label,
    drop = TRUE
  )
)


paired_summary_rows <- list()

paired_index <- 1



for (group_name in names(paired_groups)) {
  
  current_data <- paired_groups[[group_name]]
  
  
  mae_improvement <- current_data$MAE_improvement
  
  
  current_se <- sd(
    mae_improvement
  ) / sqrt(
    length(
      mae_improvement
    )
  )
  
  
  mean_improvement <- mean(
    mae_improvement
  )
  
  
  paired_summary_rows[[paired_index]] <- data.frame(
    drift_SD =
      unique(
        current_data$drift_SD
      ),
    
    half_life =
      unique(
        current_data$half_life
      ),
    
    half_life_label =
      as.character(
        unique(
          current_data$half_life_label
        )
      ),
    
    mean_MAE_improvement =
      mean_improvement,
    
    MC_SE =
      current_se,
    
    lower_95 = (
      mean_improvement -
        1.96 * current_se
    ),
    
    upper_95 = (
      mean_improvement +
        1.96 * current_se
    ),
    
    win_rate = (
      100 *
        mean(
          current_data$MAE <
            current_data$baseline_MAE
        )
    ),
    
    mean_RMSE_improvement =
      mean(
        current_data$RMSE_improvement
      ),
    
    mean_rating_RMSE_improvement =
      mean(
        current_data$rating_RMSE_improvement
      ),
    
    stringsAsFactors = FALSE
  )
  
  
  paired_index <- (
    paired_index + 1
  )
}



paired_summary <- do.call(
  rbind,
  paired_summary_rows
)


paired_summary <- paired_summary[
  order(
    paired_summary$drift_SD,
    -paired_summary$mean_MAE_improvement
  ),
]


rownames(paired_summary) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "PAIRED IMPROVEMENT VS NO DECAY\n"
)

cat(
  "========================================\n\n"
)


print(
  paired_summary
)



# ============================================================
# 33. BEST RECENCY POLICY BY DRIFT
# ============================================================
#
# Same basic best-half-life result, but now also report
# improvement over the no-decay baseline.
#
# ============================================================

best_policy_rows <- list()

policy_index <- 1



for (current_drift in drift_sd_values) {
  
  current_summary <- summary_by_drift[
    summary_by_drift$drift_SD ==
      current_drift,
  ]
  
  
  best_row <- current_summary[
    which.min(
      current_summary$MAE
    ),
  ]
  
  
  baseline_row <- current_summary[
    current_summary$half_life_label ==
      "No Decay",
  ]
  
  
  best_policy_rows[[policy_index]] <- data.frame(
    drift_SD = current_drift,
    
    no_decay_MAE =
      baseline_row$MAE,
    
    best_half_life =
      best_row$half_life,
    
    best_half_life_label =
      as.character(
        best_row$half_life_label
      ),
    
    best_MAE =
      best_row$MAE,
    
    MAE_improvement = (
      baseline_row$MAE -
        best_row$MAE
    ),
    
    no_decay_rating_RMSE =
      baseline_row$current_rating_RMSE,
    
    best_rating_RMSE =
      best_row$current_rating_RMSE,
    
    rating_RMSE_improvement = (
      baseline_row$current_rating_RMSE -
        best_row$current_rating_RMSE
    ),
    
    stringsAsFactors = FALSE
  )
  
  
  policy_index <- (
    policy_index + 1
  )
}



best_policy_by_drift <- do.call(
  rbind,
  best_policy_rows
)


rownames(best_policy_by_drift) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "BEST RECENCY POLICY BY DRIFT\n"
)

cat(
  "========================================\n\n"
)


print(
  best_policy_by_drift
)



# ============================================================
# 34. BEST HALF-LIFE BY TRUE SD + DRIFT
# ============================================================
#
# This tells us whether optimal recency depends only on drift
# or also interacts with league parity.
#
# ============================================================

cell_groups <- split(
  summary_by_cell,
  interaction(
    summary_by_cell$true_rating_SD,
    summary_by_cell$drift_SD,
    drop = TRUE
  )
)


best_cell_rows <- list()

cell_index <- 1



for (group_name in names(cell_groups)) {
  
  current_data <- cell_groups[[group_name]]
  
  
  best_row <- current_data[
    which.min(
      current_data$MAE
    ),
  ]
  
  
  best_cell_rows[[cell_index]] <- data.frame(
    true_rating_SD =
      best_row$true_rating_SD,
    
    drift_SD =
      best_row$drift_SD,
    
    best_half_life =
      best_row$half_life,
    
    best_half_life_label =
      as.character(
        best_row$half_life_label
      ),
    
    best_MAE =
      best_row$MAE,
    
    stringsAsFactors = FALSE
  )
  
  
  cell_index <- (
    cell_index + 1
  )
}



best_half_life_by_cell <- do.call(
  rbind,
  best_cell_rows
)


best_half_life_by_cell <- best_half_life_by_cell[
  order(
    best_half_life_by_cell$true_rating_SD,
    best_half_life_by_cell$drift_SD
  ),
]


rownames(best_half_life_by_cell) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "BEST HALF-LIFE BY PARITY AND DRIFT\n"
)

cat(
  "========================================\n\n"
)


print(
  best_half_life_by_cell
)



# ============================================================
# 35. HALF-LIFE WEIGHT EXAMPLE
# ============================================================
#
# Useful sanity check.
#
# Shows RELATIVE raw weights before normalization.
#
# ============================================================

weeks_old_example <- 0:12


half_life_example <- data.frame(
  weeks_old = weeks_old_example
)


for (half_life in c(
  12,
  8,
  6,
  4,
  3,
  2
)) {
  
  column_name <- paste0(
    "HL_",
    half_life
  )
  
  
  half_life_example[[column_name]] <- 0.5^(
    weeks_old_example /
      half_life
  )
}



cat(
  "\n========================================\n"
)

cat(
  "RAW RECENCY WEIGHT EXAMPLE\n"
)

cat(
  "========================================\n\n"
)


print(
  half_life_example
)



# ============================================================
# 36. EFFECTIVE SAMPLE SIZE SUMMARY
# ============================================================

ess_summary <- aggregate(
  mean_effective_sample_size ~
    half_life +
    half_life_label,
  data = season_results,
  FUN = mean
)


ess_summary <- ess_summary[
  order(
    -ess_summary$half_life
  ),
]


cat(
  "\n========================================\n"
)

cat(
  "AVERAGE EFFECTIVE SAMPLE SIZE\n"
)

cat(
  "========================================\n\n"
)


print(
  ess_summary
)



# ============================================================
# 37. PLOT MAE BY DRIFT
# ============================================================

mae_plot <- ggplot(
  summary_by_drift,
  aes(
    x = drift_SD,
    y = MAE,
    group = half_life_label,
    linetype = half_life_label
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    title =
      "Version 7B: Prediction MAE by Team-Strength Drift",
    
    subtitle =
      "Lower is better",
    
    x =
      "Weekly True Rating Drift SD",
    
    y =
      "Out-of-Sample MAE",
    
    linetype =
      "Recency Half-Life"
  ) +
  theme_minimal()


print(
  mae_plot
)



# ============================================================
# 38. PLOT IMPROVEMENT OVER NO DECAY
# ============================================================

improvement_plot_data <- paired_summary[
  paired_summary$half_life_label !=
    "No Decay",
]


improvement_plot <- ggplot(
  improvement_plot_data,
  aes(
    x = drift_SD,
    y = mean_MAE_improvement,
    group = half_life_label,
    linetype = half_life_label
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    title =
      "Recency Improvement Over No-Decay Model",
    
    subtitle =
      "Positive values mean recency weighting improved MAE",
    
    x =
      "Weekly True Rating Drift SD",
    
    y =
      "No-Decay MAE - Recency MAE",
    
    linetype =
      "Recency Half-Life"
  ) +
  theme_minimal()


print(
  improvement_plot
)



# ============================================================
# 39. PLOT CURRENT-RATING RMSE
# ============================================================

rating_rmse_plot <- ggplot(
  summary_by_drift,
  aes(
    x = drift_SD,
    y = current_rating_RMSE,
    group = half_life_label,
    linetype = half_life_label
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    title =
      "Recovery of Current Hidden Team Strength",
    
    subtitle =
      "Lower rating RMSE is better",
    
    x =
      "Weekly True Rating Drift SD",
    
    y =
      "Current-Team Rating RMSE",
    
    linetype =
      "Recency Half-Life"
  ) +
  theme_minimal()


print(
  rating_rmse_plot
)



# ============================================================
# 40. PLOT BEST MAE BY DRIFT
# ============================================================

best_policy_plot <- ggplot(
  best_policy_by_drift,
  aes(
    x = drift_SD,
    y = MAE_improvement
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
  labs(
    title =
      "Maximum Recency Gain Available at Each Drift Level",
    
    x =
      "Weekly True Rating Drift SD",
    
    y =
      "MAE Improvement Over No Decay"
  ) +
  theme_minimal()


print(
  best_policy_plot
)



# ============================================================
# 41. SAVE RESULTS
# ============================================================

write.csv(
  calibration_coefficients,
  "version7B_calibration_coefficients.csv",
  row.names = FALSE
)


write.csv(
  season_results,
  "version7B_all_season_results.csv",
  row.names = FALSE
)


write.csv(
  summary_by_cell,
  "version7B_results_by_parity_drift_half_life.csv",
  row.names = FALSE
)


write.csv(
  summary_by_drift,
  "version7B_results_by_drift_half_life.csv",
  row.names = FALSE
)


write.csv(
  best_half_life_by_drift,
  "version7B_best_half_life_by_drift.csv",
  row.names = FALSE
)


write.csv(
  paired_summary,
  "version7B_paired_improvement_vs_no_decay.csv",
  row.names = FALSE
)


write.csv(
  best_policy_by_drift,
  "version7B_best_policy_by_drift.csv",
  row.names = FALSE
)


write.csv(
  best_half_life_by_cell,
  "version7B_best_half_life_by_parity_and_drift.csv",
  row.names = FALSE
)


write.csv(
  ess_summary,
  "version7B_effective_sample_size.csv",
  row.names = FALSE
)



# ============================================================
# 42. FINAL OUTPUT
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 7B COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Half-lives tested:",
  paste(
    c(
      "No Decay",
      12,
      8,
      6,
      4,
      3,
      2
    ),
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
  "Total simulated seasons:",
  length(true_sd_values) *
    length(drift_sd_values) *
    n_evaluation_per_cell,
  "\n"
)


cat(
  "Total rating strategies evaluated:",
  length(true_sd_values) *
    length(drift_sd_values) *
    n_evaluation_per_cell *
    length(half_life_values),
  "\n\n"
)



cat(
  "BEST HALF-LIFE BY DRIFT:\n\n"
)


print(
  best_half_life_by_drift
)



cat(
  "\nBEST RECENCY POLICY BY DRIFT:\n\n"
)


print(
  best_policy_by_drift
)



cat(
  "\nPAIRED IMPROVEMENT VS NO DECAY:\n\n"
)


print(
  paired_summary
)



cat(
  "\nAVERAGE EFFECTIVE SAMPLE SIZE:\n\n"
)


print(
  ess_summary
)



cat(
  "\nBEST HALF-LIFE BY PARITY AND DRIFT:\n\n"
)


print(
  best_half_life_by_cell
)


# ============================================================
# END VERSION 7B
# ============================================================
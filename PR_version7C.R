# ============================================================
# FOOTBALL POWER RATINGS - VERSION 7C
# ESTIMATED TEAM VOLATILITY + ADAPTIVE RECENCY
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
# 2. SETTINGS
# ============================================================

true_hfa <- 2.7
noise_sd <- 8.5
hfa_prior <- 2.5

true_sd_values <- c(
  2, 3, 4, 5, 6, 7, 8
)

drift_sd_values <- c(
  0.00,
  0.25,
  0.50,
  0.75,
  1.00,
  1.25,
  1.50,
  1.75,
  2.00
)

half_life_values <- c(
  Inf,
  12,
  8,
  6
)

half_life_labels <- c(
  "No Decay",
  "HL 12",
  "HL 8",
  "HL 6"
)

names(half_life_labels) <- as.character(
  half_life_values
)

adaptive_start_week <- 5
lambda_parity_threshold <- 4.5


# ============================================================
# 3. MONTE CARLO SAMPLE SIZES
# ============================================================

n_parity_calibration_per_sd <- 300

n_volatility_calibration_per_cell <- 200

n_tuning_per_cell <- 200

n_evaluation_per_cell <- 500


# ============================================================
# 4. GENERATE TRUE RATINGS
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
# 5. UPDATE TRUE RATINGS
# ============================================================

update_true_ratings <- function(
    previous_ratings,
    target_sd,
    drift_sd
) {
  
  if (drift_sd == 0) {
    return(previous_ratings)
  }
  
  new_ratings <- previous_ratings +
    rnorm(
      length(previous_ratings),
      mean = 0,
      sd = drift_sd
    )
  
  names(new_ratings) <- names(previous_ratings)
  
  new_ratings <- new_ratings -
    mean(new_ratings)
  
  current_sd <- sd(new_ratings)
  
  if (current_sd > 0) {
    
    new_ratings <- (
      new_ratings /
        current_sd
    ) * target_sd
  }
  
  return(new_ratings)
}


# ============================================================
# 6. GENERATE DYNAMIC TRUE-RATING HISTORY
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
  
  colnames(rating_matrix) <- league_teams
  
  rownames(rating_matrix) <- paste0(
    "Week_",
    1:max_week
  )
  
  rating_matrix[1, ] <- generate_true_ratings(
    league_teams = league_teams,
    target_sd = target_sd
  )
  
  if (max_week >= 2) {
    
    for (current_week in 2:max_week) {
      
      previous_ratings <- rating_matrix[
        current_week - 1,
      ]
      
      names(previous_ratings) <- league_teams
      
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
# 7. TRUE MOVEMENT SUMMARY
# ============================================================

summarize_true_rating_movement <- function(
    rating_matrix
) {
  
  absolute_changes <- numeric()
  squared_changes <- numeric()
  correlations <- numeric()
  
  for (current_week in 2:nrow(rating_matrix)) {
    
    previous <- rating_matrix[
      current_week - 1,
    ]
    
    current <- rating_matrix[
      current_week,
    ]
    
    difference <- current - previous
    
    absolute_changes <- c(
      absolute_changes,
      abs(difference)
    )
    
    squared_changes <- c(
      squared_changes,
      difference^2
    )
    
    correlations <- c(
      correlations,
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
        correlations
      )
    )
  )
}


# ============================================================
# 8. SIMULATE DYNAMIC SEASON
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
  
  sim_games$true_expected_margin <- (
    sim_games$true_home_rating -
      sim_games$true_away_rating +
      true_hfa
  )
  
  sim_games$actual_margin <- round(
    sim_games$true_expected_margin +
      rnorm(
        nrow(sim_games),
        mean = 0,
        sd = noise_sd
      )
  )
  
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
# 9. HFA LAMBDA
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
# 10. DYNAMIC TEAM LAMBDA
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
# 11. PARITY-BASED TEAM LAMBDA
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
# 12. SMOOTH MARGIN CAP
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
# 13. RECENCY WEIGHTS
# ============================================================

make_recency_weights <- function(
    training_games,
    half_life
) {
  
  if (is.infinite(half_life)) {
    
    return(
      rep(
        1,
        nrow(training_games)
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
  
  normalized_weights <- (
    raw_weights /
      mean(raw_weights)
  )
  
  return(normalized_weights)
}


# ============================================================
# 14. POWER RATING MODEL
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
  
  if (is.null(game_weights)) {
    
    game_weights <- rep(
      1,
      n_games
    )
  }
  
  sqrt_weights <- sqrt(
    game_weights
  )
  
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
  
  penalty <- diag(
    c(
      hfa_lambda,
      rep(
        team_lambda,
        n_teams
      )
    )
  )
  
  prior_target <- c(
    hfa_prior,
    rep(
      0,
      n_teams
    )
  )
  
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
  
  estimated_hfa <- beta["HFA"]
  
  ratings <- beta[
    league_teams
  ]
  
  ratings <- ratings -
    mean(ratings)
  
  return(
    list(
      ratings = data.frame(
        team = league_teams,
        rating = as.numeric(ratings),
        stringsAsFactors = FALSE
      ),
      
      home_field =
        as.numeric(
          estimated_hfa
        )
    )
  )
}


# ============================================================
# 15. RAW RATING HISTORY
# ============================================================

compute_raw_rating_history <- function(
    games,
    league_teams,
    last_completed_week,
    hfa_prior = 2.5
) {
  
  rating_history <- matrix(
    NA_real_,
    nrow = max_week,
    ncol = length(league_teams)
  )
  
  colnames(rating_history) <-
    league_teams
  
  for (
    completed_week in
    2:last_completed_week
  ) {
    
    training_games <- games[
      games$week <= completed_week,
    ]
    
    current_hfa_lambda <- get_hfa_lambda(
      data = training_games,
      league_teams = league_teams
    )
    
    model <- fit_power_ratings(
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
    
    lookup <- model$ratings$rating
    
    names(lookup) <-
      model$ratings$team
    
    rating_history[
      completed_week,
    ] <- lookup[
      league_teams
    ]
  }
  
  return(rating_history)
}


# ============================================================
# 16. OBSERVED VOLATILITY
# ============================================================

calculate_observed_volatility <- function(
    rating_history,
    completed_week
) {
  
  available_weeks <- 2:completed_week
  
  if (length(available_weeks) < 2) {
    
    return(NA_real_)
  }
  
  squared_changes <- numeric()
  
  for (
    current_week in
    available_weeks[-1]
  ) {
    
    previous <- rating_history[
      current_week - 1,
    ]
    
    current <- rating_history[
      current_week,
    ]
    
    change <- current - previous
    
    squared_changes <- c(
      squared_changes,
      change^2
    )
  }
  
  observed_volatility <- sqrt(
    mean(
      squared_changes,
      na.rm = TRUE
    )
  )
  
  return(observed_volatility)
}


# ============================================================
# 17. PARITY CALIBRATION DATA
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "BUILDING PARITY CALIBRATION SAMPLE\n"
)

cat(
  "========================================\n\n"
)

set.seed(7101)

parity_calibration_rows <- list()
parity_index <- 1


for (current_true_sd in true_sd_values) {
  
  cat(
    "Parity calibration SD:",
    current_true_sd,
    "\n"
  )
  
  for (
    s in
    1:n_parity_calibration_per_sd
  ) {
    
    simulated <- simulate_dynamic_season(
      schedule = schedule,
      league_teams = teams,
      target_sd = current_true_sd,
      drift_sd = 0,
      true_hfa = true_hfa,
      noise_sd = noise_sd
    )
    
    games <- simulated$games
    
    rating_history <- compute_raw_rating_history(
      games = games,
      league_teams = teams,
      last_completed_week = max_week - 1,
      hfa_prior = hfa_prior
    )
    
    for (
      completed_week in
      2:(max_week - 1)
    ) {
      
      observed_parity <- sd(
        rating_history[
          completed_week,
        ]
      )
      
      parity_calibration_rows[[parity_index]] <- data.frame(
        true_rating_SD = current_true_sd,
        week = completed_week,
        observed_rating_SD = observed_parity
      )
      
      parity_index <- parity_index + 1
    }
  }
}


parity_calibration_data <- do.call(
  rbind,
  parity_calibration_rows
)

rownames(
  parity_calibration_data
) <- NULL


# ============================================================
# 18. FIT PARITY CALIBRATION
# ============================================================

parity_calibration_coefficients <- data.frame(
  week = 2:(max_week - 1),
  intercept = NA_real_,
  slope = NA_real_,
  r_squared = NA_real_
)


for (
  i in seq_len(
    nrow(
      parity_calibration_coefficients
    )
  )
) {
  
  current_week <-
    parity_calibration_coefficients$week[i]
  
  current_data <- parity_calibration_data[
    parity_calibration_data$week ==
      current_week,
  ]
  
  model <- lm(
    true_rating_SD ~
      observed_rating_SD,
    data = current_data
  )
  
  parity_calibration_coefficients$intercept[i] <-
    coef(model)[1]
  
  parity_calibration_coefficients$slope[i] <-
    coef(model)[2]
  
  parity_calibration_coefficients$r_squared[i] <-
    summary(model)$r.squared
}


# ============================================================
# 19. APPLY PARITY CALIBRATION
# ============================================================

calibrate_parity_sd <- function(
    observed_sd,
    completed_week,
    coefficients
) {
  
  current_row <- coefficients[
    coefficients$week ==
      completed_week,
  ]
  
  estimate <- (
    current_row$intercept +
      current_row$slope *
      observed_sd
  )
  
  estimate <- max(
    0,
    estimate
  )
  
  return(
    as.numeric(
      estimate
    )
  )
}


# ============================================================
# 20. VOLATILITY CALIBRATION DATA
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "BUILDING VOLATILITY CALIBRATION SAMPLE\n"
)

cat(
  "========================================\n\n"
)

set.seed(7102)

volatility_calibration_rows <- list()
volatility_index <- 1


for (current_true_sd in true_sd_values) {
  
  for (
    current_drift_sd in
    drift_sd_values
  ) {
    
    cat(
      "Calibration True SD:",
      current_true_sd,
      "| Drift:",
      current_drift_sd,
      "\n"
    )
    
    for (
      s in
      1:n_volatility_calibration_per_cell
    ) {
      
      simulated <- simulate_dynamic_season(
        schedule = schedule,
        league_teams = teams,
        target_sd = current_true_sd,
        drift_sd = current_drift_sd,
        true_hfa = true_hfa,
        noise_sd = noise_sd
      )
      
      games <- simulated$games
      
      rating_history <- compute_raw_rating_history(
        games = games,
        league_teams = teams,
        last_completed_week = max_week - 1,
        hfa_prior = hfa_prior
      )
      
      for (
        completed_week in
        (adaptive_start_week - 1):(max_week - 1)
      ) {
        
        observed_volatility <-
          calculate_observed_volatility(
            rating_history = rating_history,
            completed_week = completed_week
          )
        
        volatility_calibration_rows[[volatility_index]] <- data.frame(
          true_rating_SD =
            current_true_sd,
          
          true_drift_SD =
            current_drift_sd,
          
          completed_week =
            completed_week,
          
          observed_volatility =
            observed_volatility
        )
        
        volatility_index <-
          volatility_index + 1
      }
    }
  }
}


volatility_calibration_data <- do.call(
  rbind,
  volatility_calibration_rows
)

rownames(
  volatility_calibration_data
) <- NULL


# ============================================================
# 21. FIT VOLATILITY CALIBRATION
# ============================================================

volatility_calibration_coefficients <- data.frame(
  completed_week =
    (adaptive_start_week - 1):(max_week - 1),
  
  intercept = NA_real_,
  linear = NA_real_,
  quadratic = NA_real_,
  r_squared = NA_real_,
  calibration_RMSE = NA_real_
)


for (
  i in seq_len(
    nrow(
      volatility_calibration_coefficients
    )
  )
) {
  
  current_week <-
    volatility_calibration_coefficients$completed_week[i]
  
  current_data <- volatility_calibration_data[
    volatility_calibration_data$completed_week ==
      current_week,
  ]
  
  calibration_model <- lm(
    true_drift_SD ~
      observed_volatility +
      I(observed_volatility^2),
    data = current_data
  )
  
  predicted <- predict(
    calibration_model,
    newdata = current_data
  )
  
  volatility_calibration_coefficients$intercept[i] <-
    coef(calibration_model)[1]
  
  volatility_calibration_coefficients$linear[i] <-
    coef(calibration_model)[2]
  
  volatility_calibration_coefficients$quadratic[i] <-
    coef(calibration_model)[3]
  
  volatility_calibration_coefficients$r_squared[i] <-
    summary(calibration_model)$r.squared
  
  volatility_calibration_coefficients$calibration_RMSE[i] <-
    sqrt(
      mean(
        (
          current_data$true_drift_SD -
            predicted
        )^2
      )
    )
}


cat(
  "\n========================================\n"
)

cat(
  "VOLATILITY CALIBRATION COEFFICIENTS\n"
)

cat(
  "========================================\n\n"
)

print(
  volatility_calibration_coefficients
)


# ============================================================
# 22. APPLY VOLATILITY CALIBRATION
# ============================================================

calibrate_drift_sd <- function(
    observed_volatility,
    completed_week,
    coefficients,
    min_drift = 0,
    max_drift = 2
) {
  
  current_row <- coefficients[
    coefficients$completed_week ==
      completed_week,
  ]
  
  estimate <- (
    current_row$intercept +
      current_row$linear *
      observed_volatility +
      current_row$quadratic *
      observed_volatility^2
  )
  
  estimate <- max(
    min_drift,
    estimate
  )
  
  estimate <- min(
    max_drift,
    estimate
  )
  
  return(
    as.numeric(
      estimate
    )
  )
}


# ============================================================
# 23. SAFE CORRELATION
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
# 24. FIT ONE HALF-LIFE FOR ONE WEEK
# ============================================================

fit_candidate_week <- function(
    training_games,
    test_games,
    league_teams,
    half_life,
    margin_cap,
    team_lambda,
    hfa_lambda,
    hfa_prior,
    current_true_ratings
) {
  
  game_weights <- make_recency_weights(
    training_games = training_games,
    half_life = half_life
  )
  
  model <- fit_power_ratings(
    data = training_games,
    league_teams = league_teams,
    margin_cap = margin_cap,
    team_lambda = team_lambda,
    hfa_lambda = hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = game_weights
  )
  
  rating_lookup <- model$ratings$rating
  
  names(rating_lookup) <-
    model$ratings$team
  
  prediction <- (
    rating_lookup[
      test_games$home_team
    ] -
      rating_lookup[
        test_games$away_team
      ] +
      model$home_field
  )
  
  rating_rmse <- sqrt(
    mean(
      (
        rating_lookup[
          league_teams
        ] -
          current_true_ratings[
            league_teams
          ]
      )^2
    )
  )
  
  return(
    list(
      prediction =
        as.numeric(
          prediction
        ),
      
      rating_RMSE =
        rating_rmse
    )
  )
}


# ============================================================
# 25. BUILD TUNING SAMPLE
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "BUILDING RECENCY TUNING SAMPLE\n"
)

cat(
  "========================================\n\n"
)

set.seed(7103)

tuning_rows <- list()
tuning_index <- 1


for (current_true_sd in true_sd_values) {
  
  for (
    current_drift_sd in
    drift_sd_values
  ) {
    
    cat(
      "Tuning True SD:",
      current_true_sd,
      "| Drift:",
      current_drift_sd,
      "\n"
    )
    
    for (
      s in
      1:n_tuning_per_cell
    ) {
      
      simulated <- simulate_dynamic_season(
        schedule = schedule,
        league_teams = teams,
        target_sd = current_true_sd,
        drift_sd = current_drift_sd,
        true_hfa = true_hfa,
        noise_sd = noise_sd
      )
      
      games <- simulated$games
      rating_matrix <- simulated$rating_matrix
      
      rating_history <- compute_raw_rating_history(
        games = games,
        league_teams = teams,
        last_completed_week = max_week - 1,
        hfa_prior = hfa_prior
      )
      
      for (
        target_week in
        adaptive_start_week:max_week
      ) {
        
        completed_week <-
          target_week - 1
        
        training_games <- games[
          games$week <
            target_week,
        ]
        
        test_games <- games[
          games$week ==
            target_week,
        ]
        
        current_true_ratings <- rating_matrix[
          target_week,
        ]
        
        names(current_true_ratings) <-
          teams
        
        
        # ----------------------------------------------------
        # PARITY
        # ----------------------------------------------------
        
        observed_parity <- sd(
          rating_history[
            completed_week,
          ]
        )
        
        estimated_parity <- calibrate_parity_sd(
          observed_sd =
            observed_parity,
          
          completed_week =
            completed_week,
          
          coefficients =
            parity_calibration_coefficients
        )
        
        
        # ----------------------------------------------------
        # VOLATILITY
        # ----------------------------------------------------
        
        observed_volatility <-
          calculate_observed_volatility(
            rating_history =
              rating_history,
            
            completed_week =
              completed_week
          )
        
        estimated_drift <- calibrate_drift_sd(
          observed_volatility =
            observed_volatility,
          
          completed_week =
            completed_week,
          
          coefficients =
            volatility_calibration_coefficients,
          
          min_drift =
            min(drift_sd_values),
          
          max_drift =
            max(drift_sd_values)
        )
        
        
        # ----------------------------------------------------
        # MODEL SETTINGS
        # ----------------------------------------------------
        
        margin_cap <- choose_cap_smooth(
          estimated_parity
        )
        
        team_lambda <- choose_team_lambda(
          parity_sd =
            estimated_parity,
          
          training_games =
            training_games,
          
          league_teams =
            teams
        )
        
        current_hfa_lambda <- get_hfa_lambda(
          data = training_games,
          league_teams = teams
        )
        
        
        # ----------------------------------------------------
        # FOUR CANDIDATES
        # ----------------------------------------------------
        
        for (half_life in half_life_values) {
          
          candidate <- fit_candidate_week(
            training_games =
              training_games,
            
            test_games =
              test_games,
            
            league_teams =
              teams,
            
            half_life =
              half_life,
            
            margin_cap =
              margin_cap,
            
            team_lambda =
              team_lambda,
            
            hfa_lambda =
              current_hfa_lambda,
            
            hfa_prior =
              hfa_prior,
            
            current_true_ratings =
              current_true_ratings
          )
          
          absolute_errors <- abs(
            test_games$actual_margin -
              candidate$prediction
          )
          
          current_label <- if (
            is.infinite(
              half_life
            )
          ) {
            
            "No Decay"
            
          } else {
            
            paste0(
              "HL ",
              half_life
            )
          }
          
          tuning_rows[[tuning_index]] <- data.frame(
            true_rating_SD =
              current_true_sd,
            
            true_drift_SD =
              current_drift_sd,
            
            simulation =
              s,
            
            target_week =
              target_week,
            
            estimated_drift_SD =
              estimated_drift,
            
            estimated_parity_SD =
              estimated_parity,
            
            half_life =
              half_life,
            
            half_life_label =
              current_label,
            
            week_MAE =
              mean(
                absolute_errors
              ),
            
            week_RMSE =
              sqrt(
                mean(
                  (
                    test_games$actual_margin -
                      candidate$prediction
                  )^2
                )
              ),
            
            current_rating_RMSE =
              candidate$rating_RMSE,
            
            stringsAsFactors =
              FALSE
          )
          
          tuning_index <-
            tuning_index + 1
        }
      }
    }
  }
}


tuning_data <- do.call(
  rbind,
  tuning_rows
)

rownames(tuning_data) <- NULL


# ============================================================
# 26. EXPECTED MAE MODEL FOR EACH CANDIDATE
# ============================================================

candidate_selection_models <- list()


for (label in half_life_labels) {
  
  current_data <- tuning_data[
    tuning_data$half_life_label ==
      label,
  ]
  
  candidate_selection_models[[label]] <- lm(
    week_MAE ~
      estimated_drift_SD +
      I(estimated_drift_SD^2) +
      factor(target_week),
    data = current_data
  )
}


# ============================================================
# 27. ESTIMATED HALF-LIFE SELECTOR
# ============================================================

choose_estimated_half_life <- function(
    estimated_drift,
    target_week,
    selection_models
) {
  
  expected_mae <- numeric(
    length(
      half_life_labels
    )
  )
  
  names(expected_mae) <-
    half_life_labels
  
  new_data <- data.frame(
    estimated_drift_SD =
      estimated_drift,
    
    target_week =
      target_week
  )
  
  for (label in half_life_labels) {
    
    expected_mae[label] <- predict(
      selection_models[[label]],
      newdata = new_data
    )
  }
  
  best_label <- names(
    which.min(
      expected_mae
    )
  )
  
  if (best_label == "No Decay") {
    
    best_half_life <- Inf
    
  } else if (best_label == "HL 12") {
    
    best_half_life <- 12
    
  } else if (best_label == "HL 8") {
    
    best_half_life <- 8
    
  } else {
    
    best_half_life <- 6
  }
  
  return(
    list(
      half_life =
        best_half_life,
      
      label =
        best_label,
      
      expected_mae =
        expected_mae
    )
  )
}


# ============================================================
# 28. ORACLE POLICY
# ============================================================

oracle_summary <- aggregate(
  week_MAE ~
    true_drift_SD +
    target_week +
    half_life +
    half_life_label,
  data = tuning_data,
  FUN = mean
)


oracle_groups <- split(
  oracle_summary,
  interaction(
    oracle_summary$true_drift_SD,
    oracle_summary$target_week,
    drop = TRUE
  )
)


oracle_policy_rows <- list()
oracle_index <- 1


for (group_name in names(oracle_groups)) {
  
  current_data <-
    oracle_groups[[group_name]]
  
  best_row <- current_data[
    which.min(
      current_data$week_MAE
    ),
  ]
  
  oracle_policy_rows[[oracle_index]] <- data.frame(
    true_drift_SD =
      best_row$true_drift_SD,
    
    target_week =
      best_row$target_week,
    
    half_life =
      best_row$half_life,
    
    half_life_label =
      as.character(
        best_row$half_life_label
      ),
    
    tuning_MAE =
      best_row$week_MAE,
    
    stringsAsFactors =
      FALSE
  )
  
  oracle_index <-
    oracle_index + 1
}


oracle_policy <- do.call(
  rbind,
  oracle_policy_rows
)

rownames(oracle_policy) <- NULL


cat(
  "\n========================================\n"
)

cat(
  "ORACLE POLICY LEARNED FROM TUNING SAMPLE\n"
)

cat(
  "========================================\n\n"
)

print(
  oracle_policy
)


# ============================================================
# 29. ORACLE HALF-LIFE LOOKUP
# ============================================================

choose_oracle_half_life <- function(
    true_drift,
    target_week,
    oracle_policy
) {
  
  current_row <- oracle_policy[
    oracle_policy$true_drift_SD ==
      true_drift &
      oracle_policy$target_week ==
      target_week,
  ]
  
  return(
    list(
      half_life =
        current_row$half_life[1],
      
      label =
        current_row$half_life_label[1]
    )
  )
}


# ============================================================
# 30. FINAL HOLDOUT EVALUATION
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "STARTING VERSION 7C HOLDOUT EVALUATION\n"
)

cat(
  "========================================\n\n"
)

set.seed(7104)

season_result_rows <- list()
decision_rows <- list()

season_result_index <- 1
decision_index <- 1


strategy_names <- c(
  "No Decay",
  "HL 12",
  "HL 8",
  "HL 6",
  "Estimated Adaptive",
  "Oracle Adaptive"
)


for (current_true_sd in true_sd_values) {
  
  for (
    current_drift_sd in
    drift_sd_values
  ) {
    
    cat(
      "\nEvaluation True SD:",
      current_true_sd,
      "| Drift:",
      current_drift_sd,
      "\n"
    )
    
    for (
      s in
      1:n_evaluation_per_cell
    ) {
      
      simulated <- simulate_dynamic_season(
        schedule = schedule,
        league_teams = teams,
        target_sd = current_true_sd,
        drift_sd = current_drift_sd,
        true_hfa = true_hfa,
        noise_sd = noise_sd
      )
      
      games <- simulated$games
      
      rating_matrix <-
        simulated$rating_matrix
      
      rating_history <- compute_raw_rating_history(
        games = games,
        league_teams = teams,
        last_completed_week = max_week - 1,
        hfa_prior = hfa_prior
      )
      
      
      # ------------------------------------------------------
      # STORAGE
      # ------------------------------------------------------
      
      prediction_storage <- list()
      rating_rmse_storage <- list()
      
      for (strategy in strategy_names) {
        
        prediction_storage[[strategy]] <- list()
        
        rating_rmse_storage[[strategy]] <-
          numeric()
      }
      
      
      # ======================================================
      # WALK-FORWARD
      # ======================================================
      
      for (target_week in 2:max_week) {
        
        completed_week <-
          target_week - 1
        
        training_games <- games[
          games$week <
            target_week,
        ]
        
        test_games <- games[
          games$week ==
            target_week,
        ]
        
        current_true_ratings <- rating_matrix[
          target_week,
        ]
        
        names(current_true_ratings) <-
          teams
        
        
        # ----------------------------------------------------
        # MODEL SETTINGS
        # ----------------------------------------------------
        
        if (
          target_week <
          adaptive_start_week
        ) {
          
          estimated_parity <- NA_real_
          estimated_drift <- NA_real_
          observed_volatility <- NA_real_
          
          margin_cap <- 21
          team_lambda <- 2
          
          estimated_choice <- list(
            half_life = Inf,
            label = "No Decay"
          )
          
          oracle_choice <- list(
            half_life = Inf,
            label = "No Decay"
          )
          
        } else {
          
          # --------------------------------------------------
          # PARITY
          # --------------------------------------------------
          
          observed_parity <- sd(
            rating_history[
              completed_week,
            ]
          )
          
          estimated_parity <- calibrate_parity_sd(
            observed_sd =
              observed_parity,
            
            completed_week =
              completed_week,
            
            coefficients =
              parity_calibration_coefficients
          )
          
          margin_cap <- choose_cap_smooth(
            estimated_parity
          )
          
          team_lambda <- choose_team_lambda(
            parity_sd =
              estimated_parity,
            
            training_games =
              training_games,
            
            league_teams =
              teams
          )
          
          
          # --------------------------------------------------
          # VOLATILITY
          # --------------------------------------------------
          
          observed_volatility <-
            calculate_observed_volatility(
              rating_history =
                rating_history,
              
              completed_week =
                completed_week
            )
          
          estimated_drift <- calibrate_drift_sd(
            observed_volatility =
              observed_volatility,
            
            completed_week =
              completed_week,
            
            coefficients =
              volatility_calibration_coefficients,
            
            min_drift =
              min(drift_sd_values),
            
            max_drift =
              max(drift_sd_values)
          )
          
          
          # --------------------------------------------------
          # ESTIMATED CHOICE
          # --------------------------------------------------
          
          estimated_choice <-
            choose_estimated_half_life(
              estimated_drift =
                estimated_drift,
              
              target_week =
                target_week,
              
              selection_models =
                candidate_selection_models
            )
          
          
          # --------------------------------------------------
          # ORACLE CHOICE
          # --------------------------------------------------
          
          oracle_choice <-
            choose_oracle_half_life(
              true_drift =
                current_drift_sd,
              
              target_week =
                target_week,
              
              oracle_policy =
                oracle_policy
            )
        }
        
        
        current_hfa_lambda <- get_hfa_lambda(
          data = training_games,
          league_teams = teams
        )
        
        
        # ====================================================
        # FIT ALL FOUR CANDIDATES
        # ====================================================
        
        candidate_results <- list()
        
        
        for (half_life in half_life_values) {
          
          current_label <- if (
            is.infinite(
              half_life
            )
          ) {
            
            "No Decay"
            
          } else {
            
            paste0(
              "HL ",
              half_life
            )
          }
          
          candidate_results[[current_label]] <-
            fit_candidate_week(
              training_games =
                training_games,
              
              test_games =
                test_games,
              
              league_teams =
                teams,
              
              half_life =
                half_life,
              
              margin_cap =
                margin_cap,
              
              team_lambda =
                team_lambda,
              
              hfa_lambda =
                current_hfa_lambda,
              
              hfa_prior =
                hfa_prior,
              
              current_true_ratings =
                current_true_ratings
            )
        }
        
        
        # ====================================================
        # STORE FIXED STRATEGIES
        # ====================================================
        
        for (
          current_label in
          half_life_labels
        ) {
          
          candidate <-
            candidate_results[[current_label]]
          
          current_predictions <- data.frame(
            actual_margin =
              test_games$actual_margin,
            
            predicted_margin =
              candidate$prediction
          )
          
          current_predictions$error <- (
            current_predictions$actual_margin -
              current_predictions$predicted_margin
          )
          
          current_predictions$correct <- (
            current_predictions$actual_margin >
              0
          ) == (
            current_predictions$predicted_margin >
              0
          )
          
          next_position <- (
            length(
              prediction_storage[[current_label]]
            ) + 1
          )
          
          prediction_storage[[current_label]][[next_position]] <-
            current_predictions
          
          rating_rmse_storage[[current_label]] <- c(
            rating_rmse_storage[[current_label]],
            candidate$rating_RMSE
          )
        }
        
        
        # ====================================================
        # ESTIMATED ADAPTIVE
        # ====================================================
        
        estimated_candidate <-
          candidate_results[[estimated_choice$label]]
        
        estimated_predictions <- data.frame(
          actual_margin =
            test_games$actual_margin,
          
          predicted_margin =
            estimated_candidate$prediction
        )
        
        estimated_predictions$error <- (
          estimated_predictions$actual_margin -
            estimated_predictions$predicted_margin
        )
        
        estimated_predictions$correct <- (
          estimated_predictions$actual_margin >
            0
        ) == (
          estimated_predictions$predicted_margin >
            0
        )
        
        estimated_position <- (
          length(
            prediction_storage[["Estimated Adaptive"]]
          ) + 1
        )
        
        prediction_storage[["Estimated Adaptive"]][[estimated_position]] <-
          estimated_predictions
        
        rating_rmse_storage[["Estimated Adaptive"]] <- c(
          rating_rmse_storage[["Estimated Adaptive"]],
          estimated_candidate$rating_RMSE
        )
        
        
        # ====================================================
        # ORACLE ADAPTIVE
        # ====================================================
        
        oracle_candidate <-
          candidate_results[[oracle_choice$label]]
        
        oracle_predictions <- data.frame(
          actual_margin =
            test_games$actual_margin,
          
          predicted_margin =
            oracle_candidate$prediction
        )
        
        oracle_predictions$error <- (
          oracle_predictions$actual_margin -
            oracle_predictions$predicted_margin
        )
        
        oracle_predictions$correct <- (
          oracle_predictions$actual_margin >
            0
        ) == (
          oracle_predictions$predicted_margin >
            0
        )
        
        oracle_position <- (
          length(
            prediction_storage[["Oracle Adaptive"]]
          ) + 1
        )
        
        prediction_storage[["Oracle Adaptive"]][[oracle_position]] <-
          oracle_predictions
        
        rating_rmse_storage[["Oracle Adaptive"]] <- c(
          rating_rmse_storage[["Oracle Adaptive"]],
          oracle_candidate$rating_RMSE
        )
        
        
        # ====================================================
        # DECISION DIAGNOSTICS
        # ====================================================
        
        if (
          target_week >=
          adaptive_start_week
        ) {
          
          decision_rows[[decision_index]] <- data.frame(
            true_rating_SD =
              current_true_sd,
            
            true_drift_SD =
              current_drift_sd,
            
            simulation =
              s,
            
            target_week =
              target_week,
            
            observed_volatility =
              observed_volatility,
            
            estimated_drift_SD =
              estimated_drift,
            
            drift_absolute_error =
              abs(
                estimated_drift -
                  current_drift_sd
              ),
            
            estimated_half_life =
              estimated_choice$half_life,
            
            estimated_half_life_label =
              estimated_choice$label,
            
            oracle_half_life =
              oracle_choice$half_life,
            
            oracle_half_life_label =
              oracle_choice$label,
            
            decision_correct = (
              estimated_choice$label ==
                oracle_choice$label
            ),
            
            stringsAsFactors =
              FALSE
          )
          
          decision_index <-
            decision_index + 1
        }
      }
      
      
      # ======================================================
      # SEASON SUMMARY
      # ======================================================
      
      for (strategy in strategy_names) {
        
        predictions <- do.call(
          rbind,
          prediction_storage[[strategy]]
        )
        
        season_result_rows[[season_result_index]] <- data.frame(
          true_rating_SD =
            current_true_sd,
          
          true_drift_SD =
            current_drift_sd,
          
          simulation =
            s,
          
          strategy =
            strategy,
          
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
              predictions$correct
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
          
          current_rating_RMSE =
            mean(
              rating_rmse_storage[[strategy]]
            ),
          
          stringsAsFactors =
            FALSE
        )
        
        season_result_index <-
          season_result_index + 1
      }
      
      
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


# ============================================================
# 31. COMBINE RESULTS
# ============================================================

season_results <- do.call(
  rbind,
  season_result_rows
)

rownames(season_results) <- NULL


decision_results <- do.call(
  rbind,
  decision_rows
)

rownames(decision_results) <- NULL


# ============================================================
# 32. OVERALL SUMMARY
# ============================================================

overall_summary <- aggregate(
  cbind(
    MAE,
    RMSE,
    Accuracy,
    Correlation,
    current_rating_RMSE
  ) ~
    strategy,
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
  "OVERALL VERSION 7C MODEL COMPARISON\n"
)

cat(
  "========================================\n\n"
)

print(
  overall_summary
)


# ============================================================
# 33. RESULTS BY TRUE DRIFT
# ============================================================

summary_by_drift <- aggregate(
  cbind(
    MAE,
    RMSE,
    Accuracy,
    current_rating_RMSE
  ) ~
    true_drift_SD +
    strategy,
  data = season_results,
  FUN = mean
)

summary_by_drift <- summary_by_drift[
  order(
    summary_by_drift$true_drift_SD,
    summary_by_drift$MAE
  ),
]

rownames(summary_by_drift) <- NULL


cat(
  "\n========================================\n"
)

cat(
  "RESULTS BY TRUE DRIFT SD\n"
)

cat(
  "========================================\n\n"
)

print(
  summary_by_drift
)


# ============================================================
# 34. VOLATILITY ESTIMATION QUALITY
# ============================================================

volatility_quality <- aggregate(
  cbind(
    estimated_drift_SD,
    drift_absolute_error
  ) ~
    true_drift_SD,
  data = decision_results,
  FUN = mean
)

names(volatility_quality)[
  names(volatility_quality) ==
    "drift_absolute_error"
] <- "drift_MAE"


cat(
  "\n========================================\n"
)

cat(
  "VOLATILITY ESTIMATION QUALITY BY TRUE DRIFT\n"
)

cat(
  "========================================\n\n"
)

print(
  volatility_quality
)


# ============================================================
# 35. VOLATILITY ESTIMATION BY WEEK
# ============================================================

volatility_by_week <- aggregate(
  cbind(
    estimated_drift_SD,
    drift_absolute_error
  ) ~
    target_week,
  data = decision_results,
  FUN = mean
)

names(volatility_by_week)[
  names(volatility_by_week) ==
    "drift_absolute_error"
] <- "drift_MAE"


cat(
  "\n========================================\n"
)

cat(
  "VOLATILITY ESTIMATION QUALITY BY WEEK\n"
)

cat(
  "========================================\n\n"
)

print(
  volatility_by_week
)


# ============================================================
# 36. DECISION QUALITY
# ============================================================

decision_accuracy <- data.frame(
  overall_accuracy =
    mean(
      decision_results$decision_correct
    ),
  
  mean_drift_MAE =
    mean(
      decision_results$drift_absolute_error
    )
)


cat(
  "\n========================================\n"
)

cat(
  "ADAPTIVE RECENCY DECISION QUALITY\n"
)

cat(
  "========================================\n\n"
)

print(
  decision_accuracy
)


# ============================================================
# 37. DECISION ACCURACY BY TRUE DRIFT
# ============================================================

decision_by_drift <- aggregate(
  decision_correct ~
    true_drift_SD,
  data = decision_results,
  FUN = mean
)

decision_by_drift$decision_accuracy_percent <- (
  100 *
    decision_by_drift$decision_correct
)


cat(
  "\n========================================\n"
)

cat(
  "DECISION ACCURACY BY TRUE DRIFT\n"
)

cat(
  "========================================\n\n"
)

print(
  decision_by_drift
)


# ============================================================
# 38. CHOICE FREQUENCY
# ============================================================

choice_table <- as.data.frame(
  table(
    decision_results$true_drift_SD,
    decision_results$estimated_half_life_label
  )
)

names(choice_table) <- c(
  "true_drift_SD",
  "estimated_choice",
  "count"
)

choice_table$true_drift_SD <- as.numeric(
  as.character(
    choice_table$true_drift_SD
  )
)

choice_totals <- aggregate(
  count ~
    true_drift_SD,
  data = choice_table,
  FUN = sum
)

names(choice_totals)[2] <-
  "total"

choice_table <- merge(
  choice_table,
  choice_totals,
  by = "true_drift_SD"
)

choice_table$percent <- (
  100 *
    choice_table$count /
    choice_table$total
)


cat(
  "\n========================================\n"
)

cat(
  "ESTIMATED RECENCY CHOICE FREQUENCY\n"
)

cat(
  "========================================\n\n"
)

print(
  choice_table
)


# ============================================================
# 39. WIDE MAE TABLE
# ============================================================

mae_data <- season_results[
  ,
  c(
    "true_rating_SD",
    "true_drift_SD",
    "simulation",
    "strategy",
    "MAE"
  )
]

mae_wide <- reshape(
  mae_data,
  idvar = c(
    "true_rating_SD",
    "true_drift_SD",
    "simulation"
  ),
  timevar = "strategy",
  direction = "wide"
)


# ============================================================
# 40. PAIRED DIFFERENCES VS NO DECAY
# ============================================================

comparison_strategies <- c(
  "HL 12",
  "HL 8",
  "HL 6",
  "Estimated Adaptive",
  "Oracle Adaptive"
)

paired_rows <- list()
paired_index <- 1


for (strategy in comparison_strategies) {
  
  strategy_column <- paste0(
    "MAE.",
    strategy
  )
  
  difference <- (
    mae_wide[[strategy_column]] -
      mae_wide[["MAE.No Decay"]]
  )
  
  current_mean <- mean(
    difference
  )
  
  current_se <- sd(
    difference
  ) / sqrt(
    length(
      difference
    )
  )
  
  paired_rows[[paired_index]] <- data.frame(
    comparison = paste0(
      strategy,
      " - No Decay"
    ),
    
    mean_difference =
      current_mean,
    
    MC_SE =
      current_se,
    
    lower_95 = (
      current_mean -
        1.96 *
        current_se
    ),
    
    upper_95 = (
      current_mean +
        1.96 *
        current_se
    ),
    
    win_rate_percent =
      100 *
      mean(
        difference < 0
      ),
    
    stringsAsFactors =
      FALSE
  )
  
  paired_index <-
    paired_index + 1
}


paired_summary <- do.call(
  rbind,
  paired_rows
)


cat(
  "\n========================================\n"
)

cat(
  "PAIRED MAE DIFFERENCES VS NO DECAY\n"
)

cat(
  "========================================\n\n"
)

print(
  paired_summary
)


# ============================================================
# 41. ESTIMATED VS ORACLE
# ============================================================

adaptive_oracle_difference <- (
  mae_wide[["MAE.Estimated Adaptive"]] -
    mae_wide[["MAE.Oracle Adaptive"]]
)

adaptive_oracle_summary <- data.frame(
  mean_difference =
    mean(
      adaptive_oracle_difference
    ),
  
  MC_SE =
    sd(
      adaptive_oracle_difference
    ) /
    sqrt(
      length(
        adaptive_oracle_difference
      )
    ),
  
  estimated_beats_oracle_percent =
    100 *
    mean(
      adaptive_oracle_difference <
        0
    )
)

adaptive_oracle_summary$lower_95 <- (
  adaptive_oracle_summary$mean_difference -
    1.96 *
    adaptive_oracle_summary$MC_SE
)

adaptive_oracle_summary$upper_95 <- (
  adaptive_oracle_summary$mean_difference +
    1.96 *
    adaptive_oracle_summary$MC_SE
)


cat(
  "\n========================================\n"
)

cat(
  "ESTIMATED ADAPTIVE VS ORACLE ADAPTIVE\n"
)

cat(
  "========================================\n\n"
)

print(
  adaptive_oracle_summary
)


# ============================================================
# 42. ORACLE GAIN CAPTURED
# ============================================================

strategy_means <- tapply(
  season_results$MAE,
  season_results$strategy,
  mean
)

no_decay_mean <-
  strategy_means["No Decay"]

estimated_mean <-
  strategy_means["Estimated Adaptive"]

oracle_mean <-
  strategy_means["Oracle Adaptive"]

estimated_gain <- (
  no_decay_mean -
    estimated_mean
)

oracle_gain <- (
  no_decay_mean -
    oracle_mean
)

oracle_gain_capture <- data.frame(
  no_decay_MAE =
    no_decay_mean,
  
  estimated_adaptive_MAE =
    estimated_mean,
  
  oracle_adaptive_MAE =
    oracle_mean,
  
  estimated_gain =
    estimated_gain,
  
  oracle_gain =
    oracle_gain,
  
  percent_oracle_gain_captured =
    if (oracle_gain != 0) {
      
      100 *
        estimated_gain /
        oracle_gain
      
    } else {
      
      NA_real_
    }
)


cat(
  "\n========================================\n"
)

cat(
  "ORACLE RECENCY GAIN CAPTURED\n"
)

cat(
  "========================================\n\n"
)

print(
  oracle_gain_capture
)


# ============================================================
# 43. PLOTS
# ============================================================

mae_plot <- ggplot(
  summary_by_drift,
  aes(
    x = true_drift_SD,
    y = MAE,
    group = strategy,
    linetype = strategy
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
      "Version 7C: Adaptive Recency by True Team Volatility",
    
    x =
      "True Weekly Rating Drift SD",
    
    y =
      "Out-of-Sample MAE",
    
    linetype =
      "Strategy"
  ) +
  theme_minimal()

print(
  mae_plot
)


volatility_plot <- ggplot(
  volatility_quality,
  aes(
    x = true_drift_SD,
    y = estimated_drift_SD
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
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
      "Estimated vs True Team-Strength Volatility",
    
    x =
      "True Drift SD",
    
    y =
      "Mean Estimated Drift SD"
  ) +
  theme_minimal()

print(
  volatility_plot
)


volatility_week_plot <- ggplot(
  volatility_by_week,
  aes(
    x = target_week,
    y = drift_MAE
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
      adaptive_start_week:max_week
  ) +
  labs(
    title =
      "Volatility Estimation Error by Prediction Week",
    
    x =
      "Target Week",
    
    y =
      "Drift SD MAE"
  ) +
  theme_minimal()

print(
  volatility_week_plot
)


# ============================================================
# 44. SAVE OUTPUTS
# ============================================================

write.csv(
  parity_calibration_coefficients,
  "version7C_parity_calibration.csv",
  row.names = FALSE
)

write.csv(
  volatility_calibration_coefficients,
  "version7C_volatility_calibration.csv",
  row.names = FALSE
)

write.csv(
  tuning_data,
  "version7C_tuning_data.csv",
  row.names = FALSE
)

write.csv(
  oracle_policy,
  "version7C_oracle_policy.csv",
  row.names = FALSE
)

write.csv(
  season_results,
  "version7C_all_season_results.csv",
  row.names = FALSE
)

write.csv(
  decision_results,
  "version7C_decision_results.csv",
  row.names = FALSE
)

write.csv(
  overall_summary,
  "version7C_overall_summary.csv",
  row.names = FALSE
)

write.csv(
  summary_by_drift,
  "version7C_results_by_drift.csv",
  row.names = FALSE
)

write.csv(
  volatility_quality,
  "version7C_volatility_quality.csv",
  row.names = FALSE
)

write.csv(
  volatility_by_week,
  "version7C_volatility_by_week.csv",
  row.names = FALSE
)

write.csv(
  choice_table,
  "version7C_choice_frequency.csv",
  row.names = FALSE
)

write.csv(
  paired_summary,
  "version7C_paired_comparisons.csv",
  row.names = FALSE
)

write.csv(
  oracle_gain_capture,
  "version7C_oracle_gain_capture.csv",
  row.names = FALSE
)


# ============================================================
# 45. FINAL OUTPUT
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 7C COMPLETE\n"
)

cat(
  "========================================\n\n"
)

cat(
  "Recency candidates:\n"
)

cat(
  "No Decay, HL 12, HL 8, HL 6\n\n"
)

cat(
  "Drift levels tested:\n"
)

cat(
  paste(
    drift_sd_values,
    collapse = ", "
  ),
  "\n\n"
)

cat(
  "Final evaluation seasons:",
  length(true_sd_values) *
    length(drift_sd_values) *
    n_evaluation_per_cell,
  "\n\n"
)

cat(
  "OVERALL MODEL COMPARISON:\n\n"
)

print(
  overall_summary
)

cat(
  "\nRESULTS BY TRUE DRIFT:\n\n"
)

print(
  summary_by_drift
)

cat(
  "\nVOLATILITY ESTIMATION QUALITY:\n\n"
)

print(
  volatility_quality
)

cat(
  "\nVOLATILITY QUALITY BY WEEK:\n\n"
)

print(
  volatility_by_week
)

cat(
  "\nADAPTIVE DECISION QUALITY:\n\n"
)

print(
  decision_accuracy
)

cat(
  "\nESTIMATED CHOICE FREQUENCY:\n\n"
)

print(
  choice_table
)

cat(
  "\nPAIRED COMPARISONS:\n\n"
)

print(
  paired_summary
)

cat(
  "\nESTIMATED VS ORACLE:\n\n"
)

print(
  adaptive_oracle_summary
)

cat(
  "\nORACLE GAIN CAPTURED:\n\n"
)

print(
  oracle_gain_capture
)


# ============================================================
# END VERSION 7C
# ============================================================
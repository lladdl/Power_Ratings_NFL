# ============================================================
# FOOTBALL POWER RATINGS - VERSION 5B
# MONTE CARLO WITH RANDOM HIDDEN TEAM STRENGTHS
# ============================================================
#
# PURPOSE:
#
# Compare two team-rating shrinkage methods across 1,000
# independently simulated football seasons.
#
#
# MODEL A: CONSTANT SHRINKAGE
#
#   Team lambda = 2 throughout the season
#
#
# MODEL B: DYNAMIC SHRINKAGE
#
#   Games played/team:
#
#       1-2 games -> lambda = 5
#       3-4 games -> lambda = 4
#       5-6 games -> lambda = 3
#       7+ games  -> lambda = 2
#
#
# BOTH MODELS:
#
#   Margin cap = +/- 21
#
#   HFA prior = 2.5
#
#   HFA lambda:
#
#       1-2 games -> 10
#       3-4 games -> 8
#       5-6 games -> 5
#       7+ games  -> 2
#
#
# IMPORTANT DIFFERENCE FROM VERSION 5A:
#
# Every simulated season gets a NEW set of hidden
# true team strengths.
#
# Example:
#
# Season 1:
#   Austin +7
#   Boston -2
#   Chicago +4
#   ...
#
# Season 2:
#   Austin -3
#   Boston +8
#   Chicago +1
#   ...
#
# The hidden ratings are fixed DURING a season,
# but change BETWEEN seasons.
#
# ============================================================



# ============================================================
# 1. PACKAGE
# ============================================================

library(ggplot2)



# ============================================================
# 2. LOAD ORIGINAL FILE FOR SCHEDULE ONLY
# ============================================================

base_games <- read.csv(
  "football_power_ratings_games.csv",
  stringsAsFactors = FALSE
)


# We do NOT use the original scores.
#
# We only keep the schedule.

schedule <- base_games[
  ,
  c(
    "game_id",
    "week",
    "home_team",
    "away_team"
  )
]


head(schedule)



# ============================================================
# 3. IDENTIFY TEAMS
# ============================================================

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


cat(
  "\nTeams:",
  n_teams,
  "\n"
)

cat(
  "Games per season:",
  nrow(schedule),
  "\n"
)

cat(
  "Weeks:",
  max_week,
  "\n"
)



# ============================================================
# 4. SIMULATION PARAMETERS
# ============================================================

# ------------------------------------------------------------
# TRUE TEAM STRENGTH DISTRIBUTION
# ------------------------------------------------------------
#
# Before every simulated season:
#
#   true rating ~ Normal(0, 5)
#
# Then ratings are centered so league average = 0.
#
# ------------------------------------------------------------

true_rating_sd <- 5



# ------------------------------------------------------------
# TRUE HOME-FIELD ADVANTAGE
# ------------------------------------------------------------

true_hfa <- 2.7



# ------------------------------------------------------------
# GAME-TO-GAME RANDOMNESS
# ------------------------------------------------------------

noise_sd <- 8.5



# ------------------------------------------------------------
# MODEL SETTINGS
# ------------------------------------------------------------

margin_cap <- 21

hfa_prior <- 2.5



# ============================================================
# 5. GENERATE TRUE TEAM STRENGTHS
# ============================================================
#
# This function creates a completely new hidden league.
#
# The model NEVER sees these values.
#
# ============================================================

generate_true_ratings <- function(
    league_teams,
    rating_sd = 5
) {
  
  
  ratings <- rnorm(
    
    length(league_teams),
    
    mean = 0,
    
    sd = rating_sd
    
  )
  
  
  # ----------------------------------------------------------
  # Center league average at zero
  # ----------------------------------------------------------
  
  ratings <-
    
    ratings -
    
    mean(ratings)
  
  
  # ----------------------------------------------------------
  # Attach team names
  # ----------------------------------------------------------
  
  names(ratings) <-
    league_teams
  
  
  return(
    ratings
  )
  
}



# ============================================================
# 6. SIMULATE ONE SEASON
# ============================================================
#
# True expected margin:
#
# Home Strength
# - Away Strength
# + True HFA
#
#
# Actual margin:
#
# True Expected Margin
# + random game noise
#
# ============================================================

simulate_season <- function(
    schedule,
    true_ratings,
    true_hfa = 2.7,
    noise_sd = 8.5,
    margin_cap = 21
) {
  
  
  sim_games <-
    schedule
  
  
  
  # ----------------------------------------------------------
  # TRUE EXPECTED MARGIN
  # ----------------------------------------------------------
  
  sim_games$true_expected_margin <-
    
    true_ratings[
      sim_games$home_team
    ] -
    
    true_ratings[
      sim_games$away_team
    ] +
    
    true_hfa
  
  
  
  # Remove names left by vector lookup
  
  sim_games$true_expected_margin <-
    
    as.numeric(
      sim_games$true_expected_margin
    )
  
  
  
  # ----------------------------------------------------------
  # RANDOM GAME RESULTS
  # ----------------------------------------------------------
  
  sim_games$actual_margin <-
    
    round(
      
      sim_games$true_expected_margin +
        
        rnorm(
          
          nrow(sim_games),
          
          mean = 0,
          
          sd = noise_sd
          
        )
      
    )
  
  
  
  # ----------------------------------------------------------
  # AVOID TIES
  # ----------------------------------------------------------
  
  zero_games <- which(
    
    sim_games$actual_margin == 0
    
  )
  
  
  if (
    length(zero_games) > 0
  ) {
    
    
    sim_games$actual_margin[
      zero_games
    ] <-
      
      sample(
        
        c(
          -1,
          1
        ),
        
        size =
          length(zero_games),
        
        replace =
          TRUE
        
      )
    
  }
  
  
  
  # ----------------------------------------------------------
  # CAPPED MARGIN
  # ----------------------------------------------------------
  
  sim_games$capped_margin <- pmax(
    
    pmin(
      
      sim_games$actual_margin,
      
      margin_cap
      
    ),
    
    -margin_cap
    
  )
  
  
  
  return(
    sim_games
  )
  
}



# ============================================================
# 7. SHRINKAGE FUNCTION
# ============================================================

get_shrinkage <- function(
    data,
    league_teams,
    team_method
) {
  
  
  # ----------------------------------------------------------
  # AVERAGE GAMES PLAYED
  # ----------------------------------------------------------
  
  avg_games_played <-
    
    (2 * nrow(data)) /
    
    length(league_teams)
  
  
  
  # ==========================================================
  # TEAM RATING LAMBDA
  # ==========================================================
  
  if (
    team_method == "constant"
  ) {
    
    
    # Constant model
    
    team_lambda <- 2
    
    
  } else if (
    team_method == "dynamic"
  ) {
    
    
    # Dynamic model
    
    if (
      avg_games_played <= 2
    ) {
      
      team_lambda <- 5
      
      
    } else if (
      avg_games_played <= 4
    ) {
      
      team_lambda <- 4
      
      
    } else if (
      avg_games_played <= 6
    ) {
      
      team_lambda <- 3
      
      
    } else {
      
      team_lambda <- 2
      
    }
    
    
  } else {
    
    
    stop(
      "team_method must be 'constant' or 'dynamic'"
    )
    
  }
  
  
  
  # ==========================================================
  # HOME-FIELD LAMBDA
  # ==========================================================
  #
  # HFA treatment is identical for both models.
  # ==========================================================
  
  if (
    avg_games_played <= 2
  ) {
    
    hfa_lambda <- 10
    
    
  } else if (
    avg_games_played <= 4
  ) {
    
    hfa_lambda <- 8
    
    
  } else if (
    avg_games_played <= 6
  ) {
    
    hfa_lambda <- 5
    
    
  } else {
    
    hfa_lambda <- 2
    
  }
  
  
  
  return(
    
    list(
      
      team_lambda =
        team_lambda,
      
      hfa_lambda =
        hfa_lambda,
      
      avg_games_played =
        avg_games_played
      
    )
    
  )
  
}



# ============================================================
# 8. FIT POWER RATINGS
# ============================================================

fit_power_ratings <- function(
    data,
    league_teams,
    team_lambda,
    hfa_lambda,
    hfa_prior = 2.5,
    margin_cap = 21
) {
  
  
  n_games <-
    nrow(data)
  
  
  n_teams <-
    length(league_teams)
  
  
  
  # ==========================================================
  # RESPONSE VARIABLE
  # ==========================================================
  
  y <- pmax(
    
    pmin(
      
      data$actual_margin,
      
      margin_cap
      
    ),
    
    -margin_cap
    
  )
  
  
  
  # ==========================================================
  # DESIGN MATRIX
  # ==========================================================
  
  X <- matrix(
    
    0,
    
    nrow =
      n_games,
    
    ncol =
      n_teams + 1
    
  )
  
  
  colnames(X) <- c(
    
    "HFA",
    
    league_teams
    
  )
  
  
  
  # HFA column
  
  X[
    ,
    "HFA"
  ] <- 1
  
  
  
  # ----------------------------------------------------------
  # TEAM EFFECTS
  # ----------------------------------------------------------
  
  for (
    i in seq_len(n_games)
  ) {
    
    
    home <-
      data$home_team[i]
    
    
    away <-
      data$away_team[i]
    
    
    
    X[
      i,
      home
    ] <- 1
    
    
    X[
      i,
      away
    ] <- -1
    
  }
  
  
  
  # ==========================================================
  # PENALTY MATRIX
  # ==========================================================
  
  penalty <- diag(
    
    c(
      
      hfa_lambda,
      
      rep(
        
        team_lambda,
        
        n_teams
        
      )
      
    )
    
  )
  
  
  
  # ==========================================================
  # PRIOR TARGET
  # ==========================================================
  #
  # HFA -> 2.5
  #
  # Team ratings -> 0
  #
  # ==========================================================
  
  prior_target <- c(
    
    hfa_prior,
    
    rep(
      
      0,
      
      n_teams
      
    )
    
  )
  
  
  
  # ==========================================================
  # FIT PENALIZED REGRESSION
  # ==========================================================
  
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
  
  
  
  beta <-
    as.numeric(
      beta_matrix
    )
  
  
  names(beta) <-
    colnames(X)
  
  
  
  # ----------------------------------------------------------
  # HOME-FIELD ADVANTAGE
  # ----------------------------------------------------------
  
  estimated_hfa <-
    
    beta[
      "HFA"
    ]
  
  
  
  # ----------------------------------------------------------
  # TEAM RATINGS
  # ----------------------------------------------------------
  
  ratings <-
    
    beta[
      league_teams
    ]
  
  
  
  # Center team ratings at zero
  
  ratings <-
    
    ratings -
    
    mean(ratings)
  
  
  
  rating_table <- data.frame(
    
    team =
      league_teams,
    
    rating =
      as.numeric(ratings),
    
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
# 9. EVALUATE ONE MODEL ON ONE SEASON
# ============================================================

evaluate_season <- function(
    games,
    true_ratings,
    true_hfa,
    league_teams,
    team_method,
    hfa_prior = 2.5,
    margin_cap = 21
) {
  
  
  max_week <-
    max(
      games$week
    )
  
  
  prediction_list <-
    list()
  
  
  weekly_rating_rmse <-
    numeric()
  
  
  weekly_hfa_error <-
    numeric()
  
  
  
  # ==========================================================
  # WALK-FORWARD LOOP
  # ==========================================================
  
  for (
    target_week in 2:max_week
  ) {
    
    
    # --------------------------------------------------------
    # TRAINING DATA
    # --------------------------------------------------------
    
    training_games <- games[
      
      games$week <
        target_week,
      
    ]
    
    
    
    # --------------------------------------------------------
    # FUTURE TEST WEEK
    # --------------------------------------------------------
    
    test_games <- games[
      
      games$week ==
        target_week,
      
    ]
    
    
    
    # --------------------------------------------------------
    # SHRINKAGE SETTINGS
    # --------------------------------------------------------
    
    shrinkage <- get_shrinkage(
      
      data =
        training_games,
      
      league_teams =
        league_teams,
      
      team_method =
        team_method
      
    )
    
    
    
    # --------------------------------------------------------
    # FIT MODEL
    # --------------------------------------------------------
    
    model <- fit_power_ratings(
      
      data =
        training_games,
      
      league_teams =
        league_teams,
      
      team_lambda =
        shrinkage$team_lambda,
      
      hfa_lambda =
        shrinkage$hfa_lambda,
      
      hfa_prior =
        hfa_prior,
      
      margin_cap =
        margin_cap
      
    )
    
    
    
    # --------------------------------------------------------
    # RATING LOOKUP
    # --------------------------------------------------------
    
    rating_lookup <-
      
      model$ratings$rating
    
    
    names(rating_lookup) <-
      
      model$ratings$team
    
    
    
    # ========================================================
    # TRUE RATING RECOVERY
    # ========================================================
    
    current_rating_rmse <- sqrt(
      
      mean(
        
        (
          
          rating_lookup[
            league_teams
          ] -
            
            true_ratings[
              league_teams
            ]
          
        )^2
        
      )
      
    )
    
    
    weekly_rating_rmse <-
      
      c(
        
        weekly_rating_rmse,
        
        current_rating_rmse
        
      )
    
    
    
    # ========================================================
    # HFA RECOVERY
    # ========================================================
    
    current_hfa_error <-
      
      abs(
        
        model$home_field -
          
          true_hfa
        
      )
    
    
    weekly_hfa_error <-
      
      c(
        
        weekly_hfa_error,
        
        current_hfa_error
        
      )
    
    
    
    # ========================================================
    # PREDICT FUTURE GAMES
    # ========================================================
    
    test_games$home_rating <-
      
      rating_lookup[
        test_games$home_team
      ]
    
    
    test_games$away_rating <-
      
      rating_lookup[
        test_games$away_team
      ]
    
    
    test_games$estimated_hfa <-
      
      model$home_field
    
    
    
    test_games$predicted_margin <-
      
      test_games$home_rating -
      
      test_games$away_rating +
      
      test_games$estimated_hfa
    
    
    
    # ========================================================
    # ERRORS
    # ========================================================
    
    test_games$error_raw <-
      
      test_games$actual_margin -
      
      test_games$predicted_margin
    
    
    
    test_games$error_capped <-
      
      test_games$capped_margin -
      
      test_games$predicted_margin
    
    
    
    # ========================================================
    # WINNER
    # ========================================================
    
    test_games$predicted_home_win <-
      
      test_games$predicted_margin > 0
    
    
    test_games$actual_home_win <-
      
      test_games$actual_margin > 0
    
    
    test_games$correct_winner <-
      
      test_games$predicted_home_win ==
      
      test_games$actual_home_win
    
    
    
    prediction_list[[as.character(target_week)]] <- test_games
    
  }
  
  
  
  # ==========================================================
  # COMBINE PREDICTIONS
  # ==========================================================
  
  predictions <- do.call(
    
    rbind,
    
    prediction_list
    
  )
  
  
  rownames(predictions) <- NULL
  
  
  
  # ==========================================================
  # PERFORMANCE METRICS
  # ==========================================================
  
  raw_mae <- mean(
    
    abs(
      predictions$error_raw
    )
    
  )
  
  
  raw_rmse <- sqrt(
    
    mean(
      predictions$error_raw^2
    )
    
  )
  
  
  capped_mae <- mean(
    
    abs(
      predictions$error_capped
    )
    
  )
  
  
  capped_rmse <- sqrt(
    
    mean(
      predictions$error_capped^2
    )
    
  )
  
  
  
  prediction_correlation <- cor(
    
    predictions$predicted_margin,
    
    predictions$actual_margin
    
  )
  
  
  
  winner_accuracy <- mean(
    
    predictions$correct_winner
    
  )
  
  
  
  prediction_bias <- mean(
    
    predictions$error_raw
    
  )
  
  
  
  mean_rating_rmse <- mean(
    
    weekly_rating_rmse
    
  )
  
  
  mean_hfa_error <- mean(
    
    weekly_hfa_error
    
  )
  
  
  
  return(
    
    list(
      
      raw_MAE =
        raw_mae,
      
      raw_RMSE =
        raw_rmse,
      
      capped_MAE =
        capped_mae,
      
      capped_RMSE =
        capped_rmse,
      
      correlation =
        prediction_correlation,
      
      accuracy =
        winner_accuracy,
      
      bias =
        prediction_bias,
      
      rating_RMSE =
        mean_rating_rmse,
      
      HFA_error =
        mean_hfa_error
      
    )
    
  )
  
}



# ============================================================
# 10. TEST ONE RANDOM SEASON FIRST
# ============================================================

set.seed(
  20260915
)



# Generate brand-new hidden strengths

test_true_ratings <- generate_true_ratings(
  
  league_teams =
    teams,
  
  rating_sd =
    true_rating_sd
  
)



cat(
  "\n========================================\n"
)

cat(
  "TEST SEASON HIDDEN TRUE RATINGS\n"
)

cat(
  "========================================\n\n"
)


print(
  
  sort(
    
    test_true_ratings,
    
    decreasing = TRUE
    
  )
  
)


cat(
  "\nMean:",
  mean(test_true_ratings),
  "\n"
)


cat(
  "SD:",
  sd(test_true_ratings),
  "\n"
)



# ------------------------------------------------------------
# Generate season
# ------------------------------------------------------------

test_season <- simulate_season(
  
  schedule =
    schedule,
  
  true_ratings =
    test_true_ratings,
  
  true_hfa =
    true_hfa,
  
  noise_sd =
    noise_sd,
  
  margin_cap =
    margin_cap
  
)



# ------------------------------------------------------------
# CONSTANT MODEL
# ------------------------------------------------------------

test_constant <- evaluate_season(
  
  games =
    test_season,
  
  true_ratings =
    test_true_ratings,
  
  true_hfa =
    true_hfa,
  
  league_teams =
    teams,
  
  team_method =
    "constant",
  
  hfa_prior =
    hfa_prior,
  
  margin_cap =
    margin_cap
  
)



# ------------------------------------------------------------
# DYNAMIC MODEL
# ------------------------------------------------------------

test_dynamic <- evaluate_season(
  
  games =
    test_season,
  
  true_ratings =
    test_true_ratings,
  
  true_hfa =
    true_hfa,
  
  league_teams =
    teams,
  
  team_method =
    "dynamic",
  
  hfa_prior =
    hfa_prior,
  
  margin_cap =
    margin_cap
  
)



cat(
  "\nCONSTANT MODEL TEST:\n"
)

print(
  test_constant
)


cat(
  "\nDYNAMIC MODEL TEST:\n"
)

print(
  test_dynamic
)



# ============================================================
# 11. MONTE CARLO SETTINGS
# ============================================================

n_simulations <- 1000



# Set once.
#
# Do NOT reset seed inside loop.

set.seed(
  777
)



# ============================================================
# 12. PREALLOCATE RESULTS
# ============================================================

simulation_results <- data.frame(
  
  simulation =
    1:n_simulations,
  
  
  actual_true_rating_sd =
    NA_real_,
  
  
  true_rating_range =
    NA_real_,
  
  
  constant_raw_MAE =
    NA_real_,
  
  dynamic_raw_MAE =
    NA_real_,
  
  
  constant_raw_RMSE =
    NA_real_,
  
  dynamic_raw_RMSE =
    NA_real_,
  
  
  constant_capped_MAE =
    NA_real_,
  
  dynamic_capped_MAE =
    NA_real_,
  
  
  constant_capped_RMSE =
    NA_real_,
  
  dynamic_capped_RMSE =
    NA_real_,
  
  
  constant_correlation =
    NA_real_,
  
  dynamic_correlation =
    NA_real_,
  
  
  constant_accuracy =
    NA_real_,
  
  dynamic_accuracy =
    NA_real_,
  
  
  constant_bias =
    NA_real_,
  
  dynamic_bias =
    NA_real_,
  
  
  constant_rating_RMSE =
    NA_real_,
  
  dynamic_rating_RMSE =
    NA_real_,
  
  
  constant_HFA_error =
    NA_real_,
  
  dynamic_HFA_error =
    NA_real_
  
)



# ============================================================
# 13. MONTE CARLO LOOP
# ============================================================

for (
  s in 1:n_simulations
) {
  
  
  # ==========================================================
  # STEP 1:
  # GENERATE COMPLETELY NEW TRUE TEAM STRENGTHS
  # ==========================================================
  
  season_true_ratings <- generate_true_ratings(
    
    league_teams =
      teams,
    
    rating_sd =
      true_rating_sd
    
  )
  
  
  
  # Save information about strength of league
  
  simulation_results$actual_true_rating_sd[s] <-
    
    sd(
      season_true_ratings
    )
  
  
  simulation_results$true_rating_range[s] <-
    
    max(
      season_true_ratings
    ) -
    
    min(
      season_true_ratings
    )
  
  
  
  # ==========================================================
  # STEP 2:
  # SIMULATE A NEW SEASON
  # ==========================================================
  
  sim_games <- simulate_season(
    
    schedule =
      schedule,
    
    true_ratings =
      season_true_ratings,
    
    true_hfa =
      true_hfa,
    
    noise_sd =
      noise_sd,
    
    margin_cap =
      margin_cap
    
  )
  
  
  
  # ==========================================================
  # STEP 3:
  # CONSTANT LAMBDA = 2
  # ==========================================================
  
  constant_result <- evaluate_season(
    
    games =
      sim_games,
    
    true_ratings =
      season_true_ratings,
    
    true_hfa =
      true_hfa,
    
    league_teams =
      teams,
    
    team_method =
      "constant",
    
    hfa_prior =
      hfa_prior,
    
    margin_cap =
      margin_cap
    
  )
  
  
  
  # ==========================================================
  # STEP 4:
  # DYNAMIC LAMBDA = 5 / 4 / 3 / 2
  # ==========================================================
  
  dynamic_result <- evaluate_season(
    
    games =
      sim_games,
    
    true_ratings =
      season_true_ratings,
    
    true_hfa =
      true_hfa,
    
    league_teams =
      teams,
    
    team_method =
      "dynamic",
    
    hfa_prior =
      hfa_prior,
    
    margin_cap =
      margin_cap
    
  )
  
  
  
  # ==========================================================
  # STEP 5:
  # SAVE CONSTANT MODEL
  # ==========================================================
  
  simulation_results$constant_raw_MAE[s] <-
    
    constant_result$raw_MAE
  
  
  simulation_results$constant_raw_RMSE[s] <-
    
    constant_result$raw_RMSE
  
  
  simulation_results$constant_capped_MAE[s] <-
    
    constant_result$capped_MAE
  
  
  simulation_results$constant_capped_RMSE[s] <-
    
    constant_result$capped_RMSE
  
  
  simulation_results$constant_correlation[s] <-
    
    constant_result$correlation
  
  
  simulation_results$constant_accuracy[s] <-
    
    constant_result$accuracy
  
  
  simulation_results$constant_bias[s] <-
    
    constant_result$bias
  
  
  simulation_results$constant_rating_RMSE[s] <-
    
    constant_result$rating_RMSE
  
  
  simulation_results$constant_HFA_error[s] <-
    
    constant_result$HFA_error
  
  
  
  # ==========================================================
  # STEP 6:
  # SAVE DYNAMIC MODEL
  # ==========================================================
  
  simulation_results$dynamic_raw_MAE[s] <-
    
    dynamic_result$raw_MAE
  
  
  simulation_results$dynamic_raw_RMSE[s] <-
    
    dynamic_result$raw_RMSE
  
  
  simulation_results$dynamic_capped_MAE[s] <-
    
    dynamic_result$capped_MAE
  
  
  simulation_results$dynamic_capped_RMSE[s] <-
    
    dynamic_result$capped_RMSE
  
  
  simulation_results$dynamic_correlation[s] <-
    
    dynamic_result$correlation
  
  
  simulation_results$dynamic_accuracy[s] <-
    
    dynamic_result$accuracy
  
  
  simulation_results$dynamic_bias[s] <-
    
    dynamic_result$bias
  
  
  simulation_results$dynamic_rating_RMSE[s] <-
    
    dynamic_result$rating_RMSE
  
  
  simulation_results$dynamic_HFA_error[s] <-
    
    dynamic_result$HFA_error
  
  
  
  # ==========================================================
  # PROGRESS
  # ==========================================================
  
  if (
    s %% 100 == 0
  ) {
    
    cat(
      
      "Completed",
      s,
      "of",
      n_simulations,
      "simulations\n"
      
    )
    
  }
  
}



# ============================================================
# 14. PAIRED MODEL DIFFERENCES
# ============================================================
#
# For error metrics:
#
# Negative = dynamic better
# Positive = constant better
#
# ============================================================

simulation_results$MAE_difference <-
  
  simulation_results$dynamic_raw_MAE -
  
  simulation_results$constant_raw_MAE



simulation_results$RMSE_difference <-
  
  simulation_results$dynamic_raw_RMSE -
  
  simulation_results$constant_raw_RMSE



simulation_results$capped_MAE_difference <-
  
  simulation_results$dynamic_capped_MAE -
  
  simulation_results$constant_capped_MAE



simulation_results$capped_RMSE_difference <-
  
  simulation_results$dynamic_capped_RMSE -
  
  simulation_results$constant_capped_RMSE



simulation_results$rating_RMSE_difference <-
  
  simulation_results$dynamic_rating_RMSE -
  
  simulation_results$constant_rating_RMSE



# For accuracy:
#
# Positive = dynamic better

simulation_results$accuracy_difference <-
  
  simulation_results$dynamic_accuracy -
  
  simulation_results$constant_accuracy



# ============================================================
# 15. SUMMARY TABLE
# ============================================================

summary_table <- data.frame(
  
  metric = c(
    
    "Raw MAE",
    
    "Raw RMSE",
    
    "Capped MAE",
    
    "Capped RMSE",
    
    "Correlation",
    
    "Winner Accuracy",
    
    "Rating RMSE",
    
    "Mean HFA Error"
    
  ),
  
  
  constant = c(
    
    mean(
      simulation_results$constant_raw_MAE
    ),
    
    mean(
      simulation_results$constant_raw_RMSE
    ),
    
    mean(
      simulation_results$constant_capped_MAE
    ),
    
    mean(
      simulation_results$constant_capped_RMSE
    ),
    
    mean(
      simulation_results$constant_correlation
    ),
    
    mean(
      simulation_results$constant_accuracy
    ),
    
    mean(
      simulation_results$constant_rating_RMSE
    ),
    
    mean(
      simulation_results$constant_HFA_error
    )
    
  ),
  
  
  dynamic = c(
    
    mean(
      simulation_results$dynamic_raw_MAE
    ),
    
    mean(
      simulation_results$dynamic_raw_RMSE
    ),
    
    mean(
      simulation_results$dynamic_capped_MAE
    ),
    
    mean(
      simulation_results$dynamic_capped_RMSE
    ),
    
    mean(
      simulation_results$dynamic_correlation
    ),
    
    mean(
      simulation_results$dynamic_accuracy
    ),
    
    mean(
      simulation_results$dynamic_rating_RMSE
    ),
    
    mean(
      simulation_results$dynamic_HFA_error
    )
    
  )
  
)



summary_table$difference <-
  
  summary_table$dynamic -
  
  summary_table$constant



cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 5B MODEL COMPARISON\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_table
)



# ============================================================
# 16. MONTE CARLO WIN RATES
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "MONTE CARLO WIN RATES\n"
)

cat(
  "========================================\n\n"
)



cat(
  
  "Dynamic lower Raw MAE:",
  
  round(
    
    mean(
      
      simulation_results$dynamic_raw_MAE <
        
        simulation_results$constant_raw_MAE
      
    ) * 100,
    
    1
    
  ),
  
  "%\n"
  
)



cat(
  
  "Dynamic lower Raw RMSE:",
  
  round(
    
    mean(
      
      simulation_results$dynamic_raw_RMSE <
        
        simulation_results$constant_raw_RMSE
      
    ) * 100,
    
    1
    
  ),
  
  "%\n"
  
)



cat(
  
  "Dynamic lower Capped MAE:",
  
  round(
    
    mean(
      
      simulation_results$dynamic_capped_MAE <
        
        simulation_results$constant_capped_MAE
      
    ) * 100,
    
    1
    
  ),
  
  "%\n"
  
)



cat(
  
  "Dynamic better rating recovery:",
  
  round(
    
    mean(
      
      simulation_results$dynamic_rating_RMSE <
        
        simulation_results$constant_rating_RMSE
      
    ) * 100,
    
    1
    
  ),
  
  "%\n"
  
)



cat(
  
  "Dynamic higher winner accuracy:",
  
  round(
    
    mean(
      
      simulation_results$dynamic_accuracy >
        
        simulation_results$constant_accuracy
      
    ) * 100,
    
    1
    
  ),
  
  "%\n"
  
)



cat(
  
  "Constant higher winner accuracy:",
  
  round(
    
    mean(
      
      simulation_results$constant_accuracy >
        
        simulation_results$dynamic_accuracy
      
    ) * 100,
    
    1
    
  ),
  
  "%\n"
  
)



cat(
  
  "Winner accuracy tie:",
  
  round(
    
    mean(
      
      simulation_results$constant_accuracy ==
        
        simulation_results$dynamic_accuracy
      
    ) * 100,
    
    1
    
  ),
  
  "%\n"
  
)



# ============================================================
# 17. PAIRED RAW MAE DIFFERENCE
# ============================================================

mae_difference <-
  
  simulation_results$MAE_difference



mean_difference <- mean(
  
  mae_difference
  
)


mc_standard_error <-
  
  sd(
    mae_difference
  ) /
  
  sqrt(
    n_simulations
  )



lower_ci <-
  
  mean_difference -
  
  1.96 *
  mc_standard_error



upper_ci <-
  
  mean_difference +
  
  1.96 *
  mc_standard_error



cat(
  "\n========================================\n"
)

cat(
  "RAW MAE PAIRED DIFFERENCE\n"
)

cat(
  "========================================\n\n"
)



cat(
  
  "Mean Dynamic - Constant:",
  
  round(
    mean_difference,
    4
  ),
  
  "\n"
  
)



cat(
  
  "Monte Carlo SE:",
  
  round(
    mc_standard_error,
    4
  ),
  
  "\n"
  
)



cat(
  
  "Approx. 95% Monte Carlo Interval:",
  
  round(
    lower_ci,
    4
  ),
  
  "to",
  
  round(
    upper_ci,
    4
  ),
  
  "\n"
  
)



# ============================================================
# 18. LEAGUE-STRENGTH INFORMATION
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "SIMULATED LEAGUE STRENGTHS\n"
)

cat(
  "========================================\n\n"
)


cat(
  
  "Target hidden rating SD:",
  
  true_rating_sd,
  
  "\n"
  
)


cat(
  
  "Average realized league SD:",
  
  round(
    
    mean(
      simulation_results$actual_true_rating_sd
    ),
    
    3
    
  ),
  
  "\n"
  
)


cat(
  
  "Average strongest-to-weakest range:",
  
  round(
    
    mean(
      simulation_results$true_rating_range
    ),
    
    3
    
  ),
  
  "points\n"
  
)



# ============================================================
# 19. DOES LEAGUE PARITY AFFECT WHICH MODEL WINS?
# ============================================================

parity_analysis <- lm(
  
  MAE_difference ~
    
    actual_true_rating_sd,
  
  data =
    simulation_results
  
)


cat(
  "\n========================================\n"
)

cat(
  "LAMBDA PERFORMANCE VS LEAGUE STRENGTH SPREAD\n"
)

cat(
  "========================================\n\n"
)


print(
  summary(
    parity_analysis
  )
)



# ============================================================
# 20. PLOT MAE DIFFERENCE
# ============================================================

mae_difference_plot <- ggplot(
  
  simulation_results,
  
  aes(
    x = MAE_difference
  )
  
) +
  
  geom_histogram(
    bins = 40
  ) +
  
  geom_vline(
    
    xintercept = 0,
    
    linetype =
      "dashed"
    
  ) +
  
  labs(
    
    title =
      "Dynamic vs Constant Lambda",
    
    subtitle =
      "Random hidden team strengths each season",
    
    x =
      "Dynamic MAE - Constant MAE",
    
    y =
      "Number of Simulated Seasons"
    
  ) +
  
  theme_minimal()



print(
  mae_difference_plot
)



# ============================================================
# 21. PLOT RATING RECOVERY DIFFERENCE
# ============================================================

rating_recovery_plot <- ggplot(
  
  simulation_results,
  
  aes(
    x = rating_RMSE_difference
  )
  
) +
  
  geom_histogram(
    bins = 40
  ) +
  
  geom_vline(
    
    xintercept = 0,
    
    linetype =
      "dashed"
    
  ) +
  
  labs(
    
    title =
      "Recovery of Hidden Team Strength",
    
    subtitle =
      "Negative values favor dynamic shrinkage",
    
    x =
      "Dynamic Rating RMSE - Constant Rating RMSE",
    
    y =
      "Number of Simulated Seasons"
    
  ) +
  
  theme_minimal()



print(
  rating_recovery_plot
)



# ============================================================
# 22. PLOT EFFECT OF LEAGUE PARITY
# ============================================================

parity_plot <- ggplot(
  
  simulation_results,
  
  aes(
    
    x =
      actual_true_rating_sd,
    
    y =
      MAE_difference
    
  )
  
) +
  
  geom_point(
    alpha = 0.35
  ) +
  
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  
  geom_hline(
    
    yintercept = 0,
    
    linetype =
      "dashed"
    
  ) +
  
  labs(
    
    title =
      "Does League Parity Affect the Best Lambda?",
    
    subtitle =
      "Positive values favor constant lambda = 2",
    
    x =
      "Actual SD of Hidden Team Strength",
    
    y =
      "Dynamic MAE - Constant MAE"
    
  ) +
  
  theme_minimal()



print(
  parity_plot
)



# ============================================================
# 23. SAVE RESULTS
# ============================================================

write.csv(
  
  simulation_results,
  
  "version5B_random_team_strength_monte_carlo.csv",
  
  row.names =
    FALSE
  
)



write.csv(
  
  summary_table,
  
  "version5B_model_comparison.csv",
  
  row.names =
    FALSE
  
)



# ============================================================
# 24. FINAL SUMMARY
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 5B MONTE CARLO COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  
  "Simulated Seasons:",
  
  n_simulations,
  
  "\n"
  
)


cat(
  
  "New Hidden Team Ratings Each Season: YES\n"
  
)


cat(
  
  "Hidden Rating Distribution SD:",
  
  true_rating_sd,
  
  "\n"
  
)


cat(
  
  "True HFA:",
  
  true_hfa,
  
  "\n"
  
)


cat(
  
  "HFA Prior:",
  
  hfa_prior,
  
  "\n"
  
)


cat(
  
  "Margin Cap:",
  
  margin_cap,
  
  "\n"
  
)


cat(
  
  "Game Noise SD:",
  
  noise_sd,
  
  "\n\n"
  
)



print(
  summary_table
)



# ============================================================
# END VERSION 5B
# ============================================================
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
# VERSION 5C
# MONTE CARLO ACROSS DIFFERENT LEAGUE PARITY LEVELS
# ============================================================
#
# We now test:
#
# True Rating SD = 2, 3, 4, 5, 6, 7, 8
#
# At each SD:
#
#   1. Generate 1,000 completely new leagues
#   2. Generate a new season for each league
#   3. Test constant lambda = 2
#   4. Test dynamic lambda = 5/4/3/2
#   5. Compare average MAE
#
# ============================================================



# ============================================================
# 11. MONTE CARLO SETTINGS
# ============================================================

rating_sd_values <- c(
  2,
  3,
  4,
  5,
  6,
  7,
  8
)


n_simulations_per_sd <- 1000


# Set seed ONCE

set.seed(777)



# ============================================================
# 12. CREATE FINAL RESULTS TABLE
# ============================================================

sd_comparison <- data.frame(
  
  true_rating_SD =
    rating_sd_values,
  
  constant_MAE =
    NA_real_,
  
  dynamic_MAE =
    NA_real_,
  
  difference =
    NA_real_,
  
  dynamic_win_rate =
    NA_real_,
  
  constant_win_rate =
    NA_real_,
  
  winner =
    NA_character_
  
)



# ============================================================
# 13. OPTIONAL: STORE EVERY SIMULATION
# ============================================================
#
# This will give us:
#
# 7 SD levels * 1000 seasons = 7,000 rows
#
# Useful for later plots and analysis.
# ============================================================

all_simulation_results <- data.frame()



# ============================================================
# 14. OUTER LOOP OVER TRUE RATING SD
# ============================================================

for (
  sd_index in seq_along(rating_sd_values)
) {
  
  
  current_sd <-
    rating_sd_values[sd_index]
  
  
  cat(
    "\n========================================\n"
  )
  
  cat(
    "TRUE RATING SD:",
    current_sd,
    "\n"
  )
  
  cat(
    "========================================\n"
  )
  
  
  
  # ----------------------------------------------------------
  # Storage for this particular SD
  # ----------------------------------------------------------
  
  constant_mae_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  dynamic_mae_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  constant_rmse_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  dynamic_rmse_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  constant_accuracy_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  dynamic_accuracy_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  constant_rating_rmse_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  dynamic_rating_rmse_values <-
    numeric(
      n_simulations_per_sd
    )
  
  
  realized_rating_sd <-
    numeric(
      n_simulations_per_sd
    )
  
  
  
  # ==========================================================
  # INNER MONTE CARLO LOOP
  # ==========================================================
  
  for (
    s in 1:n_simulations_per_sd
  ) {
    
    
    # ========================================================
    # STEP 1:
    # GENERATE NEW HIDDEN TEAM STRENGTHS
    # ========================================================
    
    season_true_ratings <- generate_true_ratings(
      
      league_teams =
        teams,
      
      rating_sd =
        current_sd
      
    )
    
    
    
    # Record realized SD
    
    realized_rating_sd[s] <-
      
      sd(
        season_true_ratings
      )
    
    
    
    # ========================================================
    # STEP 2:
    # GENERATE NEW SEASON
    # ========================================================
    
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
    
    
    
    # ========================================================
    # STEP 3:
    # CONSTANT LAMBDA = 2
    # ========================================================
    
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
    
    
    
    # ========================================================
    # STEP 4:
    # DYNAMIC LAMBDA = 5 / 4 / 3 / 2
    # ========================================================
    
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
    
    
    
    # ========================================================
    # STEP 5:
    # SAVE RESULTS
    # ========================================================
    
    constant_mae_values[s] <-
      
      constant_result$raw_MAE
    
    
    dynamic_mae_values[s] <-
      
      dynamic_result$raw_MAE
    
    
    
    constant_rmse_values[s] <-
      
      constant_result$raw_RMSE
    
    
    dynamic_rmse_values[s] <-
      
      dynamic_result$raw_RMSE
    
    
    
    constant_accuracy_values[s] <-
      
      constant_result$accuracy
    
    
    dynamic_accuracy_values[s] <-
      
      dynamic_result$accuracy
    
    
    
    constant_rating_rmse_values[s] <-
      
      constant_result$rating_RMSE
    
    
    dynamic_rating_rmse_values[s] <-
      
      dynamic_result$rating_RMSE
    
    
    
    # ========================================================
    # PROGRESS
    # ========================================================
    
    if (
      s %% 100 == 0
    ) {
      
      cat(
        "SD",
        current_sd,
        "- completed",
        s,
        "of",
        n_simulations_per_sd,
        "\n"
      )
      
    }
    
  }
  
  
  
  # ==========================================================
  # 15. SUMMARY FOR THIS SD
  # ==========================================================
  
  mean_constant_mae <-
    
    mean(
      constant_mae_values
    )
  
  
  mean_dynamic_mae <-
    
    mean(
      dynamic_mae_values
    )
  
  
  mae_difference <-
    
    mean_dynamic_mae -
    
    mean_constant_mae
  
  
  
  # ----------------------------------------------------------
  # How often does each model win?
  # ----------------------------------------------------------
  
  dynamic_win_rate <-
    
    mean(
      
      dynamic_mae_values <
        
        constant_mae_values
      
    )
  
  
  constant_win_rate <-
    
    mean(
      
      constant_mae_values <
        
        dynamic_mae_values
      
    )
  
  
  
  # ----------------------------------------------------------
  # Determine overall winner
  # ----------------------------------------------------------
  
  if (
    mean_dynamic_mae <
    mean_constant_mae
  ) {
    
    winner <- "Dynamic"
    
    
  } else if (
    mean_constant_mae <
    mean_dynamic_mae
  ) {
    
    winner <- "Constant"
    
    
  } else {
    
    winner <- "Tie"
    
  }
  
  
  
  # ==========================================================
  # 16. ADD TO FINAL TABLE
  # ==========================================================
  
  sd_comparison$constant_MAE[
    sd_index
  ] <-
    
    mean_constant_mae
  
  
  sd_comparison$dynamic_MAE[
    sd_index
  ] <-
    
    mean_dynamic_mae
  
  
  sd_comparison$difference[
    sd_index
  ] <-
    
    mae_difference
  
  
  sd_comparison$dynamic_win_rate[
    sd_index
  ] <-
    
    dynamic_win_rate
  
  
  sd_comparison$constant_win_rate[
    sd_index
  ] <-
    
    constant_win_rate
  
  
  sd_comparison$winner[
    sd_index
  ] <-
    
    winner
  
  
  
  # ==========================================================
  # 17. SAVE INDIVIDUAL SIMULATIONS
  # ==========================================================
  
  current_results <- data.frame(
    
    target_rating_SD =
      current_sd,
    
    realized_rating_SD =
      realized_rating_sd,
    
    simulation =
      1:n_simulations_per_sd,
    
    constant_MAE =
      constant_mae_values,
    
    dynamic_MAE =
      dynamic_mae_values,
    
    MAE_difference =
      dynamic_mae_values -
      constant_mae_values,
    
    constant_RMSE =
      constant_rmse_values,
    
    dynamic_RMSE =
      dynamic_rmse_values,
    
    constant_accuracy =
      constant_accuracy_values,
    
    dynamic_accuracy =
      dynamic_accuracy_values,
    
    constant_rating_RMSE =
      constant_rating_rmse_values,
    
    dynamic_rating_RMSE =
      dynamic_rating_rmse_values
    
  )
  
  
  all_simulation_results <- rbind(
    
    all_simulation_results,
    
    current_results
    
  )
  
}



# ============================================================
# 18. FORMAT FINAL TABLE
# ============================================================

sd_comparison$constant_MAE <-
  
  round(
    sd_comparison$constant_MAE,
    4
  )


sd_comparison$dynamic_MAE <-
  
  round(
    sd_comparison$dynamic_MAE,
    4
  )


sd_comparison$difference <-
  
  round(
    sd_comparison$difference,
    4
  )


sd_comparison$dynamic_win_rate <-
  
  round(
    sd_comparison$dynamic_win_rate * 100,
    1
  )


sd_comparison$constant_win_rate <-
  
  round(
    sd_comparison$constant_win_rate * 100,
    1
  )



# ============================================================
# 19. PRINT COMPLETE TABLE
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 5C - LEAGUE PARITY COMPARISON\n"
)

cat(
  "========================================\n\n"
)


print(
  sd_comparison
)



# ============================================================
# 20. EXACT TABLE YOU ASKED FOR
# ============================================================

requested_table <- sd_comparison[
  ,
  c(
    "true_rating_SD",
    "constant_MAE",
    "dynamic_MAE",
    "winner"
  )
]


names(requested_table) <- c(
  
  "True Rating SD",
  
  "Constant MAE",
  
  "Dynamic MAE",
  
  "Winner"
  
)


cat(
  "\n========================================\n"
)

cat(
  "FINAL MAE TABLE\n"
)

cat(
  "========================================\n\n"
)


print(
  requested_table
)



# ============================================================
# 21. FIND APPROXIMATE CROSSOVER
# ============================================================

crossover_model <- lm(
  
  MAE_difference ~
    
    target_rating_SD,
  
  data =
    all_simulation_results
  
)


print(
  summary(
    crossover_model
  )
)



# Solve:
#
# difference = intercept + slope * SD
#
# for difference = 0

intercept <-
  
  coef(
    crossover_model
  )[1]


slope <-
  
  coef(
    crossover_model
  )[2]


estimated_crossover <-
  
  -intercept /
  slope



cat(
  "\nEstimated MAE crossover SD:",
  round(
    estimated_crossover,
    3
  ),
  "\n"
)



# ============================================================
# 22. PLOT CONSTANT VS DYNAMIC MAE
# ============================================================

plot_data <- data.frame(
  
  true_rating_SD = rep(
    sd_comparison$true_rating_SD,
    2
  ),
  
  model = c(
    
    rep(
      "Constant Lambda = 2",
      nrow(sd_comparison)
    ),
    
    rep(
      "Dynamic Lambda",
      nrow(sd_comparison)
    )
    
  ),
  
  MAE = c(
    
    sd_comparison$constant_MAE,
    
    sd_comparison$dynamic_MAE
    
  )
  
)



mae_plot <- ggplot(
  
  plot_data,
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      MAE,
    
    group =
      model,
    
    linetype =
      model
    
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = rating_sd_values
  ) +
  
  labs(
    
    title =
      "Best Lambda Depends on League Parity",
    
    subtitle =
      "1,000 simulated seasons at each hidden rating SD",
    
    x =
      "True Team Rating SD",
    
    y =
      "Average Out-of-Sample MAE",
    
    linetype =
      "Model"
    
  ) +
  
  theme_minimal()


print(
  mae_plot
)



# ============================================================
# 23. PLOT DIFFERENCE
# ============================================================
#
# Positive:
#   Constant lambda wins
#
# Negative:
#   Dynamic lambda wins
#
# ============================================================

difference_plot <- ggplot(
  
  sd_comparison,
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      difference
    
  )
  
) +
  
  geom_hline(
    
    yintercept = 0,
    
    linetype =
      "dashed"
    
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = rating_sd_values
  ) +
  
  labs(
    
    title =
      "Dynamic vs Constant Lambda",
    
    subtitle =
      "Positive values favor constant lambda = 2",
    
    x =
      "True Team Rating SD",
    
    y =
      "Dynamic MAE - Constant MAE"
    
  ) +
  
  theme_minimal()


print(
  difference_plot
)



# ============================================================
# 24. SAVE RESULTS
# ============================================================

write.csv(
  
  sd_comparison,
  
  "version5C_parity_comparison.csv",
  
  row.names =
    FALSE
  
)


write.csv(
  
  requested_table,
  
  "version5C_MAE_table.csv",
  
  row.names =
    FALSE
  
)


write.csv(
  
  all_simulation_results,
  
  "version5C_all_simulations.csv",
  
  row.names =
    FALSE
  
)



# ============================================================
# 25. FINAL SUMMARY
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 5C COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Rating SD values tested:",
  paste(
    rating_sd_values,
    collapse = ", "
  ),
  "\n"
)


cat(
  "Simulations per SD:",
  n_simulations_per_sd,
  "\n"
)


cat(
  "Total simulated seasons:",
  length(rating_sd_values) *
    n_simulations_per_sd,
  "\n"
)


cat(
  "Estimated crossover SD:",
  round(
    estimated_crossover,
    3
  ),
  "\n\n"
)


print(
  requested_table
)
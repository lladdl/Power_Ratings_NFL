# ============================================================
# FOOTBALL POWER RATINGS - VERSION 6B
# FIXED vs ESTIMATED-ADAPTIVE vs ORACLE-ADAPTIVE
# ============================================================
#
# PURPOSE
# -------
#
# Test whether estimating league parity from past games
# actually improves future power-rating predictions.
#
#
# THREE MODELS
# ------------
#
# MODEL A: FIXED BASELINE
#
#   Team lambda = 2
#   Margin cap  = 21
#
#
# MODEL B: ESTIMATED ADAPTIVE
#
#   1. Fit RAW-margin ratings using past games only.
#   2. Calculate SD of those ratings.
#   3. Calibrate observed SD into estimated TRUE rating SD.
#   4. Choose team lambda and margin cap.
#   5. Predict next week.
#
#
# MODEL C: ORACLE ADAPTIVE
#
#   Uses hidden true rating SD to choose lambda/cap.
#
#   This model CANNOT be used in real life.
#
#   It tells us the theoretical value of knowing parity
#   perfectly.
#
#
# IMPORTANT
# ---------
#
# Calibration simulations and evaluation simulations
# are completely separate.
#
# No evaluation season is used to train the parity
# calibration model.
#
# ============================================================



# ============================================================
# 1. PACKAGE
# ============================================================

library(ggplot2)



# ============================================================
# 2. LOAD SCHEDULE
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
# 3. SIMULATION PARAMETERS
# ============================================================

true_hfa <- 2.7

noise_sd <- 8.5

hfa_prior <- 2.5



# ============================================================
# 4. TRUE SD VALUES
# ============================================================

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
# 5. VERSION 6B SETTINGS
# ============================================================

# ------------------------------------------------------------
# Adaptation begins when predicting this week.
#
# Week 5 prediction uses games through Week 4.
# ------------------------------------------------------------

adaptive_start_week <- 5



# ------------------------------------------------------------
# PARITY THRESHOLD FROM VERSION 5C
# ------------------------------------------------------------

lambda_parity_threshold <- 4.5



# ------------------------------------------------------------
# CALIBRATION SAMPLE SIZE
# ------------------------------------------------------------
#
# These seasons teach us how:
#
# observed rating SD -> hidden true SD
#
# They are NOT used for final model evaluation.
# ------------------------------------------------------------

n_calibration_per_sd <- 500



# ------------------------------------------------------------
# EVALUATION SAMPLE SIZE
# ------------------------------------------------------------

n_evaluation_per_sd <- 1000



# ============================================================
# 6. GENERATE TRUE RATINGS WITH EXACT SD
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
  
  
  ratings <-
    ratings -
    mean(ratings)
  
  
  ratings <-
    ratings /
    sd(ratings)
  
  
  ratings <-
    ratings *
    target_sd
  
  
  names(ratings) <-
    league_teams
  
  
  return(
    ratings
  )
  
}



# ============================================================
# 7. SIMULATE ONE SEASON
# ============================================================

simulate_season <- function(
    schedule,
    true_ratings,
    true_hfa = 2.7,
    noise_sd = 8.5
) {
  
  
  sim_games <- schedule
  
  
  
  # ----------------------------------------------------------
  # EXPECTED MARGIN
  # ----------------------------------------------------------
  
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
  
  
  
  # ----------------------------------------------------------
  # ACTUAL MARGIN
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
  # REMOVE TIES
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
        c(-1, 1),
        size = length(zero_games),
        replace = TRUE
      )
    
  }
  
  
  
  return(
    sim_games
  )
  
}



# ============================================================
# 8. HFA LAMBDA
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
    hfa_lambda
  )
  
}



# ============================================================
# 9. DYNAMIC TEAM LAMBDA
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
  
  
  return(
    team_lambda
  )
  
}



# ============================================================
# 10. ADAPTIVE TEAM LAMBDA RULE
# ============================================================
#
# Version 5C:
#
# Low true SD:
#   stronger early shrinkage helped.
#
# High true SD:
#   lambda = 2 helped.
#
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
    
    
    return(
      2
    )
    
  }
  
}



# ============================================================
# 11. ADAPTIVE MARGIN CAP RULE
# ============================================================
#
# Based on Version 5D, but intentionally using broad bands.
#
#
# Estimated SD < 4.5:
#     Cap 14
#
# 4.5 - 5.5:
#     Cap 21
#
# 5.5 - 6.5:
#     Cap 24
#
# 6.5 - 7.5:
#     Cap 35
#
# >= 7.5:
#     Raw margin
#
#
# NOTE:
#
# At SD = 5, Cap 17 technically won Version 5D,
# but Cap 21 was virtually identical.
#
# We retain 21 to avoid overfitting a tiny difference.
# ============================================================

choose_margin_cap <- function(
    parity_sd
) {
  
  
  if (
    parity_sd < 4.5
  ) {
    
    return(
      14
    )
    
    
  } else if (
    parity_sd < 5.5
  ) {
    
    return(
      21
    )
    
    
  } else if (
    parity_sd < 6.5
  ) {
    
    return(
      24
    )
    
    
  } else if (
    parity_sd < 7.5
  ) {
    
    return(
      35
    )
    
    
  } else {
    
    return(
      Inf
    )
    
  }
  
}



# ============================================================
# 12. POWER RATING FUNCTION
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
  
  
  
  # ==========================================================
  # RESPONSE
  # ==========================================================
  
  if (
    is.infinite(margin_cap)
  ) {
    
    
    y <-
      data$actual_margin
    
    
  } else {
    
    
    y <- pmax(
      
      pmin(
        data$actual_margin,
        margin_cap
      ),
      
      -margin_cap
      
    )
    
  }
  
  
  
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
  
  
  X[
    ,
    "HFA"
  ] <- 1
  
  
  
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
  # PENALTY
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
  # PRIOR
  # ==========================================================
  
  prior_target <- c(
    
    hfa_prior,
    
    rep(
      0,
      n_teams
    )
    
  )
  
  
  
  # ==========================================================
  # ESTIMATE COEFFICIENTS
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
  
  
  
  estimated_hfa <-
    
    beta[
      "HFA"
    ]
  
  
  
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
# 13. OBSERVED RAW-MARGIN PARITY SD
# ============================================================
#
# Version 6A showed RAW margin ratings were best
# for estimating league parity.
#
# ============================================================

estimate_observed_parity_sd <- function(
    training_games,
    league_teams,
    hfa_prior = 2.5
) {
  
  
  current_hfa_lambda <- get_hfa_lambda(
    
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
      current_hfa_lambda,
    
    hfa_prior =
      hfa_prior
    
  )
  
  
  
  observed_sd <- sd(
    
    parity_model$ratings$rating
    
  )
  
  
  return(
    observed_sd
  )
  
}



# ============================================================
# 14. BUILD SEPARATE PARITY CALIBRATION DATA
# ============================================================
#
# These simulations are used ONLY to learn:
#
# True SD ~ Observed Raw-Rating SD
#
#
# This prevents calibration leakage into final evaluation.
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



calibration_rows <- list()

calibration_row_index <- 1



set.seed(
  6001
)



for (
  current_true_sd in true_sd_values
) {
  
  
  cat(
    "Calibration - True SD",
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
    
    
    
    sim_games <- simulate_season(
      
      schedule =
        schedule,
      
      true_ratings =
        true_ratings,
      
      true_hfa =
        true_hfa,
      
      noise_sd =
        noise_sd
      
    )
    
    
    
    # --------------------------------------------------------
    # Need calibration through Weeks 2-13.
    #
    # Week 4 calibration will be used to predict Week 5.
    # --------------------------------------------------------
    
    for (
      completed_week in 2:(max_week - 1)
    ) {
      
      
      training_games <- sim_games[
        
        sim_games$week <=
          completed_week,
        
      ]
      
      
      
      observed_sd <- estimate_observed_parity_sd(
        
        training_games =
          training_games,
        
        league_teams =
          teams,
        
        hfa_prior =
          hfa_prior
        
      )
      
      
      
      calibration_rows[[calibration_row_index]] <- data.frame(
        
        true_rating_SD =
          current_true_sd,
        
        simulation =
          s,
        
        week =
          completed_week,
        
        observed_rating_SD =
          observed_sd
        
      )
      
      
      calibration_row_index <-
        calibration_row_index + 1
      
    }
    
  }
  
}



calibration_data <- do.call(
  
  rbind,
  
  calibration_rows
  
)


rownames(calibration_data) <- NULL



# ============================================================
# 15. FIT WEEK-SPECIFIC CALIBRATION EQUATIONS
# ============================================================
#
# For each completed week:
#
# True SD =
#
# intercept + slope * observed raw-rating SD
#
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
    nrow(calibration_coefficients)
  )
) {
  
  
  current_week <-
    
    calibration_coefficients$week[i]
  
  
  
  current_data <- calibration_data[
    
    calibration_data$week ==
      current_week,
    
  ]
  
  
  
  calibration_model <- lm(
    
    true_rating_SD ~
      
      observed_rating_SD,
    
    data =
      current_data
    
  )
  
  
  
  calibration_coefficients$intercept[i] <-
    
    coef(
      calibration_model
    )[1]
  
  
  calibration_coefficients$slope[i] <-
    
    coef(
      calibration_model
    )[2]
  
  
  calibration_coefficients$r_squared[i] <-
    
    summary(
      calibration_model
    )$r.squared
  
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
# 16. CALIBRATE ONE OBSERVED SD
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
  
  
  
  estimated_true_sd <-
    
    row$intercept +
    
    row$slope *
    observed_sd
  
  
  
  # SD cannot be negative.
  
  estimated_true_sd <-
    
    max(
      0,
      estimated_true_sd
    )
  
  
  return(
    estimated_true_sd
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
# 18. EVALUATE THREE MODELS ON ONE SEASON
# ============================================================

evaluate_three_models <- function(
    games,
    true_ratings,
    true_rating_sd,
    league_teams,
    calibration_coefficients,
    adaptive_start_week = 5,
    hfa_prior = 2.5
) {
  
  
  prediction_rows <- list()
  
  diagnostic_rows <- list()
  
  
  prediction_index <- 1
  
  diagnostic_index <- 1
  
  
  
  # ----------------------------------------------------------
  # Store weekly rating-recovery errors
  # ----------------------------------------------------------
  
  fixed_rating_rmse <-
    numeric()
  
  
  estimated_rating_rmse <-
    numeric()
  
  
  oracle_rating_rmse <-
    numeric()
  
  
  
  # ==========================================================
  # WALK-FORWARD
  # ==========================================================
  
  for (
    target_week in 2:max_week
  ) {
    
    
    # --------------------------------------------------------
    # ONLY PRIOR GAMES
    # --------------------------------------------------------
    
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
    
    
    
    # --------------------------------------------------------
    # HFA SHRINKAGE
    # --------------------------------------------------------
    
    current_hfa_lambda <- get_hfa_lambda(
      
      data =
        training_games,
      
      league_teams =
        league_teams
      
    )
    
    
    
    # ========================================================
    # MODEL A:
    # FIXED BASELINE
    # ========================================================
    
    fixed_cap <- 21
    
    fixed_lambda <- 2
    
    
    
    fixed_model <- fit_power_ratings(
      
      data =
        training_games,
      
      league_teams =
        league_teams,
      
      margin_cap =
        fixed_cap,
      
      team_lambda =
        fixed_lambda,
      
      hfa_lambda =
        current_hfa_lambda,
      
      hfa_prior =
        hfa_prior
      
    )
    
    
    
    # ========================================================
    # ESTIMATE CURRENT PARITY
    # ========================================================
    
    observed_parity_sd <-
      NA_real_
    
    
    estimated_parity_sd <-
      NA_real_
    
    
    
    if (
      completed_week >= 2
    ) {
      
      
      observed_parity_sd <-
        
        estimate_observed_parity_sd(
          
          training_games =
            training_games,
          
          league_teams =
            league_teams,
          
          hfa_prior =
            hfa_prior
          
        )
      
      
      
      estimated_parity_sd <-
        
        calibrate_parity_sd(
          
          observed_sd =
            observed_parity_sd,
          
          completed_week =
            completed_week,
          
          calibration_coefficients =
            calibration_coefficients
          
        )
      
    }
    
    
    
    # ========================================================
    # MODEL B:
    # ESTIMATED ADAPTIVE
    # ========================================================
    
    if (
      target_week <
      adaptive_start_week
    ) {
      
      
      # Too early to adapt.
      #
      # Use baseline.
      
      estimated_cap <- 21
      
      estimated_lambda <- 2
      
      
    } else {
      
      
      estimated_cap <-
        
        choose_margin_cap(
          estimated_parity_sd
        )
      
      
      estimated_lambda <-
        
        choose_team_lambda(
          
          parity_sd =
            estimated_parity_sd,
          
          training_games =
            training_games,
          
          league_teams =
            league_teams
          
        )
      
    }
    
    
    
    estimated_model <- fit_power_ratings(
      
      data =
        training_games,
      
      league_teams =
        league_teams,
      
      margin_cap =
        estimated_cap,
      
      team_lambda =
        estimated_lambda,
      
      hfa_lambda =
        current_hfa_lambda,
      
      hfa_prior =
        hfa_prior
      
    )
    
    
    
    # ========================================================
    # MODEL C:
    # ORACLE ADAPTIVE
    # ========================================================
    #
    # Same starting rule as estimated adaptive.
    #
    # It adapts starting Week 5.
    #
    # The ONLY difference is that it knows true parity.
    # ========================================================
    
    if (
      target_week <
      adaptive_start_week
    ) {
      
      
      oracle_cap <- 21
      
      oracle_lambda <- 2
      
      
    } else {
      
      
      oracle_cap <-
        
        choose_margin_cap(
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
    
    
    
    oracle_model <- fit_power_ratings(
      
      data =
        training_games,
      
      league_teams =
        league_teams,
      
      margin_cap =
        oracle_cap,
      
      team_lambda =
        oracle_lambda,
      
      hfa_lambda =
        current_hfa_lambda,
      
      hfa_prior =
        hfa_prior
      
    )
    
    
    
    # ========================================================
    # RATING LOOKUPS
    # ========================================================
    
    fixed_lookup <-
      
      fixed_model$ratings$rating
    
    
    names(fixed_lookup) <-
      
      fixed_model$ratings$team
    
    
    
    estimated_lookup <-
      
      estimated_model$ratings$rating
    
    
    names(estimated_lookup) <-
      
      estimated_model$ratings$team
    
    
    
    oracle_lookup <-
      
      oracle_model$ratings$rating
    
    
    names(oracle_lookup) <-
      
      oracle_model$ratings$team
    
    
    
    # ========================================================
    # RATING RECOVERY
    # ========================================================
    
    fixed_rating_rmse <- c(
      
      fixed_rating_rmse,
      
      sqrt(
        
        mean(
          
          (
            
            fixed_lookup[
              league_teams
            ] -
              
              true_ratings[
                league_teams
              ]
            
          )^2
          
        )
        
      )
      
    )
    
    
    
    estimated_rating_rmse <- c(
      
      estimated_rating_rmse,
      
      sqrt(
        
        mean(
          
          (
            
            estimated_lookup[
              league_teams
            ] -
              
              true_ratings[
                league_teams
              ]
            
          )^2
          
        )
        
      )
      
    )
    
    
    
    oracle_rating_rmse <- c(
      
      oracle_rating_rmse,
      
      sqrt(
        
        mean(
          
          (
            
            oracle_lookup[
              league_teams
            ] -
              
              true_ratings[
                league_teams
              ]
            
          )^2
          
        )
        
      )
      
    )
    
    
    
    # ========================================================
    # GAME PREDICTIONS
    # ========================================================
    
    fixed_prediction <-
      
      fixed_lookup[
        test_games$home_team
      ] -
      
      fixed_lookup[
        test_games$away_team
      ] +
      
      fixed_model$home_field
    
    
    
    estimated_prediction <-
      
      estimated_lookup[
        test_games$home_team
      ] -
      
      estimated_lookup[
        test_games$away_team
      ] +
      
      estimated_model$home_field
    
    
    
    oracle_prediction <-
      
      oracle_lookup[
        test_games$home_team
      ] -
      
      oracle_lookup[
        test_games$away_team
      ] +
      
      oracle_model$home_field
    
    
    
    # ========================================================
    # SAVE GAME-LEVEL PREDICTIONS
    # ========================================================
    
    current_predictions <- data.frame(
      
      week =
        test_games$week,
      
      actual_margin =
        test_games$actual_margin,
      
      fixed_prediction =
        as.numeric(
          fixed_prediction
        ),
      
      estimated_prediction =
        as.numeric(
          estimated_prediction
        ),
      
      oracle_prediction =
        as.numeric(
          oracle_prediction
        )
      
    )
    
    
    
    current_predictions$fixed_error <-
      
      current_predictions$actual_margin -
      
      current_predictions$fixed_prediction
    
    
    current_predictions$estimated_error <-
      
      current_predictions$actual_margin -
      
      current_predictions$estimated_prediction
    
    
    current_predictions$oracle_error <-
      
      current_predictions$actual_margin -
      
      current_predictions$oracle_prediction
    
    
    
    current_predictions$actual_home_win <-
      
      current_predictions$actual_margin > 0
    
    
    current_predictions$fixed_correct <-
      
      (
        current_predictions$fixed_prediction > 0
      ) ==
      
      current_predictions$actual_home_win
    
    
    current_predictions$estimated_correct <-
      
      (
        current_predictions$estimated_prediction > 0
      ) ==
      
      current_predictions$actual_home_win
    
    
    current_predictions$oracle_correct <-
      
      (
        current_predictions$oracle_prediction > 0
      ) ==
      
      current_predictions$actual_home_win
    
    
    
    prediction_rows[[prediction_index]] <- current_predictions
    
    
    prediction_index <-
      prediction_index + 1
    
    
    
    # ========================================================
    # WEEKLY ADAPTIVE DIAGNOSTICS
    # ========================================================
    
    estimated_cap_label <-
      
      if (
        is.infinite(
          estimated_cap
        )
      ) {
        
        "Raw"
        
      } else {
        
        as.character(
          estimated_cap
        )
        
      }
    
    
    
    oracle_cap_label <-
      
      if (
        is.infinite(
          oracle_cap
        )
      ) {
        
        "Raw"
        
      } else {
        
        as.character(
          oracle_cap
        )
        
      }
    
    
    
    diagnostic_rows[[diagnostic_index]] <- data.frame(
      
      target_week =
        target_week,
      
      true_rating_SD =
        true_rating_sd,
      
      observed_parity_SD =
        observed_parity_sd,
      
      estimated_parity_SD =
        estimated_parity_sd,
      
      estimated_cap =
        estimated_cap_label,
      
      oracle_cap =
        oracle_cap_label,
      
      estimated_lambda =
        estimated_lambda,
      
      oracle_lambda =
        oracle_lambda,
      
      cap_correct =
        estimated_cap_label ==
        oracle_cap_label,
      
      lambda_correct =
        estimated_lambda ==
        oracle_lambda
      
    )
    
    
    diagnostic_index <-
      diagnostic_index + 1
    
  }
  
  
  
  # ==========================================================
  # COMBINE
  # ==========================================================
  
  predictions <- do.call(
    
    rbind,
    
    prediction_rows
    
  )
  
  
  diagnostics <- do.call(
    
    rbind,
    
    diagnostic_rows
    
  )
  
  
  
  # ==========================================================
  # MODEL METRICS
  # ==========================================================
  
  fixed_mae <- mean(
    
    abs(
      predictions$fixed_error
    )
    
  )
  
  
  estimated_mae <- mean(
    
    abs(
      predictions$estimated_error
    )
    
  )
  
  
  oracle_mae <- mean(
    
    abs(
      predictions$oracle_error
    )
    
  )
  
  
  
  fixed_rmse <- sqrt(
    
    mean(
      predictions$fixed_error^2
    )
    
  )
  
  
  estimated_rmse <- sqrt(
    
    mean(
      predictions$estimated_error^2
    )
    
  )
  
  
  oracle_rmse <- sqrt(
    
    mean(
      predictions$oracle_error^2
    )
    
  )
  
  
  
  fixed_accuracy <- mean(
    
    predictions$fixed_correct
    
  )
  
  
  estimated_accuracy <- mean(
    
    predictions$estimated_correct
    
  )
  
  
  oracle_accuracy <- mean(
    
    predictions$oracle_correct
    
  )
  
  
  
  fixed_correlation <- safe_cor(
    
    predictions$fixed_prediction,
    
    predictions$actual_margin
    
  )
  
  
  estimated_correlation <- safe_cor(
    
    predictions$estimated_prediction,
    
    predictions$actual_margin
    
  )
  
  
  oracle_correlation <- safe_cor(
    
    predictions$oracle_prediction,
    
    predictions$actual_margin
    
  )
  
  
  
  fixed_bias <- mean(
    
    predictions$fixed_error
    
  )
  
  
  estimated_bias <- mean(
    
    predictions$estimated_error
    
  )
  
  
  oracle_bias <- mean(
    
    predictions$oracle_error
    
  )
  
  
  
  # ==========================================================
  # RETURN
  # ==========================================================
  
  return(
    
    list(
      
      metrics = data.frame(
        
        fixed_MAE =
          fixed_mae,
        
        estimated_MAE =
          estimated_mae,
        
        oracle_MAE =
          oracle_mae,
        
        fixed_RMSE =
          fixed_rmse,
        
        estimated_RMSE =
          estimated_rmse,
        
        oracle_RMSE =
          oracle_rmse,
        
        fixed_accuracy =
          fixed_accuracy,
        
        estimated_accuracy =
          estimated_accuracy,
        
        oracle_accuracy =
          oracle_accuracy,
        
        fixed_correlation =
          fixed_correlation,
        
        estimated_correlation =
          estimated_correlation,
        
        oracle_correlation =
          oracle_correlation,
        
        fixed_bias =
          fixed_bias,
        
        estimated_bias =
          estimated_bias,
        
        oracle_bias =
          oracle_bias,
        
        fixed_rating_RMSE =
          mean(
            fixed_rating_rmse
          ),
        
        estimated_rating_RMSE =
          mean(
            estimated_rating_rmse
          ),
        
        oracle_rating_RMSE =
          mean(
            oracle_rating_rmse
          )
        
      ),
      
      diagnostics =
        diagnostics
      
    )
    
  )
  
}



# ============================================================
# 19. FINAL EVALUATION MONTE CARLO
# ============================================================
#
# DIFFERENT RANDOM SEED FROM CALIBRATION.
#
# These seasons were never used to fit the parity calibration.
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "STARTING VERSION 6B EVALUATION\n"
)

cat(
  "========================================\n\n"
)



set.seed(
  6002
)



season_result_list <-
  list()


diagnostic_result_list <-
  list()


season_result_index <- 1

diagnostic_result_index <- 1



for (
  current_true_sd in true_sd_values
) {
  
  
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
  
  
  
  for (
    s in 1:n_evaluation_per_sd
  ) {
    
    
    # ========================================================
    # HIDDEN TRUE RATINGS
    # ========================================================
    
    season_true_ratings <- generate_true_ratings(
      
      league_teams =
        teams,
      
      target_sd =
        current_true_sd
      
    )
    
    
    
    # ========================================================
    # SIMULATE SEASON
    # ========================================================
    
    sim_games <- simulate_season(
      
      schedule =
        schedule,
      
      true_ratings =
        season_true_ratings,
      
      true_hfa =
        true_hfa,
      
      noise_sd =
        noise_sd
      
    )
    
    
    
    # ========================================================
    # TEST THREE SYSTEMS
    # ========================================================
    
    result <- evaluate_three_models(
      
      games =
        sim_games,
      
      true_ratings =
        season_true_ratings,
      
      true_rating_sd =
        current_true_sd,
      
      league_teams =
        teams,
      
      calibration_coefficients =
        calibration_coefficients,
      
      adaptive_start_week =
        adaptive_start_week,
      
      hfa_prior =
        hfa_prior
      
    )
    
    
    
    # ========================================================
    # SAVE SEASON METRICS
    # ========================================================
    
    current_metrics <-
      
      result$metrics
    
    
    current_metrics$true_rating_SD <-
      
      current_true_sd
    
    
    current_metrics$simulation <-
      
      s
    
    
    
    season_result_list[[season_result_index]] <- current_metrics
    
    
    season_result_index <-
      season_result_index + 1
    
    
    
    # ========================================================
    # SAVE WEEKLY DIAGNOSTICS
    # ========================================================
    
    current_diagnostics <-
      
      result$diagnostics
    
    
    current_diagnostics$simulation <-
      
      s
    
    
    
    diagnostic_result_list[[diagnostic_result_index]] <- current_diagnostics
    
    
    diagnostic_result_index <-
      diagnostic_result_index + 1
    
    
    
    # ========================================================
    # PROGRESS
    # ========================================================
    
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
  
  season_result_list
  
)


rownames(season_results) <- NULL



adaptive_diagnostics <- do.call(
  
  rbind,
  
  diagnostic_result_list
  
)


rownames(adaptive_diagnostics) <- NULL



# ============================================================
# 20. MODEL DIFFERENCES
# ============================================================

season_results$estimated_vs_fixed_MAE <-
  
  season_results$estimated_MAE -
  
  season_results$fixed_MAE


season_results$oracle_vs_fixed_MAE <-
  
  season_results$oracle_MAE -
  
  season_results$fixed_MAE


season_results$estimated_vs_oracle_MAE <-
  
  season_results$estimated_MAE -
  
  season_results$oracle_MAE



# ============================================================
# 21. SUMMARY BY TRUE SD
# ============================================================

summary_by_sd_list <- lapply(
  
  split(
    season_results,
    season_results$true_rating_SD
  ),
  
  function(x) {
    
    
    data.frame(
      
      true_rating_SD =
        unique(
          x$true_rating_SD
        ),
      
      fixed_MAE =
        mean(
          x$fixed_MAE
        ),
      
      estimated_MAE =
        mean(
          x$estimated_MAE
        ),
      
      oracle_MAE =
        mean(
          x$oracle_MAE
        ),
      
      fixed_RMSE =
        mean(
          x$fixed_RMSE
        ),
      
      estimated_RMSE =
        mean(
          x$estimated_RMSE
        ),
      
      oracle_RMSE =
        mean(
          x$oracle_RMSE
        ),
      
      fixed_accuracy =
        mean(
          x$fixed_accuracy
        ),
      
      estimated_accuracy =
        mean(
          x$estimated_accuracy
        ),
      
      oracle_accuracy =
        mean(
          x$oracle_accuracy
        ),
      
      fixed_rating_RMSE =
        mean(
          x$fixed_rating_RMSE
        ),
      
      estimated_rating_RMSE =
        mean(
          x$estimated_rating_RMSE
        ),
      
      oracle_rating_RMSE =
        mean(
          x$oracle_rating_RMSE
        ),
      
      estimated_MAE_improvement =
        mean(
          x$fixed_MAE -
            x$estimated_MAE
        ),
      
      oracle_MAE_improvement =
        mean(
          x$fixed_MAE -
            x$oracle_MAE
        ),
      
      estimated_beats_fixed =
        mean(
          x$estimated_MAE <
            x$fixed_MAE
        ),
      
      oracle_beats_fixed =
        mean(
          x$oracle_MAE <
            x$fixed_MAE
        )
      
    )
    
  }
  
)



summary_by_sd <- do.call(
  
  rbind,
  
  summary_by_sd_list
  
)


rownames(summary_by_sd) <- NULL



cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 6B RESULTS BY TRUE SD\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_by_sd
)



# ============================================================
# 22. OVERALL SUMMARY
# ============================================================

overall_summary <- data.frame(
  
  model = c(
    "Fixed",
    "Estimated Adaptive",
    "Oracle Adaptive"
  ),
  
  MAE = c(
    
    mean(
      season_results$fixed_MAE
    ),
    
    mean(
      season_results$estimated_MAE
    ),
    
    mean(
      season_results$oracle_MAE
    )
    
  ),
  
  RMSE = c(
    
    mean(
      season_results$fixed_RMSE
    ),
    
    mean(
      season_results$estimated_RMSE
    ),
    
    mean(
      season_results$oracle_RMSE
    )
    
  ),
  
  Winner_Accuracy = c(
    
    mean(
      season_results$fixed_accuracy
    ),
    
    mean(
      season_results$estimated_accuracy
    ),
    
    mean(
      season_results$oracle_accuracy
    )
    
  ),
  
  Correlation = c(
    
    mean(
      season_results$fixed_correlation,
      na.rm = TRUE
    ),
    
    mean(
      season_results$estimated_correlation,
      na.rm = TRUE
    ),
    
    mean(
      season_results$oracle_correlation,
      na.rm = TRUE
    )
    
  ),
  
  Rating_RMSE = c(
    
    mean(
      season_results$fixed_rating_RMSE
    ),
    
    mean(
      season_results$estimated_rating_RMSE
    ),
    
    mean(
      season_results$oracle_rating_RMSE
    )
    
  )
  
)



cat(
  "\n========================================\n"
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
# 23. PAIRED MAE COMPARISONS
# ============================================================

estimated_difference <-
  
  season_results$estimated_MAE -
  
  season_results$fixed_MAE


oracle_difference <-
  
  season_results$oracle_MAE -
  
  season_results$fixed_MAE



estimated_se <-
  
  sd(
    estimated_difference
  ) /
  
  sqrt(
    length(
      estimated_difference
    )
  )


oracle_se <-
  
  sd(
    oracle_difference
  ) /
  
  sqrt(
    length(
      oracle_difference
    )
  )



cat(
  "\n========================================\n"
)

cat(
  "PAIRED MAE COMPARISONS\n"
)

cat(
  "========================================\n\n"
)



cat(
  "Estimated Adaptive - Fixed:\n"
)


cat(
  "Mean difference:",
  round(
    mean(
      estimated_difference
    ),
    4
  ),
  "\n"
)


cat(
  "95% Monte Carlo interval:",
  round(
    mean(
      estimated_difference
    ) -
      1.96 *
      estimated_se,
    4
  ),
  "to",
  round(
    mean(
      estimated_difference
    ) +
      1.96 *
      estimated_se,
    4
  ),
  "\n\n"
)



cat(
  "Oracle Adaptive - Fixed:\n"
)


cat(
  "Mean difference:",
  round(
    mean(
      oracle_difference
    ),
    4
  ),
  "\n"
)


cat(
  "95% Monte Carlo interval:",
  round(
    mean(
      oracle_difference
    ) -
      1.96 *
      oracle_se,
    4
  ),
  "to",
  round(
    mean(
      oracle_difference
    ) +
      1.96 *
      oracle_se,
    4
  ),
  "\n"
)



# ============================================================
# 24. HOW OFTEN EACH MODEL WINS
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "MODEL WIN RATES\n"
)

cat(
  "========================================\n\n"
)



cat(
  "Estimated adaptive beats fixed:",
  round(
    mean(
      season_results$estimated_MAE <
        season_results$fixed_MAE
    ) * 100,
    1
  ),
  "%\n"
)


cat(
  "Oracle adaptive beats fixed:",
  round(
    mean(
      season_results$oracle_MAE <
        season_results$fixed_MAE
    ) * 100,
    1
  ),
  "%\n"
)


cat(
  "Estimated adaptive beats oracle:",
  round(
    mean(
      season_results$estimated_MAE <
        season_results$oracle_MAE
    ) * 100,
    1
  ),
  "%\n"
)



# ============================================================
# 25. HOW MUCH OF ORACLE IMPROVEMENT IS CAPTURED?
# ============================================================

fixed_mean <-
  
  mean(
    season_results$fixed_MAE
  )


estimated_mean <-
  
  mean(
    season_results$estimated_MAE
  )


oracle_mean <-
  
  mean(
    season_results$oracle_MAE
  )



oracle_gain <-
  
  fixed_mean -
  oracle_mean


estimated_gain <-
  
  fixed_mean -
  estimated_mean



if (
  oracle_gain > 0
) {
  
  
  percent_oracle_gain_captured <-
    
    100 *
    estimated_gain /
    oracle_gain
  
  
} else {
  
  
  percent_oracle_gain_captured <-
    NA_real_
  
}



cat(
  "\n========================================\n"
)

cat(
  "ORACLE GAP\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Fixed MAE:",
  round(
    fixed_mean,
    4
  ),
  "\n"
)


cat(
  "Estimated Adaptive MAE:",
  round(
    estimated_mean,
    4
  ),
  "\n"
)


cat(
  "Oracle Adaptive MAE:",
  round(
    oracle_mean,
    4
  ),
  "\n\n"
)


cat(
  "Oracle improvement over fixed:",
  round(
    oracle_gain,
    4
  ),
  "\n"
)


cat(
  "Estimated improvement over fixed:",
  round(
    estimated_gain,
    4
  ),
  "\n"
)


cat(
  "Percent of oracle gain captured:",
  round(
    percent_oracle_gain_captured,
    1
  ),
  "%\n"
)



# ============================================================
# 26. ADAPTIVE DECISION ACCURACY
# ============================================================
#
# Only evaluate weeks where adaptation is actually active.
# ============================================================

active_diagnostics <- adaptive_diagnostics[
  
  adaptive_diagnostics$target_week >=
    adaptive_start_week,
  
]



adaptive_decision_summary <- data.frame(
  
  parity_MAE = mean(
    
    abs(
      
      active_diagnostics$estimated_parity_SD -
        
        active_diagnostics$true_rating_SD
      
    ),
    
    na.rm = TRUE
    
  ),
  
  cap_selection_accuracy = mean(
    
    active_diagnostics$cap_correct,
    
    na.rm = TRUE
    
  ),
  
  lambda_selection_accuracy = mean(
    
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
  adaptive_decision_summary
)



# ============================================================
# 27. ADAPTIVE DECISION ACCURACY BY WEEK
# ============================================================

decision_by_week_list <- lapply(
  
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
          abs(
            x$estimated_parity_SD -
              x$true_rating_SD
          ),
          na.rm = TRUE
        ),
      
      cap_accuracy =
        mean(
          x$cap_correct,
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
  
  decision_by_week_list
  
)


rownames(decision_by_week) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "ADAPTIVE DECISIONS BY WEEK\n"
)

cat(
  "========================================\n\n"
)


print(
  decision_by_week
)



# ============================================================
# 28. PLOT MAE BY TRUE SD
# ============================================================

mae_plot_data <- data.frame(
  
  true_rating_SD = rep(
    summary_by_sd$true_rating_SD,
    3
  ),
  
  model = c(
    
    rep(
      "Fixed",
      nrow(summary_by_sd)
    ),
    
    rep(
      "Estimated Adaptive",
      nrow(summary_by_sd)
    ),
    
    rep(
      "Oracle Adaptive",
      nrow(summary_by_sd)
    )
    
  ),
  
  MAE = c(
    
    summary_by_sd$fixed_MAE,
    
    summary_by_sd$estimated_MAE,
    
    summary_by_sd$oracle_MAE
    
  )
  
)



mae_plot <- ggplot(
  
  mae_plot_data,
  
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
    breaks =
      true_sd_values
  ) +
  
  labs(
    
    title =
      "Version 6B: Fixed vs Adaptive Power Ratings",
    
    subtitle =
      "Estimated adaptive model uses only prior-game information",
    
    x =
      "Hidden True Team Rating SD",
    
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
# 29. PLOT IMPROVEMENT OVER FIXED
# ============================================================

improvement_plot_data <- data.frame(
  
  true_rating_SD = rep(
    summary_by_sd$true_rating_SD,
    2
  ),
  
  model = c(
    
    rep(
      "Estimated Adaptive",
      nrow(summary_by_sd)
    ),
    
    rep(
      "Oracle Adaptive",
      nrow(summary_by_sd)
    )
    
  ),
  
  improvement = c(
    
    summary_by_sd$estimated_MAE_improvement,
    
    summary_by_sd$oracle_MAE_improvement
    
  )
  
)



improvement_plot <- ggplot(
  
  improvement_plot_data,
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      improvement,
    
    group =
      model,
    
    linetype =
      model
    
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
      "Improvement Over Fixed Model",
    
    subtitle =
      "Positive values mean adaptive model has lower MAE",
    
    x =
      "Hidden True Team Rating SD",
    
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
# 30. PLOT PARITY ESTIMATION ERROR BY WEEK
# ============================================================

parity_week_plot <- ggplot(
  
  decision_by_week,
  
  aes(
    
    x =
      target_week,
    
    y =
      parity_MAE
    
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
      "Parity Estimation Error Used for Predictions",
    
    x =
      "Week Being Predicted",
    
    y =
      "MAE of Estimated True Rating SD"
    
  ) +
  
  theme_minimal()


print(
  parity_week_plot
)



# ============================================================
# 31. PLOT CAP SELECTION ACCURACY
# ============================================================

cap_accuracy_plot <- ggplot(
  
  decision_by_week,
  
  aes(
    
    x =
      target_week,
    
    y =
      cap_accuracy * 100
    
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
  
  scale_y_continuous(
    limits = c(
      0,
      100
    )
  ) +
  
  labs(
    
    title =
      "Correct Adaptive Margin-Cap Selection",
    
    subtitle =
      "Compared with oracle model using hidden true parity",
    
    x =
      "Week Being Predicted",
    
    y =
      "Correct Cap Selection (%)"
    
  ) +
  
  theme_minimal()


print(
  cap_accuracy_plot
)



# ============================================================
# 32. SAVE RESULTS
# ============================================================

write.csv(
  
  calibration_data,
  
  "version6B_parity_calibration_data.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  calibration_coefficients,
  
  "version6B_parity_calibration_coefficients.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  season_results,
  
  "version6B_season_results.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  adaptive_diagnostics,
  
  "version6B_adaptive_diagnostics.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  summary_by_sd,
  
  "version6B_results_by_true_SD.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  overall_summary,
  
  "version6B_overall_model_comparison.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  decision_by_week,
  
  "version6B_adaptive_decisions_by_week.csv",
  
  row.names = FALSE
  
)



# ============================================================
# 33. FINAL SUMMARY
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 6B COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Calibration seasons per SD:",
  n_calibration_per_sd,
  "\n"
)


cat(
  "Evaluation seasons per SD:",
  n_evaluation_per_sd,
  "\n"
)


cat(
  "Evaluation seasons total:",
  n_evaluation_per_sd *
    length(true_sd_values),
  "\n"
)


cat(
  "Adaptive model begins predicting with adaptive settings in Week:",
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
  "\nADAPTIVE DECISION QUALITY:\n\n"
)


print(
  adaptive_decision_summary
)


# ============================================================
# END VERSION 6B
# ============================================================
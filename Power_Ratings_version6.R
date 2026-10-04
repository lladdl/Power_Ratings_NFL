# ============================================================
# FOOTBALL POWER RATINGS - VERSION 5D
# MARGIN CAP x LEAGUE PARITY MONTE CARLO
# ============================================================
#
# PURPOSE:
#
# Test whether the optimal margin cap changes depending on
# the true spread of team strength.
#
#
# TRUE RATING SD VALUES:
#
#   2, 3, 4, 5, 6, 7, 8
#
#
# MARGIN CAPS:
#
#   14
#   17
#   21
#   24
#   28
#   35
#   Raw / No Cap
#
#
# IMPORTANT:
#
# Team lambda = 2 throughout the season.
#
# This isolates the MARGIN CAP question.
#
#
# HFA:
#
# Prior = 2.5
#
# HFA lambda:
#
#   1-2 games = 10
#   3-4 games = 8
#   5-6 games = 5
#   7+ games  = 2
#
#
# Each simulated season:
#
# 1. Generate hidden team ratings with exact requested SD.
# 2. Simulate one complete season.
# 3. Evaluate EVERY margin cap on that SAME season.
# 4. Use walk-forward prediction.
#
# ============================================================



# ============================================================
# 1. PACKAGE
# ============================================================

library(ggplot2)



# ============================================================
# 2. LOAD ORIGINAL SCHEDULE
# ============================================================

base_games <- read.csv(
  "football_power_ratings_games.csv",
  stringsAsFactors = FALSE
)


# We use ONLY the schedule.
# Original scores are ignored.

schedule <- base_games[
  ,
  c(
    "game_id",
    "week",
    "home_team",
    "away_team"
  )
]



# ============================================================
# 3. TEAMS
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
# 4. SIMULATION SETTINGS
# ============================================================

# True home-field advantage

true_hfa <- 2.7


# Game-level random noise

noise_sd <- 8.5


# HFA prior used by rating model

hfa_prior <- 2.5


# Team lambda remains fixed

team_lambda <- 2



# ============================================================
# 5. PARITY LEVELS TO TEST
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



# ============================================================
# 6. MARGIN CAPS TO TEST
# ============================================================
#
# Inf means raw margin / no cap.
# ============================================================

margin_cap_values <- c(
  14,
  17,
  21,
  24,
  28,
  35,
  Inf
)


margin_cap_labels <- c(
  "14",
  "17",
  "21",
  "24",
  "28",
  "35",
  "Raw"
)



# ============================================================
# 7. MONTE CARLO SETTINGS
# ============================================================

n_simulations_per_sd <- 1000


# Set seed ONCE

set.seed(777)



# ============================================================
# 8. GENERATE TRUE RATINGS WITH EXACT SD
# ============================================================
#
# Unlike Version 5C:
#
# If target SD = 4, the generated league has exactly SD = 4.
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
  
  
  # Center at zero
  
  ratings <-
    ratings -
    mean(ratings)
  
  
  # Standardize to SD = 1
  
  ratings <-
    ratings /
    sd(ratings)
  
  
  # Rescale to requested exact SD
  
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
# 9. SIMULATE ONE SEASON
# ============================================================

simulate_season <- function(
    schedule,
    true_ratings,
    true_hfa = 2.7,
    noise_sd = 8.5
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
  
  
  
  sim_games$true_expected_margin <-
    
    as.numeric(
      sim_games$true_expected_margin
    )
  
  
  
  # ----------------------------------------------------------
  # ACTUAL GAME RESULT
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
# 10. HFA SHRINKAGE
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
# 11. POWER RATING FUNCTION
# ============================================================

fit_power_ratings <- function(
    data,
    league_teams,
    margin_cap,
    team_lambda = 2,
    hfa_lambda,
    hfa_prior = 2.5
) {
  
  
  n_games <-
    nrow(data)
  
  
  n_teams <-
    length(league_teams)
  
  
  
  # ==========================================================
  # RESPONSE VARIABLE
  # ==========================================================
  #
  # If margin_cap = Inf:
  #
  # use raw margin.
  #
  # Otherwise cap at +/- margin_cap.
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
  # HFA
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
  
  
  # Center ratings at zero
  
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
# 12. EVALUATE ONE CAP ON ONE SEASON
# ============================================================

evaluate_season <- function(
    games,
    true_ratings,
    true_hfa,
    league_teams,
    margin_cap,
    team_lambda = 2,
    hfa_prior = 2.5
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
    # TEST DATA
    # --------------------------------------------------------
    
    test_games <- games[
      
      games$week ==
        target_week,
      
    ]
    
    
    
    # --------------------------------------------------------
    # HFA SHRINKAGE
    # --------------------------------------------------------
    
    current_hfa_lambda <- get_hfa_lambda(
      
      data =
        training_games,
      
      league_teams =
        league_teams
      
    )
    
    
    
    # --------------------------------------------------------
    # FIT MODEL
    # --------------------------------------------------------
    
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
        current_hfa_lambda,
      
      hfa_prior =
        hfa_prior
      
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
    # HFA ERROR
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
    # PREDICT TEST WEEK
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
    # RAW MARGIN ERROR
    # ========================================================
    #
    # IMPORTANT:
    #
    # Every cap is evaluated against the SAME raw margin.
    #
    # ========================================================
    
    test_games$error_raw <-
      
      test_games$actual_margin -
      
      test_games$predicted_margin
    
    
    
    # --------------------------------------------------------
    # WINNER
    # --------------------------------------------------------
    
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
  # METRICS
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
  
  
  rating_rmse <- mean(
    
    weekly_rating_rmse
    
  )
  
  
  hfa_error <- mean(
    
    weekly_hfa_error
    
  )
  
  
  
  return(
    
    list(
      
      raw_MAE =
        raw_mae,
      
      raw_RMSE =
        raw_rmse,
      
      correlation =
        prediction_correlation,
      
      accuracy =
        winner_accuracy,
      
      bias =
        prediction_bias,
      
      rating_RMSE =
        rating_rmse,
      
      HFA_error =
        hfa_error
      
    )
    
  )
  
}



# ============================================================
# 13. PREALLOCATE ALL RESULTS
# ============================================================

total_rows <-
  
  length(rating_sd_values) *
  
  n_simulations_per_sd *
  
  length(margin_cap_values)



simulation_results <- data.frame(
  
  true_rating_SD =
    rep(
      NA_real_,
      total_rows
    ),
  
  simulation =
    rep(
      NA_integer_,
      total_rows
    ),
  
  cap_label =
    rep(
      NA_character_,
      total_rows
    ),
  
  cap_value =
    rep(
      NA_real_,
      total_rows
    ),
  
  raw_MAE =
    rep(
      NA_real_,
      total_rows
    ),
  
  raw_RMSE =
    rep(
      NA_real_,
      total_rows
    ),
  
  correlation =
    rep(
      NA_real_,
      total_rows
    ),
  
  accuracy =
    rep(
      NA_real_,
      total_rows
    ),
  
  bias =
    rep(
      NA_real_,
      total_rows
    ),
  
  rating_RMSE =
    rep(
      NA_real_,
      total_rows
    ),
  
  HFA_error =
    rep(
      NA_real_,
      total_rows
    )
  
)



row_index <- 1



# ============================================================
# 14. MONTE CARLO GRID
# ============================================================

for (
  current_sd in rating_sd_values
) {
  
  
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
  
  
  
  for (
    s in 1:n_simulations_per_sd
  ) {
    
    
    # ========================================================
    # GENERATE NEW HIDDEN LEAGUE
    # ========================================================
    
    season_true_ratings <- generate_true_ratings(
      
      league_teams =
        teams,
      
      target_sd =
        current_sd
      
    )
    
    
    
    # Check:
    #
    # sd(season_true_ratings)
    #
    # should equal current_sd
    
    
    
    # ========================================================
    # GENERATE ONE SEASON
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
    # TEST EVERY MARGIN CAP
    # ========================================================
    #
    # IMPORTANT:
    #
    # Every cap gets the SAME season.
    #
    # ========================================================
    
    for (
      cap_index in seq_along(margin_cap_values)
    ) {
      
      
      current_cap <-
        
        margin_cap_values[
          cap_index
        ]
      
      
      current_label <-
        
        margin_cap_labels[
          cap_index
        ]
      
      
      
      cap_result <- evaluate_season(
        
        games =
          sim_games,
        
        true_ratings =
          season_true_ratings,
        
        true_hfa =
          true_hfa,
        
        league_teams =
          teams,
        
        margin_cap =
          current_cap,
        
        team_lambda =
          2,
        
        hfa_prior =
          hfa_prior
        
      )
      
      
      
      # ------------------------------------------------------
      # SAVE RESULT
      # ------------------------------------------------------
      
      simulation_results$true_rating_SD[
        row_index
      ] <-
        
        current_sd
      
      
      simulation_results$simulation[
        row_index
      ] <-
        
        s
      
      
      simulation_results$cap_label[
        row_index
      ] <-
        
        current_label
      
      
      simulation_results$cap_value[
        row_index
      ] <-
        
        current_cap
      
      
      simulation_results$raw_MAE[
        row_index
      ] <-
        
        cap_result$raw_MAE
      
      
      simulation_results$raw_RMSE[
        row_index
      ] <-
        
        cap_result$raw_RMSE
      
      
      simulation_results$correlation[
        row_index
      ] <-
        
        cap_result$correlation
      
      
      simulation_results$accuracy[
        row_index
      ] <-
        
        cap_result$accuracy
      
      
      simulation_results$bias[
        row_index
      ] <-
        
        cap_result$bias
      
      
      simulation_results$rating_RMSE[
        row_index
      ] <-
        
        cap_result$rating_RMSE
      
      
      simulation_results$HFA_error[
        row_index
      ] <-
        
        cap_result$HFA_error
      
      
      
      row_index <-
        row_index + 1
      
    }
    
    
    
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
  
}



# ============================================================
# 15. CONVERT CAP LABEL TO ORDERED FACTOR
# ============================================================

simulation_results$cap_label <- factor(
  
  simulation_results$cap_label,
  
  levels =
    margin_cap_labels
  
)



# ============================================================
# 16. SUMMARY BY SD AND CAP
# ============================================================

summary_long <- aggregate(
  
  cbind(
    raw_MAE,
    raw_RMSE,
    correlation,
    accuracy,
    bias,
    rating_RMSE,
    HFA_error
  ) ~
    
    true_rating_SD +
    cap_label,
  
  data =
    simulation_results,
  
  FUN =
    mean
  
)



# Sort correctly

summary_long <- summary_long[
  
  order(
    summary_long$true_rating_SD,
    summary_long$cap_label
  ),
  
]


rownames(summary_long) <- NULL



cat(
  "\n\n========================================\n"
)

cat(
  "FULL SD x MARGIN CAP RESULTS\n"
)

cat(
  "========================================\n\n"
)


print(
  summary_long
)



# ============================================================
# 17. FIND BEST CAP AT EACH SD
# ============================================================

best_cap_list <- lapply(
  
  split(
    summary_long,
    summary_long$true_rating_SD
  ),
  
  function(x) {
    
    x[
      which.min(
        x$raw_MAE
      ),
    ]
    
  }
  
)



best_caps <- do.call(
  
  rbind,
  
  best_cap_list
  
)


rownames(best_caps) <- NULL



best_cap_table <- best_caps[
  ,
  c(
    "true_rating_SD",
    "cap_label",
    "raw_MAE",
    "raw_RMSE",
    "accuracy",
    "rating_RMSE"
  )
]


names(best_cap_table) <- c(
  
  "True Rating SD",
  
  "Best Cap",
  
  "Best MAE",
  
  "RMSE",
  
  "Winner Accuracy",
  
  "Rating RMSE"
  
)



cat(
  "\n========================================\n"
)

cat(
  "BEST MARGIN CAP BY TRUE RATING SD\n"
)

cat(
  "========================================\n\n"
)


print(
  best_cap_table
)



# ============================================================
# 18. CREATE THE MAIN MAE TABLE
# ============================================================

mae_table <- data.frame(
  
  True_Rating_SD =
    rating_sd_values
  
)



for (
  current_label in margin_cap_labels
) {
  
  
  temp <- summary_long[
    
    as.character(
      summary_long$cap_label
    ) ==
      current_label,
    
    c(
      "true_rating_SD",
      "raw_MAE"
    )
    
  ]
  
  
  new_column_name <-
    
    if (
      current_label == "Raw"
    ) {
      
      "Raw"
      
    } else {
      
      paste0(
        "Cap_",
        current_label
      )
      
    }
  
  
  mae_table[[new_column_name]] <-
    temp$raw_MAE[
      match(
        rating_sd_values,
        temp$true_rating_SD
      )
    ]
  
}



# Add best cap and best MAE

mae_table$Best_Cap <-
  
  as.character(
    best_cap_table$`Best Cap`
  )


mae_table$Best_MAE <-
  
  best_cap_table$`Best MAE`



# Round numeric columns

numeric_columns <-
  
  sapply(
    mae_table,
    is.numeric
  )


mae_table[
  numeric_columns
] <-
  
  lapply(
    
    mae_table[
      numeric_columns
    ],
    
    round,
    
    digits = 4
    
  )



cat(
  "\n========================================\n"
)

cat(
  "FINAL MARGIN CAP TABLE\n"
)

cat(
  "========================================\n\n"
)


print(
  mae_table
)



# ============================================================
# 19. COMPARE EACH CAP TO CAP 21
# ============================================================
#
# Negative:
#   alternative cap beats 21
#
# Positive:
#   cap 21 beats alternative
#
# ============================================================

cap21_results <- summary_long[
  
  as.character(
    summary_long$cap_label
  ) == "21",
  
]


cap21_lookup <-
  
  cap21_results$raw_MAE


names(cap21_lookup) <-
  
  cap21_results$true_rating_SD



summary_long$MAE_vs_cap21 <-
  
  summary_long$raw_MAE -
  
  cap21_lookup[
    as.character(
      summary_long$true_rating_SD
    )
  ]



# ============================================================
# 20. HOW OFTEN DOES EACH CAP WIN WITHIN A SEASON?
# ============================================================
#
# For each SD and simulated season:
#
# Which cap had the lowest MAE?
#
# ============================================================

season_groups <- split(
  
  simulation_results,
  
  list(
    simulation_results$true_rating_SD,
    simulation_results$simulation
  ),
  
  drop = TRUE
  
)



season_winner_list <- lapply(
  
  season_groups,
  
  function(x) {
    
    
    best_row <- x[
      
      which.min(
        x$raw_MAE
      ),
      
    ]
    
    
    data.frame(
      
      true_rating_SD =
        best_row$true_rating_SD,
      
      simulation =
        best_row$simulation,
      
      winning_cap =
        as.character(
          best_row$cap_label
        )
      
    )
    
  }
  
)



season_winners <- do.call(
  
  rbind,
  
  season_winner_list
  
)


rownames(season_winners) <- NULL



win_rate_table <- as.data.frame(
  
  prop.table(
    
    table(
      
      season_winners$true_rating_SD,
      
      season_winners$winning_cap
      
    ),
    
    margin = 1
    
  )
  
)


names(win_rate_table) <- c(
  
  "True_Rating_SD",
  
  "Margin_Cap",
  
  "Win_Rate"
  
)


win_rate_table$Win_Rate <-
  
  win_rate_table$Win_Rate *
  100



cat(
  "\n========================================\n"
)

cat(
  "MARGIN CAP WIN RATES\n"
)

cat(
  "========================================\n\n"
)


print(
  win_rate_table
)



# ============================================================
# 21. PLOT AVERAGE MAE BY SD AND CAP
# ============================================================

mae_plot <- ggplot(
  
  summary_long,
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      raw_MAE,
    
    group =
      cap_label,
    
    linetype =
      cap_label
    
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  scale_x_continuous(
    breaks =
      rating_sd_values
  ) +
  
  labs(
    
    title =
      "Optimal Margin Cap by League Parity",
    
    subtitle =
      "1,000 simulated seasons per hidden team-strength SD",
    
    x =
      "True Team Rating SD",
    
    y =
      "Out-of-Sample Raw MAE",
    
    linetype =
      "Margin Cap"
    
  ) +
  
  theme_minimal()


print(
  mae_plot
)



# ============================================================
# 22. PLOT DIFFERENCE FROM CAP 21
# ============================================================

difference_plot <- ggplot(
  
  summary_long[
    
    as.character(
      summary_long$cap_label
    ) != "21",
    
  ],
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      MAE_vs_cap21,
    
    group =
      cap_label,
    
    linetype =
      cap_label
    
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
    size = 2.5
  ) +
  
  scale_x_continuous(
    breaks =
      rating_sd_values
  ) +
  
  labs(
    
    title =
      "Alternative Margin Caps vs Cap 21",
    
    subtitle =
      "Negative values mean the alternative cap beats 21",
    
    x =
      "True Team Rating SD",
    
    y =
      "MAE Difference vs Cap 21",
    
    linetype =
      "Margin Cap"
    
  ) +
  
  theme_minimal()


print(
  difference_plot
)



# ============================================================
# 23. PLOT BEST CAP BY SD
# ============================================================
#
# Raw cannot be placed naturally on a numeric cap scale,
# so this is displayed as a categorical outcome.
#
# ============================================================

best_cap_plot <- ggplot(
  
  best_cap_table,
  
  aes(
    
    x =
      `True Rating SD`,
    
    y =
      `Best Cap`,
    
    group = 1
    
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
      rating_sd_values
  ) +
  
  labs(
    
    title =
      "Best Margin Cap by League Strength Dispersion",
    
    x =
      "True Team Rating SD",
    
    y =
      "Best Margin Cap"
    
  ) +
  
  theme_minimal()


print(
  best_cap_plot
)



# ============================================================
# 24. SAVE RESULTS
# ============================================================

write.csv(
  
  simulation_results,
  
  "version5D_all_margin_cap_simulations.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  summary_long,
  
  "version5D_margin_cap_summary.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  mae_table,
  
  "version5D_margin_cap_MAE_table.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  best_cap_table,
  
  "version5D_best_cap_by_SD.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  win_rate_table,
  
  "version5D_cap_win_rates.csv",
  
  row.names = FALSE
  
)



# ============================================================
# 25. FINAL SUMMARY
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 5D COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "True rating SD values:",
  paste(
    rating_sd_values,
    collapse = ", "
  ),
  "\n"
)


cat(
  "Margin caps tested:",
  paste(
    margin_cap_labels,
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
  "Unique simulated seasons:",
  length(rating_sd_values) *
    n_simulations_per_sd,
  "\n"
)


cat(
  "Total model evaluations:",
  length(rating_sd_values) *
    n_simulations_per_sd *
    length(margin_cap_values),
  "\n\n"
)


cat(
  "Team lambda: 2 throughout season\n"
)


cat(
  "True HFA:",
  true_hfa,
  "\n"
)


cat(
  "HFA prior:",
  hfa_prior,
  "\n"
)


cat(
  "Game noise SD:",
  noise_sd,
  "\n\n"
)



cat(
  "BEST CAP BY TRUE RATING SD:\n\n"
)


print(
  best_cap_table
)



cat(
  "\nFULL MAE TABLE:\n\n"
)


print(
  mae_table
)


# ============================================================
# END VERSION 5D
# ============================================================
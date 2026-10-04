# ============================================================
# FOOTBALL POWER RATINGS - VERSION 4
# ============================================================
#
# VERSION 4:
#
# 1. Every team begins at 0.
# 2. No preseason team rankings.
# 3. Scoring margin capped at +/- 21.
# 4. Opponent-adjusted team ratings.
# 5. Home-field advantage included.
# 6. Walk-forward / out-of-sample testing.
# 7. Stronger team shrinkage early in season.
# 8. Team ratings shrink toward 0.
# 9. HFA shrinks toward a prior of 2.5 points.
# 10. Shrinkage becomes weaker as more games are played.
#
#
# Model:
#
# Capped Margin =
#     HFA
#     + Home Team Rating
#     - Away Team Rating
#     + Error
#
#
# Team prior:
#
#     Rating = 0
#
# HFA prior:
#
#     HFA = 2.5
#
# ============================================================



# ============================================================
# 1. LOAD PACKAGE
# ============================================================

# Install once if necessary:
# install.packages("ggplot2")

library(ggplot2)



# ============================================================
# 2. LOAD DATA
# ============================================================

games <- read.csv(
  "football_power_ratings_games.csv",
  stringsAsFactors = FALSE
)


# Inspect data
head(games)

str(games)

dim(games)



# ============================================================
# 3. CREATE BASIC VARIABLES
# ============================================================

# Raw scoring margin from home-team perspective

games$margin <-
  games$home_score -
  games$away_score


# Version 4 scoring margin:
# cap all margins at +/- 21

games$capped_margin <- pmax(
  pmin(
    games$margin,
    21
  ),
  -21
)


# List of all teams

teams <- sort(
  unique(
    c(
      games$home_team,
      games$away_team
    )
  )
)


# Number of weeks

max_week <- max(
  games$week
)


cat(
  "\nTeams:",
  length(teams),
  "\n"
)

cat(
  "Games:",
  nrow(games),
  "\n"
)

cat(
  "Weeks:",
  max_week,
  "\n"
)



# ============================================================
# 4. DYNAMIC SHRINKAGE FUNCTION
# ============================================================

# Early in the season we have very little information.
#
# Therefore:
#
# Team ratings get pulled more strongly toward 0.
#
# HFA gets pulled more strongly toward 2.5.
#
# As teams play more games, shrinkage becomes weaker.


get_shrinkage <- function(
    data,
    league_teams
) {
  
  
  # ----------------------------------------------------------
  # Average games played per team
  # ----------------------------------------------------------
  
  avg_games_played <-
    
    (2 * nrow(data)) /
    
    length(league_teams)
  
  
  
  # ----------------------------------------------------------
  # Determine shrinkage strength
  # ----------------------------------------------------------
  
  if (
    avg_games_played <= 2
  ) {
    
    # Very early season
    
    team_lambda <- 2
    
    hfa_lambda <- 10
    
    
  } else if (
    avg_games_played <= 4
  ) {
    
    # Early season
    
    team_lambda <- 2
    
    hfa_lambda <- 8
    
    
  } else if (
    avg_games_played <= 6
  ) {
    
    # Middle of first half
    
    team_lambda <- 2
    
    hfa_lambda <- 5
    
    
  } else {
    
    # Enough information available
    
    team_lambda <- 2
    
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
# 5. POWER RATING FUNCTION
# ============================================================

fit_power_ratings <- function(
    data,
    league_teams,
    team_lambda = 2,
    hfa_lambda = 2,
    hfa_prior = 2.5,
    margin_cap = 21
) {
  
  
  # ----------------------------------------------------------
  # Number of games / teams
  # ----------------------------------------------------------
  
  n_games <-
    nrow(data)
  
  
  n_teams <-
    length(league_teams)
  
  
  
  # ----------------------------------------------------------
  # RAW SCORING MARGIN
  # ----------------------------------------------------------
  
  margin <-
    
    data$home_score -
    
    data$away_score
  
  
  
  # ----------------------------------------------------------
  # CAP SCORING MARGIN
  # ----------------------------------------------------------
  
  y <- pmax(
    
    pmin(
      margin,
      margin_cap
    ),
    
    -margin_cap
    
  )
  
  
  
  # ----------------------------------------------------------
  # DESIGN MATRIX
  # ----------------------------------------------------------
  
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
  
  
  
  # Every game receives HFA
  
  X[, "HFA"] <- 1
  
  
  
  # ----------------------------------------------------------
  # TEAM EFFECTS
  # ----------------------------------------------------------
  #
  # Home team = +1
  #
  # Away team = -1
  #
  # Other teams = 0
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
  # RIDGE PENALTY
  # ==========================================================
  #
  # Team ratings:
  #
  #     shrink toward 0
  #
  #
  # HFA:
  #
  #     shrink toward 2.5
  #
  #
  # Larger lambda =
  # stronger belief in prior.
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
  # PRIOR TARGETS
  # ==========================================================
  #
  # First coefficient = HFA
  #
  # Remaining coefficients = team ratings
  #
  #
  # HFA prior:
  #
  #     2.5
  #
  #
  # Team priors:
  #
  #     0
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
  #
  # Penalized regression with non-zero HFA prior.
  #
  #
  # beta =
  #
  # (X'X + P)^(-1)
  #
  # [X'y + P * prior]
  #
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
  
  
  
  # Convert matrix to vector
  
  beta <- as.numeric(
    beta_matrix
  )
  
  
  names(beta) <-
    colnames(X)
  
  
  
  # ----------------------------------------------------------
  # HFA
  # ----------------------------------------------------------
  
  hfa <-
    beta["HFA"]
  
  
  
  # ----------------------------------------------------------
  # TEAM RATINGS
  # ----------------------------------------------------------
  
  ratings <-
    beta[
      league_teams
    ]
  
  
  
  # Center ratings so league average = 0
  
  ratings <-
    
    ratings -
    
    mean(ratings)
  
  
  
  # ----------------------------------------------------------
  # GAMES PLAYED
  # ----------------------------------------------------------
  
  games_played <- sapply(
    
    league_teams,
    
    function(team) {
      
      sum(
        
        data$home_team == team |
          
          data$away_team == team
        
      )
      
    }
    
  )
  
  
  
  # ----------------------------------------------------------
  # CREATE RATING TABLE
  # ----------------------------------------------------------
  
  rating_table <- data.frame(
    
    team =
      league_teams,
    
    rating =
      as.numeric(ratings),
    
    games_played =
      as.numeric(games_played),
    
    stringsAsFactors =
      FALSE
    
  )
  
  
  
  # Rank teams
  
  rating_table$rank <- rank(
    
    -rating_table$rating,
    
    ties.method =
      "min"
    
  )
  
  
  
  # Sort best to worst
  
  rating_table <- rating_table[
    
    order(
      rating_table$rank
    ),
    
  ]
  
  
  rownames(
    rating_table
  ) <- NULL
  
  
  
  # ----------------------------------------------------------
  # RETURN MODEL
  # ----------------------------------------------------------
  
  return(
    
    list(
      
      ratings =
        rating_table,
      
      home_field =
        as.numeric(hfa),
      
      team_lambda =
        team_lambda,
      
      hfa_lambda =
        hfa_lambda,
      
      hfa_prior =
        hfa_prior,
      
      coefficients =
        beta
      
    )
    
  )
  
}



# ============================================================
# 6. WEEK ZERO
# ============================================================

# Before the season:
#
# Every team = 0.
#
# No preseason ranking.

week_zero <- data.frame(
  
  team =
    teams,
  
  rating =
    0,
  
  games_played =
    0,
  
  rank =
    NA,
  
  week =
    0,
  
  home_field =
    2.5,
  
  team_lambda =
    NA,
  
  hfa_lambda =
    NA,
  
  stringsAsFactors =
    FALSE
  
)


print(
  week_zero
)



# ============================================================
# 7. WEEK-BY-WEEK RATINGS
# ============================================================

weekly_rating_list <-
  list()


weekly_rating_list[[1]] <-
  week_zero



for (
  w in 1:max_week
) {
  
  
  # ----------------------------------------------------------
  # Use all games through this week
  # ----------------------------------------------------------
  
  training_games <- games[
    
    games$week <= w,
    
  ]
  
  
  
  # ----------------------------------------------------------
  # Determine shrinkage
  # ----------------------------------------------------------
  
  shrinkage <- get_shrinkage(
    
    data =
      training_games,
    
    league_teams =
      teams
    
  )
  
  
  
  # ----------------------------------------------------------
  # Fit model
  # ----------------------------------------------------------
  
  current_model <- fit_power_ratings(
    
    data =
      training_games,
    
    league_teams =
      teams,
    
    team_lambda =
      shrinkage$team_lambda,
    
    hfa_lambda =
      shrinkage$hfa_lambda,
    
    hfa_prior =
      2.5,
    
    margin_cap =
      21
    
  )
  
  
  
  # ----------------------------------------------------------
  # Extract ratings
  # ----------------------------------------------------------
  
  current_ratings <-
    current_model$ratings
  
  
  
  current_ratings$week <-
    w
  
  
  current_ratings$home_field <-
    current_model$home_field
  
  
  current_ratings$team_lambda <-
    shrinkage$team_lambda
  
  
  current_ratings$hfa_lambda <-
    shrinkage$hfa_lambda
  
  
  
  weekly_rating_list[[w + 1]] <- current_ratings
  
}



# Combine weeks

weekly_ratings <- do.call(
  
  rbind,
  
  weekly_rating_list
  
)


rownames(
  weekly_ratings
) <- NULL



# ============================================================
# 8. WALK-FORWARD PREDICTION
# ============================================================
#
# Week 2 prediction:
#
#     train on Week 1
#
#
# Week 3 prediction:
#
#     train on Weeks 1-2
#
#
# Week 10 prediction:
#
#     train on Weeks 1-9
#
#
# Future games are NEVER included.
# ============================================================


prediction_list <-
  list()



for (
  target_week in 2:max_week
) {
  
  
  # ----------------------------------------------------------
  # TRAIN DATA
  # ----------------------------------------------------------
  
  training_games <- games[
    
    games$week < target_week,
    
  ]
  
  
  
  # ----------------------------------------------------------
  # TEST DATA
  # ----------------------------------------------------------
  
  test_games <- games[
    
    games$week == target_week,
    
  ]
  
  
  
  # ----------------------------------------------------------
  # DETERMINE SHRINKAGE
  # ----------------------------------------------------------
  
  shrinkage <- get_shrinkage(
    
    data =
      training_games,
    
    league_teams =
      teams
    
  )
  
  
  
  # ----------------------------------------------------------
  # FIT MODEL USING PAST GAMES ONLY
  # ----------------------------------------------------------
  
  model <- fit_power_ratings(
    
    data =
      training_games,
    
    league_teams =
      teams,
    
    team_lambda =
      shrinkage$team_lambda,
    
    hfa_lambda =
      shrinkage$hfa_lambda,
    
    hfa_prior =
      2.5,
    
    margin_cap =
      21
    
  )
  
  
  
  # ----------------------------------------------------------
  # CREATE RATING LOOKUP
  # ----------------------------------------------------------
  
  rating_lookup <-
    
    model$ratings$rating
  
  
  names(
    rating_lookup
  ) <-
    
    model$ratings$team
  
  
  
  # ----------------------------------------------------------
  # RATINGS ENTERING THE WEEK
  # ----------------------------------------------------------
  
  test_games$home_rating <-
    
    rating_lookup[
      test_games$home_team
    ]
  
  
  test_games$away_rating <-
    
    rating_lookup[
      test_games$away_team
    ]
  
  
  
  # ----------------------------------------------------------
  # MODEL INFORMATION
  # ----------------------------------------------------------
  
  test_games$estimated_hfa <-
    
    model$home_field
  
  
  test_games$team_lambda <-
    
    shrinkage$team_lambda
  
  
  test_games$hfa_lambda <-
    
    shrinkage$hfa_lambda
  
  
  test_games$avg_games_played <-
    
    shrinkage$avg_games_played
  
  
  
  # ----------------------------------------------------------
  # PREDICTED MARGIN
  # ----------------------------------------------------------
  
  test_games$predicted_margin <-
    
    test_games$home_rating -
    
    test_games$away_rating +
    
    test_games$estimated_hfa
  
  
  
  # ----------------------------------------------------------
  # ACTUAL RAW MARGIN
  # ----------------------------------------------------------
  
  test_games$actual_margin <-
    
    test_games$home_score -
    
    test_games$away_score
  
  
  
  # ----------------------------------------------------------
  # ACTUAL CAPPED MARGIN
  # ----------------------------------------------------------
  
  test_games$actual_capped_margin <- pmax(
    
    pmin(
      
      test_games$actual_margin,
      
      21
      
    ),
    
    -21
    
  )
  
  
  
  # ----------------------------------------------------------
  # RAW ERROR
  # ----------------------------------------------------------
  
  test_games$error_raw <-
    
    test_games$actual_margin -
    
    test_games$predicted_margin
  
  
  
  # ----------------------------------------------------------
  # CAPPED ERROR
  # ----------------------------------------------------------
  
  test_games$error_capped <-
    
    test_games$actual_capped_margin -
    
    test_games$predicted_margin
  
  
  
  # ----------------------------------------------------------
  # WINNER PREDICTION
  # ----------------------------------------------------------
  
  test_games$predicted_home_win <-
    
    test_games$predicted_margin > 0
  
  
  
  test_games$actual_home_win <-
    
    test_games$actual_margin > 0
  
  
  
  test_games$correct_winner <-
    
    test_games$predicted_home_win ==
    
    test_games$actual_home_win
  
  
  
  # ----------------------------------------------------------
  # STORE WEEK
  # ----------------------------------------------------------
  
  prediction_list[[as.character(target_week)]] <- test_games
  
}



# ============================================================
# 9. COMBINE WALK-FORWARD PREDICTIONS
# ============================================================

predictions <- do.call(
  
  rbind,
  
  prediction_list
  
)


rownames(
  predictions
) <- NULL



# ============================================================
# 10. VIEW PREDICTIONS
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "VERSION 4 OUT-OF-SAMPLE PREDICTIONS\n"
)

cat(
  "========================================\n\n"
)


print(
  
  predictions[
    ,
    c(
      "week",
      "home_team",
      "away_team",
      "home_rating",
      "away_rating",
      "estimated_hfa",
      "team_lambda",
      "hfa_lambda",
      "predicted_margin",
      "actual_margin",
      "correct_winner"
    )
  ]
  
)



# ============================================================
# 11. RAW RMSE
# ============================================================

raw_rmse <- sqrt(
  
  mean(
    predictions$error_raw^2
  )
  
)



# ============================================================
# 12. RAW MAE
# ============================================================

raw_mae <- mean(
  
  abs(
    predictions$error_raw
  )
  
)



# ============================================================
# 13. CAPPED RMSE
# ============================================================

capped_rmse <- sqrt(
  
  mean(
    predictions$error_capped^2
  )
  
)



# ============================================================
# 14. CAPPED MAE
# ============================================================

capped_mae <- mean(
  
  abs(
    predictions$error_capped
  )
  
)



# ============================================================
# 15. PREDICTION CORRELATION
# ============================================================

prediction_correlation <- cor(
  
  predictions$predicted_margin,
  
  predictions$actual_margin
  
)



# ============================================================
# 16. WINNER ACCURACY
# ============================================================

winner_accuracy <- mean(
  
  predictions$correct_winner
  
)



# ============================================================
# 17. PREDICTION BIAS
# ============================================================

# Positive:
#
# Home teams outperform model prediction.
#
# Negative:
#
# Home teams underperform model prediction.

prediction_bias <- mean(
  
  predictions$error_raw
  
)



# ============================================================
# 18. PRINT WALK-FORWARD RESULTS
# ============================================================

cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "VERSION 4 WALK-FORWARD RESULTS\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Games Predicted:",
  nrow(predictions),
  "\n\n"
)


cat(
  "RAW MARGIN METRICS\n"
)

cat(
  "------------------\n"
)


cat(
  "RMSE:",
  round(
    raw_rmse,
    3
  ),
  "\n"
)


cat(
  "MAE:",
  round(
    raw_mae,
    3
  ),
  "\n"
)


cat(
  "Prediction Correlation:",
  round(
    prediction_correlation,
    3
  ),
  "\n"
)


cat(
  "Prediction Bias:",
  round(
    prediction_bias,
    3
  ),
  "\n\n"
)



cat(
  "CAPPED MARGIN METRICS\n"
)

cat(
  "---------------------\n"
)


cat(
  "RMSE:",
  round(
    capped_rmse,
    3
  ),
  "\n"
)


cat(
  "MAE:",
  round(
    capped_mae,
    3
  ),
  "\n\n"
)



cat(
  "WINNER ACCURACY\n"
)

cat(
  "---------------\n"
)


cat(
  round(
    winner_accuracy * 100,
    1
  ),
  "%\n"
)



# ============================================================
# 19. WEEK-BY-WEEK PERFORMANCE
# ============================================================

weekly_prediction_list <- lapply(
  
  split(
    predictions,
    predictions$week
  ),
  
  function(x) {
    
    
    data.frame(
      
      week =
        unique(x$week),
      
      games =
        nrow(x),
      
      avg_games_entering_week =
        unique(x$avg_games_played),
      
      team_lambda =
        unique(x$team_lambda),
      
      hfa_lambda =
        unique(x$hfa_lambda),
      
      estimated_hfa =
        unique(x$estimated_hfa),
      
      raw_MAE =
        mean(
          abs(x$error_raw)
        ),
      
      raw_RMSE =
        sqrt(
          mean(
            x$error_raw^2
          )
        ),
      
      capped_MAE =
        mean(
          abs(x$error_capped)
        ),
      
      capped_RMSE =
        sqrt(
          mean(
            x$error_capped^2
          )
        ),
      
      winner_accuracy =
        mean(
          x$correct_winner
        ),
      
      mean_error =
        mean(
          x$error_raw
        )
      
    )
    
  }
  
)



weekly_performance <- do.call(
  
  rbind,
  
  weekly_prediction_list
  
)


rownames(
  weekly_performance
) <- NULL



cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "VERSION 4 PERFORMANCE BY WEEK\n"
)

cat(
  "========================================\n\n"
)


print(
  weekly_performance
)



# ============================================================
# 20. FINAL MODEL
# ============================================================

final_shrinkage <- get_shrinkage(
  
  data =
    games,
  
  league_teams =
    teams
  
)



final_model <- fit_power_ratings(
  
  data =
    games,
  
  league_teams =
    teams,
  
  team_lambda =
    final_shrinkage$team_lambda,
  
  hfa_lambda =
    final_shrinkage$hfa_lambda,
  
  hfa_prior =
    2.5,
  
  margin_cap =
    21
  
)



final_ratings <-
  final_model$ratings



cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "FINAL VERSION 4 POWER RATINGS\n"
)

cat(
  "========================================\n\n"
)


print(
  final_ratings
)


cat(
  "\nFinal Estimated HFA:",
  round(
    final_model$home_field,
    2
  ),
  "points\n"
)



# ============================================================
# 21. HFA HISTORY
# ============================================================

hfa_history <- weekly_ratings[
  
  !duplicated(
    weekly_ratings$week
  ),
  
  c(
    "week",
    "home_field",
    "team_lambda",
    "hfa_lambda"
  )
  
]


print(
  hfa_history
)



# ============================================================
# 22. PLOT POWER RATINGS
# ============================================================

rating_plot <- ggplot(
  
  weekly_ratings,
  
  aes(
    x = week,
    y = rating,
    color = team
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
    size = 2
  ) +
  
  scale_x_continuous(
    breaks = 0:max_week
  ) +
  
  labs(
    
    title =
      "Version 4 Football Power Ratings",
    
    subtitle =
      "21-point margin cap with dynamic shrinkage",
    
    x =
      "Week",
    
    y =
      "Power Rating",
    
    color =
      "Team"
    
  ) +
  
  theme_minimal()


print(
  rating_plot
)



# ============================================================
# 23. PLOT HFA OVER TIME
# ============================================================

hfa_plot <- ggplot(
  
  hfa_history[
    hfa_history$week > 0,
  ],
  
  aes(
    x = week,
    y = home_field
  )
  
) +
  
  geom_hline(
    
    yintercept = 2.5,
    
    linetype = "dashed"
    
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 1:max_week
  ) +
  
  labs(
    
    title =
      "Estimated Home-Field Advantage",
    
    subtitle =
      "Dashed line = 2.5-point prior",
    
    x =
      "Week",
    
    y =
      "Estimated HFA"
    
  ) +
  
  theme_minimal()


print(
  hfa_plot
)



# ============================================================
# 24. PLOT TEAM LAMBDA
# ============================================================

lambda_plot <- ggplot(
  
  hfa_history[
    hfa_history$week > 0,
  ],
  
  aes(
    x = week,
    y = team_lambda
  )
  
) +
  
  geom_step(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 1:max_week
  ) +
  
  labs(
    
    title =
      "Team Rating Shrinkage by Week",
    
    subtitle =
      "Higher lambda = stronger shrinkage toward zero",
    
    x =
      "Week",
    
    y =
      "Team Lambda"
    
  ) +
  
  theme_minimal()


print(
  lambda_plot
)



# ============================================================
# 25. PLOT PREDICTED VS ACTUAL
# ============================================================

prediction_plot <- ggplot(
  
  predictions,
  
  aes(
    x = predicted_margin,
    y = actual_margin
  )
  
) +
  
  geom_point(
    size = 3,
    alpha = 0.7
  ) +
  
  geom_abline(
    
    intercept = 0,
    
    slope = 1,
    
    linetype = "dashed"
    
  ) +
  
  labs(
    
    title =
      "Version 4 Out-of-Sample Predictions",
    
    subtitle =
      "Each prediction uses only games from earlier weeks",
    
    x =
      "Predicted Margin",
    
    y =
      "Actual Margin"
    
  ) +
  
  theme_minimal()


print(
  prediction_plot
)



# ============================================================
# 26. PLOT WEEKLY MAE
# ============================================================

weekly_error_plot <- ggplot(
  
  weekly_performance,
  
  aes(
    x = week,
    y = raw_MAE
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 2:max_week
  ) +
  
  labs(
    
    title =
      "Version 4 Prediction Error by Week",
    
    subtitle =
      "Raw-margin mean absolute error",
    
    x =
      "Week Predicted",
    
    y =
      "MAE (Points)"
    
  ) +
  
  theme_minimal()


print(
  weekly_error_plot
)



# ============================================================
# 27. PLOT WINNER ACCURACY
# ============================================================

weekly_accuracy_plot <- ggplot(
  
  weekly_performance,
  
  aes(
    x = week,
    y = winner_accuracy * 100
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 2:max_week
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      100
    )
  ) +
  
  labs(
    
    title =
      "Version 4 Winner Accuracy by Week",
    
    x =
      "Week Predicted",
    
    y =
      "Correct Winner (%)"
    
  ) +
  
  theme_minimal()


print(
  weekly_accuracy_plot
)



# ============================================================
# 28. BEST AND WORST PREDICTIONS
# ============================================================

predictions$absolute_error <-
  
  abs(
    predictions$error_raw
  )



# Best predictions

best_predictions <- predictions[
  
  order(
    predictions$absolute_error
  ),
  
]


cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "10 BEST VERSION 4 PREDICTIONS\n"
)

cat(
  "========================================\n\n"
)


print(
  
  head(
    
    best_predictions[
      ,
      c(
        "week",
        "home_team",
        "away_team",
        "predicted_margin",
        "actual_margin",
        "absolute_error"
      )
    ],
    
    10
    
  )
  
)



# Worst predictions

worst_predictions <- predictions[
  
  order(
    -predictions$absolute_error
  ),
  
]


cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "10 WORST VERSION 4 PREDICTIONS\n"
)

cat(
  "========================================\n\n"
)


print(
  
  head(
    
    worst_predictions[
      ,
      c(
        "week",
        "home_team",
        "away_team",
        "predicted_margin",
        "actual_margin",
        "absolute_error"
      )
    ],
    
    10
    
  )
  
)



# ============================================================
# 29. SAVE RESULTS
# ============================================================

write.csv(
  
  predictions,
  
  "version4_walk_forward_predictions.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  weekly_performance,
  
  "version4_weekly_performance.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  weekly_ratings,
  
  "version4_weekly_ratings.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  final_ratings,
  
  "version4_final_power_ratings.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  hfa_history,
  
  "version4_hfa_history.csv",
  
  row.names = FALSE
  
)



# ============================================================
# 30. FINAL SUMMARY
# ============================================================

cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "VERSION 4 FINAL SUMMARY\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Margin Cap: +/- 21 points\n"
)


cat(
  "HFA Prior: 2.5 points\n"
)


cat(
  "First Predicted Week: 2\n"
)


cat(
  "Last Predicted Week:",
  max_week,
  "\n"
)


cat(
  "Out-of-Sample Games:",
  nrow(predictions),
  "\n\n"
)



cat(
  "Raw Margin RMSE:",
  round(
    raw_rmse,
    2
  ),
  "\n"
)


cat(
  "Raw Margin MAE:",
  round(
    raw_mae,
    2
  ),
  "\n"
)


cat(
  "Capped Margin RMSE:",
  round(
    capped_rmse,
    2
  ),
  "\n"
)


cat(
  "Capped Margin MAE:",
  round(
    capped_mae,
    2
  ),
  "\n"
)


cat(
  "Predicted vs Actual Correlation:",
  round(
    prediction_correlation,
    3
  ),
  "\n"
)


cat(
  "Winner Accuracy:",
  round(
    winner_accuracy * 100,
    1
  ),
  "%\n"
)


cat(
  "Prediction Bias:",
  round(
    prediction_bias,
    2
  ),
  "points\n\n"
)



cat(
  "Final Team Lambda:",
  final_shrinkage$team_lambda,
  "\n"
)


cat(
  "Final HFA Lambda:",
  final_shrinkage$hfa_lambda,
  "\n"
)


cat(
  "Final Estimated HFA:",
  round(
    final_model$home_field,
    2
  ),
  "points\n\n"
)


cat(
  "FINAL RATINGS:\n\n"
)


print(
  final_ratings
)



# ============================================================
# END VERSION 4
# ============================================================
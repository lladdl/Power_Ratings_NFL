# ============================================================
# FOOTBALL POWER RATINGS - VERSION 3
# ============================================================
#
# VERSION 3 GOAL:
#
# Create a true walk-forward / out-of-sample power rating system.
#
# Rating model:
#
#   Capped Margin =
#       Home Field Advantage
#       + Home Team Rating
#       - Away Team Rating
#       + Error
#
#
# Changes from Version 2:
#
#   1. Scoring margin is still capped at +/- 21.
#   2. Every team still begins at 0.
#   3. No preseason rankings are used.
#   4. Ratings are recalculated after every week.
#   5. Ratings through Week t predict Week t+1.
#   6. Future games are NEVER used to predict earlier games.
#
#
# Example:
#
# Weeks 1-4 -> calculate ratings
# Week 5    -> predict using only Weeks 1-4
#
# After Week 5 is played:
#
# Weeks 1-5 -> recalculate ratings
# Week 6    -> predict using only Weeks 1-5
#
# ============================================================



# ============================================================
# 1. LOAD PACKAGES
# ============================================================

# Install once if needed:
# install.packages("ggplot2")

library(ggplot2)



# ============================================================
# 2. LOAD DATA
# ============================================================

games <- read.csv(
  "football_power_ratings_games.csv",
  stringsAsFactors = FALSE
)


# Inspect
head(games)

str(games)

dim(games)



# ============================================================
# 3. BASIC VARIABLES
# ============================================================

# Actual raw scoring margin
games$margin <-
  games$home_score -
  games$away_score


# Capped scoring margin
games$capped_margin <- pmax(
  pmin(games$margin, 21),
  -21
)


# All league teams
teams <- sort(
  unique(
    c(
      games$home_team,
      games$away_team
    )
  )
)


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
# 4. POWER RATING FUNCTION
# ============================================================

fit_power_ratings <- function(
    data,
    league_teams,
    lambda = 2,
    margin_cap = 21
) {
  
  
  # ----------------------------------------------------------
  # Number of games and teams
  # ----------------------------------------------------------
  
  n_games <- nrow(data)
  
  n_teams <- length(league_teams)
  
  
  
  # ----------------------------------------------------------
  # RAW SCORING MARGIN
  # ----------------------------------------------------------
  
  margin <-
    data$home_score -
    data$away_score
  
  
  
  # ----------------------------------------------------------
  # CAP MARGIN
  # ----------------------------------------------------------
  
  # Anything above +21 becomes +21.
  # Anything below -21 becomes -21.
  
  y <- pmax(
    pmin(margin, margin_cap),
    -margin_cap
  )
  
  
  
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
  
  
  # Every game has a home-field effect
  X[, "HFA"] <- 1
  
  
  
  # Home team = +1
  # Away team = -1
  
  for (i in seq_len(n_games)) {
    
    home <- data$home_team[i]
    
    away <- data$away_team[i]
    
    
    X[i, home] <- 1
    
    X[i, away] <- -1
    
  }
  
  
  
  # ----------------------------------------------------------
  # RIDGE PENALTY
  # ----------------------------------------------------------
  
  # Home-field advantage is not penalized.
  #
  # Team ratings are slightly shrunk toward zero.
  
  penalty <- diag(
    c(
      0,
      rep(lambda, n_teams)
    )
  )
  
  
  
  # ----------------------------------------------------------
  # ESTIMATE MODEL
  # ----------------------------------------------------------
  
  beta_matrix <- solve(
    crossprod(X) + penalty,
    crossprod(X, y)
  )
  
  
  # Convert matrix to named vector
  beta <- as.numeric(
    beta_matrix
  )
  
  
  names(beta) <- colnames(X)
  
  
  
  # ----------------------------------------------------------
  # HOME-FIELD ADVANTAGE
  # ----------------------------------------------------------
  
  hfa <- beta["HFA"]
  
  
  
  # ----------------------------------------------------------
  # TEAM RATINGS
  # ----------------------------------------------------------
  
  ratings <- beta[
    league_teams
  ]
  
  
  # Center league at zero
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
  # RATING TABLE
  # ----------------------------------------------------------
  
  rating_table <- data.frame(
    
    team = league_teams,
    
    rating = as.numeric(ratings),
    
    games_played =
      as.numeric(games_played),
    
    stringsAsFactors = FALSE
    
  )
  
  
  
  # Rank teams
  rating_table$rank <- rank(
    -rating_table$rating,
    ties.method = "min"
  )
  
  
  
  # Best to worst
  rating_table <- rating_table[
    order(
      rating_table$rank
    ),
  ]
  
  
  rownames(rating_table) <- NULL
  
  
  
  # ----------------------------------------------------------
  # RETURN RESULTS
  # ----------------------------------------------------------
  
  return(
    
    list(
      
      ratings =
        rating_table,
      
      home_field =
        as.numeric(hfa),
      
      coefficients =
        beta
      
    )
    
  )
  
}



# ============================================================
# 5. WEEK ZERO
# ============================================================

# Before any games:
#
# Every team = 0

week_zero <- data.frame(
  
  team = teams,
  
  rating = 0,
  
  games_played = 0,
  
  rank = NA,
  
  week = 0,
  
  home_field = 0,
  
  stringsAsFactors = FALSE
  
)


print(
  week_zero
)



# ============================================================
# 6. CALCULATE WEEK-BY-WEEK RATINGS
# ============================================================

# These ratings are useful for visualization.
#
# Week 5 rating means:
#
# "Rating after all Week 5 games have been completed."


weekly_rating_list <- list()


weekly_rating_list[[1]] <-
  week_zero



for (w in 1:max_week) {
  
  
  # Use all games through current week
  
  training_games <- games[
    games$week <= w,
  ]
  
  
  
  # Fit model
  
  current_model <- fit_power_ratings(
    
    data =
      training_games,
    
    league_teams =
      teams,
    
    lambda =
      2,
    
    margin_cap =
      21
    
  )
  
  
  
  # Extract ratings
  
  current_ratings <-
    current_model$ratings
  
  
  
  current_ratings$week <-
    w
  
  
  
  current_ratings$home_field <-
    current_model$home_field
  
  
  
  weekly_rating_list[[w + 1]] <-
    current_ratings
  
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
# 7. WALK-FORWARD PREDICTIONS
# ============================================================

# THIS IS THE IMPORTANT VERSION 3 SECTION.
#
#
# To predict Week 2:
#
# Train using Week 1.
#
#
# To predict Week 3:
#
# Train using Weeks 1-2.
#
#
# To predict Week 10:
#
# Train using Weeks 1-9.
#
#
# Week being predicted is NEVER included in training.


prediction_list <- list()



for (target_week in 2:max_week) {
  
  
  # ----------------------------------------------------------
  # TRAINING DATA
  # ----------------------------------------------------------
  
  # Everything BEFORE the week being predicted
  
  training_games <- games[
    games$week < target_week,
  ]
  
  
  
  # ----------------------------------------------------------
  # TEST DATA
  # ----------------------------------------------------------
  
  # Only games from the upcoming week
  
  test_games <- games[
    games$week == target_week,
  ]
  
  
  
  # ----------------------------------------------------------
  # FIT RATINGS USING PAST DATA ONLY
  # ----------------------------------------------------------
  
  model <- fit_power_ratings(
    
    data =
      training_games,
    
    league_teams =
      teams,
    
    lambda =
      2,
    
    margin_cap =
      21
    
  )
  
  
  
  # ----------------------------------------------------------
  # RATING LOOKUP
  # ----------------------------------------------------------
  
  rating_lookup <-
    model$ratings$rating
  
  
  names(rating_lookup) <-
    model$ratings$team
  
  
  
  # ----------------------------------------------------------
  # RATINGS ENTERING THIS WEEK
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
  # HOME-FIELD ADVANTAGE ENTERING WEEK
  # ----------------------------------------------------------
  
  test_games$estimated_hfa <-
    model$home_field
  
  
  
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
  # RAW PREDICTION ERROR
  # ----------------------------------------------------------
  
  # Positive:
  # Home team did better than predicted.
  #
  # Negative:
  # Home team did worse than predicted.
  
  test_games$error_raw <-
    
    test_games$actual_margin -
    
    test_games$predicted_margin
  
  
  
  # ----------------------------------------------------------
  # CAPPED PREDICTION ERROR
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
# 8. COMBINE ALL OUT-OF-SAMPLE PREDICTIONS
# ============================================================

predictions <- do.call(
  rbind,
  prediction_list
)


rownames(
  predictions
) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "OUT-OF-SAMPLE PREDICTIONS\n"
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
      "predicted_margin",
      "actual_margin",
      "correct_winner"
    )
  ]
  
)



# ============================================================
# 9. OUT-OF-SAMPLE RAW RMSE
# ============================================================

raw_rmse <- sqrt(
  
  mean(
    predictions$error_raw^2
  )
  
)



# ============================================================
# 10. OUT-OF-SAMPLE RAW MAE
# ============================================================

raw_mae <- mean(
  
  abs(
    predictions$error_raw
  )
  
)



# ============================================================
# 11. CAPPED-MARGIN RMSE
# ============================================================

# Because the MODEL was actually trained to predict
# capped margins, this is an important metric.

capped_rmse <- sqrt(
  
  mean(
    predictions$error_capped^2
  )
  
)



# ============================================================
# 12. CAPPED-MARGIN MAE
# ============================================================

capped_mae <- mean(
  
  abs(
    predictions$error_capped
  )
  
)



# ============================================================
# 13. WINNER ACCURACY
# ============================================================

winner_accuracy <- mean(
  predictions$correct_winner
)



# ============================================================
# 14. PREDICTED VS ACTUAL CORRELATION
# ============================================================

prediction_correlation <- cor(
  
  predictions$predicted_margin,
  
  predictions$actual_margin
  
)



# ============================================================
# 15. PREDICTION BIAS
# ============================================================

# Ideally approximately zero.
#
# Positive:
# Home teams outperform predictions.
#
# Negative:
# Home teams underperform predictions.

prediction_bias <- mean(
  predictions$error_raw
)



# ============================================================
# 16. PRINT VERSION 3 RESULTS
# ============================================================

cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "VERSION 3 WALK-FORWARD RESULTS\n"
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
# 17. WEEK-BY-WEEK PERFORMANCE
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
  "PERFORMANCE BY WEEK\n"
)

cat(
  "========================================\n\n"
)


print(
  weekly_performance
)



# ============================================================
# 18. FINAL RATINGS AFTER ALL 14 WEEKS
# ============================================================

final_model <- fit_power_ratings(
  
  data =
    games,
  
  league_teams =
    teams,
  
  lambda =
    2,
  
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
  "FINAL VERSION 3 POWER RATINGS\n"
)

cat(
  "========================================\n\n"
)


print(
  final_ratings
)


cat(
  "\nFinal Estimated Home-Field Advantage:",
  round(
    final_model$home_field,
    2
  ),
  "points\n"
)



# ============================================================
# 19. PLOT WEEKLY POWER RATINGS
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
      "Version 3 Football Power Ratings",
    
    subtitle =
      "21-point margin cap",
    
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
# 20. PLOT PREDICTED VS ACTUAL MARGINS
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
      "Out-of-Sample Predicted vs Actual Margin",
    
    subtitle =
      "Each prediction uses only games played before that week",
    
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
# 21. PLOT ABSOLUTE ERROR BY WEEK
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
      "Prediction Error by Week",
    
    subtitle =
      "Mean Absolute Error",
    
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
# 22. PLOT WINNER ACCURACY BY WEEK
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
    limits = c(0, 100)
  ) +
  
  labs(
    
    title =
      "Winner Prediction Accuracy by Week",
    
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
# 23. FIND BEST AND WORST PREDICTIONS
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
  "10 BEST PREDICTIONS\n"
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
  "10 WORST PREDICTIONS\n"
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
# 24. SAVE RESULTS
# ============================================================

write.csv(
  
  predictions,
  
  "version3_walk_forward_predictions.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  weekly_performance,
  
  "version3_weekly_performance.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  weekly_ratings,
  
  "version3_weekly_ratings.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  final_ratings,
  
  "version3_final_power_ratings.csv",
  
  row.names = FALSE
  
)



# ============================================================
# 25. FINAL SUMMARY
# ============================================================

cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "VERSION 3 FINAL SUMMARY\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Margin Cap: +/- 21 points\n"
)


cat(
  "Ridge Lambda: 2\n"
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
# END VERSION 3
# ============================================================
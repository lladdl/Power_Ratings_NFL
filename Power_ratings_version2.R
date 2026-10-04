# ============================================================
# FOOTBALL POWER RATINGS - VERSION 1
# ============================================================
#
# Goal:
# Create a completely results-based power rating system.
#
# Every team begins at 0.
# No preseason rankings are used.
#
# Ratings are based on:
#   1. Scoring margin
#   2. Opponent strength
#   3. Home-field advantage
#
#
# Basic model:
#
# Home Margin =
#     Home Field Advantage
#     + Home Team Rating
#     - Away Team Rating
#     + Random Error
#
#
# Interpretation:
#
# Rating = expected points better/worse than an average team
# on a neutral field.
#
# Example:
#
# Team A = +7
# Team B = -2
#
# Neutral-field expected margin:
#
#     7 - (-2) = 9
#
# Team A would be about a 9-point favorite.
#
# ============================================================



# ============================================================
# 1. LOAD PACKAGES
# ============================================================

# Install this once if necessary:
# install.packages("ggplot2")

library(ggplot2)



# ============================================================
# 2. LOAD GAME DATA
# ============================================================

# Make sure this CSV is in your R working directory.
#
# You can check your working directory using:
#
# getwd()
#
# You can change it using:
#
# setwd("/path/to/your/folder")


games <- read.csv(
  "football_power_ratings_games.csv",
  stringsAsFactors = FALSE
)


# Examine the data
head(games)

str(games)

dim(games)



# ============================================================
# 3. BASIC DATA CHECKS
# ============================================================

# Calculate scoring margin from the home team's perspective
#
# Positive = home team won
# Negative = away team won

games$margin <-
  games$home_score -
  games$away_score


# Get every team in the league

teams <- sort(
  unique(
    c(
      games$home_team,
      games$away_team
    )
  )
)


# Print teams
teams


# Number of teams
length(teams)


# Number of games
nrow(games)


# Number of weeks
max(games$week)



# ============================================================
# 4. CHECK GAMES PLAYED BY EACH TEAM
# ============================================================

games_played_total <- sapply(
  teams,
  function(team) {
    
    sum(
      games$home_team == team |
        games$away_team == team
    )
    
  }
)


games_played_total



# ============================================================
# 5. CHECK HOME AND AWAY GAMES
# ============================================================

home_games <- sapply(
  teams,
  function(team) {
    
    sum(
      games$home_team == team
    )
    
  }
)


away_games <- sapply(
  teams,
  function(team) {
    
    sum(
      games$away_team == team
    )
    
  }
)


home_games

away_games



# ============================================================
# 6. POWER RATING FUNCTION
# ============================================================

# This is the core of the entire system.
#
# We estimate:
#
# margin =
#     home-field advantage
#     + home rating
#     - away rating
#
#
# We also use a small ridge penalty.
#
# Why?
#
# Early in the season there are not enough games to precisely
# identify each team's true strength.
#
# The ridge penalty pulls uncertain estimates slightly toward 0.
#
# IMPORTANT:
#
# Every team is treated exactly the same.
#
# Therefore this DOES NOT introduce preseason rankings.
#
# lambda controls the strength of the shrinkage.
#
# Larger lambda = ratings pulled more toward 0
# Smaller lambda = ratings react more aggressively
#
# We will begin with:
#
# lambda = 2


fit_power_ratings <- function(
    data,
    league_teams,
    lambda = 2
) {
  
  # ----------------------------------------------------------
  # Number of games
  # ----------------------------------------------------------
  
  n_games <- nrow(data)
  
  
  # Number of teams
  n_teams <- length(league_teams)
  
  
  # ----------------------------------------------------------
  # Response variable
  # ----------------------------------------------------------
  
  # Home scoring margin
  
  margin <-
    data$home_score -
    data$away_score
  y <- pmax(
    pmin(margin, 14),
    -14
  )
  
  
  # ----------------------------------------------------------
  # Create model matrix
  # ----------------------------------------------------------
  
  # Columns will be:
  #
  # HFA
  # Team 1
  # Team 2
  # ...
  # Team 8
  
  X <- matrix(
    0,
    nrow = n_games,
    ncol = n_teams + 1
  )
  
  
  # Give columns names
  
  colnames(X) <-
    c(
      "HFA",
      league_teams
    )
  
  
  # ----------------------------------------------------------
  # Home-field column
  # ----------------------------------------------------------
  
  # Every game has a home team,
  # so the HFA column is always 1.
  
  X[, "HFA"] <- 1
  
  
  # ----------------------------------------------------------
  # Team columns
  # ----------------------------------------------------------
  
  # Home team receives +1.
  # Away team receives -1.
  #
  # Example:
  #
  # Austin vs Boston
  #
  # Austin = +1
  # Boston = -1
  # everyone else = 0
  
  
  for (i in seq_len(n_games)) {
    
    home_team <- data$home_team[i]
    
    away_team <- data$away_team[i]
    
    
    X[
      i,
      home_team
    ] <- 1
    
    
    X[
      i,
      away_team
    ] <- -1
    
  }
  
  
  
  # ----------------------------------------------------------
  # Ridge penalty matrix
  # ----------------------------------------------------------
  
  # HFA is NOT penalized.
  #
  # Team ratings ARE penalized slightly.
  
  penalty <- diag(
    c(
      0,
      rep(lambda, n_teams)
    )
  )
  
  
  
  # ----------------------------------------------------------
  # Estimate parameters
  # ----------------------------------------------------------
  
  # Ridge regression formula:
  #
  # beta =
  # (X'X + lambda I)^(-1) X'y
  
  
  beta <- solve(
    crossprod(X) + penalty,
    crossprod(X, y)
  )
  
  
  
  # ----------------------------------------------------------
  # Extract home-field advantage
  # ----------------------------------------------------------
  
  hfa <- as.numeric(
    beta["HFA", 1]
  )
  

  
  
  # ----------------------------------------------------------
  # Extract team ratings
  # ----------------------------------------------------------
  
  ratings <- as.numeric(
    beta[league_teams, 1]
  )
  
  names(ratings) <- league_teams
  
  
  
  # ----------------------------------------------------------
  # Center ratings at zero
  # ----------------------------------------------------------
  
  # Average team should always equal exactly 0.
  
  ratings <-
    ratings -
    mean(ratings)
  
  
  
  # ----------------------------------------------------------
  # Count games played
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
  # Create results table
  # ----------------------------------------------------------
  
  rating_table <- data.frame(
    
    team = league_teams,
    
    rating = as.numeric(ratings),
    
    games_played = as.numeric(games_played),
    
    stringsAsFactors = FALSE
    
  )
  
  
  
  # ----------------------------------------------------------
  # Create rankings
  # ----------------------------------------------------------
  
  # Highest rating receives Rank 1.
  
  rating_table$rank <- rank(
    -rating_table$rating,
    ties.method = "min"
  )
  
  
  
  # Sort from best to worst
  
  rating_table <- rating_table[
    order(
      rating_table$rank
    ),
  ]
  
  
  rownames(rating_table) <- NULL
  
  
  
  # ----------------------------------------------------------
  # Return model results
  # ----------------------------------------------------------
  
  return(
    
    list(
      
      ratings = rating_table,
      
      home_field = hfa,
      
      coefficients = beta
      
    )
    
  )
  
}



# ============================================================
# 7. WEEK 0
# ============================================================

# Before any games are played:
#
# EVERY TEAM = 0
#
# There are no preseason assumptions.

week_zero <- data.frame(
  
  team = teams,
  
  rating = 0,
  
  games_played = 0,
  
  rank = NA,
  
  week = 0,
  
  home_field = 0,
  
  stringsAsFactors = FALSE
  
)


week_zero



# ============================================================
# 8. CALCULATE WEEK-BY-WEEK RATINGS
# ============================================================

# We recalculate the entire system every week.
#
# Week 1:
# Only Week 1 games
#
# Week 2:
# Weeks 1-2
#
# Week 3:
# Weeks 1-3
#
# etc.
#
# NO future games are allowed into previous weeks.


max_week <- max(
  games$week
)


weekly_list <- list()


# Add Week 0
weekly_list[[1]] <- week_zero



# Loop through the season

for (w in 1:max_week) {
  
  
  # ----------------------------------------------------------
  # Keep only games played through this week
  # ----------------------------------------------------------
  
  current_games <- games[
    games$week <= w,
  ]
  
  
  
  # ----------------------------------------------------------
  # Fit rating system
  # ----------------------------------------------------------
  
  current_model <- fit_power_ratings(
    
    data = current_games,
    
    league_teams = teams,
    
    lambda = 2
    
  )
  
  
  
  # ----------------------------------------------------------
  # Extract ratings
  # ----------------------------------------------------------
  
  week_ratings <-
    current_model$ratings
  
  
  
  # Add week number
  
  week_ratings$week <- w
  
  
  
  # Add current HFA estimate
  
  week_ratings$home_field <-
    current_model$home_field
  
  
  
  # Store results
  
  weekly_list[[w + 1]] <-
    week_ratings
  
}



# ============================================================
# 9. COMBINE ALL WEEKS
# ============================================================

weekly_ratings <- do.call(
  rbind,
  weekly_list
)


rownames(weekly_ratings) <- NULL



# Look at results

weekly_ratings



# ============================================================
# 10. FINAL-SEASON POWER RATINGS
# ============================================================

final_model <- fit_power_ratings(
  
  data = games,
  
  league_teams = teams,
  
  lambda = 2
  
)


final_ratings <-
  final_model$ratings


cat(
  "\n========================================\n"
)

cat(
  "FINAL POWER RATINGS\n"
)

cat(
  "========================================\n\n"
)


print(
  final_ratings
)



# ============================================================
# 11. FINAL HOME-FIELD ADVANTAGE
# ============================================================

cat(
  "\nEstimated Home-Field Advantage:",
  round(
    final_model$home_field,
    2
  ),
  "points\n"
)



# ============================================================
# 12. LOOK AT A SPECIFIC WEEK
# ============================================================

# Change this number to whichever week you want.

week_to_view <- 5


week_results <- weekly_ratings[
  
  weekly_ratings$week == week_to_view,
  
]


# Sort by rank

week_results <- week_results[
  order(
    week_results$rank
  ),
]


cat(
  "\n========================================\n"
)

cat(
  "POWER RATINGS AFTER WEEK",
  week_to_view,
  "\n"
)

cat(
  "========================================\n\n"
)


print(
  week_results
)



# ============================================================
# 13. WEEKLY RANKINGS TABLE
# ============================================================

weekly_rankings <- weekly_ratings[
  order(
    weekly_ratings$week,
    weekly_ratings$rank
  ),
]


weekly_rankings



# ============================================================
# 14. PLOT POWER RATINGS OVER TIME
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
    linetype = "dashed",
    linewidth = 0.6
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
    
    title = "Football Power Ratings by Week",
    
    subtitle =
      "Ratings are points above/below an average team",
    
    x = "Week",
    
    y = "Power Rating",
    
    color = "Team"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title =
      element_text(
        face = "bold",
        size = 16
      ),
    
    legend.position = "right"
    
  )


print(
  rating_plot
)



# ============================================================
# 15. PLOT WEEKLY RANK
# ============================================================

rank_plot <- ggplot(
  
  weekly_ratings[
    weekly_ratings$week > 0,
  ],
  
  aes(
    x = week,
    y = rank,
    color = team
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  scale_y_reverse(
    
    breaks = 1:length(teams)
    
  ) +
  
  scale_x_continuous(
    
    breaks = 1:max_week
    
  ) +
  
  labs(
    
    title = "Power Ranking Position by Week",
    
    x = "Week",
    
    y = "Rank",
    
    color = "Team"
    
  ) +
  
  theme_minimal()


print(
  rank_plot
)



# ============================================================
# 16. PREDICT A MATCHUP
# ============================================================

predict_game <- function(
    home_team,
    away_team,
    model
) {
  
  
  ratings <-
    model$ratings
  
  
  
  # Get home team rating
  
  home_rating <- ratings$rating[
    ratings$team == home_team
  ]
  
  
  
  # Get away team rating
  
  away_rating <- ratings$rating[
    ratings$team == away_team
  ]
  
  
  
  # Error checking
  
  if (
    length(home_rating) == 0
  ) {
    
    stop(
      paste(
        "Home team not found:",
        home_team
      )
    )
    
  }
  
  
  if (
    length(away_rating) == 0
  ) {
    
    stop(
      paste(
        "Away team not found:",
        away_team
      )
    )
    
  }
  
  
  
  # Home-field advantage
  
  hfa <-
    model$home_field
  
  
  
  # Predicted margin
  
  predicted_margin <-
    
    home_rating -
    away_rating +
    hfa
  
  
  
  # Print results
  
  cat(
    "\n========================================\n"
  )
  
  cat(
    "MATCHUP PREDICTION\n"
  )
  
  cat(
    "========================================\n\n"
  )
  
  
  cat(
    "Home Team:",
    home_team,
    "\n"
  )
  
  
  cat(
    "Away Team:",
    away_team,
    "\n\n"
  )
  
  
  cat(
    "Home Rating:",
    round(
      home_rating,
      2
    ),
    "\n"
  )
  
  
  cat(
    "Away Rating:",
    round(
      away_rating,
      2
    ),
    "\n"
  )
  
  
  cat(
    "Home-Field Advantage:",
    round(
      hfa,
      2
    ),
    "\n\n"
  )
  
  
  cat(
    "Predicted Home Margin:",
    round(
      predicted_margin,
      2
    ),
    "\n\n"
  )
  
  
  
  # Favorite
  
  if (
    predicted_margin > 0
  ) {
    
    cat(
      home_team,
      "is favored by",
      round(
        predicted_margin,
        1
      ),
      "points.\n"
    )
    
    
  } else if (
    predicted_margin < 0
  ) {
    
    cat(
      away_team,
      "is favored by",
      round(
        abs(predicted_margin),
        1
      ),
      "points.\n"
    )
    
    
  } else {
    
    cat(
      "The matchup is rated as even.\n"
    )
    
  }
  
  
  
  # Return value invisibly
  
  invisible(
    predicted_margin
  )
  
}



# ============================================================
# 17. EXAMPLE MATCHUP PREDICTION
# ============================================================

predict_game(
  
  home_team =
    "Austin Outlaws",
  
  away_team =
    "Boston Admirals",
  
  model =
    final_model
  
)



# ============================================================
# 18. GET RATING FOR EACH GAME
# ============================================================

# Make a lookup vector.

rating_lookup <-
  final_ratings$rating


names(rating_lookup) <-
  final_ratings$team



# Add ratings to game data

games$home_rating <-
  rating_lookup[
    games$home_team
  ]


games$away_rating <-
  rating_lookup[
    games$away_team
  ]



# ============================================================
# 19. PREDICT EVERY GAME
# ============================================================

games$predicted_margin <-
  
  final_model$home_field +
  
  games$home_rating -
  
  games$away_rating



# Actual margin

games$actual_margin <-
  
  games$home_score -
  
  games$away_score



# Prediction error

games$residual <-
  
  games$actual_margin -
  
  games$predicted_margin



# Look at predictions

head(
  games[
    ,
    c(
      "week",
      "home_team",
      "away_team",
      "actual_margin",
      "predicted_margin",
      "residual"
    )
  ]
)



# ============================================================
# 20. MODEL RMSE
# ============================================================

rmse <- sqrt(
  
  mean(
    games$residual^2
  )
  
)


cat(
  "\nModel RMSE:",
  round(
    rmse,
    3
  ),
  "points\n"
)



# ============================================================
# 21. MODEL MAE
# ============================================================

mae <- mean(
  abs(
    games$residual
  )
)


cat(
  "Model MAE:",
  round(
    mae,
    3
  ),
  "points\n"
)



# ============================================================
# 22. CORRELATION BETWEEN PREDICTED AND ACTUAL MARGINS
# ============================================================

margin_correlation <- cor(
  
  games$predicted_margin,
  
  games$actual_margin
  
)


cat(
  "Predicted/Actual Margin Correlation:",
  round(
    margin_correlation,
    3
  ),
  "\n"
)



# ============================================================
# 23. DID THE MODEL PICK THE CORRECT WINNER?
# ============================================================

games$predicted_home_win <-
  games$predicted_margin > 0


games$actual_home_win <-
  games$actual_margin > 0


games$correct_winner <-
  
  games$predicted_home_win ==
  games$actual_home_win



winner_accuracy <- mean(
  games$correct_winner
)


cat(
  "Winner Prediction Accuracy:",
  round(
    winner_accuracy * 100,
    1
  ),
  "%\n"
)



# ============================================================
# 24. PLOT PREDICTED VS ACTUAL MARGINS
# ============================================================

prediction_plot <- ggplot(
  
  games,
  
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
      "Predicted vs Actual Game Margins",
    
    subtitle =
      "Points near the dashed line were predicted well",
    
    x =
      "Predicted Home Margin",
    
    y =
      "Actual Home Margin"
    
  ) +
  
  theme_minimal()


print(
  prediction_plot
)



# ============================================================
# 25. VIEW ONE TEAM'S RATING HISTORY
# ============================================================

team_to_view <-
  "Austin Outlaws"


team_history <- weekly_ratings[
  
  weekly_ratings$team ==
    team_to_view,
  
]


team_history



# ============================================================
# 26. PLOT ONE TEAM
# ============================================================

single_team_plot <- ggplot(
  
  team_history,
  
  aes(
    x = week,
    y = rating
  )
  
) +
  
  geom_hline(
    
    yintercept = 0,
    
    linetype = "dashed"
    
  ) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  geom_point(
    size = 3
  ) +
  
  scale_x_continuous(
    breaks = 0:max_week
  ) +
  
  labs(
    
    title = paste(
      team_to_view,
      "Power Rating"
    ),
    
    x = "Week",
    
    y = "Power Rating"
    
  ) +
  
  theme_minimal()


print(
  single_team_plot
)



# ============================================================
# 27. HOME-FIELD ADVANTAGE OVER TIME
# ============================================================

# One HFA estimate exists for every team in each week,
# so keep only one observation per week.

hfa_history <- weekly_ratings[
  
  !duplicated(
    weekly_ratings$week
  ),
  
  c(
    "week",
    "home_field"
  )
  
]


hfa_history



# ============================================================
# 28. PLOT HOME-FIELD ADVANTAGE
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
      "Estimated Home-Field Advantage by Week",
    
    x = "Week",
    
    y = "Home-Field Advantage (Points)"
    
  ) +
  
  theme_minimal()


print(
  hfa_plot
)



# ============================================================
# 29. FINAL SUMMARY
# ============================================================

cat(
  "\n\n"
)

cat(
  "========================================\n"
)

cat(
  "FINAL MODEL SUMMARY\n"
)

cat(
  "========================================\n\n"
)


cat(
  "Teams:",
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


cat(
  "Estimated Home-Field Advantage:",
  round(
    final_model$home_field,
    2
  ),
  "\n"
)


cat(
  "RMSE:",
  round(
    rmse,
    2
  ),
  "\n"
)


cat(
  "MAE:",
  round(
    mae,
    2
  ),
  "\n"
)


cat(
  "Winner Accuracy:",
  round(
    winner_accuracy * 100,
    1
  ),
  "%",
  "\n\n"
)


cat(
  "FINAL POWER RATINGS:\n\n"
)


print(
  final_ratings
)



# ============================================================
# 30. SAVE RESULTS
# ============================================================

# Save weekly ratings

write.csv(
  
  weekly_ratings,
  
  "weekly_power_ratings.csv",
  
  row.names = FALSE
  
)


# Save final ratings

write.csv(
  
  final_ratings,
  
  "final_power_ratings.csv",
  
  row.names = FALSE
  
)


# Save game predictions

write.csv(
  
  games,
  
  "power_rating_game_predictions.csv",
  
  row.names = FALSE
  
)



# ============================================================
# END
# ============================================================
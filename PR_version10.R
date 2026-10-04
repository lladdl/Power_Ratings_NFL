# ============================================================
# FOOTBALL POWER RATINGS - VERSION 10.1
# PRODUCTION WEEKLY RATINGS + GAME FORECASTS
# ============================================================
#
# FINALIZED RESULTS-ONLY ARCHITECTURE
# -----------------------------------
#
# TEAM RATING ENGINE
#   Score Offense/Defense Huber
#   component ridge lambda = 2
#   Huber k = 1.5
#   recency half-life = 12 weeks
#   HFA estimated separately with prior centered at 2.5 points
#
# OVERALL RATING
#
#   Overall Rating = Offense Rating + Defense Rating
#
# interpreted as estimated neutral-field points above/below an
# average team.
#
# GAME FORECAST
#
#   Expected Home Score =
#       League Mean Points
#       + Home Offense
#       - Away Defense
#       + HFA / 2
#
#   Expected Away Score =
#       League Mean Points
#       + Away Offense
#       - Home Defense
#       - HFA / 2
#
#   Expected Margin =
#       Home Overall Rating
#       - Away Overall Rating
#       + HFA
#
# WIN PROBABILITY
#
#   z = Expected Margin / Predictive Margin SD
#
#   P(Home Win) =
#       logistic(
#           0.0192183
#           + 1.440608 * z
#       )
#
# The logistic coefficients were frozen after Version 9C-MC:
#   720 independent calibration seasons
#   2,400 independent evaluation seasons
#   24 drift/noise environments
#
# RATING UNCERTAINTY
# ------------------
# Uses the pooled simulation-calibrated 95% half-widths selected
# in Version 9A-H2.
#
# IMPORTANT:
# These uncertainty widths were calibrated for the project's
# 16-team / 12-game research league. Recalibrate them before
# treating them as official intervals for a materially different
# league structure such as the 32-team NFL.
#
# INPUT FILES
# -----------
# schedule file REQUIRED columns:
#
#   game_id
#   week
#   home_team
#   away_team
#
# completed-results file REQUIRED columns:
#
#   game_id
#   week
#   home_team
#   away_team
#   home_score
#   away_score
#
# Extra columns are preserved where practical.
#
# HOW "AS OF" WORKS
# -----------------
# If as_of_week is NULL:
#   every row with nonmissing home_score and away_score is treated
#   as completed, and the publication week is the latest completed
#   week.
#
# If as_of_week is a number:
#   only scored games with week <= as_of_week are treated as known.
#   This is useful for replaying historical weeks even when the
#   results file already contains the full season.
#
# OUTPUTS
# -------
#   version10_current_power_rankings.csv
#   version10_weekly_power_history.csv
#   version10_next_week_predictions.csv
#   version10_all_remaining_predictions.csv
#   version10_oos_predictions.csv
#   version10_oos_summary.csv
#   version10_oos_by_week.csv
#   version10_model_snapshot.csv
#
# ============================================================


# ============================================================
# 1. USER SETTINGS
# ============================================================

library(ggplot2)

# ------------------------------------------------------------
# Input files
# ------------------------------------------------------------
#
# These defaults point to the files created by Version 9A-H.
# Replace them with real files later without changing the model.
# ------------------------------------------------------------

schedule_file <- "version9A_H_schedule.csv"
results_file <- "version9A_H_simulated_results.csv"

# For the existing simulated full-season file, set a historical
# week here to demonstrate a live production snapshot.
#
# Example:
#   as_of_week <- 11
#
# For a real current-season results file containing only completed
# scores:
#   as_of_week <- NULL

as_of_week <- 11

output_prefix <- "version10"

# ------------------------------------------------------------
# Finalized model constants
# ------------------------------------------------------------

component_lambda <- 2
huber_k <- 1.5
recency_half_life <- 12

hfa_prior <- 2.5

max_huber_iterations <- 40
huber_tolerance <- 1e-6

# ------------------------------------------------------------
# Predictive margin uncertainty
# ------------------------------------------------------------

generic_margin_noise_prior_sd <- 8.5
margin_noise_prior_weight <- 20

# ------------------------------------------------------------
# Finalized empirical-logistic probability calibration
# ------------------------------------------------------------

win_probability_intercept <- 0.0192183
win_probability_slope <- 1.440608

probability_epsilon <- 1e-6

# ------------------------------------------------------------
# Pooled 95% rating-uncertainty calibration
# ------------------------------------------------------------
#
# x = average completed games per team
# y = pooled calibrated 95% half-width
#
# Values inherited from Version 9A-H2.
# ------------------------------------------------------------

rating_uncertainty_calibration <- data.frame(
  completed_games_per_team = 1:12,
  pooled_95_half_width = c(
    8.214061,
    7.119396,
    6.772461,
    6.288753,
    5.942193,
    5.623049,
    5.318552,
    5.166041,
    4.992024,
    4.945733,
    4.809558,
    4.865109
  )
)

rating_uncertainty_reference_teams <- 16
rating_uncertainty_reference_games <- 12


# ============================================================
# 2. BASIC INPUT VALIDATION
# ============================================================

require_columns <- function(
    data,
    required_columns,
    data_name
) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (
    length(
      missing_columns
    ) >
    0
  ) {
    
    stop(
      paste0(
        data_name,
        " is missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
  }
}


validate_unique_game_ids <- function(
    data,
    data_name
) {
  
  if (
    anyDuplicated(
      data$game_id
    ) >
    0
  ) {
    
    stop(
      paste0(
        data_name,
        " contains duplicate game_id values."
      )
    )
  }
}


# ============================================================
# 3. LOAD DATA
# ============================================================

if (
  !file.exists(
    schedule_file
  )
) {
  
  stop(
    paste0(
      "Schedule file not found: ",
      schedule_file
    )
  )
}

if (
  !file.exists(
    results_file
  )
) {
  
  stop(
    paste0(
      "Results file not found: ",
      results_file
    )
  )
}

schedule <- read.csv(
  schedule_file,
  stringsAsFactors = FALSE
)

results_raw <- read.csv(
  results_file,
  stringsAsFactors = FALSE
)

require_columns(
  schedule,
  c(
    "game_id",
    "week",
    "home_team",
    "away_team"
  ),
  "schedule"
)

require_columns(
  results_raw,
  c(
    "game_id",
    "week",
    "home_team",
    "away_team",
    "home_score",
    "away_score"
  ),
  "results"
)

validate_unique_game_ids(
  schedule,
  "schedule"
)

validate_unique_game_ids(
  results_raw,
  "results"
)

schedule$week <- as.integer(
  schedule$week
)

results_raw$week <- as.integer(
  results_raw$week
)

schedule$home_team <- as.character(
  schedule$home_team
)

schedule$away_team <- as.character(
  schedule$away_team
)

results_raw$home_team <- as.character(
  results_raw$home_team
)

results_raw$away_team <- as.character(
  results_raw$away_team
)

league_teams <- sort(
  unique(
    c(
      schedule$home_team,
      schedule$away_team,
      results_raw$home_team,
      results_raw$away_team
    )
  )
)

n_teams <- length(
  league_teams
)

if (
  n_teams !=
  rating_uncertainty_reference_teams
) {
  
  warning(
    paste0(
      "Pooled rating uncertainty was calibrated on ",
      rating_uncertainty_reference_teams,
      " teams, but the current data contain ",
      n_teams,
      " teams. Central ratings and forecasts remain valid, ",
      "but the published rating intervals should be recalibrated ",
      "before formal use in this league."
    )
  )
}


# ============================================================
# 4. DEFINE COMPLETED RESULTS / PUBLICATION WEEK
# ============================================================

scored_result <- (
  !is.na(
    results_raw$home_score
  ) &
    !is.na(
      results_raw$away_score
    )
)

if (
  is.null(
    as_of_week
  )
) {
  
  completed_games <- results_raw[
    scored_result,
  ]
  
  if (
    nrow(
      completed_games
    ) ==
    0
  ) {
    
    stop(
      "No completed scored games were found."
    )
  }
  
  publication_week <- max(
    completed_games$week
  )
  
} else {
  
  publication_week <- as.integer(
    as_of_week
  )
  
  completed_games <- results_raw[
    scored_result &
      results_raw$week <=
      publication_week,
  ]
  
  if (
    nrow(
      completed_games
    ) ==
    0
  ) {
    
    stop(
      paste0(
        "No completed games were found through week ",
        publication_week,
        "."
      )
    )
  }
}

completed_games$actual_margin <- (
  completed_games$home_score -
    completed_games$away_score
)

completed_games$actual_total <- (
  completed_games$home_score +
    completed_games$away_score
)

# Ensure completed game IDs exist in the official schedule.
unknown_game_ids <- setdiff(
  completed_games$game_id,
  schedule$game_id
)

if (
  length(
    unknown_game_ids
  ) >
  0
) {
  
  stop(
    paste0(
      "Completed-results file contains game_id values not found ",
      "in the schedule: ",
      paste(
        unknown_game_ids,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 5. HOME-FIELD SHRINKAGE
# ============================================================

get_hfa_lambda <- function(
    data,
    league_teams
) {
  
  average_games_played <- (
    2 *
      nrow(
        data
      )
  ) /
    length(
      league_teams
    )
  
  if (
    average_games_played <=
    2
  ) {
    
    return(10)
    
  } else if (
    average_games_played <=
    4
  ) {
    
    return(8)
    
  } else if (
    average_games_played <=
    6
  ) {
    
    return(5)
    
  } else {
    
    return(2)
  }
}


# ============================================================
# 6. RECENCY WEIGHTS
# ============================================================

make_recency_weights <- function(
    training_games,
    half_life =
      recency_half_life
) {
  
  if (
    nrow(
      training_games
    ) ==
    0
  ) {
    
    return(
      numeric(0)
    )
  }
  
  if (
    is.infinite(
      half_life
    )
  ) {
    
    return(
      rep(
        1,
        nrow(
          training_games
        )
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
  
  raw_weights / mean(
    raw_weights
  )
}


# ============================================================
# 7. SCORE OFFENSE / DEFENSE DESIGN MATRIX
# ============================================================

build_score_design <- function(
    data,
    league_teams
) {
  
  n_games <- nrow(
    data
  )
  
  n_teams <- length(
    league_teams
  )
  
  offense_columns <- paste0(
    "O__",
    league_teams
  )
  
  defense_columns <- paste0(
    "D__",
    league_teams
  )
  
  X <- matrix(
    0,
    nrow =
      2 *
      n_games,
    ncol =
      2 +
      2 *
      n_teams
  )
  
  colnames(X) <- c(
    "Intercept",
    "HFA",
    offense_columns,
    defense_columns
  )
  
  y <- numeric(
    2 *
      n_games
  )
  
  for (
    i in
    seq_len(
      n_games
    )
  ) {
    
    home_row <- 2 * i - 1
    away_row <- 2 * i
    
    home <- data$home_team[i]
    away <- data$away_team[i]
    
    X[home_row, "Intercept"] <- 1
    X[home_row, "HFA"] <- 0.5
    X[home_row, paste0("O__", home)] <- 1
    X[home_row, paste0("D__", away)] <- -1
    
    X[away_row, "Intercept"] <- 1
    X[away_row, "HFA"] <- -0.5
    X[away_row, paste0("O__", away)] <- 1
    X[away_row, paste0("D__", home)] <- -1
    
    y[home_row] <- data$home_score[i]
    y[away_row] <- data$away_score[i]
  }
  
  list(
    X = X,
    y = y,
    offense_columns =
      offense_columns,
    defense_columns =
      defense_columns
  )
}


# ============================================================
# 8. WEIGHTED RIDGE SOLVER
# ============================================================

solve_weighted_ridge <- function(
    X,
    y,
    observation_weights,
    penalty,
    prior_target
) {
  
  sqrt_weights <- sqrt(
    observation_weights
  )
  
  X_weighted <- sweep(
    X,
    MARGIN = 1,
    STATS =
      sqrt_weights,
    FUN = "*"
  )
  
  y_weighted <- (
    y *
      sqrt_weights
  )
  
  beta_matrix <- solve(
    crossprod(
      X_weighted
    ) +
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
  
  names(beta) <- colnames(
    X
  )
  
  beta
}


# ============================================================
# 9. HUBER OFFENSE / DEFENSE MODEL
# ============================================================

fit_score_offdef_huber <- function(
    data,
    league_teams,
    component_lambda,
    huber_k,
    hfa_lambda,
    hfa_prior,
    game_weights,
    max_iterations =
      max_huber_iterations,
    tolerance =
      huber_tolerance
) {
  
  design <- build_score_design(
    data =
      data,
    league_teams =
      league_teams
  )
  
  X <- design$X
  y <- design$y
  
  n_teams <- length(
    league_teams
  )
  
  base_weights <- rep(
    game_weights,
    each = 2
  )
  
  penalty <- diag(
    c(
      0,
      hfa_lambda,
      rep(
        component_lambda,
        2 *
          n_teams
      )
    )
  )
  
  prior_target <- c(
    0,
    hfa_prior,
    rep(
      0,
      2 *
        n_teams
    )
  )
  
  beta <- solve_weighted_ridge(
    X =
      X,
    y =
      y,
    observation_weights =
      base_weights,
    penalty =
      penalty,
    prior_target =
      prior_target
  )
  
  converged <- FALSE
  iterations_used <- 0
  
  for (
    iteration in
    seq_len(
      max_iterations
    )
  ) {
    
    iterations_used <- iteration
    
    residuals <- as.numeric(
      y -
        X %*%
        beta
    )
    
    residual_median <- median(
      residuals
    )
    
    robust_scale <- (
      1.4826 *
        median(
          abs(
            residuals -
              residual_median
          )
        )
    )
    
    if (
      !is.finite(
        robust_scale
      ) ||
      robust_scale <
      1e-8
    ) {
      
      robust_scale <- sd(
        residuals
      )
    }
    
    if (
      !is.finite(
        robust_scale
      ) ||
      robust_scale <
      1e-8
    ) {
      
      robust_scale <- 1
    }
    
    standardized_absolute_residual <- (
      abs(
        residuals
      ) /
        robust_scale
    )
    
    huber_weights <- rep(
      1,
      length(
        residuals
      )
    )
    
    large_residual <- (
      standardized_absolute_residual >
        huber_k
    )
    
    huber_weights[
      large_residual
    ] <- (
      huber_k /
        standardized_absolute_residual[
          large_residual
        ]
    )
    
    combined_weights <- (
      base_weights *
        huber_weights
    )
    
    new_beta <- solve_weighted_ridge(
      X =
        X,
      y =
        y,
      observation_weights =
        combined_weights,
      penalty =
        penalty,
      prior_target =
        prior_target
    )
    
    maximum_change <- max(
      abs(
        new_beta -
          beta
      )
    )
    
    beta <- new_beta
    
    if (
      maximum_change <
      tolerance
    ) {
      
      converged <- TRUE
      break
    }
  }
  
  offense <- beta[
    design$offense_columns
  ]
  
  defense <- beta[
    design$defense_columns
  ]
  
  names(
    offense
  ) <- league_teams
  
  names(
    defense
  ) <- league_teams
  
  list(
    offense =
      offense,
    defense =
      defense,
    hfa =
      as.numeric(
        beta["HFA"]
      ),
    league_mean =
      as.numeric(
        beta["Intercept"]
      ),
    converged =
      converged,
    iterations =
      iterations_used
  )
}


# ============================================================
# 10. CENTER OFFENSE / DEFENSE COMPONENTS
# ============================================================

center_rating_components <- function(
    fit,
    league_teams
) {
  
  offense_rating <- fit$offense[
    league_teams
  ]
  
  defense_rating <- fit$defense[
    league_teams
  ]
  
  offense_mean <- mean(
    offense_rating
  )
  
  defense_mean <- mean(
    defense_rating
  )
  
  offense_rating <- (
    offense_rating -
      offense_mean
  )
  
  defense_rating <- (
    defense_rating -
      defense_mean
  )
  
  rating <- (
    offense_rating +
      defense_rating
  )
  
  rating <- (
    rating -
      mean(
        rating
      )
  )
  
  adjusted_league_mean_points <- (
    fit$league_mean +
      offense_mean -
      defense_mean
  )
  
  names(
    offense_rating
  ) <- league_teams
  
  names(
    defense_rating
  ) <- league_teams
  
  names(
    rating
  ) <- league_teams
  
  list(
    offense_rating =
      offense_rating,
    defense_rating =
      defense_rating,
    rating =
      rating,
    league_mean_points =
      adjusted_league_mean_points
  )
}


# ============================================================
# 11. FIT FINAL VERSION 10 POWER MODEL
# ============================================================

fit_power_model <- function(
    training_games,
    league_teams
) {
  
  if (
    nrow(
      training_games
    ) ==
    0
  ) {
    
    stop(
      "Cannot fit the finalized team model with zero completed games."
    )
  }
  
  hfa_lambda <- get_hfa_lambda(
    data =
      training_games,
    league_teams =
      league_teams
  )
  
  game_weights <- make_recency_weights(
    training_games =
      training_games,
    half_life =
      recency_half_life
  )
  
  raw_fit <- fit_score_offdef_huber(
    data =
      training_games,
    league_teams =
      league_teams,
    component_lambda =
      component_lambda,
    huber_k =
      huber_k,
    hfa_lambda =
      hfa_lambda,
    hfa_prior =
      hfa_prior,
    game_weights =
      game_weights
  )
  
  centered <- center_rating_components(
    fit =
      raw_fit,
    league_teams =
      league_teams
  )
  
  list(
    ratings =
      centered$rating,
    offense_rating =
      centered$offense_rating,
    defense_rating =
      centered$defense_rating,
    hfa =
      raw_fit$hfa,
    league_mean_points =
      centered$league_mean_points,
    hfa_lambda =
      hfa_lambda,
    component_lambda =
      component_lambda,
    huber_k =
      huber_k,
    recency_half_life =
      recency_half_life,
    converged =
      raw_fit$converged,
    iterations =
      raw_fit$iterations
  )
}


# ============================================================
# 12. RATING UNCERTAINTY LOOKUP
# ============================================================

get_pooled_rating_half_width <- function(
    training_games,
    league_teams
) {
  
  average_games_per_team <- (
    2 *
      nrow(
        training_games
      )
  ) /
    length(
      league_teams
    )
  
  minimum_calibrated_games <- min(
    rating_uncertainty_calibration$completed_games_per_team
  )
  
  maximum_calibrated_games <- max(
    rating_uncertainty_calibration$completed_games_per_team
  )
  
  if (
    average_games_per_team <
    minimum_calibrated_games ||
    average_games_per_team >
    maximum_calibrated_games
  ) {
    
    warning(
      paste0(
        "Average completed games per team = ",
        round(
          average_games_per_team,
          3
        ),
        " is outside the pooled uncertainty calibration range [",
        minimum_calibrated_games,
        ", ",
        maximum_calibrated_games,
        "]. Rating interval columns will be NA."
      )
    )
    
    return(
      NA_real_
    )
  }
  
  interpolated <- approx(
    x =
      rating_uncertainty_calibration$completed_games_per_team,
    y =
      rating_uncertainty_calibration$pooled_95_half_width,
    xout =
      average_games_per_team,
    method =
      "linear",
    rule =
      1
  )$y
  
  as.numeric(
    interpolated
  )
}


# ============================================================
# 13. RECORD TABLE
# ============================================================

calculate_records <- function(
    training_games,
    league_teams
) {
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    current_team <- league_teams[i]
    
    team_games <- training_games[
      training_games$home_team ==
        current_team |
        training_games$away_team ==
        current_team,
    ]
    
    wins <- 0
    losses <- 0
    ties <- 0
    
    if (
      nrow(
        team_games
      ) >
      0
    ) {
      
      home_wins <- (
        team_games$home_team ==
          current_team &
          team_games$actual_margin >
          0
      )
      
      away_wins <- (
        team_games$away_team ==
          current_team &
          team_games$actual_margin <
          0
      )
      
      ties_vector <- (
        team_games$actual_margin ==
          0
      )
      
      wins <- sum(
        home_wins |
          away_wins
      )
      
      ties <- sum(
        ties_vector
      )
      
      losses <- (
        nrow(
          team_games
        ) -
          wins -
          ties
      )
    }
    
    games_played <- (
      wins +
        losses +
        ties
    )
    
    win_pct <- if (
      games_played >
      0
    ) {
      
      (
        wins +
          0.5 *
          ties
      ) /
        games_played
      
    } else {
      
      NA_real_
    }
    
    record <- if (
      ties >
      0
    ) {
      
      paste0(
        wins,
        "-",
        losses,
        "-",
        ties
      )
      
    } else {
      
      paste0(
        wins,
        "-",
        losses
      )
    }
    
    rows[[i]] <- data.frame(
      team =
        current_team,
      wins =
        wins,
      losses =
        losses,
      ties =
        ties,
      games_played =
        games_played,
      record =
        record,
      win_pct =
        win_pct,
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


# ============================================================
# 14. STRENGTH OF SCHEDULE
# ============================================================

calculate_sos <- function(
    schedule,
    training_games,
    publication_week,
    current_ratings,
    estimated_hfa,
    league_teams
) {
  
  played_ids <- training_games$game_id
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    current_team <- league_teams[i]
    
    team_schedule <- schedule[
      schedule$home_team ==
        current_team |
        schedule$away_team ==
        current_team,
    ]
    
    team_schedule$opponent <- ifelse(
      team_schedule$home_team ==
        current_team,
      team_schedule$away_team,
      team_schedule$home_team
    )
    
    team_schedule$is_home <- (
      team_schedule$home_team ==
        current_team
    )
    
    team_schedule$played <- (
      team_schedule$game_id %in%
        played_ids
    )
    
    played_games <- team_schedule[
      team_schedule$played,
    ]
    
    remaining_games <- team_schedule[
      !team_schedule$played &
        team_schedule$week >
        publication_week,
    ]
    
    # If the schedule contains future games in the same publication
    # week, keep them as remaining rather than dropping them.
    same_week_unplayed <- team_schedule[
      !team_schedule$played &
        team_schedule$week ==
        publication_week,
    ]
    
    if (
      nrow(
        same_week_unplayed
      ) >
      0
    ) {
      
      remaining_games <- rbind(
        remaining_games,
        same_week_unplayed
      )
    }
    
    played_sos <- if (
      nrow(
        played_games
      ) >
      0
    ) {
      
      mean(
        current_ratings[
          played_games$opponent
        ]
      )
      
    } else {
      
      NA_real_
    }
    
    remaining_sos <- if (
      nrow(
        remaining_games
      ) >
      0
    ) {
      
      mean(
        current_ratings[
          remaining_games$opponent
        ]
      )
      
    } else {
      
      NA_real_
    }
    
    full_sos <- if (
      nrow(
        team_schedule
      ) >
      0
    ) {
      
      mean(
        current_ratings[
          team_schedule$opponent
        ]
      )
      
    } else {
      
      NA_real_
    }
    
    played_location_difficulty <- if (
      nrow(
        played_games
      ) >
      0
    ) {
      
      location_values <- ifelse(
        played_games$is_home,
        current_ratings[
          played_games$opponent
        ] -
          estimated_hfa,
        current_ratings[
          played_games$opponent
        ] +
          estimated_hfa
      )
      
      mean(
        location_values
      )
      
    } else {
      
      NA_real_
    }
    
    remaining_location_difficulty <- if (
      nrow(
        remaining_games
      ) >
      0
    ) {
      
      location_values <- ifelse(
        remaining_games$is_home,
        current_ratings[
          remaining_games$opponent
        ] -
          estimated_hfa,
        current_ratings[
          remaining_games$opponent
        ] +
          estimated_hfa
      )
      
      mean(
        location_values
      )
      
    } else {
      
      NA_real_
    }
    
    rows[[i]] <- data.frame(
      team =
        current_team,
      played_SOS =
        played_sos,
      remaining_SOS =
        remaining_sos,
      full_SOS =
        full_sos,
      played_location_difficulty =
        played_location_difficulty,
      remaining_location_difficulty =
        remaining_location_difficulty,
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


# ============================================================
# 15. PUBLICATION STAGE
# ============================================================

get_publication_stage <- function(
    average_games_per_team
) {
  
  if (
    average_games_per_team <=
    2
  ) {
    
    return(
      "Provisional"
    )
    
  }
  
  if (
    average_games_per_team <=
    4
  ) {
    
    return(
      "Early Season"
    )
  }
  
  "Standard"
}


# ============================================================
# 16. BUILD ONE POWER-RATING SNAPSHOT
# ============================================================

build_power_snapshot <- function(
    training_games,
    schedule,
    publication_week,
    league_teams,
    previous_snapshot = NULL
) {
  
  model <- fit_power_model(
    training_games =
      training_games,
    league_teams =
      league_teams
  )
  
  records <- calculate_records(
    training_games =
      training_games,
    league_teams =
      league_teams
  )
  
  sos <- calculate_sos(
    schedule =
      schedule,
    training_games =
      training_games,
    publication_week =
      publication_week,
    current_ratings =
      model$ratings,
    estimated_hfa =
      model$hfa,
    league_teams =
      league_teams
  )
  
  half_width <- get_pooled_rating_half_width(
    training_games =
      training_games,
    league_teams =
      league_teams
  )
  
  average_games_per_team <- (
    2 *
      nrow(
        training_games
      )
  ) /
    length(
      league_teams
    )
  
  table <- data.frame(
    week =
      publication_week,
    team =
      league_teams,
    rating =
      as.numeric(
        model$ratings[
          league_teams
        ]
      ),
    offense_rating =
      as.numeric(
        model$offense_rating[
          league_teams
        ]
      ),
    defense_rating =
      as.numeric(
        model$defense_rating[
          league_teams
        ]
      ),
    stringsAsFactors =
      FALSE
  )
  
  table$rating_lower_95 <- (
    table$rating -
      half_width
  )
  
  table$rating_upper_95 <- (
    table$rating +
      half_width
  )
  
  table$uncertainty_95_half_width <-
    half_width
  
  table <- merge(
    table,
    records,
    by =
      "team",
    all.x =
      TRUE
  )
  
  table <- merge(
    table,
    sos,
    by =
      "team",
    all.x =
      TRUE
  )
  
  table$rank <- as.integer(
    rank(
      -table$rating,
      ties.method =
        "min"
    )
  )
  
  table$offense_rank <- as.integer(
    rank(
      -table$offense_rating,
      ties.method =
        "min"
    )
  )
  
  table$defense_rank <- as.integer(
    rank(
      -table$defense_rating,
      ties.method =
        "min"
    )
  )
  
  table$previous_rank <- NA_integer_
  table$rank_change <- NA_integer_
  
  table$previous_rating <- NA_real_
  table$rating_change <- NA_real_
  
  table$previous_offense_rating <- NA_real_
  table$offense_rating_change <- NA_real_
  
  table$previous_defense_rating <- NA_real_
  table$defense_rating_change <- NA_real_
  
  if (
    !is.null(
      previous_snapshot
    )
  ) {
    
    previous_lookup <- previous_snapshot[
      ,
      c(
        "team",
        "rank",
        "rating",
        "offense_rating",
        "defense_rating"
      )
    ]
    
    names(
      previous_lookup
    ) <- c(
      "team",
      "previous_rank_lookup",
      "previous_rating_lookup",
      "previous_offense_lookup",
      "previous_defense_lookup"
    )
    
    table <- merge(
      table,
      previous_lookup,
      by =
        "team",
      all.x =
        TRUE
    )
    
    table$previous_rank <-
      table$previous_rank_lookup
    
    table$rank_change <- (
      table$previous_rank -
        table$rank
    )
    
    table$previous_rating <-
      table$previous_rating_lookup
    
    table$rating_change <- (
      table$rating -
        table$previous_rating
    )
    
    table$previous_offense_rating <-
      table$previous_offense_lookup
    
    table$offense_rating_change <- (
      table$offense_rating -
        table$previous_offense_rating
    )
    
    table$previous_defense_rating <-
      table$previous_defense_lookup
    
    table$defense_rating_change <- (
      table$defense_rating -
        table$previous_defense_rating
    )
    
    table$previous_rank_lookup <- NULL
    table$previous_rating_lookup <- NULL
    table$previous_offense_lookup <- NULL
    table$previous_defense_lookup <- NULL
  }
  
  table$publication_stage <-
    get_publication_stage(
      average_games_per_team
    )
  
  table$estimated_HFA <-
    model$hfa
  
  table$league_mean_points <-
    model$league_mean_points
  
  table$average_games_per_team <-
    average_games_per_team
  
  table$component_lambda <-
    component_lambda
  
  table$huber_k <-
    huber_k
  
  table$recency_half_life <-
    recency_half_life
  
  table$hfa_lambda <-
    model$hfa_lambda
  
  table$model_converged <-
    model$converged
  
  table$model_iterations <-
    model$iterations
  
  table <- table[
    order(
      table$rank,
      -table$rating,
      table$team
    ),
  ]
  
  rownames(
    table
  ) <- NULL
  
  table <- table[
    ,
    c(
      "week",
      "publication_stage",
      "rank",
      "team",
      "rating",
      "rating_lower_95",
      "rating_upper_95",
      "uncertainty_95_half_width",
      "offense_rating",
      "offense_rank",
      "defense_rating",
      "defense_rank",
      "record",
      "wins",
      "losses",
      "ties",
      "games_played",
      "win_pct",
      "previous_rank",
      "rank_change",
      "previous_rating",
      "rating_change",
      "previous_offense_rating",
      "offense_rating_change",
      "previous_defense_rating",
      "defense_rating_change",
      "played_SOS",
      "remaining_SOS",
      "full_SOS",
      "played_location_difficulty",
      "remaining_location_difficulty",
      "estimated_HFA",
      "league_mean_points",
      "average_games_per_team",
      "component_lambda",
      "huber_k",
      "recency_half_life",
      "hfa_lambda",
      "model_converged",
      "model_iterations"
    )
  ]
  
  list(
    table =
      table,
    model =
      model
  )
}


# ============================================================
# 17. WEEKLY POWER-RATING HISTORY
# ============================================================

build_weekly_history <- function(
    completed_games,
    schedule,
    through_week,
    league_teams
) {
  
  history_storage <- list()
  settings_storage <- list()
  
  previous_snapshot <- NULL
  history_position <- 1
  settings_position <- 1
  
  for (
    current_week in
    seq_len(
      through_week
    )
  ) {
    
    current_games <- completed_games[
      completed_games$week <=
        current_week,
    ]
    
    if (
      nrow(
        current_games
      ) ==
      0
    ) {
      
      next
    }
    
    result <- build_power_snapshot(
      training_games =
        current_games,
      schedule =
        schedule,
      publication_week =
        current_week,
      league_teams =
        league_teams,
      previous_snapshot =
        previous_snapshot
    )
    
    history_storage[[history_position]] <-
      result$table
    
    settings_storage[[settings_position]] <-
      data.frame(
        week =
          current_week,
        estimated_HFA =
          result$model$hfa,
        league_mean_points =
          result$model$league_mean_points,
        average_games_per_team =
          (
            2 *
              nrow(
                current_games
              )
          ) /
          length(
            league_teams
          ),
        model_converged =
          result$model$converged,
        model_iterations =
          result$model$iterations,
        stringsAsFactors =
          FALSE
      )
    
    previous_snapshot <- result$table
    
    history_position <-
      history_position +
      1
    
    settings_position <-
      settings_position +
      1
  }
  
  list(
    history =
      do.call(
        rbind,
        history_storage
      ),
    settings =
      do.call(
        rbind,
        settings_storage
      )
  )
}


# ============================================================
# 18. EXPECTED SCORE / MARGIN PREDICTIONS
# ============================================================

predict_games_from_model <- function(
    model,
    target_schedule
) {
  
  if (
    nrow(
      target_schedule
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  home_team <- target_schedule$home_team
  away_team <- target_schedule$away_team
  
  expected_home_score <- (
    model$league_mean_points +
      model$offense_rating[
        home_team
      ] -
      model$defense_rating[
        away_team
      ] +
      model$hfa /
      2
  )
  
  expected_away_score <- (
    model$league_mean_points +
      model$offense_rating[
        away_team
      ] -
      model$defense_rating[
        home_team
      ] -
      model$hfa /
      2
  )
  
  expected_margin <- (
    expected_home_score -
      expected_away_score
  )
  
  rating_identity_margin <- (
    model$ratings[
      home_team
    ] -
      model$ratings[
        away_team
      ] +
      model$hfa
  )
  
  if (
    max(
      abs(
        expected_margin -
        rating_identity_margin
      )
    ) >
    1e-7
  ) {
    
    stop(
      "Expected-score and rating-margin identities do not agree."
    )
  }
  
  output <- target_schedule
  
  output$predicted_home_score <-
    as.numeric(
      expected_home_score
    )
  
  output$predicted_away_score <-
    as.numeric(
      expected_away_score
    )
  
  output$predicted_margin <-
    as.numeric(
      expected_margin
    )
  
  output$predicted_total <- (
    output$predicted_home_score +
      output$predicted_away_score
  )
  
  output$pregame_home_rating <-
    as.numeric(
      model$ratings[
        home_team
      ]
    )
  
  output$pregame_away_rating <-
    as.numeric(
      model$ratings[
        away_team
      ]
    )
  
  output$pregame_home_offense <-
    as.numeric(
      model$offense_rating[
        home_team
      ]
    )
  
  output$pregame_away_offense <-
    as.numeric(
      model$offense_rating[
        away_team
      ]
    )
  
  output$pregame_home_defense <-
    as.numeric(
      model$defense_rating[
        home_team
      ]
    )
  
  output$pregame_away_defense <-
    as.numeric(
      model$defense_rating[
        away_team
      ]
    )
  
  output$pregame_estimated_HFA <-
    model$hfa
  
  output$pregame_league_mean_points <-
    model$league_mean_points
  
  output
}


# ============================================================
# 19. PREDICTIVE MARGIN SD
# ============================================================

calculate_training_margin_residuals <- function(
    model,
    training_games
) {
  
  fitted_games <- predict_games_from_model(
    model =
      model,
    target_schedule =
      training_games
  )
  
  (
    training_games$actual_margin -
      fitted_games$predicted_margin
  )
}


estimate_predictive_margin_sd <- function(
    model,
    training_games
) {
  
  residuals <- calculate_training_margin_residuals(
    model =
      model,
    training_games =
      training_games
  )
  
  n_residuals <- length(
    residuals
  )
  
  if (
    n_residuals <=
    1
  ) {
    
    return(
      generic_margin_noise_prior_sd
    )
  }
  
  empirical_variance <- mean(
    residuals^2
  )
  
  shrunk_variance <- (
    margin_noise_prior_weight *
      generic_margin_noise_prior_sd^2 +
      n_residuals *
      empirical_variance
  ) /
    (
      margin_noise_prior_weight +
        n_residuals
    )
  
  sqrt(
    shrunk_variance
  )
}


# ============================================================
# 20. FINAL EMPIRICAL-LOGISTIC WIN PROBABILITY
# ============================================================

clip_probability <- function(
    probability
) {
  
  pmin(
    pmax(
      probability,
      probability_epsilon
    ),
    1 -
      probability_epsilon
  )
}


margin_to_home_win_probability <- function(
    predicted_margin,
    predictive_margin_sd
) {
  
  standardized_margin <- (
    predicted_margin /
      predictive_margin_sd
  )
  
  linear_predictor <- (
    win_probability_intercept +
      win_probability_slope *
      standardized_margin
  )
  
  clip_probability(
    plogis(
      linear_predictor
    )
  )
}


# ============================================================
# 21. ADD WIN PROBABILITIES TO PREDICTION TABLE
# ============================================================

add_probability_forecasts <- function(
    predictions,
    predictive_margin_sd
) {
  
  if (
    nrow(
      predictions
    ) ==
    0
  ) {
    
    return(
      predictions
    )
  }
  
  predictions$predictive_margin_sd <-
    predictive_margin_sd
  
  predictions$standardized_predicted_margin <- (
    predictions$predicted_margin /
      predictive_margin_sd
  )
  
  predictions$home_win_probability <-
    margin_to_home_win_probability(
      predicted_margin =
        predictions$predicted_margin,
      predictive_margin_sd =
        predictive_margin_sd
    )
  
  predictions$away_win_probability <- (
    1 -
      predictions$home_win_probability
  )
  
  predictions$predicted_winner <- ifelse(
    predictions$home_win_probability >=
      0.5,
    predictions$home_team,
    predictions$away_team
  )
  
  predictions$predicted_winner_probability <- pmax(
    predictions$home_win_probability,
    predictions$away_win_probability
  )
  
  predictions$probability_calibration_intercept <-
    win_probability_intercept
  
  predictions$probability_calibration_slope <-
    win_probability_slope
  
  predictions
}


# ============================================================
# 22. CURRENT MODEL + UPCOMING PREDICTIONS
# ============================================================

# Build the immediately preceding calendar-week snapshot so the
# current publication table can report genuine week-over-week
# rank/rating/component movement.
previous_current_snapshot <- NULL

if (
  publication_week >
  min(
    completed_games$week
  )
) {
  
  previous_training_games <- completed_games[
    completed_games$week <
      publication_week,
  ]
  
  if (
    nrow(
      previous_training_games
    ) >
    0
  ) {
    
    previous_current_snapshot_result <-
      build_power_snapshot(
        training_games =
          previous_training_games,
        schedule =
          schedule,
        publication_week =
          publication_week -
          1,
        league_teams =
          league_teams
      )
    
    previous_current_snapshot <-
      previous_current_snapshot_result$table
  }
}

current_snapshot_result <- build_power_snapshot(
  training_games =
    completed_games,
  schedule =
    schedule,
  publication_week =
    publication_week,
  league_teams =
    league_teams,
  previous_snapshot =
    previous_current_snapshot
)

current_power_rankings <-
  current_snapshot_result$table

current_model <-
  current_snapshot_result$model

current_predictive_margin_sd <-
  estimate_predictive_margin_sd(
    model =
      current_model,
    training_games =
      completed_games
  )

remaining_schedule <- schedule[
  !schedule$game_id %in%
    completed_games$game_id,
]

if (
  nrow(
    remaining_schedule
  ) >
  0
) {
  
  remaining_schedule <- remaining_schedule[
    order(
      remaining_schedule$week,
      remaining_schedule$game_id
    ),
  ]
  
  all_remaining_predictions <-
    predict_games_from_model(
      model =
        current_model,
      target_schedule =
        remaining_schedule
    )
  
  all_remaining_predictions <-
    add_probability_forecasts(
      predictions =
        all_remaining_predictions,
      predictive_margin_sd =
        current_predictive_margin_sd
    )
  
  next_week <- min(
    remaining_schedule$week
  )
  
  next_week_predictions <-
    all_remaining_predictions[
      all_remaining_predictions$week ==
        next_week,
    ]
  
} else {
  
  next_week <- NA_integer_
  next_week_predictions <- data.frame()
  all_remaining_predictions <- data.frame()
}


# ============================================================
# 23. WALK-FORWARD OUT-OF-SAMPLE EVALUATION
# ============================================================

create_oos_predictions <- function(
    completed_games,
    league_teams
) {
  
  active_weeks <- sort(
    unique(
      completed_games$week
    )
  )
  
  if (
    length(
      active_weeks
    ) <
    2
  ) {
    
    return(
      data.frame()
    )
  }
  
  forecast_weeks <- active_weeks[
    active_weeks >
      min(
        active_weeks
      )
  ]
  
  prediction_storage <- list()
  prediction_position <- 1
  
  for (
    target_week in
    forecast_weeks
  ) {
    
    training_games <- completed_games[
      completed_games$week <
        target_week,
    ]
    
    target_games <- completed_games[
      completed_games$week ==
        target_week,
    ]
    
    if (
      nrow(
        training_games
      ) ==
      0 ||
      nrow(
        target_games
      ) ==
      0
    ) {
      
      next
    }
    
    model <- fit_power_model(
      training_games =
        training_games,
      league_teams =
        league_teams
    )
    
    predictive_margin_sd <-
      estimate_predictive_margin_sd(
        model =
          model,
        training_games =
          training_games
      )
    
    predictions <- predict_games_from_model(
      model =
        model,
      target_schedule =
        target_games
    )
    
    predictions <- add_probability_forecasts(
      predictions =
        predictions,
      predictive_margin_sd =
        predictive_margin_sd
    )
    
    predictions$actual_margin <- (
      target_games$home_score -
        target_games$away_score
    )
    
    predictions$actual_total <- (
      target_games$home_score +
        target_games$away_score
    )
    
    predictions$margin_error <- (
      predictions$predicted_margin -
        predictions$actual_margin
    )
    
    predictions$absolute_margin_error <- abs(
      predictions$margin_error
    )
    
    predictions$squared_margin_error <- (
      predictions$margin_error^2
    )
    
    predictions$home_score_error <- (
      predictions$predicted_home_score -
        target_games$home_score
    )
    
    predictions$away_score_error <- (
      predictions$predicted_away_score -
        target_games$away_score
    )
    
    predictions$total_score_error <- (
      predictions$predicted_total -
        predictions$actual_total
    )
    
    predictions$actual_home_win_soft <- ifelse(
      predictions$actual_margin >
        0,
      1,
      ifelse(
        predictions$actual_margin <
          0,
        0,
        0.5
      )
    )
    
    predictions$actual_home_win_non_tie <- ifelse(
      predictions$actual_margin >
        0,
      1,
      ifelse(
        predictions$actual_margin <
          0,
        0,
        NA_real_
      )
    )
    
    predictions$winner_correct <- ifelse(
      is.na(
        predictions$actual_home_win_non_tie
      ),
      NA_real_,
      as.numeric(
        (
          predictions$home_win_probability >=
            0.5
        ) ==
          (
            predictions$actual_home_win_non_tie ==
              1
          )
      )
    )
    
    predictions$brier_score <- (
      predictions$home_win_probability -
        predictions$actual_home_win_soft
    )^2
    
    predictions$log_loss <- ifelse(
      is.na(
        predictions$actual_home_win_non_tie
      ),
      NA_real_,
      -(
        predictions$actual_home_win_non_tie *
          log(
            predictions$home_win_probability
          ) +
          (
            1 -
              predictions$actual_home_win_non_tie
          ) *
          log(
            1 -
              predictions$home_win_probability
          )
      )
    )
    
    prediction_storage[[prediction_position]] <-
      predictions
    
    prediction_position <-
      prediction_position +
      1
  }
  
  if (
    length(
      prediction_storage
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  output <- do.call(
    rbind,
    prediction_storage
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


summarize_oos_predictions <- function(
    predictions
) {
  
  if (
    nrow(
      predictions
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  non_tie <- predictions[
    !is.na(
      predictions$winner_correct
    ),
  ]
  
  score_errors <- c(
    predictions$home_score_error,
    predictions$away_score_error
  )
  
  data.frame(
    forecast_games =
      nrow(
        predictions
      ),
    non_tied_forecast_games =
      nrow(
        non_tie
      ),
    margin_MAE =
      mean(
        predictions$absolute_margin_error
      ),
    margin_RMSE =
      sqrt(
        mean(
          predictions$squared_margin_error
        )
      ),
    margin_bias =
      mean(
        predictions$margin_error
      ),
    margin_correlation =
      if (
        sd(
          predictions$predicted_margin
        ) ==
        0 ||
        sd(
          predictions$actual_margin
        ) ==
        0
      ) {
        NA_real_
      } else {
        cor(
          predictions$predicted_margin,
          predictions$actual_margin
        )
      },
    all_score_MAE =
      mean(
        abs(
          score_errors
        )
      ),
    all_score_RMSE =
      sqrt(
        mean(
          score_errors^2
        )
      ),
    total_score_MAE =
      mean(
        abs(
          predictions$total_score_error
        )
      ),
    total_score_RMSE =
      sqrt(
        mean(
          predictions$total_score_error^2
        )
      ),
    winner_accuracy =
      if (
        nrow(
          non_tie
        ) ==
        0
      ) {
        NA_real_
      } else {
        mean(
          non_tie$winner_correct
        )
      },
    brier_score =
      mean(
        predictions$brier_score
      ),
    log_loss =
      mean(
        predictions$log_loss,
        na.rm =
          TRUE
      ),
    mean_predicted_home_win_probability =
      mean(
        predictions$home_win_probability
      ),
    actual_home_win_rate_soft =
      mean(
        predictions$actual_home_win_soft
      ),
    stringsAsFactors =
      FALSE
  )
}


summarize_oos_by_week <- function(
    predictions
) {
  
  if (
    nrow(
      predictions
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  weeks <- sort(
    unique(
      predictions$week
    )
  )
  
  rows <- list()
  
  for (
    i in
    seq_along(
      weeks
    )
  ) {
    
    current_week <- weeks[i]
    
    current <- predictions[
      predictions$week ==
        current_week,
    ]
    
    non_tie <- current[
      !is.na(
        current$winner_correct
      ),
    ]
    
    rows[[i]] <- data.frame(
      week =
        current_week,
      games =
        nrow(
          current
        ),
      margin_MAE =
        mean(
          current$absolute_margin_error
        ),
      margin_RMSE =
        sqrt(
          mean(
            current$squared_margin_error
          )
        ),
      margin_bias =
        mean(
          current$margin_error
        ),
      winner_accuracy =
        if (
          nrow(
            non_tie
          ) ==
          0
        ) {
          NA_real_
        } else {
          mean(
            non_tie$winner_correct
          )
        },
      brier_score =
        mean(
          current$brier_score
        ),
      log_loss =
        mean(
          current$log_loss,
          na.rm =
            TRUE
        ),
      mean_predictive_margin_sd =
        mean(
          current$predictive_margin_sd
        ),
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


oos_predictions <- create_oos_predictions(
  completed_games =
    completed_games,
  league_teams =
    league_teams
)

oos_summary <- summarize_oos_predictions(
  predictions =
    oos_predictions
)

oos_by_week <- summarize_oos_by_week(
  predictions =
    oos_predictions
)


# ============================================================
# 24. WEEKLY RATING HISTORY
# ============================================================

weekly_history_result <- build_weekly_history(
  completed_games =
    completed_games,
  schedule =
    schedule,
  through_week =
    publication_week,
  league_teams =
    league_teams
)

weekly_power_history <-
  weekly_history_result$history

weekly_model_history <-
  weekly_history_result$settings


# ============================================================
# 25. MODEL SNAPSHOT
# ============================================================

model_snapshot <- data.frame(
  publication_week =
    publication_week,
  completed_games =
    nrow(
      completed_games
    ),
  teams =
    n_teams,
  average_games_per_team =
    (
      2 *
        nrow(
          completed_games
        )
    ) /
    n_teams,
  estimated_HFA =
    current_model$hfa,
  league_mean_points =
    current_model$league_mean_points,
  predictive_margin_sd =
    current_predictive_margin_sd,
  component_lambda =
    component_lambda,
  huber_k =
    huber_k,
  recency_half_life =
    recency_half_life,
  hfa_prior =
    hfa_prior,
  hfa_lambda =
    current_model$hfa_lambda,
  model_converged =
    current_model$converged,
  model_iterations =
    current_model$iterations,
  win_probability_intercept =
    win_probability_intercept,
  win_probability_slope =
    win_probability_slope,
  rating_uncertainty_reference_teams =
    rating_uncertainty_reference_teams,
  rating_uncertainty_reference_games =
    rating_uncertainty_reference_games,
  stringsAsFactors =
    FALSE
)


# ============================================================
# 26. TEAM COLORS
# ============================================================

team_colors <- setNames(
  grDevices::hcl.colors(
    length(
      league_teams
    ),
    palette =
      "Dark 3"
  ),
  league_teams
)


# ============================================================
# 27. PLOTS
# ============================================================

current_plot_data <- current_power_rankings

current_plot_data$team_factor <- factor(
  current_plot_data$team,
  levels =
    rev(
      current_plot_data$team[
        order(
          current_plot_data$rank
        )
      ]
    )
)

current_rating_plot <- ggplot(
  current_plot_data,
  aes(
    x =
      team_factor,
    y =
      rating,
    fill =
      team
  )
) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept =
      0,
    linetype =
      "dashed"
  ) +
  scale_fill_manual(
    values =
      team_colors
  ) +
  guides(
    fill =
      "none"
  ) +
  labs(
    title = paste0(
      "Version 10 Power Ratings - Through Week ",
      publication_week
    ),
    subtitle =
      "Neutral-field points above/below league average",
    x =
      "Team",
    y =
      "Overall Rating"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_current_power_ratings.png"
  ),
  plot =
    current_rating_plot,
  width =
    10,
  height =
    8,
  dpi =
    170
)


rating_history_plot <- ggplot(
  weekly_power_history,
  aes(
    x =
      week,
    y =
      rating,
    color =
      team,
    group =
      team
  )
) +
  geom_line(
    linewidth =
      0.9
  ) +
  geom_point(
    size =
      1.5
  ) +
  geom_hline(
    yintercept =
      0,
    linetype =
      "dashed"
  ) +
  scale_color_manual(
    values =
      team_colors
  ) +
  labs(
    title =
      "Version 10 Weekly Power-Rating History",
    x =
      "Week",
    y =
      "Overall Rating",
    color =
      "Team"
  ) +
  theme_minimal()

ggsave(
  paste0(
    output_prefix,
    "_rating_history.png"
  ),
  plot =
    rating_history_plot,
  width =
    13,
  height =
    8,
  dpi =
    170
)


if (
  nrow(
    next_week_predictions
  ) >
  0
) {
  
  next_game_plot_data <-
    next_week_predictions
  
  next_game_plot_data$matchup <- paste0(
    next_game_plot_data$away_team,
    " @ ",
    next_game_plot_data$home_team
  )
  
  next_game_plot_data$matchup <- factor(
    next_game_plot_data$matchup,
    levels =
      next_game_plot_data$matchup[
        order(
          next_game_plot_data$home_win_probability
        )
      ]
  )
  
  next_game_probability_plot <- ggplot(
    next_game_plot_data,
    aes(
      x =
        matchup,
      y =
        home_win_probability
    )
  ) +
    geom_col() +
    coord_flip() +
    geom_hline(
      yintercept =
        0.5,
      linetype =
        "dashed"
    ) +
    scale_y_continuous(
      limits =
        c(
          0,
          1
        )
    ) +
    labs(
      title = paste0(
        "Version 10 Next-Week Home Win Probabilities - Week ",
        next_week
      ),
      subtitle =
        "Empirical-logistic calibration from Version 9C-MC",
      x =
        "Matchup",
      y =
        "Home Win Probability"
    ) +
    theme_minimal()
  
  ggsave(
    paste0(
      output_prefix,
      "_next_week_win_probabilities.png"
    ),
    plot =
      next_game_probability_plot,
    width =
      11,
    height =
      8,
    dpi =
      170
  )
}


if (
  nrow(
    oos_predictions
  ) >
  0
) {
  
  oos_margin_plot <- ggplot(
    oos_predictions,
    aes(
      x =
        predicted_margin,
      y =
        actual_margin
    )
  ) +
    geom_abline(
      intercept =
        0,
      slope =
        1,
      linetype =
        "dashed"
    ) +
    geom_point(
      size =
        2
    ) +
    labs(
      title =
        "Version 10 Walk-Forward Predicted vs Actual Margin",
      x =
        "Predicted Home Margin",
      y =
        "Actual Home Margin"
    ) +
    theme_minimal()
  
  ggsave(
    paste0(
      output_prefix,
      "_oos_predicted_vs_actual_margin.png"
    ),
    plot =
      oos_margin_plot,
    width =
      9,
    height =
      7,
    dpi =
      170
  )
}


# ============================================================
# 28. WRITE OUTPUT FILES
# ============================================================

write.csv(
  current_power_rankings,
  paste0(
    output_prefix,
    "_current_power_rankings.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  weekly_power_history,
  paste0(
    output_prefix,
    "_weekly_power_history.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  weekly_model_history,
  paste0(
    output_prefix,
    "_weekly_model_history.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  next_week_predictions,
  paste0(
    output_prefix,
    "_next_week_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  all_remaining_predictions,
  paste0(
    output_prefix,
    "_all_remaining_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  oos_predictions,
  paste0(
    output_prefix,
    "_oos_predictions.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  oos_summary,
  paste0(
    output_prefix,
    "_oos_summary.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  oos_by_week,
  paste0(
    output_prefix,
    "_oos_by_week.csv"
  ),
  row.names =
    FALSE
)

write.csv(
  model_snapshot,
  paste0(
    output_prefix,
    "_model_snapshot.csv"
  ),
  row.names =
    FALSE
)


# ============================================================
# 29. CONSOLE OUTPUT
# ============================================================

cat("\n========================================\n")
cat("VERSION 10 PRODUCTION SYSTEM\n")
cat("========================================\n\n")

cat(
  "Publication week:",
  publication_week,
  "\n"
)

cat(
  "Completed games:",
  nrow(
    completed_games
  ),
  "\n"
)

cat(
  "Teams:",
  n_teams,
  "\n"
)

cat(
  "Estimated HFA:",
  round(
    current_model$hfa,
    3
  ),
  "\n"
)

cat(
  "League mean points/team:",
  round(
    current_model$league_mean_points,
    3
  ),
  "\n"
)

cat(
  "Predictive margin SD:",
  round(
    current_predictive_margin_sd,
    3
  ),
  "\n\n"
)

cat("FINALIZED MODEL:\n")
cat(
  "  Score Offense/Defense Huber + HL12\n"
)
cat(
  "  component lambda = ",
  component_lambda,
  "\n",
  sep = ""
)
cat(
  "  Huber k = ",
  huber_k,
  "\n",
  sep = ""
)
cat(
  "  empirical logistic = logit^-1(",
  win_probability_intercept,
  " + ",
  win_probability_slope,
  " * standardized margin)\n\n",
  sep = ""
)

cat("CURRENT POWER RANKINGS:\n\n")

print(
  current_power_rankings[
    ,
    c(
      "rank",
      "team",
      "rating",
      "rating_lower_95",
      "rating_upper_95",
      "offense_rating",
      "offense_rank",
      "defense_rating",
      "defense_rank",
      "record",
      "rank_change",
      "rating_change",
      "played_SOS",
      "remaining_SOS"
    )
  ],
  row.names =
    FALSE
)

if (
  nrow(
    next_week_predictions
  ) >
  0
) {
  
  cat("\n========================================\n")
  cat(
    "NEXT WEEK PREDICTIONS - WEEK ",
    next_week,
    "\n",
    sep = ""
  )
  cat("========================================\n\n")
  
  print(
    next_week_predictions[
      ,
      c(
        "game_id",
        "home_team",
        "away_team",
        "predicted_home_score",
        "predicted_away_score",
        "predicted_margin",
        "home_win_probability",
        "away_win_probability",
        "predicted_winner",
        "predicted_winner_probability"
      )
    ],
    row.names =
      FALSE
  )
  
} else {
  
  cat(
    "\nNo future scheduled games remain after the current snapshot.\n"
  )
}

if (
  nrow(
    oos_summary
  ) >
  0
) {
  
  cat("\n========================================\n")
  cat("SEASON-TO-DATE WALK-FORWARD OOS EVALUATION\n")
  cat("========================================\n\n")
  
  print(
    oos_summary,
    row.names =
      FALSE
  )
}

cat("\n========================================\n")
cat("VERSION 10 COMPLETE\n")
cat("========================================\n\n")

cat(
  "Outputs written with prefix: ",
  output_prefix,
  "\n",
  sep = ""
)

cat(
  "For live use, update schedule_file/results_file and set as_of_week <- NULL.\n"
)

cat(
  "Recalibrate rating uncertainty before official use in a materially different league structure.\n"
)

# ============================================================
# END VERSION 10
# ============================================================

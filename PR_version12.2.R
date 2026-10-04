#!/usr/bin/env Rscript

# ============================================================
# VERSION 12.1 HISTORICAL NFL POWER RATINGS
# 1970-1998 REGULAR SEASONS ONLY
# ============================================================
#
# Purpose
# -------
# Run the validated 1970-1998 historical regular-season game
# dataset through the same finalized Version 12.1 power-rating
# engine used for the 1999-2025 archive.
#
# IMPORTANT
# ---------
# - Postseason games are completely ignored.
# - Each season is fit independently.
# - No preseason team-strength priors are used.
# - Every season starts from an equal-team framework.
#
# Version 12.1 rating engine:
#   * Score offense/defense Huber model
#   * component ridge lambda = 2
#   * Huber k = 1.5
#   * recency half-life = 12 weeks
#   * generic HFA prior = 2.5
#   * adaptive HFA penalty based on games played
#   * neutral-site games receive zero HFA effect
#   * overall rating = centered offense + centered defense
#
# Default input:
# ~/Downloads/historical_nfl_1970_1998/
# historical_nfl_1970_1998_games_COMPLETE.csv
#
# Main output directory:
# ~/Downloads/historical_nfl_1970_1998/
# version12_1_historical_1970_1998_season_team_summaries/
#
# Creates:
#   version12_1_1970_team_summary.csv
#   ...
#   version12_1_1998_team_summary.csv
#
# Also creates:
#   version12_1_1970_1998_all_season_team_summaries.csv
#   version12_1_1970_1998_model_diagnostics.csv
#   version12_1_1970_1998_validation.csv
#
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

required_packages <- c(
  "readr",
  "dplyr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (
  length(
    missing_packages
  ) >
  0
) {
  
  stop(
    paste0(
      "Install these packages first: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})


# ============================================================
# 1. FILE SETTINGS
# ============================================================

data_directory <- file.path(
  path.expand(
    "~/Downloads"
  ),
  "historical_nfl_1970_1998"
)

input_file <- file.path(
  data_directory,
  "historical_nfl_1970_1998_games_COMPLETE.csv"
)

output_directory <- file.path(
  data_directory,
  "version12_1_historical_1970_1998_season_team_summaries"
)

if (
  !dir.exists(
    output_directory
  )
) {
  
  dir.create(
    output_directory,
    recursive = TRUE
  )
}

if (
  !file.exists(
    input_file
  )
) {
  
  stop(
    paste0(
      "Input file not found:\n",
      input_file,
      "\n\n",
      "Change data_directory near the top of this script if needed."
    )
  )
}


# ============================================================
# 2. VERSION 12.1 FINALIZED CONSTANTS
# ============================================================

component_lambda <- 2
huber_k <- 1.5
recency_half_life <- 12

hfa_prior <- 2.5

max_huber_iterations <- 40
huber_tolerance <- 1e-6

generic_margin_noise_prior_sd <- 8.5
margin_noise_prior_weight <- 20


# ============================================================
# 3. HOME-FIELD SHRINKAGE
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
    
    return(
      10
    )
    
  } else if (
    average_games_played <=
    4
  ) {
    
    return(
      8
    )
    
  } else if (
    average_games_played <=
    6
  ) {
    
    return(
      5
    )
    
  } else {
    
    return(
      2
    )
  }
}


# ============================================================
# 4. RECENCY WEIGHTS
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
      numeric(
        0
      )
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
    training_games$week,
    na.rm = TRUE
  )
  
  weeks_old <- (
    reference_week -
      training_games$week
  )
  
  raw_weights <- 0.5^(
    weeks_old /
      half_life
  )
  
  raw_weights /
    mean(
      raw_weights
    )
}


# ============================================================
# 5. NEUTRAL-SITE SCORE DESIGN
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
  
  colnames(
    X
  ) <- c(
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
    
    home_row <- (
      2 *
        i -
        1
    )
    
    away_row <- (
      2 *
        i
    )
    
    home <- data$home_team[i]
    away <- data$away_team[i]
    
    hfa_half <- if (
      isTRUE(
        data$neutral_site[i]
      )
    ) {
      0
    } else {
      0.5
    }
    
    X[
      home_row,
      "Intercept"
    ] <- 1
    
    X[
      home_row,
      "HFA"
    ] <- hfa_half
    
    X[
      home_row,
      paste0(
        "O__",
        home
      )
    ] <- 1
    
    X[
      home_row,
      paste0(
        "D__",
        away
      )
    ] <- -1
    
    X[
      away_row,
      "Intercept"
    ] <- 1
    
    X[
      away_row,
      "HFA"
    ] <- -hfa_half
    
    X[
      away_row,
      paste0(
        "O__",
        away
      )
    ] <- 1
    
    X[
      away_row,
      paste0(
        "D__",
        home
      )
    ] <- -1
    
    y[
      home_row
    ] <- data$home_score[i]
    
    y[
      away_row
    ] <- data$away_score[i]
  }
  
  list(
    X =
      X,
    y =
      y,
    offense_columns =
      offense_columns,
    defense_columns =
      defense_columns
  )
}


# ============================================================
# 6. WEIGHTED RIDGE SOLVER
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
  
  names(
    beta
  ) <- colnames(
    X
  )
  
  beta
}


# ============================================================
# 7. HUBER OFFENSE / DEFENSE MODEL
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
        beta[
          "HFA"
        ]
      ),
    league_mean =
      as.numeric(
        beta[
          "Intercept"
        ]
      ),
    converged =
      converged,
    iterations =
      iterations_used
  )
}


# ============================================================
# 8. CENTER OFFENSE / DEFENSE COMPONENTS
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
# 9. FIT FINAL VERSION 12.1 POWER MODEL
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
      "Cannot fit power model with zero completed games."
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
# 10. GAME PREDICTOR FOR FITTED-MARGIN RESIDUALS
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
  
  hfa_effect <- ifelse(
    target_schedule$neutral_site,
    0,
    model$hfa
  )
  
  expected_home_score <- (
    model$league_mean_points +
      model$offense_rating[
        home_team
      ] -
      model$defense_rating[
        away_team
      ] +
      hfa_effect /
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
      hfa_effect /
      2
  )
  
  expected_margin <- (
    expected_home_score -
      expected_away_score
  )
  
  rating_margin <- (
    model$ratings[
      home_team
    ] -
      model$ratings[
        away_team
      ] +
      hfa_effect
  )
  
  if (
    max(
      abs(
        expected_margin -
        rating_margin
      )
    ) >
    1e-7
  ) {
    
    stop(
      "Expected-score margin and rating-margin identity disagree."
    )
  }
  
  output <- target_schedule
  
  output$predicted_home_score <- as.numeric(
    expected_home_score
  )
  
  output$predicted_away_score <- as.numeric(
    expected_away_score
  )
  
  output$predicted_margin <- as.numeric(
    expected_margin
  )
  
  output$predicted_total <- (
    output$predicted_home_score +
      output$predicted_away_score
  )
  
  output
}


# ============================================================
# 11. PREDICTIVE MARGIN SD
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
# 12. RECORDS
# ============================================================

calculate_records <- function(
    completed_games,
    league_teams
) {
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    team <- league_teams[i]
    
    team_games <- completed_games[
      completed_games$home_team ==
        team |
        completed_games$away_team ==
        team,
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
      
      home_win <- (
        team_games$home_team ==
          team &
          team_games$actual_margin >
          0
      )
      
      away_win <- (
        team_games$away_team ==
          team &
          team_games$actual_margin <
          0
      )
      
      ties_vector <- (
        team_games$actual_margin ==
          0
      )
      
      wins <- sum(
        home_win |
          away_win
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
        team,
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
        if (
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
        },
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
# 13. RANKING HELPER
# ============================================================

rank_desc_keep_na <- function(
    values
) {
  
  output <- rep(
    NA_integer_,
    length(
      values
    )
  )
  
  valid <- !is.na(
    values
  )
  
  if (
    any(
      valid
    )
  ) {
    
    output[
      valid
    ] <- as.integer(
      rank(
        -values[
          valid
        ],
        ties.method = "min"
      )
    )
  }
  
  output
}


# ============================================================
# 14. STRENGTH OF SCHEDULE
# ============================================================

calculate_sos_metrics <- function(
    season_schedule,
    completed_games,
    current_ratings,
    estimated_hfa,
    league_teams
) {
  
  played_ids <- completed_games$game_id
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    team <- league_teams[i]
    
    team_schedule <- season_schedule[
      season_schedule$home_team ==
        team |
        season_schedule$away_team ==
        team,
    ]
    
    team_schedule$opponent <- ifelse(
      team_schedule$home_team ==
        team,
      team_schedule$away_team,
      team_schedule$home_team
    )
    
    team_schedule$is_home <- (
      team_schedule$home_team ==
        team
    )
    
    team_schedule$is_played <- (
      team_schedule$game_id %in%
        played_ids
    )
    
    played <- team_schedule[
      team_schedule$is_played,
    ]
    
    remaining <- team_schedule[
      !team_schedule$is_played,
    ]
    
    get_raw_sos <- function(
    games_subset
    ) {
      
      if (
        nrow(
          games_subset
        ) ==
        0
      ) {
        
        return(
          NA_real_
        )
      }
      
      mean(
        current_ratings[
          games_subset$opponent
        ]
      )
    }
    
    get_location_difficulty <- function(
    games_subset
    ) {
      
      if (
        nrow(
          games_subset
        ) ==
        0
      ) {
        
        return(
          NA_real_
        )
      }
      
      opponent_rating <- current_ratings[
        games_subset$opponent
      ]
      
      location_adjustment <- ifelse(
        games_subset$neutral_site,
        0,
        ifelse(
          games_subset$is_home,
          -estimated_hfa,
          estimated_hfa
        )
      )
      
      mean(
        opponent_rating +
          location_adjustment
      )
    }
    
    rows[[i]] <- data.frame(
      team =
        team,
      played_SOS =
        get_raw_sos(
          played
        ),
      remaining_SOS =
        get_raw_sos(
          remaining
        ),
      full_SOS =
        get_raw_sos(
          team_schedule
        ),
      played_location_difficulty =
        get_location_difficulty(
          played
        ),
      remaining_location_difficulty =
        get_location_difficulty(
          remaining
        ),
      full_location_difficulty =
        get_location_difficulty(
          team_schedule
        ),
      stringsAsFactors =
        FALSE
    )
  }
  
  output <- do.call(
    rbind,
    rows
  )
  
  output$played_SOS_rank <- rank_desc_keep_na(
    output$played_SOS
  )
  
  output$remaining_SOS_rank <- rank_desc_keep_na(
    output$remaining_SOS
  )
  
  output$full_SOS_rank <- rank_desc_keep_na(
    output$full_SOS
  )
  
  output$played_location_difficulty_rank <- rank_desc_keep_na(
    output$played_location_difficulty
  )
  
  output$remaining_location_difficulty_rank <- rank_desc_keep_na(
    output$remaining_location_difficulty
  )
  
  output$full_location_difficulty_rank <- rank_desc_keep_na(
    output$full_location_difficulty
  )
  
  rownames(
    output
  ) <- NULL
  
  output
}


# ============================================================
# 15. TEAM SCORING SUMMARY
# ============================================================

calculate_team_scoring_summary <- function(
    completed_games,
    season_schedule,
    league_teams
) {
  
  rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    team <- league_teams[i]
    
    team_completed <- completed_games[
      completed_games$home_team ==
        team |
        completed_games$away_team ==
        team,
    ]
    
    team_schedule <- season_schedule[
      season_schedule$home_team ==
        team |
        season_schedule$away_team ==
        team,
    ]
    
    games_played <- nrow(
      team_completed
    )
    
    scheduled_games <- nrow(
      team_schedule
    )
    
    remaining_games <- max(
      0,
      scheduled_games -
        games_played
    )
    
    if (
      games_played >
      0
    ) {
      
      points_for <- sum(
        ifelse(
          team_completed$home_team ==
            team,
          team_completed$home_score,
          team_completed$away_score
        )
      )
      
      points_against <- sum(
        ifelse(
          team_completed$home_team ==
            team,
          team_completed$away_score,
          team_completed$home_score
        )
      )
      
    } else {
      
      points_for <- 0
      points_against <- 0
    }
    
    point_differential <- (
      points_for -
        points_against
    )
    
    rows[[i]] <- data.frame(
      team =
        team,
      scheduled_games =
        scheduled_games,
      games_played_scoring =
        games_played,
      remaining_games =
        remaining_games,
      points_for =
        points_for,
      points_against =
        points_against,
      point_differential =
        point_differential,
      points_for_per_game =
        if (
          games_played >
          0
        ) {
          points_for /
            games_played
        } else {
          NA_real_
        },
      points_against_per_game =
        if (
          games_played >
          0
        ) {
          points_against /
            games_played
        } else {
          NA_real_
        },
      point_differential_per_game =
        if (
          games_played >
          0
        ) {
          point_differential /
            games_played
        } else {
          NA_real_
        },
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
# 16. VERSION 12.1 SEASON TEAM SUMMARY
# ============================================================

build_season_team_summary <- function(
    season_value,
    through_week,
    completed_games,
    season_schedule,
    model,
    league_teams
) {
  
  records <- calculate_records(
    completed_games =
      completed_games,
    league_teams =
      league_teams
  )
  
  scoring <- calculate_team_scoring_summary(
    completed_games =
      completed_games,
    season_schedule =
      season_schedule,
    league_teams =
      league_teams
  )
  
  sos <- calculate_sos_metrics(
    season_schedule =
      season_schedule,
    completed_games =
      completed_games,
    current_ratings =
      model$ratings,
    estimated_hfa =
      model$hfa,
    league_teams =
      league_teams
  )
  
  predictive_margin_sd <- estimate_predictive_margin_sd(
    model =
      model,
    training_games =
      completed_games
  )
  
  output <- data.frame(
    season =
      season_value,
    through_week =
      through_week,
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
  
  output <- merge(
    output,
    records,
    by = "team",
    all.x = TRUE
  )
  
  output <- merge(
    output,
    scoring,
    by = "team",
    all.x = TRUE
  )
  
  output <- merge(
    output,
    sos,
    by = "team",
    all.x = TRUE
  )
  
  output$rank <- as.integer(
    rank(
      -output$rating,
      ties.method = "min"
    )
  )
  
  output$offense_rank <- as.integer(
    rank(
      -output$offense_rating,
      ties.method = "min"
    )
  )
  
  output$defense_rank <- as.integer(
    rank(
      -output$defense_rating,
      ties.method = "min"
    )
  )
  
  output$rating_minus_point_diff_per_game <- (
    output$rating -
      output$point_differential_per_game
  )
  
  output$estimated_HFA <- model$hfa
  output$league_mean_points <- model$league_mean_points
  output$predictive_margin_sd <- predictive_margin_sd
  
  output$average_games_per_team <- (
    2 *
      nrow(
        completed_games
      )
  ) /
    length(
      league_teams
    )
  
  output <- output[
    ,
    c(
      "season",
      "through_week",
      "team",
      "rank",
      "rating",
      "offense_rank",
      "offense_rating",
      "defense_rank",
      "defense_rating",
      "record",
      "wins",
      "losses",
      "ties",
      "win_pct",
      "scheduled_games",
      "games_played",
      "remaining_games",
      "points_for",
      "points_against",
      "point_differential",
      "points_for_per_game",
      "points_against_per_game",
      "point_differential_per_game",
      "rating_minus_point_diff_per_game",
      "played_SOS",
      "played_SOS_rank",
      "remaining_SOS",
      "remaining_SOS_rank",
      "full_SOS",
      "full_SOS_rank",
      "played_location_difficulty",
      "played_location_difficulty_rank",
      "remaining_location_difficulty",
      "remaining_location_difficulty_rank",
      "full_location_difficulty",
      "full_location_difficulty_rank",
      "estimated_HFA",
      "league_mean_points",
      "predictive_margin_sd",
      "average_games_per_team"
    )
  ]
  
  output <- output[
    order(
      output$rank,
      output$team
    ),
  ]
  
  rownames(
    output
  ) <- NULL
  
  output
}


# ============================================================
# 17. LOAD + VALIDATE HISTORICAL GAME FILE
# ============================================================

games_all <- suppressMessages(
  readr::read_csv(
    input_file,
    show_col_types = FALSE,
    progress = FALSE
  )
)

required_columns <- c(
  "season",
  "week",
  "game_type",
  "game_date",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "neutral_site"
)

missing_columns <- setdiff(
  required_columns,
  names(
    games_all
  )
)

if (
  length(
    missing_columns
  ) >
  0
) {
  
  stop(
    paste0(
      "Input file is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 18. REGULAR SEASON ONLY
# ============================================================

regular_games <- games_all |>
  filter(
    season >=
      1970,
    season <=
      1998,
    game_type ==
      "REG"
  ) |>
  mutate(
    season =
      as.integer(
        season
      ),
    week =
      as.integer(
        week
      ),
    home_score =
      as.numeric(
        home_score
      ),
    away_score =
      as.numeric(
        away_score
      ),
    neutral_site =
      as.logical(
        neutral_site
      )
  )


if (
  nrow(
    regular_games
  ) !=
  6140
) {
  
  warning(
    paste0(
      "Expected 6,140 regular-season games, but found ",
      nrow(
        regular_games
      ),
      "."
    )
  )
}


if (
  any(
    is.na(
      regular_games$week
    )
  )
) {
  
  bad_rows <- regular_games[
    is.na(
      regular_games$week
    ),
    c(
      "season",
      "game_date",
      "home_team",
      "away_team"
    )
  ]
  
  print(
    bad_rows
  )
  
  stop(
    "At least one regular-season game has a missing numeric week."
  )
}


if (
  any(
    is.na(
      regular_games$home_score
    )
  ) ||
  any(
    is.na(
      regular_games$away_score
    )
  )
) {
  
  stop(
    "Regular-season data contain missing final scores."
  )
}


if (
  any(
    is.na(
      regular_games$home_team
    )
  ) ||
  any(
    is.na(
      regular_games$away_team
    )
  )
) {
  
  stop(
    "Regular-season data contain missing team codes."
  )
}


regular_games$actual_margin <- (
  regular_games$home_score -
    regular_games$away_score
)

regular_games$game_id <- sprintf(
  "%d_%04d",
  regular_games$season,
  ave(
    seq_len(
      nrow(
        regular_games
      )
    ),
    regular_games$season,
    FUN = seq_along
  )
)


# ============================================================
# 19. FIT EACH SEASON
# ============================================================

season_values <- 1970:1998

all_summary_storage <- list()
diagnostic_storage <- list()

all_summary_position <- 1
diagnostic_position <- 1

cat(
  "\n========================================\n"
)

cat(
  "VERSION 12.1 HISTORICAL RATING RUN\n"
)

cat(
  "1970-1998 REGULAR SEASONS ONLY\n"
)

cat(
  "========================================\n\n"
)


for (
  season_value in
  season_values
) {
  
  season_games <- regular_games[
    regular_games$season ==
      season_value,
  ]
  
  if (
    nrow(
      season_games
    ) ==
    0
  ) {
    
    warning(
      paste0(
        "No games found for ",
        season_value,
        ". Skipping."
      )
    )
    
    next
  }
  
  league_teams <- sort(
    unique(
      c(
        season_games$home_team,
        season_games$away_team
      )
    )
  )
  
  final_week <- max(
    season_games$week
  )
  
  cat(
    season_value,
    ": ",
    nrow(
      season_games
    ),
    " games, ",
    length(
      league_teams
    ),
    " teams, final week ",
    final_week,
    " ... ",
    sep = ""
  )
  
  model <- fit_power_model(
    training_games =
      season_games,
    league_teams =
      league_teams
  )
  
  summary <- build_season_team_summary(
    season_value =
      season_value,
    through_week =
      final_week,
    completed_games =
      season_games,
    season_schedule =
      season_games,
    model =
      model,
    league_teams =
      league_teams
  )
  
  season_output_file <- file.path(
    output_directory,
    paste0(
      "version12_1_",
      season_value,
      "_team_summary.csv"
    )
  )
  
  readr::write_csv(
    summary,
    season_output_file
  )
  
  all_summary_storage[[all_summary_position]] <- summary
  all_summary_position <- all_summary_position + 1
  
  diagnostic_storage[[diagnostic_position]] <- data.frame(
    season =
      season_value,
    games =
      nrow(
        season_games
      ),
    teams =
      length(
        league_teams
      ),
    final_week =
      final_week,
    estimated_HFA =
      model$hfa,
    hfa_lambda =
      model$hfa_lambda,
    league_mean_points =
      model$league_mean_points,
    predictive_margin_sd =
      estimate_predictive_margin_sd(
        model =
          model,
        training_games =
          season_games
      ),
    model_converged =
      model$converged,
    huber_iterations =
      model$iterations,
    rating_mean =
      mean(
        model$ratings
      ),
    offense_mean =
      mean(
        model$offense_rating
      ),
    defense_mean =
      mean(
        model$defense_rating
      ),
    rating_sd =
      sd(
        model$ratings
      ),
    stringsAsFactors =
      FALSE
  )
  
  diagnostic_position <- diagnostic_position + 1
  
  cat(
    "done\n"
  )
}


# ============================================================
# 20. COMBINED ARCHIVE
# ============================================================

all_season_team_summaries <- do.call(
  rbind,
  all_summary_storage
)

rownames(
  all_season_team_summaries
) <- NULL

combined_output_file <- file.path(
  data_directory,
  "version12_1_1970_1998_all_season_team_summaries.csv"
)

readr::write_csv(
  all_season_team_summaries,
  combined_output_file
)


# ============================================================
# 21. MODEL DIAGNOSTICS
# ============================================================

model_diagnostics <- do.call(
  rbind,
  diagnostic_storage
)

rownames(
  model_diagnostics
) <- NULL

diagnostics_output_file <- file.path(
  data_directory,
  "version12_1_1970_1998_model_diagnostics.csv"
)

readr::write_csv(
  model_diagnostics,
  diagnostics_output_file
)


# ============================================================
# 22. VALIDATION
# ============================================================

summary_counts <- all_season_team_summaries |>
  group_by(
    season
  ) |>
  summarise(
    team_rows =
      n(),
    mean_rating =
      mean(
        rating
      ),
    mean_offense_rating =
      mean(
        offense_rating
      ),
    mean_defense_rating =
      mean(
        defense_rating
      ),
    remaining_games_total =
      sum(
        remaining_games
      ),
    .groups =
      "drop"
  )


expected_team_counts <- data.frame(
  season =
    1970:1998,
  expected_teams =
    c(
      rep(
        26,
        6
      ),
      rep(
        28,
        19
      ),
      rep(
        30,
        4
      )
    )
)


validation_by_season <- summary_counts |>
  left_join(
    expected_team_counts,
    by = "season"
  ) |>
  mutate(
    team_count_match =
      team_rows ==
      expected_teams,
    rating_centered =
      abs(
        mean_rating
      ) <
      1e-8,
    offense_centered =
      abs(
        mean_offense_rating
      ) <
      1e-8,
    defense_centered =
      abs(
        mean_defense_rating
      ) <
      1e-8,
    no_remaining_games =
      remaining_games_total ==
      0
  )


validation_output_file <- file.path(
  data_directory,
  "version12_1_1970_1998_validation.csv"
)

readr::write_csv(
  validation_by_season,
  validation_output_file
)


# ============================================================
# 23. FINAL REPORT
# ============================================================

cat(
  "\n========================================\n"
)

cat(
  "VERSION 12.1 HISTORICAL RUN COMPLETE\n"
)

cat(
  "========================================\n"
)

cat(
  "\nSeasons created: ",
  length(
    unique(
      all_season_team_summaries$season
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Team-season rows: ",
  nrow(
    all_season_team_summaries
  ),
  "\n",
  sep = ""
)

cat(
  "Expected team-season rows: 808\n"
)

cat(
  "Postseason games used: 0\n"
)

cat(
  "\nSeason CSV directory:\n",
  output_directory,
  "\n",
  sep = ""
)

cat(
  "\nCombined archive:\n",
  combined_output_file,
  "\n",
  sep = ""
)

cat(
  "\nDiagnostics:\n",
  diagnostics_output_file,
  "\n",
  sep = ""
)

cat(
  "\nValidation:\n",
  validation_output_file,
  "\n",
  sep = ""
)


cat(
  "\nValidation summary:\n"
)

print(
  validation_by_season,
  n =
    nrow(
      validation_by_season
    )
)


if (
  nrow(
    all_season_team_summaries
  ) !=
  808
) {
  
  warning(
    paste0(
      "Expected 808 team-season rows but produced ",
      nrow(
        all_season_team_summaries
      ),
      "."
    )
  )
}


if (
  !all(
    validation_by_season$team_count_match
  )
) {
  
  warning(
    "At least one season has an unexpected team count."
  )
}


if (
  !all(
    validation_by_season$rating_centered
  )
) {
  
  warning(
    "At least one season's overall ratings are not centered at zero."
  )
}


if (
  !all(
    validation_by_season$offense_centered
  )
) {
  
  warning(
    "At least one season's offense ratings are not centered at zero."
  )
}


if (
  !all(
    validation_by_season$defense_centered
  )
) {
  
  warning(
    "At least one season's defense ratings are not centered at zero."
  )
}


cat(
  "\nUse the individual version12_1_<year>_team_summary.csv files ",
  "as the direct 1970-1998 extension of the existing 1999-2025 archive.\n",
  sep = ""
)

# ============================================================
# POWER-RATING CORE
# Extracted from the user's finalized regular-season rating engine.
# This helper does not load data or write files.
# ============================================================

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



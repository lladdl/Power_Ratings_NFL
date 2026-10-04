# ============================================================
# FOOTBALL POWER RATINGS - VERSION 9B-MC5
# HUBER OFFENSE/DEFENSE RECENCY UNDER DYNAMIC TEAM STRENGTH
# ============================================================
#
# PURPOSE
# -------
# Determine the best recency treatment for the new results-only
# Score Offense/Defense Huber rating engine.
#
# This experiment compares five versions of the SAME model:
#
#   1. Huber No Decay
#   2. Huber HL16
#   3. Huber HL12
#   4. Huber HL8
#   5. Huber Adaptive
#
# The only difference between models is game recency weighting.
#
# TRUE TEAM-STRENGTH DRIFT
# ------------------------
# The hidden offense/defense components evolve every CALENDAR week.
# The combined hidden rating is always:
#
#   True Rating = True Offense + True Defense
#
# and is rescaled so that the cross-sectional true rating SD is
# exactly 5 every week.
#
# More importantly, the transition generator solves for the exact
# amount of innovation needed so that:
#
#   sd(TrueRating_week_t - TrueRating_week_t_minus_1)
#
# equals the requested drift SD.
#
# Drift scenarios:
#
#   0.00
#   0.25
#   0.50
#   0.75
#   1.00
#   1.50
#
# Thus:
#   drift = 0       -> completely static team strength
#   drift = 1.5     -> highly dynamic team strength
#
# ADAPTIVE RECENCY
# ----------------
# Adaptive recency does NOT know the true drift.
#
# It observes how much the No-Decay Huber ratings move from one
# completed-game snapshot to the next. A separate calibration set
# learns the relationship:
#
#   observable recent rating movement -> hidden drift SD
#
# separately by completed games per team.
#
# In evaluation seasons, the calibration is frozen.
#
# Estimated drift is converted to a smooth half-life:
#
#   estimated drift < 0.40 -> No Decay
#   otherwise:
#
#       half_life = 20 - 8 * estimated_drift
#
# bounded to [8, 16].
#
# Examples:
#   estimated drift = 0.50 -> HL16
#   estimated drift = 1.00 -> HL12
#   estimated drift = 1.50 -> HL8
#
# This is intentionally observable and non-oracle.
#
# NO EXTRA FOOTBALL INFORMATION IS USED.
# Only schedule and final scores are available to the models.
#
# PRIMARY DECISION
# ----------------
# Choose the recency strategy that gives the best combination of:
#
#   - current-rating RMSE
#   - current-rating MAE
#   - rank recovery
#   - within-1/2/3-point accuracy
#   - calibrated 95% interval width
#   - low regret across unknown drift environments
#
# ============================================================

library(ggplot2)


# ============================================================
# 1. GLOBAL SETTINGS
# ============================================================

n_weeks <- 14
games_per_team <- 12

true_hfa <- 2.7
hfa_prior <- 2.5

target_true_rating_sd <- 5

# Use the offense-heavy structure from the original MC4 experiment.
# The prior sensitivity study showed the Huber conclusion was not
# dependent on this exact split.
initial_offense_scale <- 1.15
initial_defense_scale <- 0.85

# Huber settings locked from Version 9B-MC4 / MC4S.
huber_component_lambda <- 2
huber_k <- 1.5
max_huber_iterations <- 40
huber_tolerance <- 1e-6

# Student-t4 is the primary scoring environment for this recency
# experiment because Huber was selected specifically for robust
# score estimation. Tail-shape sensitivity was already tested in
# Version 9B-MC4S.
noise_scenario <- "Student_t4"

target_margin_noise_sd <- 8.5
score_noise_sd <- target_margin_noise_sd / sqrt(2)

drift_sd_grid <- c(
  0.00,
  0.25,
  0.50,
  0.75,
  1.00,
  1.50
)

model_names <- c(
  "Huber No Decay",
  "Huber HL16",
  "Huber HL12",
  "Huber HL8",
  "Huber Adaptive"
)

model_colors <- setNames(
  grDevices::hcl.colors(
    length(model_names),
    palette = "Dark 3"
  ),
  model_names
)

validate_each_generated_schedule <- FALSE

# ------------------------------------------------------------
# Monte Carlo size
# ------------------------------------------------------------

quick_test <- FALSE

if (quick_test) {
  
  n_calibration_seasons_per_drift <- 5
  n_evaluation_seasons_per_drift <- 10
  
} else {
  
  n_calibration_seasons_per_drift <- 80
  n_evaluation_seasons_per_drift <- 200
}

# First half of EVALUATION seasons is used only to calibrate
# empirical interval width; the second half tests coverage.
interval_calibration_fraction <- 0.50

calibration_seed_base <- 970000
evaluation_seed_base <- 980000

# Adaptive estimator settings.
adaptive_signal_window <- 3
adaptive_no_decay_threshold <- 0.40
adaptive_min_half_life <- 8
adaptive_max_half_life <- 16


# 2. CREATE 16-TEAM LEAGUE
# ============================================================

team_info <- data.frame(
  team_id = 1:16,
  
  team = c(
    "Austin Outlaws",
    "Boston Admirals",
    "Atlanta Firebirds",
    "Baltimore Brigade",
    
    "Chicago Cougars",
    "Denver Peaks",
    "Dallas Wranglers",
    "Detroit Motors",
    
    "Nashville Knights",
    "Virginia Royals",
    "Miami Waves",
    "Minneapolis Northstars",
    
    "Portland Pioneers",
    "San Diego Sharks",
    "Phoenix Scorpions",
    "Seattle Orcas"
  ),
  
  division = rep(
    c(
      "Division A",
      "Division B",
      "Division C",
      "Division D"
    ),
    each = 4
  ),
  
  stringsAsFactors = FALSE
)

teams <- team_info$team

# Consistent team colors used in every team-level plot.
team_colors <- setNames(
  grDevices::hcl.colors(
    length(teams),
    palette = "Dark 3"
  ),
  teams
)

# 3. PAIR KEY HELPERS
# ============================================================

make_pair_key <- function(
    team_a,
    team_b
) {
  
  first_team <- ifelse(
    team_a < team_b,
    team_a,
    team_b
  )
  
  second_team <- ifelse(
    team_a < team_b,
    team_b,
    team_a
  )
  
  return(
    paste(
      first_team,
      second_team,
      sep = "|||"
    )
  )
}


make_division_pair_key <- function(
    division_a,
    division_b
) {
  
  first_division <- ifelse(
    division_a < division_b,
    division_a,
    division_b
  )
  
  second_division <- ifelse(
    division_a < division_b,
    division_b,
    division_a
  )
  
  return(
    paste(
      first_division,
      second_division,
      sep = "|||"
    )
  )
}


# ============================================================
# 4. GENERATE THE 7 DIVISIONAL ROUNDS
# ============================================================
#
# Within each 4-team division, randomize the team ordering:
#
#   A B C D
#
# Protected rivalries:
#
#   A-B
#   C-D
#
# Three possible perfect matchings:
#
#   M1: A-B, C-D
#   M2: A-C, B-D
#   M3: A-D, B-C
#
# Seven divisional rounds:
#
#   M1, M2, M3, M1, M2, M3, M1
#
# Therefore protected rivals meet 3 times and every other
# divisional pair meets twice.
# ============================================================

generate_divisional_rounds <- function(
    team_info
) {
  
  divisional_rows <- list()
  
  row_index <- 1
  
  division_names <- unique(team_info$division)
  
  for (current_division in division_names) {
    
    division_teams <- team_info$team[
      team_info$division == current_division
    ]
    
    if (length(division_teams) != 4) {
      stop("Each division must contain exactly four teams.")
    }
    
    division_teams <- sample(division_teams)
    
    team_a <- division_teams[1]
    team_b <- division_teams[2]
    team_c <- division_teams[3]
    team_d <- division_teams[4]
    
    matching_1 <- data.frame(
      team1 = c(team_a, team_c),
      team2 = c(team_b, team_d),
      stringsAsFactors = FALSE
    )
    
    matching_2 <- data.frame(
      team1 = c(team_a, team_b),
      team2 = c(team_c, team_d),
      stringsAsFactors = FALSE
    )
    
    matching_3 <- data.frame(
      team1 = c(team_a, team_b),
      team2 = c(team_d, team_c),
      stringsAsFactors = FALSE
    )
    
    round_matchings <- list(
      matching_1,
      matching_2,
      matching_3,
      matching_1,
      matching_2,
      matching_3,
      matching_1
    )
    
    protected_keys <- c(
      make_pair_key(team_a, team_b),
      make_pair_key(team_c, team_d)
    )
    
    for (division_round in seq_along(round_matchings)) {
      
      current_matching <- round_matchings[[division_round]]
      
      for (game_number in seq_len(nrow(current_matching))) {
        
        current_pair_key <- make_pair_key(
          current_matching$team1[game_number],
          current_matching$team2[game_number]
        )
        
        current_type <- if (
          current_pair_key %in% protected_keys
        ) {
          "Protected Rival"
        } else {
          "Division"
        }
        
        divisional_rows[[row_index]] <- data.frame(
          round_type = "Division",
          original_round = division_round,
          team1 = current_matching$team1[game_number],
          team2 = current_matching$team2[game_number],
          matchup_type = current_type,
          division1 = current_division,
          division2 = current_division,
          stringsAsFactors = FALSE
        )
        
        row_index <- row_index + 1
      }
    }
  }
  
  divisional_games <- do.call(
    rbind,
    divisional_rows
  )
  
  rownames(divisional_games) <- NULL
  
  return(divisional_games)
}


# ============================================================
# 5. CREATE ONE PERFECT MATCHING BETWEEN TWO DIVISIONS
# ============================================================
#
# If the same two divisions have already met, the second
# matching is forced to give every team a different opponent.
# ============================================================

create_division_matching <- function(
    division_a_teams,
    division_b_teams,
    previous_matching = NULL
) {
  
  if (length(division_a_teams) != length(division_b_teams)) {
    stop("Interdivision matching requires equal division sizes.")
  }
  
  repeat {
    
    shuffled_b <- sample(division_b_teams)
    
    matching <- data.frame(
      team1 = division_a_teams,
      team2 = shuffled_b,
      stringsAsFactors = FALSE
    )
    
    if (is.null(previous_matching)) {
      break
    }
    
    if (all(matching$team2 != previous_matching$team2)) {
      break
    }
  }
  
  return(matching)
}


# ============================================================
# 6. GENERATE THE 5 INTERDIVISION ROUNDS
# ============================================================
#
# Four divisions have three possible division-level perfect
# matchings:
#
#   Pattern 1: A-B and C-D
#   Pattern 2: A-C and B-D
#   Pattern 3: A-D and B-C
#
# One pattern is used once and the other two are used twice.
# Therefore each team gets:
#
#   1 + 2 + 2 = 5 unique interdivision opponents.
# ============================================================

generate_interdivision_rounds <- function(
    team_info
) {
  
  division_names <- unique(team_info$division)
  
  if (length(division_names) != 4) {
    stop("Version 8A requires exactly four divisions.")
  }
  
  division_a <- division_names[1]
  division_b <- division_names[2]
  division_c <- division_names[3]
  division_d <- division_names[4]
  
  pattern_1 <- list(
    c(division_a, division_b),
    c(division_c, division_d)
  )
  
  pattern_2 <- list(
    c(division_a, division_c),
    c(division_b, division_d)
  )
  
  pattern_3 <- list(
    c(division_a, division_d),
    c(division_b, division_c)
  )
  
  patterns <- list(
    pattern_1,
    pattern_2,
    pattern_3
  )
  
  single_pattern_index <- sample(
    1:3,
    size = 1
  )
  
  repeated_pattern_indices <- setdiff(
    1:3,
    single_pattern_index
  )
  
  round_patterns <- list(
    patterns[[single_pattern_index]],
    patterns[[repeated_pattern_indices[1]]],
    patterns[[repeated_pattern_indices[2]]],
    patterns[[repeated_pattern_indices[1]]],
    patterns[[repeated_pattern_indices[2]]]
  )
  
  previous_matchings <- list()
  
  interdivision_rows <- list()
  
  row_index <- 1
  
  for (inter_round in seq_along(round_patterns)) {
    
    current_pattern <- round_patterns[[inter_round]]
    
    for (pair_number in seq_along(current_pattern)) {
      
      current_division_pair <- current_pattern[[pair_number]]
      
      first_division <- current_division_pair[1]
      second_division <- current_division_pair[2]
      
      first_teams <- team_info$team[
        team_info$division == first_division
      ]
      
      second_teams <- team_info$team[
        team_info$division == second_division
      ]
      
      division_pair_key <- make_division_pair_key(
        first_division,
        second_division
      )
      
      previous_matching <- NULL
      
      if (division_pair_key %in% names(previous_matchings)) {
        previous_matching <- previous_matchings[[division_pair_key]]
      }
      
      current_matching <- create_division_matching(
        division_a_teams = first_teams,
        division_b_teams = second_teams,
        previous_matching = previous_matching
      )
      
      previous_matchings[[division_pair_key]] <- current_matching
      
      for (game_number in seq_len(nrow(current_matching))) {
        
        interdivision_rows[[row_index]] <- data.frame(
          round_type = "Interdivision",
          original_round = inter_round,
          team1 = current_matching$team1[game_number],
          team2 = current_matching$team2[game_number],
          matchup_type = "Interdivision",
          division1 = first_division,
          division2 = second_division,
          stringsAsFactors = FALSE
        )
        
        row_index <- row_index + 1
      }
    }
  }
  
  interdivision_games <- do.call(
    rbind,
    interdivision_rows
  )
  
  rownames(interdivision_games) <- NULL
  
  return(interdivision_games)
}


# ============================================================
# 7. BUILD THE 12 ACTIVE ROUNDS
# ============================================================
#
# Every active round is a perfect matching of all 16 teams.
# Therefore every active round has exactly eight games and
# every team appears exactly once.
# ============================================================

build_active_rounds <- function(
    team_info
) {
  
  division_games <- generate_divisional_rounds(
    team_info
  )
  
  interdivision_games <- generate_interdivision_rounds(
    team_info
  )
  
  round_labels <- c(
    paste0("D", 1:7),
    paste0("I", 1:5)
  )
  
  round_order <- sample(round_labels)
  
  active_rows <- list()
  
  active_index <- 1
  
  for (active_round in seq_along(round_order)) {
    
    current_label <- round_order[active_round]
    
    if (substr(current_label, 1, 1) == "D") {
      
      original_round <- as.integer(
        substring(current_label, 2)
      )
      
      current_games <- division_games[
        division_games$original_round == original_round,
      ]
      
    } else {
      
      original_round <- as.integer(
        substring(current_label, 2)
      )
      
      current_games <- interdivision_games[
        interdivision_games$original_round == original_round,
      ]
    }
    
    current_games$active_round <- active_round
    
    active_rows[[active_index]] <- current_games
    
    active_index <- active_index + 1
  }
  
  active_games <- do.call(
    rbind,
    active_rows
  )
  
  rownames(active_games) <- NULL
  
  return(active_games)
}


# ============================================================
# 8. INSERT TWO LEAGUE-WIDE BYE WEEKS
# ============================================================
#
# There are 12 active rounds and 14 calendar weeks.
# Two interior weeks are selected as league-wide bye weeks.
# This keeps Version 8A focused on opponent imbalance rather
# than bye timing.
# ============================================================

assign_calendar_weeks <- function(
    active_games,
    n_weeks = 14
) {
  
  bye_weeks <- sort(
    sample(
      2:(n_weeks - 1),
      size = 2,
      replace = FALSE
    )
  )
  
  active_weeks <- setdiff(
    1:n_weeks,
    bye_weeks
  )
  
  if (length(active_weeks) != 12) {
    stop("There must be exactly 12 active calendar weeks.")
  }
  
  active_games$week <- active_weeks[
    active_games$active_round
  ]
  
  return(
    list(
      schedule = active_games,
      bye_weeks = bye_weeks
    )
  )
}


# ============================================================
# 9. CHECK CONNECTIVITY USING TEAM1 / TEAM2
# ============================================================

is_edge_schedule_connected <- function(
    games,
    teams
) {
  
  visited <- teams[1]
  frontier <- teams[1]
  
  while (length(frontier) > 0) {
    
    current_team <- frontier[1]
    
    if (length(frontier) == 1) {
      frontier <- character()
    } else {
      frontier <- frontier[-1]
    }
    
    neighbors <- unique(
      c(
        games$team2[
          games$team1 == current_team
        ],
        games$team1[
          games$team2 == current_team
        ]
      )
    )
    
    new_neighbors <- setdiff(
      neighbors,
      visited
    )
    
    if (length(new_neighbors) > 0) {
      visited <- c(visited, new_neighbors)
      frontier <- c(frontier, new_neighbors)
    }
  }
  
  return(
    length(unique(visited)) == length(teams)
  )
}


# ============================================================
# 10. ORIENT HOME AND AWAY WITH HIERHOLZER'S ALGORITHM
# ============================================================
#
# Every team has degree 12, which is even, and the schedule
# graph is connected. Therefore the full 96-game multigraph
# has an Euler circuit.
#
# Hierholzer's algorithm constructs that Euler circuit while
# correctly handling repeated opponents and cycles that share
# vertices.
#
# Once the Euler circuit is found, each game is oriented in
# the direction in which the circuit traverses that edge.
# Every arrival at a team is paired with a departure from that
# team, so an even-degree team with 12 games receives exactly:
#
#   6 home games
#   6 away games
#
# Here "home" is defined as the outgoing endpoint of the
# oriented Euler edge.
# ============================================================

orient_games_home_away <- function(
    games,
    teams
) {
  
  if (!is_edge_schedule_connected(games, teams)) {
    stop(
      "Cannot orient home/away because the schedule graph is disconnected."
    )
  }
  
  n_games <- nrow(games)
  
  # ----------------------------------------------------------
  # CHECK DEGREES
  # ----------------------------------------------------------
  
  degree_counts <- table(
    factor(
      c(
        games$team1,
        games$team2
      ),
      levels = teams
    )
  )
  
  degree_counts <- as.integer(
    degree_counts
  )
  
  names(degree_counts) <- teams
  
  if (any(degree_counts %% 2 != 0)) {
    stop(
      "Euler orientation requires every team to have even degree."
    )
  }
  
  if (!all(degree_counts == 12)) {
    print(degree_counts)
    
    stop(
      "Expected every team to have degree 12 before home/away orientation."
    )
  }
  
  # ----------------------------------------------------------
  # BUILD INCIDENT-EDGE LIST
  # ----------------------------------------------------------
  
  adjacency <- vector(
    "list",
    length(teams)
  )
  
  names(adjacency) <- teams
  
  for (edge_index in seq_len(n_games)) {
    
    team_a <- games$team1[edge_index]
    team_b <- games$team2[edge_index]
    
    adjacency[[team_a]] <- c(
      adjacency[[team_a]],
      edge_index
    )
    
    adjacency[[team_b]] <- c(
      adjacency[[team_b]],
      edge_index
    )
  }
  
  # ----------------------------------------------------------
  # HIERHOLZER EULER CIRCUIT
  # ----------------------------------------------------------
  #
  # vertex_stack contains the current unfinished walk.
  # edge_stack contains the edge used to enter each vertex
  # after the starting vertex.
  #
  # When the current vertex has no unused incident edge, it is
  # added to the final circuit and the walk backtracks.
  # ----------------------------------------------------------
  
  used_edge <- rep(
    FALSE,
    n_games
  )
  
  start_team <- teams[1]
  
  vertex_stack <- start_team
  
  edge_stack <- integer()
  
  reverse_vertices <- character()
  
  reverse_edges <- integer()
  
  while (length(vertex_stack) > 0) {
    
    current_team <- vertex_stack[
      length(vertex_stack)
    ]
    
    incident_edges <- adjacency[[current_team]]
    
    available_edges <- incident_edges[
      !used_edge[incident_edges]
    ]
    
    if (length(available_edges) > 0) {
      
      # Randomly choose among unused incident edges so that
      # home/away orientation is not tied to data-frame order.
      
      current_edge <- available_edges[
        sample.int(
          length(available_edges),
          size = 1
        )
      ]
      
      used_edge[current_edge] <- TRUE
      
      team_a <- games$team1[current_edge]
      team_b <- games$team2[current_edge]
      
      next_team <- if (
        current_team == team_a
      ) {
        team_b
      } else {
        team_a
      }
      
      vertex_stack <- c(
        vertex_stack,
        next_team
      )
      
      edge_stack <- c(
        edge_stack,
        current_edge
      )
      
    } else {
      
      # Add this vertex to the Euler circuit in reverse order.
      
      reverse_vertices <- c(
        reverse_vertices,
        current_team
      )
      
      # Every stacked vertex except the original start vertex
      # has one edge by which it was entered.
      
      if (length(edge_stack) > 0) {
        
        reverse_edges <- c(
          reverse_edges,
          edge_stack[
            length(edge_stack)
          ]
        )
        
        if (length(edge_stack) == 1) {
          edge_stack <- integer()
        } else {
          edge_stack <- edge_stack[
            -length(edge_stack)
          ]
        }
      }
      
      if (length(vertex_stack) == 1) {
        vertex_stack <- character()
      } else {
        vertex_stack <- vertex_stack[
          -length(vertex_stack)
        ]
      }
    }
  }
  
  # ----------------------------------------------------------
  # VALIDATE EULER CIRCUIT
  # ----------------------------------------------------------
  
  if (!all(used_edge)) {
    stop(
      "Hierholzer's algorithm did not use every schedule edge."
    )
  }
  
  circuit_vertices <- rev(
    reverse_vertices
  )
  
  circuit_edges <- rev(
    reverse_edges
  )
  
  if (length(circuit_edges) != n_games) {
    stop(
      paste0(
        "Euler circuit used ",
        length(circuit_edges),
        " edges but the schedule contains ",
        n_games,
        "."
      )
    )
  }
  
  if (length(circuit_vertices) != n_games + 1) {
    stop(
      paste0(
        "Euler circuit has ",
        length(circuit_vertices),
        " vertices but expected ",
        n_games + 1,
        "."
      )
    )
  }
  
  if (circuit_vertices[1] != circuit_vertices[length(circuit_vertices)]) {
    stop(
      "Euler traversal did not return to its starting team."
    )
  }
  
  if (length(unique(circuit_edges)) != n_games) {
    stop(
      "Euler circuit did not contain every game exactly once."
    )
  }
  
  # ----------------------------------------------------------
  # ORIENT EVERY EDGE ALONG THE EULER CIRCUIT
  # ----------------------------------------------------------
  
  home_team <- rep(
    NA_character_,
    n_games
  )
  
  away_team <- rep(
    NA_character_,
    n_games
  )
  
  for (position in seq_len(n_games)) {
    
    current_edge <- circuit_edges[position]
    
    from_team <- circuit_vertices[position]
    to_team <- circuit_vertices[position + 1]
    
    team_a <- games$team1[current_edge]
    team_b <- games$team2[current_edge]
    
    edge_matches <- (
      from_team == team_a &
        to_team == team_b
    ) || (
      from_team == team_b &
        to_team == team_a
    )
    
    if (!edge_matches) {
      stop(
        paste0(
          "Euler edge/vertex mismatch at circuit position ",
          position,
          "."
        )
      )
    }
    
    home_team[current_edge] <- from_team
    away_team[current_edge] <- to_team
  }
  
  # ----------------------------------------------------------
  # VALIDATE HOME/AWAY BALANCE
  # ----------------------------------------------------------
  
  if (any(is.na(home_team)) || any(is.na(away_team))) {
    stop(
      "At least one game was left without a home/away assignment."
    )
  }
  
  home_counts <- table(
    factor(
      home_team,
      levels = teams
    )
  )
  
  away_counts <- table(
    factor(
      away_team,
      levels = teams
    )
  )
  
  if (!all(as.integer(home_counts) == 6)) {
    print(home_counts)
    
    stop(
      "Euler orientation did not produce exactly six home games per team."
    )
  }
  
  if (!all(as.integer(away_counts) == 6)) {
    print(away_counts)
    
    stop(
      "Euler orientation did not produce exactly six away games per team."
    )
  }
  
  # ----------------------------------------------------------
  # ATTACH ORIENTATION TO SCHEDULE
  # ----------------------------------------------------------
  
  games$home_team <- home_team
  games$away_team <- away_team
  
  games$pair_key <- make_pair_key(
    games$home_team,
    games$away_team
  )
  
  games$meeting_number <- ave(
    seq_len(nrow(games)),
    games$pair_key,
    FUN = seq_along
  )
  
  games <- games[
    order(
      games$week,
      games$home_team,
      games$away_team
    ),
  ]
  
  rownames(games) <- NULL
  
  games$game_id <- seq_len(
    nrow(games)
  )
  
  return(games)
}


# ============================================================
# 11. GENERATE ONE COMPLETE UNBALANCED SCHEDULE
# ============================================================

generate_unbalanced_schedule <- function(
    team_info,
    n_weeks = 14
) {
  
  active_games <- build_active_rounds(
    team_info
  )
  
  calendar <- assign_calendar_weeks(
    active_games = active_games,
    n_weeks = n_weeks
  )
  
  schedule <- orient_games_home_away(
    games = calendar$schedule,
    teams = team_info$team
  )
  
  return(
    list(
      schedule = schedule,
      bye_weeks = calendar$bye_weeks
    )
  )
}


# ============================================================
# 12. CONNECTIVITY CHECK USING HOME / AWAY
# ============================================================

is_schedule_connected <- function(
    schedule,
    teams
) {
  
  visited <- teams[1]
  frontier <- teams[1]
  
  while (length(frontier) > 0) {
    
    current_team <- frontier[1]
    
    if (length(frontier) == 1) {
      frontier <- character()
    } else {
      frontier <- frontier[-1]
    }
    
    neighbors <- unique(
      c(
        schedule$away_team[
          schedule$home_team == current_team
        ],
        schedule$home_team[
          schedule$away_team == current_team
        ]
      )
    )
    
    new_neighbors <- setdiff(
      neighbors,
      visited
    )
    
    if (length(new_neighbors) > 0) {
      visited <- c(visited, new_neighbors)
      frontier <- c(frontier, new_neighbors)
    }
  }
  
  return(
    length(unique(visited)) == length(teams)
  )
}


# ============================================================
# 13. COMPLETE PAIR-FREQUENCY TABLE
# ============================================================

create_pair_frequency_table <- function(
    schedule,
    teams
) {
  
  all_pairs_matrix <- t(
    combn(
      teams,
      2
    )
  )
  
  all_pairs <- data.frame(
    team1 = all_pairs_matrix[, 1],
    team2 = all_pairs_matrix[, 2],
    stringsAsFactors = FALSE
  )
  
  all_pairs$pair_key <- make_pair_key(
    all_pairs$team1,
    all_pairs$team2
  )
  
  played_counts <- as.data.frame(
    table(schedule$pair_key),
    stringsAsFactors = FALSE
  )
  
  names(played_counts) <- c(
    "pair_key",
    "games"
  )
  
  played_counts$games <- as.integer(
    played_counts$games
  )
  
  pair_frequency <- merge(
    all_pairs,
    played_counts,
    by = "pair_key",
    all.x = TRUE
  )
  
  pair_frequency$games[
    is.na(pair_frequency$games)
  ] <- 0
  
  pair_frequency <- pair_frequency[
    order(
      -pair_frequency$games,
      pair_frequency$team1,
      pair_frequency$team2
    ),
  ]
  
  rownames(pair_frequency) <- NULL
  
  return(pair_frequency)
}


# ============================================================
# 14. TEAM SCHEDULE SUMMARY
# ============================================================

create_team_schedule_summary <- function(
    schedule,
    teams,
    n_weeks
) {
  
  summary_rows <- list()
  
  for (i in seq_along(teams)) {
    
    current_team <- teams[i]
    
    team_games <- schedule[
      schedule$home_team == current_team |
        schedule$away_team == current_team,
    ]
    
    opponents <- ifelse(
      team_games$home_team == current_team,
      team_games$away_team,
      team_games$home_team
    )
    
    played_weeks <- sort(
      unique(team_games$week)
    )
    
    bye_weeks <- setdiff(
      1:n_weeks,
      played_weeks
    )
    
    opponent_frequency <- table(opponents)
    
    summary_rows[[i]] <- data.frame(
      team = current_team,
      games = nrow(team_games),
      home_games = sum(team_games$home_team == current_team),
      away_games = sum(team_games$away_team == current_team),
      distinct_opponents = length(unique(opponents)),
      opponents_played_3_times = sum(opponent_frequency == 3),
      opponents_played_2_times = sum(opponent_frequency == 2),
      opponents_played_1_time = sum(opponent_frequency == 1),
      opponents_not_played = length(teams) - 1 - length(unique(opponents)),
      bye_count = length(bye_weeks),
      bye_weeks = paste(bye_weeks, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  
  team_summary <- do.call(
    rbind,
    summary_rows
  )
  
  rownames(team_summary) <- NULL
  
  return(team_summary)
}


# ============================================================
# 15. VALIDATE THE SCHEDULE
# ============================================================

validate_schedule <- function(
    schedule,
    team_info,
    n_weeks = 14
) {
  
  teams <- team_info$team
  
  expected_games <- length(teams) * games_per_team / 2
  
  if (nrow(schedule) != expected_games) {
    stop(
      paste(
        "Expected",
        expected_games,
        "games but found",
        nrow(schedule)
      )
    )
  }
  
  # ----------------------------------------------------------
  # No team may appear twice in the same week
  # ----------------------------------------------------------
  
  appearances <- rbind(
    data.frame(
      week = schedule$week,
      team = schedule$home_team,
      stringsAsFactors = FALSE
    ),
    data.frame(
      week = schedule$week,
      team = schedule$away_team,
      stringsAsFactors = FALSE
    )
  )
  
  duplicate_key <- duplicated(
    appearances[, c("week", "team")]
  )
  
  if (any(duplicate_key)) {
    
    duplicate_rows <- appearances[
      duplicate_key,
    ]
    
    print(duplicate_rows)
    
    stop("A team was scheduled more than once in the same week.")
  }
  
  # ----------------------------------------------------------
  # Active weeks must contain exactly eight games
  # ----------------------------------------------------------
  
  week_game_counts <- table(
    factor(
      schedule$week,
      levels = 1:n_weeks
    )
  )
  
  active_week_counts <- as.integer(
    week_game_counts[
      week_game_counts > 0
    ]
  )
  
  if (!all(active_week_counts == 8)) {
    stop("Every active week must contain exactly eight games.")
  }
  
  if (sum(week_game_counts == 0) != 2) {
    stop("There must be exactly two league-wide bye weeks.")
  }
  
  # ----------------------------------------------------------
  # Team-level checks
  # ----------------------------------------------------------
  
  team_summary <- create_team_schedule_summary(
    schedule = schedule,
    teams = teams,
    n_weeks = n_weeks
  )
  
  if (!all(team_summary$games == 12)) {
    stop("Not every team has exactly 12 games.")
  }
  
  if (!all(team_summary$home_games == 6)) {
    stop("Not every team has exactly six home games.")
  }
  
  if (!all(team_summary$away_games == 6)) {
    stop("Not every team has exactly six away games.")
  }
  
  if (!all(team_summary$distinct_opponents == 8)) {
    stop("Not every team has exactly eight distinct opponents.")
  }
  
  if (!all(team_summary$opponents_played_3_times == 1)) {
    stop("Every team must have exactly one opponent played three times.")
  }
  
  if (!all(team_summary$opponents_played_2_times == 2)) {
    stop("Every team must have exactly two opponents played twice.")
  }
  
  if (!all(team_summary$opponents_played_1_time == 5)) {
    stop("Every team must have exactly five opponents played once.")
  }
  
  if (!all(team_summary$opponents_not_played == 7)) {
    stop("Every team must have exactly seven opponents not played.")
  }
  
  if (!all(team_summary$bye_count == 2)) {
    stop("Every team must have exactly two bye weeks.")
  }
  
  # ----------------------------------------------------------
  # Pair-frequency checks
  # ----------------------------------------------------------
  
  pair_frequency <- create_pair_frequency_table(
    schedule = schedule,
    teams = teams
  )
  
  frequency_distribution <- table(
    factor(
      pair_frequency$games,
      levels = 0:3
    )
  )
  
  if (as.integer(frequency_distribution["0"]) != 56) {
    stop("Expected 56 team pairs that do not play.")
  }
  
  if (as.integer(frequency_distribution["1"]) != 40) {
    stop("Expected 40 one-game matchups.")
  }
  
  if (as.integer(frequency_distribution["2"]) != 16) {
    stop("Expected 16 two-game matchups.")
  }
  
  if (as.integer(frequency_distribution["3"]) != 8) {
    stop("Expected 8 three-game matchups.")
  }
  
  connected <- is_schedule_connected(
    schedule = schedule,
    teams = teams
  )
  
  if (!connected) {
    stop("Schedule graph is disconnected.")
  }
  
  return(
    list(
      valid = TRUE,
      connected = connected,
      team_summary = team_summary,
      pair_frequency = pair_frequency,
      frequency_distribution = frequency_distribution,
      week_game_counts = week_game_counts
    )
  )
}



# ============================================================


# ============================================================
# 17. INITIAL LATENT OFFENSE / DEFENSE
# ============================================================

generate_initial_true_components <- function(
    league_teams,
    target_sd,
    offense_scale,
    defense_scale
) {
  
  true_offense <- rnorm(
    length(league_teams),
    mean = 0,
    sd = offense_scale
  )
  
  true_defense <- rnorm(
    length(league_teams),
    mean = 0,
    sd = defense_scale
  )
  
  true_offense <- true_offense -
    mean(true_offense)
  
  true_defense <- true_defense -
    mean(true_defense)
  
  raw_rating <- true_offense +
    true_defense
  
  scaling_factor <- target_sd /
    sd(raw_rating)
  
  true_offense <- true_offense *
    scaling_factor
  
  true_defense <- true_defense *
    scaling_factor
  
  true_rating <- true_offense +
    true_defense
  
  true_rating <- true_rating -
    mean(true_rating)
  
  names(true_offense) <- league_teams
  names(true_defense) <- league_teams
  names(true_rating) <- league_teams
  
  return(
    list(
      offense = true_offense,
      defense = true_defense,
      rating = true_rating
    )
  )
}


# ============================================================
# 18. ADVANCE COMPONENTS WITH EXACT RATING DRIFT
# ============================================================

advance_true_components_exact_drift <- function(
    previous_components,
    requested_drift_sd,
    target_rating_sd,
    league_teams
) {
  
  previous_offense <- previous_components$offense[
    league_teams
  ]
  
  previous_defense <- previous_components$defense[
    league_teams
  ]
  
  previous_rating <- previous_components$rating[
    league_teams
  ]
  
  if (
    requested_drift_sd <= 0
  ) {
    
    return(
      list(
        offense = previous_offense,
        defense = previous_defense,
        rating = previous_rating,
        realized_drift_sd = 0
      )
    )
  }
  
  n_teams <- length(
    league_teams
  )
  
  raw_offense_innovation <- rnorm(
    n_teams
  )
  
  raw_defense_innovation <- rnorm(
    n_teams
  )
  
  raw_offense_innovation <-
    raw_offense_innovation -
    mean(raw_offense_innovation)
  
  raw_defense_innovation <-
    raw_defense_innovation -
    mean(raw_defense_innovation)
  
  build_candidate <- function(
    innovation_multiplier
  ) {
    
    candidate_offense <- (
      previous_offense +
        innovation_multiplier *
        raw_offense_innovation
    )
    
    candidate_defense <- (
      previous_defense +
        innovation_multiplier *
        raw_defense_innovation
    )
    
    candidate_offense <- candidate_offense -
      mean(candidate_offense)
    
    candidate_defense <- candidate_defense -
      mean(candidate_defense)
    
    candidate_rating <- (
      candidate_offense +
        candidate_defense
    )
    
    candidate_scale <- target_rating_sd /
      sd(candidate_rating)
    
    candidate_offense <- candidate_offense *
      candidate_scale
    
    candidate_defense <- candidate_defense *
      candidate_scale
    
    candidate_rating <- (
      candidate_offense +
        candidate_defense
    )
    
    candidate_rating <- candidate_rating -
      mean(candidate_rating)
    
    return(
      list(
        offense = candidate_offense,
        defense = candidate_defense,
        rating = candidate_rating
      )
    )
  }
  
  drift_objective <- function(
    innovation_multiplier
  ) {
    
    candidate <- build_candidate(
      innovation_multiplier
    )
    
    realized_drift <- sd(
      candidate$rating -
        previous_rating
    )
    
    return(
      realized_drift -
        requested_drift_sd
    )
  }
  
  lower_multiplier <- 0
  upper_multiplier <- 1
  
  upper_value <- drift_objective(
    upper_multiplier
  )
  
  while (
    upper_value <
    0 &&
    upper_multiplier <
    1024
  ) {
    
    upper_multiplier <- upper_multiplier *
      2
    
    upper_value <- drift_objective(
      upper_multiplier
    )
  }
  
  if (
    upper_value <
    0
  ) {
    
    stop(
      paste0(
        "Could not achieve requested drift SD = ",
        requested_drift_sd,
        "."
      )
    )
  }
  
  solved_multiplier <- uniroot(
    drift_objective,
    interval = c(
      lower_multiplier,
      upper_multiplier
    ),
    tol = 1e-10
  )$root
  
  candidate <- build_candidate(
    solved_multiplier
  )
  
  realized_drift_sd <- sd(
    candidate$rating -
      previous_rating
  )
  
  names(candidate$offense) <- league_teams
  names(candidate$defense) <- league_teams
  names(candidate$rating) <- league_teams
  
  return(
    list(
      offense = candidate$offense,
      defense = candidate$defense,
      rating = candidate$rating,
      realized_drift_sd = realized_drift_sd
    )
  )
}


# ============================================================
# 19. GENERATE FULL DYNAMIC TRUE-STRENGTH PATH
# ============================================================

generate_dynamic_true_path <- function(
    league_teams,
    n_calendar_weeks,
    target_rating_sd,
    requested_drift_sd,
    offense_scale,
    defense_scale
) {
  
  path <- vector(
    "list",
    n_calendar_weeks
  )
  
  path[[1]] <- generate_initial_true_components(
    league_teams = league_teams,
    target_sd = target_rating_sd,
    offense_scale = offense_scale,
    defense_scale = defense_scale
  )
  
  path[[1]]$realized_drift_sd <- 0
  
  if (
    n_calendar_weeks >= 2
  ) {
    
    for (
      week_index in
      2:n_calendar_weeks
    ) {
      
      path[[week_index]] <-
        advance_true_components_exact_drift(
          previous_components =
            path[[week_index - 1]],
          requested_drift_sd =
            requested_drift_sd,
          target_rating_sd =
            target_rating_sd,
          league_teams =
            league_teams
        )
    }
  }
  
  return(path)
}


# ============================================================
# 20. SCORE-ERROR GENERATOR
# ============================================================

generate_score_errors <- function(
    n,
    noise_scenario,
    target_sd
) {
  
  if (
    noise_scenario ==
    "Gaussian"
  ) {
    
    return(
      rnorm(
        n,
        mean = 0,
        sd = target_sd
      )
    )
  }
  
  if (
    noise_scenario %in%
    c(
      "Student_t8",
      "Student_t4",
      "Student_t3"
    )
  ) {
    
    degrees_freedom <- as.numeric(
      sub(
        "Student_t",
        "",
        noise_scenario,
        fixed = TRUE
      )
    )
    
    scale_factor <- target_sd *
      sqrt(
        (
          degrees_freedom - 2
        ) /
          degrees_freedom
      )
    
    return(
      rt(
        n,
        df = degrees_freedom
      ) *
        scale_factor
    )
  }
  
  stop(
    paste(
      "Unknown noise scenario:",
      noise_scenario
    )
  )
}


# ============================================================
# 21. SIMULATE DYNAMIC SCORE SEASON
# ============================================================

simulate_dynamic_score_season <- function(
    schedule,
    true_path,
    noise_scenario,
    true_hfa,
    mean_points = 24,
    score_error_sd
) {
  
  games <- schedule
  
  n_games <- nrow(
    games
  )
  
  expected_home_score <- numeric(
    n_games
  )
  
  expected_away_score <- numeric(
    n_games
  )
  
  true_home_rating <- numeric(
    n_games
  )
  
  true_away_rating <- numeric(
    n_games
  )
  
  for (
    game_index in
    seq_len(n_games)
  ) {
    
    game_week <- games$week[
      game_index
    ]
    
    home_team <- games$home_team[
      game_index
    ]
    
    away_team <- games$away_team[
      game_index
    ]
    
    components <- true_path[[game_week]]
    
    true_home_rating[game_index] <-
      components$rating[
        home_team
      ]
    
    true_away_rating[game_index] <-
      components$rating[
        away_team
      ]
    
    expected_home_score[game_index] <- (
      mean_points +
        components$offense[
          home_team
        ] -
        components$defense[
          away_team
        ] +
        true_hfa / 2
    )
    
    expected_away_score[game_index] <- (
      mean_points +
        components$offense[
          away_team
        ] -
        components$defense[
          home_team
        ] -
        true_hfa / 2
    )
  }
  
  home_error <- generate_score_errors(
    n = n_games,
    noise_scenario = noise_scenario,
    target_sd = score_error_sd
  )
  
  away_error <- generate_score_errors(
    n = n_games,
    noise_scenario = noise_scenario,
    target_sd = score_error_sd
  )
  
  games$expected_home_score <-
    expected_home_score
  
  games$expected_away_score <-
    expected_away_score
  
  games$true_home_rating <-
    true_home_rating
  
  games$true_away_rating <-
    true_away_rating
  
  games$home_score <- round(
    pmax(
      0,
      expected_home_score +
        home_error
    )
  )
  
  games$away_score <- round(
    pmax(
      0,
      expected_away_score +
        away_error
    )
  )
  
  games$actual_margin <- (
    games$home_score -
      games$away_score
  )
  
  games$true_expected_margin <- (
    true_home_rating -
      true_away_rating +
      true_hfa
  )
  
  return(games)
}


# ============================================================
# 22. GENERATE ONE DYNAMIC EXPERIMENT SEASON
# ============================================================

generate_dynamic_experiment_season <- function(
    season_seed,
    requested_drift_sd
) {
  
  set.seed(
    season_seed
  )
  
  schedule_result <- generate_unbalanced_schedule(
    team_info = team_info,
    n_weeks = n_weeks
  )
  
  schedule <- schedule_result$schedule
  
  if (
    validate_each_generated_schedule
  ) {
    
    validate_schedule(
      schedule = schedule,
      team_info = team_info,
      n_weeks = n_weeks
    )
  }
  
  true_path <- generate_dynamic_true_path(
    league_teams = teams,
    n_calendar_weeks = n_weeks,
    target_rating_sd =
      target_true_rating_sd,
    requested_drift_sd =
      requested_drift_sd,
    offense_scale =
      initial_offense_scale,
    defense_scale =
      initial_defense_scale
  )
  
  games <- simulate_dynamic_score_season(
    schedule = schedule,
    true_path = true_path,
    noise_scenario = noise_scenario,
    true_hfa = true_hfa,
    mean_points = 24,
    score_error_sd = score_noise_sd
  )
  
  realized_drift_values <- vapply(
    true_path,
    function(x) {
      x$realized_drift_sd
    },
    numeric(1)
  )
  
  return(
    list(
      schedule = schedule,
      games = games,
      true_path = true_path,
      realized_drift_values =
        realized_drift_values
    )
  )
}


# ============================================================
# 23. HFA SHRINKAGE
# ============================================================

get_hfa_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <- (
    2 *
      nrow(data)
  ) /
    length(league_teams)
  
  if (
    avg_games_played <= 2
  ) {
    
    return(10)
    
  } else if (
    avg_games_played <= 4
  ) {
    
    return(8)
    
  } else if (
    avg_games_played <= 6
  ) {
    
    return(5)
    
  } else {
    
    return(2)
  }
}


# ============================================================
# 24. RECENCY WEIGHTS
# ============================================================

make_recency_weights_custom <- function(
    training_games,
    half_life
) {
  
  if (
    is.infinite(half_life)
  ) {
    
    return(
      rep(
        1,
        nrow(training_games)
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
  
  normalized_weights <- (
    raw_weights /
      mean(raw_weights)
  )
  
  return(
    normalized_weights
  )
}


# ============================================================
# 25. FIT HUBER MODEL AT SPECIFIED HALF-LIFE
# ============================================================

fit_huber_recency_model <- function(
    training_games,
    league_teams,
    half_life
) {
  
  game_weights <- make_recency_weights_custom(
    training_games = training_games,
    half_life = half_life
  )
  
  hfa_lambda <- get_hfa_lambda(
    data = training_games,
    league_teams = league_teams
  )
  
  fit <- fit_score_offdef_huber(
    data = training_games,
    league_teams = league_teams,
    component_lambda =
      huber_component_lambda,
    huber_k = huber_k,
    hfa_lambda = hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = game_weights,
    max_iterations =
      max_huber_iterations,
    tolerance =
      huber_tolerance
  )
  
  fit <- standardize_model_output(
    fit = fit,
    model_name = paste(
      "Huber",
      half_life
    ),
    league_teams = league_teams
  )
  
  return(fit)
}


# ============================================================
# 26. RECENT RATING-MOVEMENT SIGNAL
# ============================================================

calculate_recent_movement_signal <- function(
    no_decay_rating_history,
    signal_window = 3
) {
  
  history_length <- length(
    no_decay_rating_history
  )
  
  if (
    history_length < 2
  ) {
    
    return(NA_real_)
  }
  
  step_signals <- numeric(
    history_length - 1
  )
  
  for (
    i in
    2:history_length
  ) {
    
    current_rating <-
      no_decay_rating_history[[i]]
    
    previous_rating <-
      no_decay_rating_history[[i - 1]]
    
    step_signals[i - 1] <- sd(
      current_rating -
        previous_rating
    )
  }
  
  number_to_use <- min(
    signal_window,
    length(step_signals)
  )
  
  recent_signals <- tail(
    step_signals,
    number_to_use
  )
  
  return(
    mean(
      recent_signals
    )
  )
}


# ============================================================
# 27. ADAPTIVE HALF-LIFE MAPPING
# ============================================================

map_estimated_drift_to_half_life <- function(
    estimated_drift
) {
  
  if (
    is.na(estimated_drift) ||
    estimated_drift <
    adaptive_no_decay_threshold
  ) {
    
    return(Inf)
  }
  
  half_life <- (
    20 -
      8 *
      estimated_drift
  )
  
  half_life <- max(
    adaptive_min_half_life,
    half_life
  )
  
  half_life <- min(
    adaptive_max_half_life,
    half_life
  )
  
  return(
    half_life
  )
}


# ============================================================
# 28. ESTIMATE DRIFT FROM FROZEN CALIBRATION
# ============================================================

estimate_drift_from_signal <- function(
    signal,
    completed_games_per_team,
    calibration_table
) {
  
  if (
    is.na(signal) ||
    completed_games_per_team < 2
  ) {
    
    return(0)
  }
  
  row <- calibration_table[
    calibration_table$completed_games_per_team ==
      completed_games_per_team,
  ]
  
  if (
    nrow(row) != 1
  ) {
    
    stop(
      paste0(
        "Expected exactly one drift-calibration row for completed_games = ",
        completed_games_per_team,
        "."
      )
    )
  }
  
  estimate <- (
    row$intercept +
      row$slope *
      signal
  )
  
  estimate <- max(
    min(
      estimate,
      max(drift_sd_grid)
    ),
    min(drift_sd_grid)
  )
  
  return(
    as.numeric(
      estimate
    )
  )
}


# ============================================================
# 29. TRUE RATING AT CURRENT SNAPSHOT
# ============================================================

get_true_rating_at_week <- function(
    true_path,
    calendar_week,
    league_teams
) {
  
  ratings <- true_path[[calendar_week]]$rating[
    league_teams
  ]
  
  names(ratings) <- league_teams
  
  return(ratings)
}



# 30A. SCORE OFFENSE / DEFENSE DESIGN MATRIX
# ============================================================

build_score_design <- function(
    data,
    league_teams
) {
  
  n_games <- nrow(data)
  n_teams <- length(league_teams)
  
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
    nrow = 2 * n_games,
    ncol = 2 + 2 * n_teams
  )
  
  colnames(X) <- c(
    "Intercept",
    "HFA",
    offense_columns,
    defense_columns
  )
  
  y <- numeric(
    2 * n_games
  )
  
  for (i in seq_len(n_games)) {
    
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
  
  return(
    list(
      X = X,
      y = y,
      offense_columns = offense_columns,
      defense_columns = defense_columns
    )
  )
}


# ============================================================
# 30B. WEIGHTED RIDGE SOLVER
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
    STATS = sqrt_weights,
    FUN = "*"
  )
  
  y_weighted <- y *
    sqrt_weights
  
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
  
  names(beta) <- colnames(X)
  
  return(beta)
}


# ============================================================
# 30C. NORMAL OFFENSE / DEFENSE SCORE RIDGE MODEL
# ============================================================

fit_score_offdef_ridge <- function(
    data,
    league_teams,
    component_lambda,
    hfa_lambda,
    hfa_prior = 2.5,
    game_weights = NULL
) {
  
  design <- build_score_design(
    data = data,
    league_teams = league_teams
  )
  
  X <- design$X
  y <- design$y
  
  n_teams <- length(
    league_teams
  )
  
  if (is.null(game_weights)) {
    
    game_weights <- rep(
      1,
      nrow(data)
    )
  }
  
  observation_weights <- rep(
    game_weights,
    each = 2
  )
  
  penalty <- diag(
    c(
      0,
      hfa_lambda,
      rep(
        component_lambda,
        2 * n_teams
      )
    )
  )
  
  prior_target <- c(
    0,
    hfa_prior,
    rep(
      0,
      2 * n_teams
    )
  )
  
  beta <- solve_weighted_ridge(
    X = X,
    y = y,
    observation_weights = observation_weights,
    penalty = penalty,
    prior_target = prior_target
  )
  
  offense <- beta[
    design$offense_columns
  ]
  
  defense <- beta[
    design$defense_columns
  ]
  
  names(offense) <- league_teams
  names(defense) <- league_teams
  
  ratings <- offense +
    defense
  
  ratings <- ratings -
    mean(ratings)
  
  names(ratings) <- league_teams
  
  return(
    list(
      ratings = ratings,
      offense = offense,
      defense = defense,
      hfa = as.numeric(
        beta["HFA"]
      ),
      home_field = as.numeric(
        beta["HFA"]
      ),
      league_mean = as.numeric(
        beta["Intercept"]
      )
    )
  )
}


# ============================================================
# 30D. ROBUST HUBER OFFENSE / DEFENSE SCORE MODEL
# ============================================================

fit_score_offdef_huber <- function(
    data,
    league_teams,
    component_lambda,
    huber_k,
    hfa_lambda,
    hfa_prior = 2.5,
    game_weights = NULL,
    max_iterations = 40,
    tolerance = 1e-6
) {
  
  design <- build_score_design(
    data = data,
    league_teams = league_teams
  )
  
  X <- design$X
  y <- design$y
  
  n_teams <- length(
    league_teams
  )
  
  if (is.null(game_weights)) {
    
    game_weights <- rep(
      1,
      nrow(data)
    )
  }
  
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
        2 * n_teams
      )
    )
  )
  
  prior_target <- c(
    0,
    hfa_prior,
    rep(
      0,
      2 * n_teams
    )
  )
  
  beta <- solve_weighted_ridge(
    X = X,
    y = y,
    observation_weights = base_weights,
    penalty = penalty,
    prior_target = prior_target
  )
  
  for (iteration in seq_len(max_iterations)) {
    
    residuals <- as.numeric(
      y -
        X %*%
        beta
    )
    
    residual_median <- median(
      residuals
    )
    
    robust_scale <- 1.4826 *
      median(
        abs(
          residuals -
            residual_median
        )
      )
    
    if (
      !is.finite(robust_scale) ||
      robust_scale < 1e-8
    ) {
      
      robust_scale <- sd(
        residuals
      )
    }
    
    if (
      !is.finite(robust_scale) ||
      robust_scale < 1e-8
    ) {
      
      robust_scale <- 1
    }
    
    standardized_absolute_residual <- abs(
      residuals
    ) /
      robust_scale
    
    huber_weights <- rep(
      1,
      length(residuals)
    )
    
    large_residual <- standardized_absolute_residual >
      huber_k
    
    huber_weights[
      large_residual
    ] <- huber_k /
      standardized_absolute_residual[
        large_residual
      ]
    
    combined_weights <- base_weights *
      huber_weights
    
    new_beta <- solve_weighted_ridge(
      X = X,
      y = y,
      observation_weights = combined_weights,
      penalty = penalty,
      prior_target = prior_target
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
      
      break
    }
  }
  
  offense <- beta[
    design$offense_columns
  ]
  
  defense <- beta[
    design$defense_columns
  ]
  
  names(offense) <- league_teams
  names(defense) <- league_teams
  
  ratings <- offense +
    defense
  
  ratings <- ratings -
    mean(ratings)
  
  names(ratings) <- league_teams
  
  return(
    list(
      ratings = ratings,
      offense = offense,
      defense = defense,
      hfa = as.numeric(
        beta["HFA"]
      ),
      home_field = as.numeric(
        beta["HFA"]
      ),
      league_mean = as.numeric(
        beta["Intercept"]
      )
    )
  )
}


# ============================================================
# 30E. SAFE CORRELATION
# ============================================================

safe_cor <- function(
    x,
    y,
    method = "pearson"
) {
  
  if (
    length(x) < 2 ||
    length(y) < 2 ||
    sd(x) == 0 ||
    sd(y) == 0
  ) {
    
    return(NA_real_)
  }
  
  return(
    cor(
      x,
      y,
      method = method
    )
  )
}


# ============================================================

# 30F. SELECT TRAINING DATA AFTER N COMPLETED GAMES / TEAM
# ============================================================

get_training_snapshot <- function(
    games,
    completed_games_per_team
) {
  
  active_weeks <- sort(
    unique(
      games$week
    )
  )
  
  current_week <- active_weeks[
    completed_games_per_team
  ]
  
  training_games <- games[
    games$week <=
      current_week,
  ]
  
  return(
    list(
      calendar_week = current_week,
      training_games = training_games
    )
  )
}


# ============================================================
# 30G. STANDARDIZE MODEL OUTPUTS
# ============================================================

standardize_model_output <- function(
    fit,
    model_name,
    league_teams
) {
  
  if (
    is.null(fit$ratings) ||
    length(fit$ratings) != length(league_teams)
  ) {
    
    stop(
      paste0(
        "Model ",
        model_name,
        " returned invalid ratings. Expected ",
        length(league_teams),
        " values; received ",
        length(fit$ratings),
        "."
      )
    )
  }
  
  if (
    is.null(fit$hfa) &&
    !is.null(fit$home_field)
  ) {
    
    fit$hfa <- fit$home_field
  }
  
  if (
    is.null(fit$home_field) &&
    !is.null(fit$hfa)
  ) {
    
    fit$home_field <- fit$hfa
  }
  
  if (
    is.null(fit$hfa) ||
    length(fit$hfa) != 1L ||
    is.na(fit$hfa)
  ) {
    
    stop(
      paste0(
        "Model ",
        model_name,
        " returned invalid HFA. hfa length = ",
        length(fit$hfa),
        "; home_field length = ",
        length(fit$home_field),
        "."
      )
    )
  }
  
  return(
    fit
  )
}


# ============================================================


# ============================================================
# 30. CALIBRATE OBSERVABLE DRIFT ESTIMATOR
# ============================================================

cat("\n========================================\n")
cat("VERSION 9B-MC5\n")
cat("HUBER RECENCY UNDER DYNAMIC STRENGTH\n")
cat("========================================\n\n")

cat("Teams:", length(teams), "\n")
cat("Games per team:", games_per_team, "\n")
cat("Calendar weeks:", n_weeks, "\n")
cat("True cross-sectional rating SD:", target_true_rating_sd, "\n")
cat("Noise scenario:", noise_scenario, "\n")
cat("Huber lambda:", huber_component_lambda, "\n")
cat("Huber k:", huber_k, "\n")
cat(
  "Drift SD grid:",
  paste(
    drift_sd_grid,
    collapse = ", "
  ),
  "\n"
)
cat(
  "Calibration seasons per drift:",
  n_calibration_seasons_per_drift,
  "\n"
)
cat(
  "Evaluation seasons per drift:",
  n_evaluation_seasons_per_drift,
  "\n\n"
)

calibration_rows <- list()
calibration_row_index <- 1

for (
  drift_index in
  seq_along(drift_sd_grid)
) {
  
  true_drift_sd <- drift_sd_grid[
    drift_index
  ]
  
  cat(
    "Calibrating drift",
    true_drift_sd,
    "\n"
  )
  
  for (
    simulation in
    seq_len(
      n_calibration_seasons_per_drift
    )
  ) {
    
    season_seed <- (
      calibration_seed_base +
        drift_index *
        100000 +
        simulation
    )
    
    season_data <-
      generate_dynamic_experiment_season(
        season_seed = season_seed,
        requested_drift_sd =
          true_drift_sd
      )
    
    no_decay_history <- list()
    
    for (
      completed_games in
      1:games_per_team
    ) {
      
      snapshot <- get_training_snapshot(
        games = season_data$games,
        completed_games_per_team =
          completed_games
      )
      
      no_decay_fit <- fit_huber_recency_model(
        training_games =
          snapshot$training_games,
        league_teams = teams,
        half_life = Inf
      )
      
      no_decay_history[[completed_games]] <-
        no_decay_fit$ratings[
          teams
        ]
      
      movement_signal <-
        calculate_recent_movement_signal(
          no_decay_rating_history =
            no_decay_history,
          signal_window =
            adaptive_signal_window
        )
      
      if (
        completed_games >= 2
      ) {
        
        calibration_rows[[calibration_row_index]] <-
          data.frame(
            true_drift_sd =
              true_drift_sd,
            simulation =
              simulation,
            completed_games_per_team =
              completed_games,
            calendar_week =
              snapshot$calendar_week,
            movement_signal =
              movement_signal,
            stringsAsFactors =
              FALSE
          )
        
        calibration_row_index <-
          calibration_row_index + 1
      }
    }
  }
}

drift_calibration_data <- do.call(
  rbind,
  calibration_rows
)

rownames(
  drift_calibration_data
) <- NULL

drift_calibration_table_rows <- list()

for (
  completed_games in
  2:games_per_team
) {
  
  current <- drift_calibration_data[
    drift_calibration_data$completed_games_per_team ==
      completed_games,
  ]
  
  calibration_model <- lm(
    true_drift_sd ~
      movement_signal,
    data = current
  )
  
  model_summary <- summary(
    calibration_model
  )
  
  predicted <- predict(
    calibration_model,
    newdata = current
  )
  
  drift_calibration_table_rows[[completed_games - 1]] <-
    data.frame(
      completed_games_per_team =
        completed_games,
      intercept =
        unname(
          coef(
            calibration_model
          )[1]
        ),
      slope =
        unname(
          coef(
            calibration_model
          )[2]
        ),
      r_squared =
        model_summary$r.squared,
      calibration_RMSE =
        sqrt(
          mean(
            (
              predicted -
                current$true_drift_sd
            )^2
          )
        ),
      calibration_MAE =
        mean(
          abs(
            predicted -
              current$true_drift_sd
          )
        ),
      mean_signal =
        mean(
          current$movement_signal
        ),
      stringsAsFactors =
        FALSE
    )
}

drift_calibration_table <- do.call(
  rbind,
  drift_calibration_table_rows
)

rownames(
  drift_calibration_table
) <- NULL

cat("\nDRIFT CALIBRATION TABLE:\n\n")
print(
  drift_calibration_table,
  row.names = FALSE
)


# ============================================================
# 31. EVALUATE FIVE RECENCY STRATEGIES
# ============================================================

team_level_rows <- list()
season_level_rows <- list()
adaptive_diagnostic_rows <- list()

team_row_index <- 1
season_row_index <- 1
adaptive_row_index <- 1

for (
  drift_index in
  seq_along(drift_sd_grid)
) {
  
  true_drift_sd <- drift_sd_grid[
    drift_index
  ]
  
  cat("\n========================================\n")
  cat(
    "EVALUATING TRUE DRIFT SD =",
    true_drift_sd,
    "\n"
  )
  cat("========================================\n")
  
  for (
    simulation in
    seq_len(
      n_evaluation_seasons_per_drift
    )
  ) {
    
    if (
      simulation == 1 ||
      simulation %% 50 == 0 ||
      simulation ==
      n_evaluation_seasons_per_drift
    ) {
      
      cat(
        "Season",
        simulation,
        "of",
        n_evaluation_seasons_per_drift,
        "\n"
      )
    }
    
    season_seed <- (
      evaluation_seed_base +
        drift_index *
        100000 +
        simulation
    )
    
    season_data <-
      generate_dynamic_experiment_season(
        season_seed = season_seed,
        requested_drift_sd =
          true_drift_sd
      )
    
    no_decay_history <- list()
    
    for (
      completed_games in
      1:games_per_team
    ) {
      
      snapshot <- get_training_snapshot(
        games = season_data$games,
        completed_games_per_team =
          completed_games
      )
      
      current_true_rating <-
        get_true_rating_at_week(
          true_path =
            season_data$true_path,
          calendar_week =
            snapshot$calendar_week,
          league_teams =
            teams
        )
      
      true_rank <- rank(
        -current_true_rating,
        ties.method = "average"
      )
      
      # ------------------------------------------------------
      # Fit No Decay first because its movement history drives
      # the observable adaptive-recency signal.
      # ------------------------------------------------------
      
      no_decay_fit <-
        fit_huber_recency_model(
          training_games =
            snapshot$training_games,
          league_teams = teams,
          half_life = Inf
        )
      
      no_decay_history[[completed_games]] <-
        no_decay_fit$ratings[
          teams
        ]
      
      movement_signal <-
        calculate_recent_movement_signal(
          no_decay_rating_history =
            no_decay_history,
          signal_window =
            adaptive_signal_window
        )
      
      estimated_drift <-
        estimate_drift_from_signal(
          signal = movement_signal,
          completed_games_per_team =
            completed_games,
          calibration_table =
            drift_calibration_table
        )
      
      adaptive_half_life <-
        map_estimated_drift_to_half_life(
          estimated_drift
        )
      
      model_fits <- list()
      
      model_fits[["Huber No Decay"]] <-
        no_decay_fit
      
      model_fits[["Huber HL16"]] <-
        fit_huber_recency_model(
          training_games =
            snapshot$training_games,
          league_teams = teams,
          half_life = 16
        )
      
      model_fits[["Huber HL12"]] <-
        fit_huber_recency_model(
          training_games =
            snapshot$training_games,
          league_teams = teams,
          half_life = 12
        )
      
      model_fits[["Huber HL8"]] <-
        fit_huber_recency_model(
          training_games =
            snapshot$training_games,
          league_teams = teams,
          half_life = 8
        )
      
      model_fits[["Huber Adaptive"]] <-
        fit_huber_recency_model(
          training_games =
            snapshot$training_games,
          league_teams = teams,
          half_life =
            adaptive_half_life
        )
      
      adaptive_diagnostic_rows[[adaptive_row_index]] <-
        data.frame(
          true_drift_sd =
            true_drift_sd,
          simulation =
            simulation,
          completed_games_per_team =
            completed_games,
          calendar_week =
            snapshot$calendar_week,
          movement_signal =
            movement_signal,
          estimated_drift =
            estimated_drift,
          drift_estimation_error =
            estimated_drift -
            true_drift_sd,
          absolute_drift_estimation_error =
            abs(
              estimated_drift -
                true_drift_sd
            ),
          adaptive_half_life =
            if (
              is.infinite(
                adaptive_half_life
              )
            ) {
              NA_real_
            } else {
              adaptive_half_life
            },
          adaptive_no_decay =
            as.integer(
              is.infinite(
                adaptive_half_life
              )
            ),
          stringsAsFactors =
            FALSE
        )
      
      adaptive_row_index <-
        adaptive_row_index + 1
      
      for (
        model_name in
        model_names
      ) {
        
        current_fit <-
          model_fits[[model_name]]
        
        estimated_rating <-
          current_fit$ratings[
            teams
          ]
        
        rating_error <- (
          estimated_rating -
            current_true_rating[
              teams
            ]
        )
        
        estimated_rank <- rank(
          -estimated_rating,
          ties.method = "average"
        )
        
        absolute_rank_error <- abs(
          estimated_rank -
            true_rank[
              teams
            ]
        )
        
        true_top4 <- names(
          sort(
            current_true_rating,
            decreasing = TRUE
          )
        )[1:4]
        
        estimated_top4 <- names(
          sort(
            estimated_rating,
            decreasing = TRUE
          )
        )[1:4]
        
        top4_overlap <- (
          length(
            intersect(
              true_top4,
              estimated_top4
            )
          ) /
            4
        )
        
        true_number1 <- names(
          which.max(
            current_true_rating
          )
        )
        
        estimated_number1 <- names(
          which.max(
            estimated_rating
          )
        )
        
        team_level_rows[[team_row_index]] <-
          data.frame(
            true_drift_sd =
              true_drift_sd,
            simulation =
              simulation,
            completed_games_per_team =
              completed_games,
            calendar_week =
              snapshot$calendar_week,
            model =
              model_name,
            team =
              teams,
            true_rating =
              as.numeric(
                current_true_rating[
                  teams
                ]
              ),
            estimated_rating =
              as.numeric(
                estimated_rating
              ),
            rating_error =
              as.numeric(
                rating_error
              ),
            absolute_rating_error =
              abs(
                as.numeric(
                  rating_error
                )
              ),
            squared_rating_error =
              as.numeric(
                rating_error
              )^2,
            true_rank =
              as.numeric(
                true_rank[
                  teams
                ]
              ),
            estimated_rank =
              as.numeric(
                estimated_rank
              ),
            absolute_rank_error =
              as.numeric(
                absolute_rank_error
              ),
            stringsAsFactors =
              FALSE
          )
        
        team_row_index <-
          team_row_index + 1
        
        season_level_rows[[season_row_index]] <-
          data.frame(
            true_drift_sd =
              true_drift_sd,
            simulation =
              simulation,
            completed_games_per_team =
              completed_games,
            calendar_week =
              snapshot$calendar_week,
            model =
              model_name,
            season_RMSE =
              sqrt(
                mean(
                  rating_error^2
                )
              ),
            season_MAE =
              mean(
                abs(
                  rating_error
                )
              ),
            Pearson =
              safe_cor(
                estimated_rating,
                current_true_rating[
                  teams
                ],
                method = "pearson"
              ),
            Spearman =
              safe_cor(
                estimated_rating,
                current_true_rating[
                  teams
                ],
                method = "spearman"
              ),
            mean_absolute_rank_error =
              mean(
                absolute_rank_error
              ),
            top4_overlap =
              top4_overlap,
            number1_correct =
              as.integer(
                estimated_number1 ==
                  true_number1
              ),
            estimated_HFA =
              current_fit$hfa,
            stringsAsFactors =
              FALSE
          )
        
        season_row_index <-
          season_row_index + 1
      }
    }
  }
}

team_level <- do.call(
  rbind,
  team_level_rows
)

season_level <- do.call(
  rbind,
  season_level_rows
)

adaptive_diagnostics <- do.call(
  rbind,
  adaptive_diagnostic_rows
)

rownames(team_level) <- NULL
rownames(season_level) <- NULL
rownames(adaptive_diagnostics) <- NULL


# ============================================================
# 32. ACCURACY BY DRIFT / GAMES / MODEL
# ============================================================

summary_rows <- list()
summary_index <- 1

for (
  true_drift_sd in
  drift_sd_grid
) {
  
  for (
    completed_games in
    1:games_per_team
  ) {
    
    for (
      model_name in
      model_names
    ) {
      
      team_subset <- team_level[
        team_level$true_drift_sd ==
          true_drift_sd &
          team_level$completed_games_per_team ==
          completed_games &
          team_level$model ==
          model_name,
      ]
      
      season_subset <- season_level[
        season_level$true_drift_sd ==
          true_drift_sd &
          season_level$completed_games_per_team ==
          completed_games &
          season_level$model ==
          model_name,
      ]
      
      summary_rows[[summary_index]] <-
        data.frame(
          true_drift_sd =
            true_drift_sd,
          completed_games_per_team =
            completed_games,
          mean_calendar_week =
            mean(
              season_subset$calendar_week
            ),
          model =
            model_name,
          Rating_RMSE =
            sqrt(
              mean(
                team_subset$squared_rating_error
              )
            ),
          Rating_MAE =
            mean(
              team_subset$absolute_rating_error
            ),
          Rating_Bias =
            mean(
              team_subset$rating_error
            ),
          Mean_Season_RMSE =
            mean(
              season_subset$season_RMSE
            ),
          Median_Season_RMSE =
            median(
              season_subset$season_RMSE
            ),
          Mean_Season_MAE =
            mean(
              season_subset$season_MAE
            ),
          Mean_Pearson =
            mean(
              season_subset$Pearson,
              na.rm = TRUE
            ),
          Mean_Spearman =
            mean(
              season_subset$Spearman,
              na.rm = TRUE
            ),
          Mean_Absolute_Rank_Error =
            mean(
              season_subset$mean_absolute_rank_error
            ),
          Mean_Top4_Overlap =
            mean(
              season_subset$top4_overlap
            ),
          Probability_Number1_Correct =
            mean(
              season_subset$number1_correct
            ),
          Probability_Within_1_Point =
            mean(
              team_subset$absolute_rating_error <=
                1
            ),
          Probability_Within_2_Points =
            mean(
              team_subset$absolute_rating_error <=
                2
            ),
          Probability_Within_3_Points =
            mean(
              team_subset$absolute_rating_error <=
                3
            ),
          Mean_Estimated_HFA =
            mean(
              season_subset$estimated_HFA
            ),
          Mean_Absolute_HFA_Error =
            mean(
              abs(
                season_subset$estimated_HFA -
                  true_hfa
              )
            ),
          stringsAsFactors =
            FALSE
        )
      
      summary_index <-
        summary_index + 1
    }
  }
}

accuracy_summary <- do.call(
  rbind,
  summary_rows
)

rownames(
  accuracy_summary
) <- NULL


# ============================================================
# 33. CALIBRATED 95% INTERVALS
# ============================================================

interval_rows <- list()
interval_index <- 1

for (
  true_drift_sd in
  drift_sd_grid
) {
  
  for (
    completed_games in
    1:games_per_team
  ) {
    
    for (
      model_name in
      model_names
    ) {
      
      current <- team_level[
        team_level$true_drift_sd ==
          true_drift_sd &
          team_level$completed_games_per_team ==
          completed_games &
          team_level$model ==
          model_name,
      ]
      
      season_ids <- sort(
        unique(
          current$simulation
        )
      )
      
      n_calibration <- floor(
        length(season_ids) *
          interval_calibration_fraction
      )
      
      calibration_ids <- season_ids[
        seq_len(
          n_calibration
        )
      ]
      
      holdout_ids <- season_ids[
        (
          n_calibration + 1
        ):
          length(season_ids)
      ]
      
      calibration_data <- current[
        current$simulation %in%
          calibration_ids,
      ]
      
      holdout_data <- current[
        current$simulation %in%
          holdout_ids,
      ]
      
      half_width <- as.numeric(
        quantile(
          calibration_data$absolute_rating_error,
          probs = 0.95,
          names = FALSE,
          type = 8
        )
      )
      
      interval_rows[[interval_index]] <-
        data.frame(
          true_drift_sd =
            true_drift_sd,
          completed_games_per_team =
            completed_games,
          model =
            model_name,
          empirical_95_half_width =
            half_width,
          empirical_95_full_width =
            2 *
            half_width,
          holdout_coverage =
            mean(
              holdout_data$absolute_rating_error <=
                half_width
            ),
          holdout_RMSE =
            sqrt(
              mean(
                holdout_data$squared_rating_error
              )
            ),
          holdout_MAE =
            mean(
              holdout_data$absolute_rating_error
            ),
          stringsAsFactors =
            FALSE
        )
      
      interval_index <-
        interval_index + 1
    }
  }
}

interval_summary <- do.call(
  rbind,
  interval_rows
)

rownames(
  interval_summary
) <- NULL


# ============================================================
# 34. ADAPTIVE ESTIMATOR DIAGNOSTICS
# ============================================================

adaptive_summary_rows <- list()
adaptive_summary_index <- 1

for (
  true_drift_sd in
  drift_sd_grid
) {
  
  for (
    completed_games in
    1:games_per_team
  ) {
    
    current <- adaptive_diagnostics[
      adaptive_diagnostics$true_drift_sd ==
        true_drift_sd &
        adaptive_diagnostics$completed_games_per_team ==
        completed_games,
    ]
    
    finite_half_life <- current$adaptive_half_life[
      is.finite(
        current$adaptive_half_life
      )
    ]
    
    adaptive_summary_rows[[adaptive_summary_index]] <-
      data.frame(
        true_drift_sd =
          true_drift_sd,
        completed_games_per_team =
          completed_games,
        mean_estimated_drift =
          mean(
            current$estimated_drift
          ),
        drift_estimate_MAE =
          mean(
            current$absolute_drift_estimation_error
          ),
        drift_estimate_RMSE =
          sqrt(
            mean(
              current$drift_estimation_error^2
            )
          ),
        mean_movement_signal =
          mean(
            current$movement_signal,
            na.rm = TRUE
          ),
        probability_no_decay =
          mean(
            current$adaptive_no_decay
          ),
        mean_finite_half_life =
          if (
            length(
              finite_half_life
            ) == 0
          ) {
            NA_real_
          } else {
            mean(
              finite_half_life
            )
          },
        stringsAsFactors =
          FALSE
      )
    
    adaptive_summary_index <-
      adaptive_summary_index + 1
  }
}

adaptive_summary <- do.call(
  rbind,
  adaptive_summary_rows
)

rownames(
  adaptive_summary
) <- NULL


# ============================================================
# 35. FINAL 12-GAME SUMMARY
# ============================================================

final_accuracy <- accuracy_summary[
  accuracy_summary$completed_games_per_team ==
    games_per_team,
]

final_intervals <- interval_summary[
  interval_summary$completed_games_per_team ==
    games_per_team,
]

final_summary <- merge(
  final_accuracy,
  final_intervals[
    ,
    c(
      "true_drift_sd",
      "model",
      "empirical_95_half_width",
      "empirical_95_full_width",
      "holdout_coverage"
    )
  ],
  by = c(
    "true_drift_sd",
    "model"
  ),
  all.x = TRUE,
  sort = FALSE
)

final_summary <- final_summary[
  order(
    final_summary$true_drift_sd,
    final_summary$Rating_RMSE
  ),
]

rownames(
  final_summary
) <- NULL


# ============================================================
# 36. PAIRED FINAL DIFFERENCES VS HL12
# ============================================================

paired_rows <- list()
paired_index <- 1

for (
  true_drift_sd in
  drift_sd_grid
) {
  
  reference <- season_level[
    season_level$true_drift_sd ==
      true_drift_sd &
      season_level$completed_games_per_team ==
      games_per_team &
      season_level$model ==
      "Huber HL12",
    c(
      "simulation",
      "season_RMSE",
      "season_MAE"
    )
  ]
  
  names(reference)[2:3] <- c(
    "reference_RMSE",
    "reference_MAE"
  )
  
  for (
    model_name in
    model_names
  ) {
    
    candidate <- season_level[
      season_level$true_drift_sd ==
        true_drift_sd &
        season_level$completed_games_per_team ==
        games_per_team &
        season_level$model ==
        model_name,
      c(
        "simulation",
        "season_RMSE",
        "season_MAE"
      )
    ]
    
    names(candidate)[2:3] <- c(
      "candidate_RMSE",
      "candidate_MAE"
    )
    
    paired <- merge(
      reference,
      candidate,
      by = "simulation",
      sort = TRUE
    )
    
    rmse_difference <- (
      paired$candidate_RMSE -
        paired$reference_RMSE
    )
    
    mae_difference <- (
      paired$candidate_MAE -
        paired$reference_MAE
    )
    
    rmse_mcse <- sd(
      rmse_difference
    ) /
      sqrt(
        length(
          rmse_difference
        )
      )
    
    mae_mcse <- sd(
      mae_difference
    ) /
      sqrt(
        length(
          mae_difference
        )
      )
    
    paired_rows[[paired_index]] <-
      data.frame(
        true_drift_sd =
          true_drift_sd,
        model =
          model_name,
        mean_RMSE_difference_vs_HL12 =
          mean(
            rmse_difference
          ),
        RMSE_CI_lower =
          mean(
            rmse_difference
          ) -
          1.96 *
          rmse_mcse,
        RMSE_CI_upper =
          mean(
            rmse_difference
          ) +
          1.96 *
          rmse_mcse,
        probability_lower_RMSE_than_HL12 =
          mean(
            rmse_difference < 0
          ),
        mean_MAE_difference_vs_HL12 =
          mean(
            mae_difference
          ),
        MAE_CI_lower =
          mean(
            mae_difference
          ) -
          1.96 *
          mae_mcse,
        MAE_CI_upper =
          mean(
            mae_difference
          ) +
          1.96 *
          mae_mcse,
        probability_lower_MAE_than_HL12 =
          mean(
            mae_difference < 0
          ),
        stringsAsFactors =
          FALSE
      )
    
    paired_index <-
      paired_index + 1
  }
}

paired_final <- do.call(
  rbind,
  paired_rows
)

rownames(
  paired_final
) <- NULL


# ============================================================
# 37. WINNER BY DRIFT SCENARIO
# ============================================================

winner_rows <- list()

for (
  drift_index in
  seq_along(
    drift_sd_grid
  )
) {
  
  true_drift_sd <- drift_sd_grid[
    drift_index
  ]
  
  current <- final_summary[
    final_summary$true_drift_sd ==
      true_drift_sd,
  ]
  
  best_rmse_row <- current[
    which.min(
      current$Rating_RMSE
    ),
  ]
  
  best_mae_row <- current[
    which.min(
      current$Rating_MAE
    ),
  ]
  
  narrow_row <- current[
    which.min(
      current$empirical_95_full_width
    ),
  ]
  
  winner_rows[[drift_index]] <-
    data.frame(
      true_drift_sd =
        true_drift_sd,
      best_RMSE_model =
        best_rmse_row$model,
      best_RMSE =
        best_rmse_row$Rating_RMSE,
      best_MAE_model =
        best_mae_row$model,
      best_MAE =
        best_mae_row$Rating_MAE,
      narrowest_calibrated_95_model =
        narrow_row$model,
      calibrated_95_width =
        narrow_row$empirical_95_full_width,
      calibrated_95_coverage =
        narrow_row$holdout_coverage,
      stringsAsFactors =
        FALSE
    )
}

winner_summary <- do.call(
  rbind,
  winner_rows
)

rownames(
  winner_summary
) <- NULL


# ============================================================
# 38. ROBUSTNESS / REGRET ACROSS UNKNOWN DRIFT
# ============================================================

regret_rows <- list()
regret_index <- 1

for (
  true_drift_sd in
  drift_sd_grid
) {
  
  current <- final_summary[
    final_summary$true_drift_sd ==
      true_drift_sd,
  ]
  
  best_rmse <- min(
    current$Rating_RMSE
  )
  
  best_width <- min(
    current$empirical_95_full_width
  )
  
  for (
    row_index in
    seq_len(
      nrow(current)
    )
  ) {
    
    regret_rows[[regret_index]] <-
      data.frame(
        true_drift_sd =
          true_drift_sd,
        model =
          current$model[
            row_index
          ],
        Rating_RMSE =
          current$Rating_RMSE[
            row_index
          ],
        Rating_MAE =
          current$Rating_MAE[
            row_index
          ],
        interval_width =
          current$empirical_95_full_width[
            row_index
          ],
        interval_coverage =
          current$holdout_coverage[
            row_index
          ],
        RMSE_regret =
          current$Rating_RMSE[
            row_index
          ] -
          best_rmse,
        interval_width_regret =
          current$empirical_95_full_width[
            row_index
          ] -
          best_width,
        stringsAsFactors =
          FALSE
      )
    
    regret_index <-
      regret_index + 1
  }
}

regret_table <- do.call(
  rbind,
  regret_rows
)

robustness_rows <- list()

for (
  model_index in
  seq_along(
    model_names
  )
) {
  
  model_name <- model_names[
    model_index
  ]
  
  current <- regret_table[
    regret_table$model ==
      model_name,
  ]
  
  robustness_rows[[model_index]] <-
    data.frame(
      model =
        model_name,
      mean_RMSE =
        mean(
          current$Rating_RMSE
        ),
      worst_drift_RMSE =
        max(
          current$Rating_RMSE
        ),
      mean_MAE =
        mean(
          current$Rating_MAE
        ),
      mean_RMSE_regret =
        mean(
          current$RMSE_regret
        ),
      worst_RMSE_regret =
        max(
          current$RMSE_regret
        ),
      RMSE_drift_wins =
        sum(
          current$RMSE_regret <
            1e-12
        ),
      mean_calibrated_95_width =
        mean(
          current$interval_width
        ),
      worst_calibrated_95_width =
        max(
          current$interval_width
        ),
      mean_holdout_coverage =
        mean(
          current$interval_coverage
        ),
      mean_interval_width_regret =
        mean(
          current$interval_width_regret
        ),
      width_drift_wins =
        sum(
          current$interval_width_regret <
            1e-12
        ),
      stringsAsFactors =
        FALSE
    )
}

robustness_summary <- do.call(
  rbind,
  robustness_rows
)

robustness_summary <- robustness_summary[
  order(
    robustness_summary$mean_RMSE_regret,
    robustness_summary$mean_RMSE
  ),
]

rownames(
  robustness_summary
) <- NULL


# ============================================================
# 39. PRINT RESULTS
# ============================================================

cat("\n========================================\n")
cat("FINAL 12-GAME RECENCY RESULTS\n")
cat("========================================\n\n")

print(
  final_summary[
    ,
    c(
      "true_drift_sd",
      "model",
      "Rating_RMSE",
      "Rating_MAE",
      "Mean_Pearson",
      "Mean_Spearman",
      "Mean_Absolute_Rank_Error",
      "Probability_Number1_Correct",
      "Probability_Within_2_Points",
      "Probability_Within_3_Points",
      "empirical_95_full_width",
      "holdout_coverage"
    )
  ],
  row.names = FALSE
)

cat("\n========================================\n")
cat("WINNER BY TRUE DRIFT SD\n")
cat("========================================\n\n")

print(
  winner_summary,
  row.names = FALSE
)

cat("\n========================================\n")
cat("ROBUSTNESS ACROSS UNKNOWN DRIFT\n")
cat("========================================\n\n")

print(
  robustness_summary,
  row.names = FALSE
)

cat("\n========================================\n")
cat("ADAPTIVE RECENCY - FINAL SNAPSHOT\n")
cat("========================================\n\n")

print(
  adaptive_summary[
    adaptive_summary$completed_games_per_team ==
      games_per_team,
  ],
  row.names = FALSE
)


# ============================================================
# 40. WRITE CSV OUTPUTS
# ============================================================

write.csv(
  drift_calibration_data,
  "version9B_MC5_drift_calibration_raw.csv",
  row.names = FALSE
)

write.csv(
  drift_calibration_table,
  "version9B_MC5_drift_calibration_coefficients.csv",
  row.names = FALSE
)

write.csv(
  accuracy_summary,
  "version9B_MC5_accuracy_by_games.csv",
  row.names = FALSE
)

write.csv(
  interval_summary,
  "version9B_MC5_calibrated_intervals.csv",
  row.names = FALSE
)

write.csv(
  adaptive_summary,
  "version9B_MC5_adaptive_diagnostics.csv",
  row.names = FALSE
)

write.csv(
  final_summary,
  "version9B_MC5_final_model_summary.csv",
  row.names = FALSE
)

write.csv(
  paired_final,
  "version9B_MC5_paired_final_comparison.csv",
  row.names = FALSE
)

write.csv(
  winner_summary,
  "version9B_MC5_winner_by_drift.csv",
  row.names = FALSE
)

write.csv(
  robustness_summary,
  "version9B_MC5_robustness_summary.csv",
  row.names = FALSE
)


# ============================================================
# 41. PLOTS
# ============================================================

plot_final_rmse <- ggplot(
  final_summary,
  aes(
    x = true_drift_sd,
    y = Rating_RMSE,
    color = model,
    group = model
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  labs(
    title = "Version 9B-MC5: Final Rating RMSE by True Drift",
    subtitle = "Score OffDef Huber; only recency weighting changes",
    x = "True weekly rating drift SD",
    y = "Final rating RMSE",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC5_final_RMSE_by_drift.png",
  plot = plot_final_rmse,
  width = 11,
  height = 7,
  dpi = 160
)


plot_final_width <- ggplot(
  final_summary,
  aes(
    x = true_drift_sd,
    y = empirical_95_full_width,
    color = model,
    group = model
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  labs(
    title = "Version 9B-MC5: Calibrated 95% Width by True Drift",
    subtitle = "Lower is better when holdout coverage remains near 95%",
    x = "True weekly rating drift SD",
    y = "Full calibrated 95% interval width",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC5_interval_width_by_drift.png",
  plot = plot_final_width,
  width = 11,
  height = 7,
  dpi = 160
)


plot_learning <- ggplot(
  accuracy_summary,
  aes(
    x = completed_games_per_team,
    y = Rating_RMSE,
    color = model,
    group = model
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.5
  ) +
  facet_wrap(
    ~true_drift_sd,
    scales = "free_y"
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  scale_x_continuous(
    breaks = 1:12
  ) +
  labs(
    title = "Version 9B-MC5: Rating RMSE as Games Accumulate",
    x = "Completed games per team",
    y = "Rating RMSE",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC5_learning_curves.png",
  plot = plot_learning,
  width = 14,
  height = 9,
  dpi = 160
)


plot_adaptive <- ggplot(
  adaptive_summary[
    adaptive_summary$completed_games_per_team >=
      2,
  ],
  aes(
    x = completed_games_per_team,
    y = mean_estimated_drift,
    color = factor(
      true_drift_sd
    ),
    group = factor(
      true_drift_sd
    )
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 1.7
  ) +
  geom_hline(
    data = data.frame(
      true_drift_sd =
        drift_sd_grid
    ),
    aes(
      yintercept =
        true_drift_sd
    ),
    inherit.aes = FALSE,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = 2:12
  ) +
  labs(
    title = "Version 9B-MC5: Adaptive Drift Estimates",
    subtitle = "Dashed reference levels correspond to true drift grid",
    x = "Completed games per team",
    y = "Mean estimated drift",
    color = "True drift"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC5_adaptive_drift_estimates.png",
  plot = plot_adaptive,
  width = 11,
  height = 7,
  dpi = 160
)


# ============================================================
# 42. FINAL CONSOLE SUMMARY
# ============================================================

cat("\n========================================\n")
cat("VERSION 9B-MC5 COMPLETE\n")
cat("========================================\n\n")

cat("Models compared:\n")
for (
  model_name in
  model_names
) {
  
  cat(
    "  ",
    model_name,
    "\n"
  )
}

cat("\nDrift grid:\n  ")
cat(
  paste(
    drift_sd_grid,
    collapse = ", "
  ),
  "\n"
)

cat("\nROBUSTNESS SUMMARY:\n\n")
print(
  robustness_summary,
  row.names = FALSE
)

cat("\nDecision rule:\n")
cat(
  "Prefer the recency strategy with low mean and worst-case RMSE regret,\n"
)
cat(
  "while preserving narrow empirically calibrated uncertainty.\n\n"
)
cat(
  "No Decay should dominate at drift near zero. Shorter half-lives should\n"
)
cat(
  "become more valuable as true strength changes faster. The Adaptive\n"
)
cat(
  "variant only earns production use if its observable drift estimate is\n"
)
cat(
  "strong enough to capture that tradeoff without access to hidden truth.\n\n"
)

cat(
  "Output CSVs and PNG plots were written to the current working directory.\n"
)

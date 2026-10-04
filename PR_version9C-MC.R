# ============================================================
# FOOTBALL POWER RATINGS - VERSION 9C-MC
# FORECAST-PROBABILITY CALIBRATION MONTE CARLO EXPERIMENT
# ============================================================
#
# PURPOSE
# -------
# Keep the finalized team-rating and expected-score system FIXED:
#
#   Score Offense/Defense Huber
#   component lambda = 2
#   Huber k = 1.5
#   recency half-life = 12
#
# Then compare ONLY the conversion from:
#
#   predicted margin -> probability of home win
#
# Every probability mapping receives the exact same paired seasons,
# exact same schedules, exact same fitted ratings, exact same
# expected scores, exact same predicted margins, and exact same
# predictive margin SD.
#
# CANDIDATE PROBABILITY MAPPINGS
# ------------------------------
#
#   1. Student-t4              <- current Version 9C method
#   2. Student-t6
#   3. Student-t8
#   4. Normal
#   5. Logistic Variance-Matched
#   6. Empirical Logistic      <- fit ONLY on independent calibration seasons
#
# BENCHMARKS
# ----------
#
#   7. Baseline 50/50
#   8. Baseline Home Rate      <- constant learned from calibration seasons
#
# ROBUSTNESS GRID
# ---------------
# Hidden weekly strength drift:
#
#   0.00, 0.25, 0.50, 0.75, 1.00, 1.50
#
# Score-noise distribution:
#
#   Gaussian
#   Student_t8
#   Student_t4
#   Student_t3
#
# This gives 24 distinct forecast environments.
#
# CALIBRATION / EVALUATION SEPARATION
# -----------------------------------
# Calibration seasons:
#   - generate walk-forward predicted margins
#   - fit Empirical Logistic mapping
#   - estimate the constant baseline home-win rate
#
# Evaluation seasons:
#   - generated independently with different seeds
#   - empirical mapping and home-rate benchmark are frozen
#   - no evaluation outcomes influence probability fitting
#
# PRIMARY METRICS
# ---------------
#   Brier score        lower is better
#   Log loss           lower is better
#   ECE                expected calibration error; lower is better
#   Max calibration error
#   Winner accuracy    secondary
#
# The final decision should favor:
#
#   - low mean Brier/log loss
#   - low worst-cell regret
#   - low calibration error
#   - robust behavior across drift + noise environments
#
# IMPORTANT
# ---------
# This experiment does NOT retune or compare the rating engine.
# It calibrates only the probability layer.
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

huber_component_lambda <- 2
huber_k <- 1.5
recency_half_life <- 12

max_huber_iterations <- 40
huber_tolerance <- 1e-6

target_true_rating_sd <- 5
initial_offense_scale <- 1.15
initial_defense_scale <- 0.85

target_margin_noise_sd <- 8.5
score_noise_sd <- target_margin_noise_sd / sqrt(2)

league_mean_points <- 24

validate_each_generated_schedule <- FALSE

# Predictive-margin SD settings inherited from 9C.
generic_margin_noise_prior_sd <- 8.5
margin_noise_prior_weight <- 20

drift_sd_grid <- c(
  0.00,
  0.25,
  0.50,
  0.75,
  1.00,
  1.50
)

noise_scenario_grid <- c(
  "Gaussian",
  "Student_t8",
  "Student_t4",
  "Student_t3"
)

probability_model_names <- c(
  "Student-t4",
  "Student-t6",
  "Student-t8",
  "Normal",
  "Logistic Variance-Matched",
  "Empirical Logistic",
  "Baseline 50/50",
  "Baseline Home Rate"
)

model_colors <- setNames(
  grDevices::hcl.colors(
    length(
      probability_model_names
    ),
    palette = "Dark 3"
  ),
  probability_model_names
)

probability_bin_breaks <- seq(
  0,
  1,
  by = 0.10
)

quick_test <- FALSE

if (
  quick_test
) {
  
  n_calibration_seasons_per_cell <- 3
  n_evaluation_seasons_per_cell <- 5
  
} else {
  
  # 24 cells:
  #   calibration = 24 * 30 = 720 seasons
  #   evaluation  = 24 * 100 = 2400 seasons
  #
  # Each season produces 88 genuinely OOS forecasts.
  n_calibration_seasons_per_cell <- 30
  n_evaluation_seasons_per_cell <- 100
}

calibration_seed_base <- 1200000
evaluation_seed_base <- 2200000

# Numerical clipping protects log loss from log(0).
probability_epsilon <- 1e-6



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
    mean_points = league_mean_points,
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
    mean_points = league_mean_points,
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
# 25. FIT FINAL HUBER MODEL
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

# 26. SCORE OFFENSE / DEFENSE DESIGN MATRIX
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
# 27. WEIGHTED RIDGE SOLVER
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
# 28. SCORE OFFENSE / DEFENSE RIDGE HELPER
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
# 29. FINAL HUBER OFFENSE / DEFENSE MODEL
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
# 30. SAFE CORRELATION
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


# 31. STANDARDIZE MODEL OUTPUTS
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


# ============================================================
# 32. CENTER PUBLISHED OFFENSE / DEFENSE COMPONENTS
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
  
  offense_rating <- offense_rating -
    offense_mean
  
  defense_rating <- defense_rating -
    defense_mean
  
  rating <- (
    offense_rating +
      defense_rating
  )
  
  rating <- rating -
    mean(rating)
  
  adjusted_league_mean_points <- (
    fit$league_mean +
      offense_mean -
      defense_mean
  )
  
  names(offense_rating) <- league_teams
  names(defense_rating) <- league_teams
  names(rating) <- league_teams
  
  return(
    list(
      offense_rating = offense_rating,
      defense_rating = defense_rating,
      rating = rating,
      league_mean_points =
        adjusted_league_mean_points
    )
  )
}


# ============================================================
# 33. FIT FINAL PRODUCTION RATING MODEL
# ============================================================

fit_final_power_model <- function(
    training_games,
    league_teams
) {
  
  fit <- fit_huber_recency_model(
    training_games =
      training_games,
    league_teams =
      league_teams,
    half_life =
      recency_half_life
  )
  
  centered <- center_rating_components(
    fit = fit,
    league_teams = league_teams
  )
  
  fit$offense_rating <-
    centered$offense_rating
  
  fit$defense_rating <-
    centered$defense_rating
  
  fit$ratings <-
    centered$rating
  
  fit$league_mean_points <-
    centered$league_mean_points
  
  fit$hfa_lambda <- get_hfa_lambda(
    data = training_games,
    league_teams = league_teams
  )
  
  return(fit)
}


# ============================================================
# 34. CALCULATE COMPLETED GAMES PER TEAM
# ============================================================

get_completed_games_per_team <- function(
    training_games,
    league_teams
) {
  
  completed_games_per_team <- (
    2 *
      nrow(training_games)
  ) /
    length(league_teams)
  
  if (
    abs(
      completed_games_per_team -
      round(
        completed_games_per_team
      )
    ) >
    1e-8
  ) {
    
    warning(
      "Completed games per team is not an integer. The schedule may not use league-wide bye weeks."
    )
  }
  
  return(
    as.integer(
      round(
        completed_games_per_team
      )
    )
  )
}


# ============================================================

# 32A. PREDICT EXPECTED SCORES FOR GAMES
# ============================================================

predict_games_from_rating_model <- function(
    rating_model,
    target_games,
    league_teams
) {
  
  if (
    nrow(
      target_games
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  home_team <- target_games$home_team
  away_team <- target_games$away_team
  
  expected_home_score <- (
    rating_model$league_mean_points +
      rating_model$offense_rating[
        home_team
      ] -
      rating_model$defense_rating[
        away_team
      ] +
      rating_model$hfa /
      2
  )
  
  expected_away_score <- (
    rating_model$league_mean_points +
      rating_model$offense_rating[
        away_team
      ] -
      rating_model$defense_rating[
        home_team
      ] -
      rating_model$hfa /
      2
  )
  
  expected_margin <- (
    expected_home_score -
      expected_away_score
  )
  
  rating_margin_check <- (
    rating_model$ratings[
      home_team
    ] -
      rating_model$ratings[
        away_team
      ] +
      rating_model$hfa
  )
  
  if (
    max(
      abs(
        expected_margin -
        rating_margin_check
      )
    ) >
    1e-7
  ) {
    
    stop(
      "Expected-score margin does not equal rating-margin identity."
    )
  }
  
  output <- target_games
  
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
      rating_model$ratings[
        home_team
      ]
    )
  
  output$pregame_away_rating <-
    as.numeric(
      rating_model$ratings[
        away_team
      ]
    )
  
  output$pregame_home_offense <-
    as.numeric(
      rating_model$offense_rating[
        home_team
      ]
    )
  
  output$pregame_away_offense <-
    as.numeric(
      rating_model$offense_rating[
        away_team
      ]
    )
  
  output$pregame_home_defense <-
    as.numeric(
      rating_model$defense_rating[
        home_team
      ]
    )
  
  output$pregame_away_defense <-
    as.numeric(
      rating_model$defense_rating[
        away_team
      ]
    )
  
  output$pregame_estimated_HFA <-
    rating_model$hfa
  
  output$pregame_league_mean_points <-
    rating_model$league_mean_points
  
  return(output)
}


# ============================================================
# 32B. TRAINING RESIDUALS FOR PREDICTIVE NOISE
# ============================================================

calculate_training_margin_residuals <- function(
    rating_model,
    training_games,
    league_teams
) {
  
  training_predictions <-
    predict_games_from_rating_model(
      rating_model =
        rating_model,
      target_games =
        training_games,
      league_teams =
        league_teams
    )
  
  residuals <- (
    training_predictions$actual_margin -
      training_predictions$predicted_margin
  )
  
  return(
    as.numeric(
      residuals
    )
  )
}


# ============================================================
# 32C. SHRUNK PREDICTIVE MARGIN SD
# ============================================================
#
# Early-season residual SD is unstable and somewhat optimistic
# because the same games were used to estimate team ratings.
#
# We therefore shrink the empirical residual variance toward the
# generic 8.5-point margin-noise prior.
#
# The influence of the generic prior naturally declines as the
# season accumulates.
# ============================================================

estimate_predictive_margin_sd <- function(
    rating_model,
    training_games,
    league_teams,
    prior_sd =
      generic_margin_noise_prior_sd,
    prior_weight =
      margin_noise_prior_weight
) {
  
  residuals <-
    calculate_training_margin_residuals(
      rating_model =
        rating_model,
      training_games =
        training_games,
      league_teams =
        league_teams
    )
  
  n_residuals <- length(
    residuals
  )
  
  if (
    n_residuals <=
    1
  ) {
    
    return(
      prior_sd
    )
  }
  
  empirical_variance <- mean(
    residuals^2
  )
  
  shrunk_variance <- (
    prior_weight *
      prior_sd^2 +
      n_residuals *
      empirical_variance
  ) /
    (
      prior_weight +
        n_residuals
    )
  
  predictive_sd <- sqrt(
    shrunk_variance
  )
  
  return(
    as.numeric(
      predictive_sd
    )
  )
}


# ============================================================


# ============================================================
# 35. GENERATE ONE CALIBRATION / EVALUATION SEASON
# ============================================================

generate_forecast_mc_season <- function(
    season_seed,
    requested_drift_sd,
    requested_noise_scenario
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
    noise_scenario =
      requested_noise_scenario,
    true_hfa = true_hfa,
    mean_points =
      league_mean_points,
    score_error_sd =
      score_noise_sd
  )
  
  return(
    list(
      schedule = schedule,
      games = games,
      true_path = true_path
    )
  )
}


# ============================================================
# 36. BASE WALK-FORWARD PREDICTIONS
# ============================================================
#
# No win probability is assigned here.
#
# These are the common inputs supplied to EVERY probability model.
# ============================================================

create_base_walk_forward_predictions <- function(
    games,
    team_info
) {
  
  active_weeks <- sort(
    unique(
      games$week
    )
  )
  
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
    
    training_games <- games[
      games$week <
        target_week,
    ]
    
    target_games <- games[
      games$week ==
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
    
    rating_model <-
      fit_final_power_model(
        training_games =
          training_games,
        league_teams =
          team_info$team
      )
    
    predictive_margin_sd <-
      estimate_predictive_margin_sd(
        rating_model =
          rating_model,
        training_games =
          training_games,
        league_teams =
          team_info$team
      )
    
    current_predictions <-
      predict_games_from_rating_model(
        rating_model =
          rating_model,
        target_games =
          target_games,
        league_teams =
          team_info$team
      )
    
    current_predictions$predictive_margin_sd <-
      predictive_margin_sd
    
    current_predictions$completed_games_per_team_before_week <-
      get_completed_games_per_team(
        training_games =
          training_games,
        league_teams =
          team_info$team
      )
    
    current_predictions$actual_home_win_soft <- ifelse(
      current_predictions$actual_margin >
        0,
      1,
      ifelse(
        current_predictions$actual_margin <
          0,
        0,
        0.5
      )
    )
    
    current_predictions$actual_home_win_non_tie <- ifelse(
      current_predictions$actual_margin >
        0,
      1,
      ifelse(
        current_predictions$actual_margin <
          0,
        0,
        NA_real_
      )
    )
    
    current_predictions$margin_error <- (
      current_predictions$predicted_margin -
        current_predictions$actual_margin
    )
    
    current_predictions$absolute_margin_error <- abs(
      current_predictions$margin_error
    )
    
    current_predictions$squared_margin_error <- (
      current_predictions$margin_error^2
    )
    
    current_predictions$latent_margin_error <- (
      current_predictions$predicted_margin -
        current_predictions$true_expected_margin
    )
    
    current_predictions$absolute_latent_margin_error <- abs(
      current_predictions$latent_margin_error
    )
    
    current_predictions$squared_latent_margin_error <- (
      current_predictions$latent_margin_error^2
    )
    
    prediction_storage[[prediction_position]] <-
      current_predictions
    
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
  
  rownames(output) <- NULL
  
  return(output)
}


# ============================================================
# 37. PROBABILITY MAPPING HELPERS
# ============================================================

clip_probability <- function(
    probability
) {
  
  return(
    pmin(
      pmax(
        probability,
        probability_epsilon
      ),
      1 -
        probability_epsilon
    )
  )
}


student_t_probability <- function(
    predicted_margin,
    predictive_margin_sd,
    degrees_freedom
) {
  
  t_scale <- (
    predictive_margin_sd *
      sqrt(
        (
          degrees_freedom -
            2
        ) /
          degrees_freedom
      )
  )
  
  probability <- pt(
    predicted_margin /
      t_scale,
    df =
      degrees_freedom
  )
  
  return(
    clip_probability(
      probability
    )
  )
}


normal_probability <- function(
    predicted_margin,
    predictive_margin_sd
) {
  
  probability <- pnorm(
    predicted_margin /
      predictive_margin_sd
  )
  
  return(
    clip_probability(
      probability
    )
  )
}


variance_matched_logistic_probability <- function(
    predicted_margin,
    predictive_margin_sd
) {
  
  logistic_scale <- (
    predictive_margin_sd *
      sqrt(
        3
      ) /
      pi
  )
  
  probability <- plogis(
    predicted_margin /
      logistic_scale
  )
  
  return(
    clip_probability(
      probability
    )
  )
}


# ============================================================
# 38. FIT EMPIRICAL LOGISTIC CALIBRATOR
# ============================================================
#
# Input:
#
#   z_margin = predicted_margin / predictive_margin_sd
#
# This learns both:
#
#   calibration intercept
#   calibration slope
#
# using ONLY independent calibration seasons.
# ============================================================

fit_empirical_logistic_calibrator <- function(
    calibration_predictions
) {
  
  non_tie <- calibration_predictions[
    !is.na(
      calibration_predictions$actual_home_win_non_tie
    ),
  ]
  
  non_tie$z_margin <- (
    non_tie$predicted_margin /
      non_tie$predictive_margin_sd
  )
  
  model <- glm(
    actual_home_win_non_tie ~
      z_margin,
    family =
      binomial(),
    data =
      non_tie
  )
  
  return(model)
}


empirical_logistic_probability <- function(
    predicted_margin,
    predictive_margin_sd,
    empirical_model
) {
  
  new_data <- data.frame(
    z_margin = (
      predicted_margin /
        predictive_margin_sd
    )
  )
  
  probability <- predict(
    empirical_model,
    newdata =
      new_data,
    type =
      "response"
  )
  
  return(
    clip_probability(
      as.numeric(
        probability
      )
    )
  )
}


# ============================================================
# 39. APPLY ONE PROBABILITY MODEL
# ============================================================

apply_probability_model <- function(
    base_predictions,
    model_name,
    empirical_model,
    calibration_home_win_rate
) {
  
  if (
    model_name ==
    "Student-t4"
  ) {
    
    return(
      student_t_probability(
        predicted_margin =
          base_predictions$predicted_margin,
        predictive_margin_sd =
          base_predictions$predictive_margin_sd,
        degrees_freedom =
          4
      )
    )
  }
  
  if (
    model_name ==
    "Student-t6"
  ) {
    
    return(
      student_t_probability(
        predicted_margin =
          base_predictions$predicted_margin,
        predictive_margin_sd =
          base_predictions$predictive_margin_sd,
        degrees_freedom =
          6
      )
    )
  }
  
  if (
    model_name ==
    "Student-t8"
  ) {
    
    return(
      student_t_probability(
        predicted_margin =
          base_predictions$predicted_margin,
        predictive_margin_sd =
          base_predictions$predictive_margin_sd,
        degrees_freedom =
          8
      )
    )
  }
  
  if (
    model_name ==
    "Normal"
  ) {
    
    return(
      normal_probability(
        predicted_margin =
          base_predictions$predicted_margin,
        predictive_margin_sd =
          base_predictions$predictive_margin_sd
      )
    )
  }
  
  if (
    model_name ==
    "Logistic Variance-Matched"
  ) {
    
    return(
      variance_matched_logistic_probability(
        predicted_margin =
          base_predictions$predicted_margin,
        predictive_margin_sd =
          base_predictions$predictive_margin_sd
      )
    )
  }
  
  if (
    model_name ==
    "Empirical Logistic"
  ) {
    
    return(
      empirical_logistic_probability(
        predicted_margin =
          base_predictions$predicted_margin,
        predictive_margin_sd =
          base_predictions$predictive_margin_sd,
        empirical_model =
          empirical_model
      )
    )
  }
  
  if (
    model_name ==
    "Baseline 50/50"
  ) {
    
    return(
      rep(
        0.5,
        nrow(
          base_predictions
        )
      )
    )
  }
  
  if (
    model_name ==
    "Baseline Home Rate"
  ) {
    
    return(
      rep(
        calibration_home_win_rate,
        nrow(
          base_predictions
        )
      )
    )
  }
  
  stop(
    paste(
      "Unknown probability model:",
      model_name
    )
  )
}


# ============================================================
# 40. CALIBRATION ERROR FOR ONE SET OF GAMES
# ============================================================

calculate_calibration_metrics <- function(
    probabilities,
    outcomes_soft,
    bin_breaks =
      probability_bin_breaks
) {
  
  bins <- cut(
    probabilities,
    breaks =
      bin_breaks,
    include.lowest =
      TRUE,
    right =
      TRUE
  )
  
  bin_levels <- levels(
    bins
  )
  
  weighted_absolute_error <- 0
  maximum_error <- 0
  
  for (
    current_bin in
    bin_levels
  ) {
    
    in_bin <- (
      bins ==
        current_bin
    )
    
    if (
      !any(
        in_bin
      )
    ) {
      
      next
    }
    
    mean_probability <- mean(
      probabilities[
        in_bin
      ]
    )
    
    mean_outcome <- mean(
      outcomes_soft[
        in_bin
      ]
    )
    
    bin_error <- abs(
      mean_outcome -
        mean_probability
    )
    
    bin_weight <- sum(
      in_bin
    ) /
      length(
        probabilities
      )
    
    weighted_absolute_error <- (
      weighted_absolute_error +
        bin_weight *
        bin_error
    )
    
    maximum_error <- max(
      maximum_error,
      bin_error
    )
  }
  
  return(
    list(
      ECE =
        weighted_absolute_error,
      max_calibration_error =
        maximum_error
    )
  )
}


# ============================================================
# 41. SCORE ONE PROBABILITY MODEL FOR ONE SEASON
# ============================================================

score_probability_model <- function(
    base_predictions,
    probabilities,
    model_name
) {
  
  probabilities <- clip_probability(
    probabilities
  )
  
  y_soft <-
    base_predictions$actual_home_win_soft
  
  y_non_tie <-
    base_predictions$actual_home_win_non_tie
  
  brier <- (
    probabilities -
      y_soft
  )^2
  
  non_tie_index <- !is.na(
    y_non_tie
  )
  
  log_loss_values <- -(
    y_non_tie[
      non_tie_index
    ] *
      log(
        probabilities[
          non_tie_index
        ]
      ) +
      (
        1 -
          y_non_tie[
            non_tie_index
          ]
      ) *
      log(
        1 -
          probabilities[
            non_tie_index
          ]
      )
  )
  
  predicted_home_win <- (
    probabilities >=
      0.5
  )
  
  winner_correct <- (
    predicted_home_win[
      non_tie_index
    ] ==
      (
        y_non_tie[
          non_tie_index
        ] ==
          1
      )
  )
  
  calibration_metrics <-
    calculate_calibration_metrics(
      probabilities =
        probabilities,
      outcomes_soft =
        y_soft
    )
  
  return(
    data.frame(
      model =
        model_name,
      games =
        nrow(
          base_predictions
        ),
      non_tied_games =
        sum(
          non_tie_index
        ),
      Brier =
        mean(
          brier
        ),
      LogLoss =
        mean(
          log_loss_values
        ),
      ECE =
        calibration_metrics$ECE,
      Max_Calibration_Error =
        calibration_metrics$max_calibration_error,
      Winner_Accuracy =
        mean(
          winner_correct
        ),
      Mean_Probability =
        mean(
          probabilities
        ),
      Mean_Confidence =
        mean(
          pmax(
            probabilities,
            1 -
              probabilities
          )
        ),
      Probability_Over_90 =
        mean(
          probabilities >=
            0.90 |
            probabilities <=
            0.10
        ),
      stringsAsFactors =
        FALSE
    )
  )
}


# ============================================================
# 42. RELIABILITY ACCUMULATOR
# ============================================================

initialize_reliability_accumulator <- function(
    model_names,
    bin_breaks
) {
  
  bin_labels <- levels(
    cut(
      c(
        0,
        1
      ),
      breaks =
        bin_breaks,
      include.lowest =
        TRUE,
      right =
        TRUE
    )
  )
  
  accumulator <- expand.grid(
    model =
      model_names,
    probability_bin =
      bin_labels,
    stringsAsFactors =
      FALSE
  )
  
  accumulator$games <- 0
  accumulator$sum_probability <- 0
  accumulator$sum_outcome <- 0
  
  return(accumulator)
}


update_reliability_accumulator <- function(
    accumulator,
    model_name,
    probabilities,
    outcomes_soft,
    bin_breaks
) {
  
  bins <- cut(
    probabilities,
    breaks =
      bin_breaks,
    include.lowest =
      TRUE,
    right =
      TRUE
  )
  
  for (
    current_bin in
    levels(
      bins
    )
  ) {
    
    in_bin <- (
      bins ==
        current_bin
    )
    
    if (
      !any(
        in_bin
      )
    ) {
      
      next
    }
    
    row_index <- which(
      accumulator$model ==
        model_name &
        accumulator$probability_bin ==
        current_bin
    )
    
    accumulator$games[
      row_index
    ] <- (
      accumulator$games[
        row_index
      ] +
        sum(
          in_bin
        )
    )
    
    accumulator$sum_probability[
      row_index
    ] <- (
      accumulator$sum_probability[
        row_index
      ] +
        sum(
          probabilities[
            in_bin
          ]
        )
    )
    
    accumulator$sum_outcome[
      row_index
    ] <- (
      accumulator$sum_outcome[
        row_index
      ] +
        sum(
          outcomes_soft[
            in_bin
          ]
        )
    )
  }
  
  return(accumulator)
}


finalize_reliability_accumulator <- function(
    accumulator
) {
  
  output <- accumulator[
    accumulator$games >
      0,
  ]
  
  output$mean_predicted_probability <- (
    output$sum_probability /
      output$games
  )
  
  output$observed_home_win_rate <- (
    output$sum_outcome /
      output$games
  )
  
  output$calibration_difference <- (
    output$observed_home_win_rate -
      output$mean_predicted_probability
  )
  
  return(output)
}


# ============================================================
# 43. BUILD INDEPENDENT CALIBRATION DATASET
# ============================================================

cat("\n========================================\n")
cat("VERSION 9C-MC\n")
cat("FORECAST-PROBABILITY CALIBRATION\n")
cat("========================================\n\n")

cat(
  "Calibration seasons per drift/noise cell:",
  n_calibration_seasons_per_cell,
  "\n"
)
cat(
  "Evaluation seasons per drift/noise cell:",
  n_evaluation_seasons_per_cell,
  "\n"
)
cat(
  "Sensitivity cells:",
  length(
    drift_sd_grid
  ) *
    length(
      noise_scenario_grid
    ),
  "\n\n"
)

calibration_prediction_storage <- list()
calibration_position <- 1

cell_index <- 0

for (
  noise_index in
  seq_along(
    noise_scenario_grid
  )
) {
  
  current_noise <- noise_scenario_grid[
    noise_index
  ]
  
  for (
    drift_index in
    seq_along(
      drift_sd_grid
    )
  ) {
    
    current_drift <- drift_sd_grid[
      drift_index
    ]
    
    cell_index <- cell_index + 1
    
    cat(
      "Calibration cell",
      cell_index,
      ": noise =",
      current_noise,
      ", drift =",
      current_drift,
      "\n"
    )
    
    for (
      simulation in
      seq_len(
        n_calibration_seasons_per_cell
      )
    ) {
      
      season_seed <- (
        calibration_seed_base +
          noise_index *
          1000000 +
          drift_index *
          10000 +
          simulation
      )
      
      season_data <-
        generate_forecast_mc_season(
          season_seed =
            season_seed,
          requested_drift_sd =
            current_drift,
          requested_noise_scenario =
            current_noise
        )
      
      base_predictions <-
        create_base_walk_forward_predictions(
          games =
            season_data$games,
          team_info =
            team_info
        )
      
      base_predictions$noise_scenario <-
        current_noise
      
      base_predictions$true_drift_sd <-
        current_drift
      
      base_predictions$simulation <-
        simulation
      
      calibration_prediction_storage[[calibration_position]] <-
        base_predictions[
          ,
          c(
            "noise_scenario",
            "true_drift_sd",
            "simulation",
            "week",
            "game_id",
            "predicted_margin",
            "predictive_margin_sd",
            "actual_home_win_soft",
            "actual_home_win_non_tie"
          )
        ]
      
      calibration_position <-
        calibration_position +
        1
    }
  }
}

calibration_predictions <- do.call(
  rbind,
  calibration_prediction_storage
)

rownames(
  calibration_predictions
) <- NULL

empirical_logistic_model <-
  fit_empirical_logistic_calibrator(
    calibration_predictions =
      calibration_predictions
  )

calibration_non_tie <- calibration_predictions[
  !is.na(
    calibration_predictions$actual_home_win_non_tie
  ),
]

calibration_home_win_rate <- mean(
  calibration_non_tie$actual_home_win_non_tie
)

cat("\nEMPIRICAL LOGISTIC COEFFICIENTS:\n\n")

print(
  coef(
    empirical_logistic_model
  )
)

cat(
  "\nCalibration home-win rate:",
  calibration_home_win_rate,
  "\n\n"
)


# ============================================================
# 44. EVALUATION MONTE CARLO
# ============================================================

season_metric_storage <- list()
base_metric_storage <- list()

season_metric_position <- 1
base_metric_position <- 1

reliability_accumulator <-
  initialize_reliability_accumulator(
    model_names =
      probability_model_names,
    bin_breaks =
      probability_bin_breaks
  )

cell_index <- 0

for (
  noise_index in
  seq_along(
    noise_scenario_grid
  )
) {
  
  current_noise <- noise_scenario_grid[
    noise_index
  ]
  
  for (
    drift_index in
    seq_along(
      drift_sd_grid
    )
  ) {
    
    current_drift <- drift_sd_grid[
      drift_index
    ]
    
    cell_index <- cell_index + 1
    
    cat(
      "\nEvaluation cell",
      cell_index,
      ": noise =",
      current_noise,
      ", drift =",
      current_drift,
      "\n"
    )
    
    for (
      simulation in
      seq_len(
        n_evaluation_seasons_per_cell
      )
    ) {
      
      if (
        simulation ==
        1 ||
        simulation %%
        25 ==
        0 ||
        simulation ==
        n_evaluation_seasons_per_cell
      ) {
        
        cat(
          "  season",
          simulation,
          "of",
          n_evaluation_seasons_per_cell,
          "\n"
        )
      }
      
      season_seed <- (
        evaluation_seed_base +
          noise_index *
          1000000 +
          drift_index *
          10000 +
          simulation
      )
      
      season_data <-
        generate_forecast_mc_season(
          season_seed =
            season_seed,
          requested_drift_sd =
            current_drift,
          requested_noise_scenario =
            current_noise
        )
      
      base_predictions <-
        create_base_walk_forward_predictions(
          games =
            season_data$games,
          team_info =
            team_info
        )
      
      # ------------------------------------------------------
      # Point-forecast metrics are identical for all probability
      # models and are recorded once per season.
      # ------------------------------------------------------
      
      base_metric_storage[[base_metric_position]] <-
        data.frame(
          noise_scenario =
            current_noise,
          true_drift_sd =
            current_drift,
          simulation =
            simulation,
          games =
            nrow(
              base_predictions
            ),
          margin_MAE =
            mean(
              base_predictions$absolute_margin_error
            ),
          margin_RMSE =
            sqrt(
              mean(
                base_predictions$squared_margin_error
              )
            ),
          margin_bias =
            mean(
              base_predictions$margin_error
            ),
          latent_margin_MAE =
            mean(
              base_predictions$absolute_latent_margin_error
            ),
          latent_margin_RMSE =
            sqrt(
              mean(
                base_predictions$squared_latent_margin_error
              )
            ),
          stringsAsFactors =
            FALSE
        )
      
      base_metric_position <-
        base_metric_position +
        1
      
      for (
        model_name in
        probability_model_names
      ) {
        
        probabilities <-
          apply_probability_model(
            base_predictions =
              base_predictions,
            model_name =
              model_name,
            empirical_model =
              empirical_logistic_model,
            calibration_home_win_rate =
              calibration_home_win_rate
          )
        
        model_metrics <-
          score_probability_model(
            base_predictions =
              base_predictions,
            probabilities =
              probabilities,
            model_name =
              model_name
          )
        
        model_metrics$noise_scenario <-
          current_noise
        
        model_metrics$true_drift_sd <-
          current_drift
        
        model_metrics$simulation <-
          simulation
        
        season_metric_storage[[season_metric_position]] <-
          model_metrics[
            ,
            c(
              "noise_scenario",
              "true_drift_sd",
              "simulation",
              "model",
              "games",
              "non_tied_games",
              "Brier",
              "LogLoss",
              "ECE",
              "Max_Calibration_Error",
              "Winner_Accuracy",
              "Mean_Probability",
              "Mean_Confidence",
              "Probability_Over_90"
            )
          ]
        
        season_metric_position <-
          season_metric_position +
          1
        
        reliability_accumulator <-
          update_reliability_accumulator(
            accumulator =
              reliability_accumulator,
            model_name =
              model_name,
            probabilities =
              probabilities,
            outcomes_soft =
              base_predictions$actual_home_win_soft,
            bin_breaks =
              probability_bin_breaks
          )
      }
    }
  }
}

season_metrics <- do.call(
  rbind,
  season_metric_storage
)

base_forecast_metrics <- do.call(
  rbind,
  base_metric_storage
)

rownames(
  season_metrics
) <- NULL

rownames(
  base_forecast_metrics
) <- NULL

reliability_table <-
  finalize_reliability_accumulator(
    reliability_accumulator
  )


# ============================================================
# 45. SUMMARY BY SENSITIVITY CELL
# ============================================================

cell_summary_rows <- list()
cell_summary_position <- 1

for (
  current_noise in
  noise_scenario_grid
) {
  
  for (
    current_drift in
    drift_sd_grid
  ) {
    
    for (
      model_name in
      probability_model_names
    ) {
      
      current <- season_metrics[
        season_metrics$noise_scenario ==
          current_noise &
          season_metrics$true_drift_sd ==
          current_drift &
          season_metrics$model ==
          model_name,
      ]
      
      cell_summary_rows[[cell_summary_position]] <-
        data.frame(
          noise_scenario =
            current_noise,
          true_drift_sd =
            current_drift,
          model =
            model_name,
          mean_Brier =
            mean(
              current$Brier
            ),
          mean_LogLoss =
            mean(
              current$LogLoss
            ),
          mean_ECE =
            mean(
              current$ECE
            ),
          mean_Max_Calibration_Error =
            mean(
              current$Max_Calibration_Error
            ),
          mean_Winner_Accuracy =
            mean(
              current$Winner_Accuracy
            ),
          mean_Confidence =
            mean(
              current$Mean_Confidence
            ),
          mean_Probability_Over_90 =
            mean(
              current$Probability_Over_90
            ),
          stringsAsFactors =
            FALSE
        )
      
      cell_summary_position <-
        cell_summary_position +
        1
    }
  }
}

cell_summary <- do.call(
  rbind,
  cell_summary_rows
)

rownames(
  cell_summary
) <- NULL


# ============================================================
# 46. CELL WINNERS + REGRET
# ============================================================

regret_rows <- list()
winner_rows <- list()

regret_position <- 1
winner_position <- 1

for (
  current_noise in
  noise_scenario_grid
) {
  
  for (
    current_drift in
    drift_sd_grid
  ) {
    
    current <- cell_summary[
      cell_summary$noise_scenario ==
        current_noise &
        cell_summary$true_drift_sd ==
        current_drift,
    ]
    
    best_brier <- min(
      current$mean_Brier
    )
    
    best_log_loss <- min(
      current$mean_LogLoss
    )
    
    best_brier_row <- current[
      which.min(
        current$mean_Brier
      ),
    ]
    
    best_log_loss_row <- current[
      which.min(
        current$mean_LogLoss
      ),
    ]
    
    winner_rows[[winner_position]] <-
      data.frame(
        noise_scenario =
          current_noise,
        true_drift_sd =
          current_drift,
        best_Brier_model =
          best_brier_row$model,
        best_Brier =
          best_brier_row$mean_Brier,
        best_LogLoss_model =
          best_log_loss_row$model,
        best_LogLoss =
          best_log_loss_row$mean_LogLoss,
        stringsAsFactors =
          FALSE
      )
    
    winner_position <-
      winner_position +
      1
    
    for (
      row_index in
      seq_len(
        nrow(
          current
        )
      )
    ) {
      
      regret_rows[[regret_position]] <-
        data.frame(
          noise_scenario =
            current_noise,
          true_drift_sd =
            current_drift,
          model =
            current$model[
              row_index
            ],
          mean_Brier =
            current$mean_Brier[
              row_index
            ],
          mean_LogLoss =
            current$mean_LogLoss[
              row_index
            ],
          mean_ECE =
            current$mean_ECE[
              row_index
            ],
          Brier_regret =
            current$mean_Brier[
              row_index
            ] -
            best_brier,
          LogLoss_regret =
            current$mean_LogLoss[
              row_index
            ] -
            best_log_loss,
          stringsAsFactors =
            FALSE
        )
      
      regret_position <-
        regret_position +
        1
    }
  }
}

winner_by_cell <- do.call(
  rbind,
  winner_rows
)

regret_table <- do.call(
  rbind,
  regret_rows
)

rownames(
  winner_by_cell
) <- NULL

rownames(
  regret_table
) <- NULL


# ============================================================
# 47. ROBUSTNESS ACROSS ALL 24 CELLS
# ============================================================

robustness_rows <- list()

for (
  model_index in
  seq_along(
    probability_model_names
  )
) {
  
  model_name <- probability_model_names[
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
      mean_Brier =
        mean(
          current$mean_Brier
        ),
      worst_cell_Brier =
        max(
          current$mean_Brier
        ),
      mean_Brier_regret =
        mean(
          current$Brier_regret
        ),
      worst_Brier_regret =
        max(
          current$Brier_regret
        ),
      Brier_cell_wins =
        sum(
          current$Brier_regret <
            1e-12
        ),
      mean_LogLoss =
        mean(
          current$mean_LogLoss
        ),
      worst_cell_LogLoss =
        max(
          current$mean_LogLoss
        ),
      mean_LogLoss_regret =
        mean(
          current$LogLoss_regret
        ),
      worst_LogLoss_regret =
        max(
          current$LogLoss_regret
        ),
      LogLoss_cell_wins =
        sum(
          current$LogLoss_regret <
            1e-12
        ),
      mean_ECE =
        mean(
          current$mean_ECE
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
    robustness_summary$mean_Brier_regret,
    robustness_summary$mean_LogLoss_regret
  ),
]

rownames(
  robustness_summary
) <- NULL


# ============================================================
# 48. SUMMARY BY NOISE FAMILY
# ============================================================

noise_summary_rows <- list()
noise_summary_position <- 1

for (
  current_noise in
  noise_scenario_grid
) {
  
  for (
    model_name in
    probability_model_names
  ) {
    
    current <- cell_summary[
      cell_summary$noise_scenario ==
        current_noise &
        cell_summary$model ==
        model_name,
    ]
    
    noise_summary_rows[[noise_summary_position]] <-
      data.frame(
        noise_scenario =
          current_noise,
        model =
          model_name,
        mean_Brier_across_drift =
          mean(
            current$mean_Brier
          ),
        mean_LogLoss_across_drift =
          mean(
            current$mean_LogLoss
          ),
        mean_ECE_across_drift =
          mean(
            current$mean_ECE
          ),
        mean_Winner_Accuracy_across_drift =
          mean(
            current$mean_Winner_Accuracy
          ),
        stringsAsFactors =
          FALSE
      )
    
    noise_summary_position <-
      noise_summary_position +
      1
  }
}

summary_by_noise <- do.call(
  rbind,
  noise_summary_rows
)

rownames(
  summary_by_noise
) <- NULL


# ============================================================
# 49. PAIRED DIFFERENCES VS CURRENT STUDENT-t4 METHOD
# ============================================================

paired_rows <- list()
paired_position <- 1

reference_metrics <- season_metrics[
  season_metrics$model ==
    "Student-t4",
  c(
    "noise_scenario",
    "true_drift_sd",
    "simulation",
    "Brier",
    "LogLoss",
    "ECE"
  )
]

names(
  reference_metrics
)[4:6] <- c(
  "reference_Brier",
  "reference_LogLoss",
  "reference_ECE"
)

for (
  model_name in
  probability_model_names
) {
  
  candidate <- season_metrics[
    season_metrics$model ==
      model_name,
    c(
      "noise_scenario",
      "true_drift_sd",
      "simulation",
      "Brier",
      "LogLoss",
      "ECE"
    )
  ]
  
  names(
    candidate
  )[4:6] <- c(
    "candidate_Brier",
    "candidate_LogLoss",
    "candidate_ECE"
  )
  
  paired <- merge(
    reference_metrics,
    candidate,
    by = c(
      "noise_scenario",
      "true_drift_sd",
      "simulation"
    ),
    sort = FALSE
  )
  
  brier_difference <- (
    paired$candidate_Brier -
      paired$reference_Brier
  )
  
  log_loss_difference <- (
    paired$candidate_LogLoss -
      paired$reference_LogLoss
  )
  
  ece_difference <- (
    paired$candidate_ECE -
      paired$reference_ECE
  )
  
  brier_se <- sd(
    brier_difference
  ) /
    sqrt(
      length(
        brier_difference
      )
    )
  
  log_loss_se <- sd(
    log_loss_difference
  ) /
    sqrt(
      length(
        log_loss_difference
      )
    )
  
  ece_se <- sd(
    ece_difference
  ) /
    sqrt(
      length(
        ece_difference
      )
    )
  
  paired_rows[[paired_position]] <-
    data.frame(
      model =
        model_name,
      mean_Brier_difference_vs_t4 =
        mean(
          brier_difference
        ),
      Brier_CI_lower =
        mean(
          brier_difference
        ) -
        1.96 *
        brier_se,
      Brier_CI_upper =
        mean(
          brier_difference
        ) +
        1.96 *
        brier_se,
      probability_lower_Brier_than_t4 =
        mean(
          brier_difference <
            0
        ),
      mean_LogLoss_difference_vs_t4 =
        mean(
          log_loss_difference
        ),
      LogLoss_CI_lower =
        mean(
          log_loss_difference
        ) -
        1.96 *
        log_loss_se,
      LogLoss_CI_upper =
        mean(
          log_loss_difference
        ) +
        1.96 *
        log_loss_se,
      probability_lower_LogLoss_than_t4 =
        mean(
          log_loss_difference <
            0
        ),
      mean_ECE_difference_vs_t4 =
        mean(
          ece_difference
        ),
      ECE_CI_lower =
        mean(
          ece_difference
        ) -
        1.96 *
        ece_se,
      ECE_CI_upper =
        mean(
          ece_difference
        ) +
        1.96 *
        ece_se,
      stringsAsFactors =
        FALSE
    )
  
  paired_position <-
    paired_position +
    1
}

paired_comparison_vs_t4 <- do.call(
  rbind,
  paired_rows
)

rownames(
  paired_comparison_vs_t4
) <- NULL


# ============================================================
# 50. BASE POINT-FORECAST SUMMARY
# ============================================================

base_summary_rows <- list()
base_summary_position <- 1

for (
  current_noise in
  noise_scenario_grid
) {
  
  for (
    current_drift in
    drift_sd_grid
  ) {
    
    current <- base_forecast_metrics[
      base_forecast_metrics$noise_scenario ==
        current_noise &
        base_forecast_metrics$true_drift_sd ==
        current_drift,
    ]
    
    base_summary_rows[[base_summary_position]] <-
      data.frame(
        noise_scenario =
          current_noise,
        true_drift_sd =
          current_drift,
        mean_margin_MAE =
          mean(
            current$margin_MAE
          ),
        mean_margin_RMSE =
          mean(
            current$margin_RMSE
          ),
        mean_margin_bias =
          mean(
            current$margin_bias
          ),
        mean_latent_margin_MAE =
          mean(
            current$latent_margin_MAE
          ),
        mean_latent_margin_RMSE =
          mean(
            current$latent_margin_RMSE
          ),
        stringsAsFactors =
          FALSE
      )
    
    base_summary_position <-
      base_summary_position +
      1
  }
}

base_forecast_summary <- do.call(
  rbind,
  base_summary_rows
)

rownames(
  base_forecast_summary
) <- NULL


# ============================================================
# 51. PRINT DECISION TABLES
# ============================================================

cat("\n========================================\n")
cat("9C-MC ROBUSTNESS SUMMARY\n")
cat("========================================\n\n")

print(
  robustness_summary,
  row.names = FALSE
)

cat("\n========================================\n")
cat("WINNER BY DRIFT / NOISE CELL\n")
cat("========================================\n\n")

print(
  winner_by_cell,
  row.names = FALSE
)

cat("\n========================================\n")
cat("AVERAGE PERFORMANCE BY NOISE FAMILY\n")
cat("========================================\n\n")

print(
  summary_by_noise[
    order(
      summary_by_noise$noise_scenario,
      summary_by_noise$mean_Brier_across_drift
    ),
  ],
  row.names = FALSE
)

cat("\n========================================\n")
cat("PAIRED COMPARISON VS CURRENT t4 MAPPING\n")
cat("========================================\n\n")

print(
  paired_comparison_vs_t4,
  row.names = FALSE
)

cat("\n========================================\n")
cat("POINT-FORECAST ENGINE CHECK\n")
cat("========================================\n\n")

print(
  base_forecast_summary,
  row.names = FALSE
)


# ============================================================
# 52. SAVE OUTPUTS
# ============================================================

write.csv(
  data.frame(
    term =
      names(
        coef(
          empirical_logistic_model
        )
      ),
    estimate =
      as.numeric(
        coef(
          empirical_logistic_model
        )
      ),
    stringsAsFactors =
      FALSE
  ),
  "version9C_MC_empirical_logistic_coefficients.csv",
  row.names = FALSE
)

write.csv(
  season_metrics,
  "version9C_MC_probability_metrics_by_season.csv",
  row.names = FALSE
)

write.csv(
  cell_summary,
  "version9C_MC_probability_summary_by_cell.csv",
  row.names = FALSE
)

write.csv(
  winner_by_cell,
  "version9C_MC_winner_by_cell.csv",
  row.names = FALSE
)

write.csv(
  robustness_summary,
  "version9C_MC_robustness_summary.csv",
  row.names = FALSE
)

write.csv(
  summary_by_noise,
  "version9C_MC_summary_by_noise.csv",
  row.names = FALSE
)

write.csv(
  paired_comparison_vs_t4,
  "version9C_MC_paired_vs_t4.csv",
  row.names = FALSE
)

write.csv(
  reliability_table,
  "version9C_MC_reliability_table.csv",
  row.names = FALSE
)

write.csv(
  base_forecast_summary,
  "version9C_MC_base_forecast_summary.csv",
  row.names = FALSE
)


# ============================================================
# 53. PLOTS
# ============================================================

candidate_models <- probability_model_names[
  !probability_model_names %in%
    c(
      "Baseline 50/50",
      "Baseline Home Rate"
    )
]

candidate_cell_summary <- cell_summary[
  cell_summary$model %in%
    candidate_models,
]

brier_plot <- ggplot(
  candidate_cell_summary,
  aes(
    x =
      true_drift_sd,
    y =
      mean_Brier,
    color =
      model,
    group =
      model
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.7
  ) +
  facet_wrap(
    ~noise_scenario
  ) +
  scale_color_manual(
    values =
      model_colors
  ) +
  labs(
    title =
      "Version 9C-MC: Brier Score Across Forecast Environments",
    subtitle =
      "Lower is better; probability mapping only",
    x =
      "True Weekly Rating Drift SD",
    y =
      "Mean Brier Score",
    color =
      "Probability Model"
  ) +
  theme_minimal()

ggsave(
  "version9C_MC_brier_by_environment.png",
  plot =
    brier_plot,
  width =
    14,
  height =
    9,
  dpi =
    170
)


log_loss_plot <- ggplot(
  candidate_cell_summary,
  aes(
    x =
      true_drift_sd,
    y =
      mean_LogLoss,
    color =
      model,
    group =
      model
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.7
  ) +
  facet_wrap(
    ~noise_scenario
  ) +
  scale_color_manual(
    values =
      model_colors
  ) +
  labs(
    title =
      "Version 9C-MC: Log Loss Across Forecast Environments",
    subtitle =
      "Lower is better; strongly penalizes overconfident misses",
    x =
      "True Weekly Rating Drift SD",
    y =
      "Mean Log Loss",
    color =
      "Probability Model"
  ) +
  theme_minimal()

ggsave(
  "version9C_MC_logloss_by_environment.png",
  plot =
    log_loss_plot,
  width =
    14,
  height =
    9,
  dpi =
    170
)


candidate_reliability <- reliability_table[
  reliability_table$model %in%
    candidate_models,
]

reliability_plot <- ggplot(
  candidate_reliability,
  aes(
    x =
      mean_predicted_probability,
    y =
      observed_home_win_rate,
    color =
      model,
    group =
      model
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    aes(
      size =
        games
    )
  ) +
  coord_equal(
    xlim =
      c(
        0,
        1
      ),
    ylim =
      c(
        0,
        1
      )
  ) +
  scale_color_manual(
    values =
      model_colors
  ) +
  labs(
    title =
      "Version 9C-MC: Pooled Win-Probability Reliability",
    subtitle =
      "Closer to the dashed line is better",
    x =
      "Mean Predicted Home Win Probability",
    y =
      "Observed Home Win Rate",
    color =
      "Probability Model",
    size =
      "Games"
  ) +
  theme_minimal()

ggsave(
  "version9C_MC_reliability.png",
  plot =
    reliability_plot,
  width =
    10,
  height =
    9,
  dpi =
    170
)


regret_plot_data <- regret_table[
  regret_table$model %in%
    candidate_models,
]

regret_plot <- ggplot(
  regret_plot_data,
  aes(
    x =
      model,
    y =
      Brier_regret
  )
) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title =
      "Version 9C-MC: Brier Regret Across 24 Environments",
    subtitle =
      "Distance from the best probability model in each cell",
    x =
      "Probability Model",
    y =
      "Brier Regret"
  ) +
  theme_minimal()

ggsave(
  "version9C_MC_brier_regret.png",
  plot =
    regret_plot,
  width =
    10,
  height =
    7,
  dpi =
    170
)


# ============================================================
# 54. FINAL CONSOLE SUMMARY
# ============================================================

cat("\n========================================\n")
cat("VERSION 9C-MC COMPLETE\n")
cat("========================================\n\n")

cat("Rating engine remained fixed:\n")
cat("  Score Offense/Defense Huber + HL12\n")
cat(
  "  component lambda =",
  huber_component_lambda,
  "\n"
)
cat(
  "  Huber k =",
  huber_k,
  "\n"
)
cat(
  "  recency half-life =",
  recency_half_life,
  "\n\n"
)

cat("Probability calibration was evaluated across:\n")
cat(
  "  ",
  length(
    drift_sd_grid
  ),
  " drift levels\n",
  sep = ""
)
cat(
  "  ",
  length(
    noise_scenario_grid
  ),
  " noise families\n",
  sep = ""
)
cat(
  "  ",
  length(
    drift_sd_grid
  ) *
    length(
      noise_scenario_grid
    ),
  " total cells\n\n",
  sep = ""
)

cat(
  "Independent calibration seasons:",
  length(
    drift_sd_grid
  ) *
    length(
      noise_scenario_grid
    ) *
    n_calibration_seasons_per_cell,
  "\n"
)
cat(
  "Independent evaluation seasons:",
  length(
    drift_sd_grid
  ) *
    length(
      noise_scenario_grid
    ) *
    n_evaluation_seasons_per_cell,
  "\n\n"
)

cat(
  "Empirical logistic intercept:",
  coef(
    empirical_logistic_model
  )[1],
  "\n"
)
cat(
  "Empirical logistic slope:",
  coef(
    empirical_logistic_model
  )[2],
  "\n"
)
cat(
  "Calibration home-win rate:",
  calibration_home_win_rate,
  "\n\n"
)

cat("Decision rule:\n")
cat(
  "  Prefer the mapping with low mean and worst-cell Brier/log-loss regret,\n"
)
cat(
  "  low ECE, and reliable behavior across drift and noise environments.\n"
)
cat(
  "  Winner accuracy is secondary because monotone probability mappings\n"
)
cat(
  "  generally preserve the same predicted winner.\n\n"
)

cat(
  "CSV outputs and PNG plots were written to the current working directory.\n"
)

# ============================================================
# END VERSION 9C-MC
# ============================================================

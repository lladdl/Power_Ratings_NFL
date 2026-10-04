# ============================================================
# FOOTBALL POWER RATINGS - VERSION 9C
# EXTENDS VERSION 9A-H2
# WEEKLY PUBLICATION SYSTEM
# FINALIZED SCORE OFFENSE/DEFENSE HUBER + HL12 ENGINE
# ============================================================
#
# CORE MODEL
# ----------
#   Score Offense/Defense Huber
#   component ridge lambda = 2
#   Huber k = 1.5
#   recency half-life = 12 calendar weeks
#   HFA estimated separately with structural prior = 2.5 points
#
# ONLY RESULTS-BASED INFORMATION IS USED:
#   - final scores
#   - opponent
#   - home / away
#   - game chronology
#   - future schedule for SOS reporting only
#
# NO EPA, PLAYER DATA, BETTING LINES, PRESEASON RANKINGS,
# RECRUITING DATA, OR TEAM-SPECIFIC PRIORS ARE USED.
#
# PUBLISHED COMPONENTS
# --------------------
# offense_rating:
#   expected scoring strength relative to league average.
#   Higher = better offense.
#
# defense_rating:
#   expected opponent-points suppression relative to league average.
#   Higher = better defense.
#
# overall rating:
#
#   rating = offense_rating + defense_rating
#
# and is interpreted as estimated neutral-field points above/below
# an average team.
#
# UNCERTAINTY
# -----------
# The publication table uses simulation-calibrated 95% rating ranges
# for the FINALIZED model rather than raw bootstrap percentile ranges.
#
# On the first run, if no saved calibration file exists, this script
# builds the calibration from dynamic simulated seasons spanning:
#
#   weekly drift SD = 0, .25, .50, .75, 1.00, 1.50
#
# under Student-t4 score noise.
#
# Two uncertainty widths are calibrated:
#
#   pooled:
#     one 95th percentile across the full equal-weight drift mixture.
#
#   robust:
#     maximum of the drift-specific 95th percentiles.
#
# The default publication table uses POOLED calibrated uncertainty.
# This targets honest average 95% coverage across the equal-weight
# drift mixture without paying the full worst-case-width penalty.
#
# ============================================================

library(ggplot2)


# ============================================================
# 1. PRODUCTION SETTINGS
# ============================================================

n_weeks <- 14
games_per_team <- 12

true_hfa <- 2.7
hfa_prior <- 2.5

# Finalized rating-engine settings.
huber_component_lambda <- 2
huber_k <- 1.5
recency_half_life <- 12

max_huber_iterations <- 40
huber_tolerance <- 1e-6

# Simulation environment used ONLY for the example season and
# uncertainty calibration.
target_true_rating_sd <- 5
initial_offense_scale <- 1.15
initial_defense_scale <- 0.85

noise_scenario <- "Student_t4"

target_margin_noise_sd <- 8.5
score_noise_sd <- target_margin_noise_sd / sqrt(2)

league_mean_points <- 24

validate_each_generated_schedule <- FALSE

# Example season uses moderate hidden weekly change.
example_true_weekly_drift_sd <- 0.75
example_season_seed <- 9902

# ------------------------------------------------------------
# Uncertainty-calibration settings
# ------------------------------------------------------------

uncertainty_calibration_file <-
  "version9A_H_uncertainty_calibration.csv"

rebuild_uncertainty_calibration <- FALSE

uncertainty_drift_grid <- c(
  0.00,
  0.25,
  0.50,
  0.75,
  1.00,
  1.50
)

uncertainty_calibration_fraction <- 0.50

quick_test_uncertainty <- FALSE

if (quick_test_uncertainty) {
  
  n_uncertainty_seasons_per_drift <- 10
  
} else {
  
  n_uncertainty_seasons_per_drift <- 80
}

uncertainty_seed_base <- 991000

# "robust" or "pooled"
publication_uncertainty_mode <- "pooled"

if (
  !publication_uncertainty_mode %in%
  c(
    "robust",
    "pooled"
  )
) {
  
  stop(
    "publication_uncertainty_mode must be 'robust' or 'pooled'."
  )
}



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
# 35. BUILD PUBLICATION UNCERTAINTY CALIBRATION
# ============================================================
#
# Calibration and holdout seasons are independent within each
# hidden drift scenario.
#
# For each completed-games value:
#
#   pooled_half_width:
#       q95 of calibration absolute errors pooled across drift.
#
#   robust_half_width:
#       maximum drift-specific q95.
#
# Holdout coverage is then measured using the frozen widths.
# ============================================================

build_publication_uncertainty_calibration <- function(
    drift_grid,
    seasons_per_drift,
    calibration_fraction,
    seed_base
) {
  
  calibration_error_rows <- list()
  
  row_index <- 1
  
  for (
    drift_index in
    seq_along(
      drift_grid
    )
  ) {
    
    true_drift_sd <- drift_grid[
      drift_index
    ]
    
    cat(
      "Uncertainty calibration - drift",
      true_drift_sd,
      "\n"
    )
    
    for (
      simulation in
      seq_len(
        seasons_per_drift
      )
    ) {
      
      season_seed <- (
        seed_base +
          drift_index *
          100000 +
          simulation
      )
      
      season_data <-
        generate_dynamic_experiment_season(
          season_seed =
            season_seed,
          requested_drift_sd =
            true_drift_sd
        )
      
      active_weeks <- sort(
        unique(
          season_data$games$week
        )
      )
      
      split_index <- floor(
        seasons_per_drift *
          calibration_fraction
      )
      
      sample_role <- if (
        simulation <=
        split_index
      ) {
        
        "calibration"
        
      } else {
        
        "holdout"
      }
      
      for (
        completed_games in
        seq_along(
          active_weeks
        )
      ) {
        
        current_week <- active_weeks[
          completed_games
        ]
        
        training_games <-
          season_data$games[
            season_data$games$week <=
              current_week,
          ]
        
        current_fit <-
          fit_final_power_model(
            training_games =
              training_games,
            league_teams =
              teams
          )
        
        true_rating <-
          season_data$true_path[[current_week]]$rating[
            teams
          ]
        
        absolute_error <- abs(
          current_fit$ratings[
            teams
          ] -
            true_rating
        )
        
        calibration_error_rows[[row_index]] <-
          data.frame(
            true_drift_sd =
              true_drift_sd,
            simulation =
              simulation,
            sample_role =
              sample_role,
            completed_games_per_team =
              completed_games,
            calendar_week =
              current_week,
            team =
              teams,
            absolute_rating_error =
              as.numeric(
                absolute_error
              ),
            stringsAsFactors =
              FALSE
          )
        
        row_index <- row_index + 1
      }
    }
  }
  
  error_data <- do.call(
    rbind,
    calibration_error_rows
  )
  
  rownames(error_data) <- NULL
  
  output_rows <- list()
  
  for (
    completed_games in
    1:games_per_team
  ) {
    
    current_calibration <- error_data[
      error_data$completed_games_per_team ==
        completed_games &
        error_data$sample_role ==
        "calibration",
    ]
    
    current_holdout <- error_data[
      error_data$completed_games_per_team ==
        completed_games &
        error_data$sample_role ==
        "holdout",
    ]
    
    pooled_half_width <- as.numeric(
      quantile(
        current_calibration$absolute_rating_error,
        probs = 0.95,
        names = FALSE,
        type = 8
      )
    )
    
    drift_specific_widths <- numeric(
      length(
        drift_grid
      )
    )
    
    holdout_drift_coverages_pooled <- numeric(
      length(
        drift_grid
      )
    )
    
    for (
      drift_index in
      seq_along(
        drift_grid
      )
    ) {
      
      true_drift_sd <- drift_grid[
        drift_index
      ]
      
      drift_calibration <-
        current_calibration[
          current_calibration$true_drift_sd ==
            true_drift_sd,
        ]
      
      drift_specific_widths[
        drift_index
      ] <- as.numeric(
        quantile(
          drift_calibration$absolute_rating_error,
          probs = 0.95,
          names = FALSE,
          type = 8
        )
      )
      
      drift_holdout <-
        current_holdout[
          current_holdout$true_drift_sd ==
            true_drift_sd,
        ]
      
      holdout_drift_coverages_pooled[
        drift_index
      ] <- mean(
        drift_holdout$absolute_rating_error <=
          pooled_half_width
      )
    }
    
    robust_half_width <- max(
      drift_specific_widths
    )
    
    holdout_drift_coverages_robust <- numeric(
      length(
        drift_grid
      )
    )
    
    for (
      drift_index in
      seq_along(
        drift_grid
      )
    ) {
      
      true_drift_sd <- drift_grid[
        drift_index
      ]
      
      drift_holdout <-
        current_holdout[
          current_holdout$true_drift_sd ==
            true_drift_sd,
        ]
      
      holdout_drift_coverages_robust[
        drift_index
      ] <- mean(
        drift_holdout$absolute_rating_error <=
          robust_half_width
      )
    }
    
    output_rows[[completed_games]] <-
      data.frame(
        completed_games_per_team =
          completed_games,
        pooled_95_half_width =
          pooled_half_width,
        pooled_95_full_width =
          2 *
          pooled_half_width,
        pooled_holdout_coverage =
          mean(
            current_holdout$absolute_rating_error <=
              pooled_half_width
          ),
        pooled_min_drift_coverage =
          min(
            holdout_drift_coverages_pooled
          ),
        robust_95_half_width =
          robust_half_width,
        robust_95_full_width =
          2 *
          robust_half_width,
        robust_holdout_coverage =
          mean(
            current_holdout$absolute_rating_error <=
              robust_half_width
          ),
        robust_min_drift_coverage =
          min(
            holdout_drift_coverages_robust
          ),
        calibration_team_observations =
          nrow(
            current_calibration
          ),
        holdout_team_observations =
          nrow(
            current_holdout
          ),
        stringsAsFactors =
          FALSE
      )
  }
  
  output <- do.call(
    rbind,
    output_rows
  )
  
  rownames(output) <- NULL
  
  return(output)
}


# ============================================================
# 36. LOAD OR BUILD UNCERTAINTY CALIBRATION
# ============================================================

if (
  file.exists(
    uncertainty_calibration_file
  ) &&
  !rebuild_uncertainty_calibration
) {
  
  cat(
    "\nLoading saved uncertainty calibration:",
    uncertainty_calibration_file,
    "\n"
  )
  
  publication_uncertainty <-
    read.csv(
      uncertainty_calibration_file,
      stringsAsFactors = FALSE
    )
  
} else {
  
  cat(
    "\nBuilding publication uncertainty calibration...\n"
  )
  
  publication_uncertainty <-
    build_publication_uncertainty_calibration(
      drift_grid =
        uncertainty_drift_grid,
      seasons_per_drift =
        n_uncertainty_seasons_per_drift,
      calibration_fraction =
        uncertainty_calibration_fraction,
      seed_base =
        uncertainty_seed_base
    )
  
  write.csv(
    publication_uncertainty,
    uncertainty_calibration_file,
    row.names = FALSE
  )
}


# ============================================================
# 37. UNCERTAINTY LOOKUP
# ============================================================

get_publication_uncertainty <- function(
    completed_games_per_team
) {
  
  current <- publication_uncertainty[
    publication_uncertainty$completed_games_per_team ==
      completed_games_per_team,
  ]
  
  if (
    nrow(current) != 1
  ) {
    
    stop(
      paste0(
        "Expected one uncertainty row for completed_games_per_team = ",
        completed_games_per_team,
        "."
      )
    )
  }
  
  if (
    publication_uncertainty_mode ==
    "robust"
  ) {
    
    half_width <-
      current$robust_95_half_width
    
    holdout_coverage <-
      current$robust_holdout_coverage
    
    min_drift_coverage <-
      current$robust_min_drift_coverage
    
  } else {
    
    half_width <-
      current$pooled_95_half_width
    
    holdout_coverage <-
      current$pooled_holdout_coverage
    
    min_drift_coverage <-
      current$pooled_min_drift_coverage
  }
  
  return(
    list(
      half_width =
        as.numeric(
          half_width
        ),
      holdout_coverage =
        as.numeric(
          holdout_coverage
        ),
      min_drift_coverage =
        as.numeric(
          min_drift_coverage
        )
    )
  )
}


# ============================================================
# 38. TEAM RECORDS THROUGH A PUBLISHED WEEK
# ============================================================

calculate_team_records <- function(
    games,
    through_week,
    league_teams
) {
  
  completed_games <- games[
    games$week <=
      through_week,
  ]
  
  record_rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    current_team <- league_teams[
      i
    ]
    
    team_games <- completed_games[
      completed_games$home_team ==
        current_team |
        completed_games$away_team ==
        current_team,
    ]
    
    if (
      nrow(
        team_games
      ) ==
      0
    ) {
      
      wins <- 0
      losses <- 0
      ties <- 0
      
    } else {
      
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
    
    total_games <- (
      wins +
        losses +
        ties
    )
    
    win_pct <- if (
      total_games >
      0
    ) {
      
      (
        wins +
          0.5 *
          ties
      ) /
        total_games
      
    } else {
      
      NA_real_
    }
    
    record_text <- if (
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
    
    record_rows[[i]] <-
      data.frame(
        team =
          current_team,
        wins =
          wins,
        losses =
          losses,
        ties =
          ties,
        games_played =
          total_games,
        win_pct =
          win_pct,
        record =
          record_text,
        stringsAsFactors =
          FALSE
      )
  }
  
  records <- do.call(
    rbind,
    record_rows
  )
  
  rownames(records) <- NULL
  
  return(records)
}


# ============================================================
# 39. END-OF-WEEK SOS TABLE
# ============================================================

calculate_published_sos <- function(
    schedule,
    current_ratings,
    through_week,
    league_teams,
    estimated_hfa
) {
  
  sos_rows <- list()
  
  for (
    i in
    seq_along(
      league_teams
    )
  ) {
    
    current_team <- league_teams[
      i
    ]
    
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
    
    played_games <- team_schedule[
      team_schedule$week <=
        through_week,
    ]
    
    remaining_games <- team_schedule[
      team_schedule$week >
        through_week,
    ]
    
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
    
    full_sos <- mean(
      current_ratings[
        team_schedule$opponent
      ]
    )
    
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
    
    sos_rows[[i]] <-
      data.frame(
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
    sos_rows
  )
  
  rownames(output) <- NULL
  
  return(output)
}


# ============================================================
# 40. PUBLICATION STAGE
# ============================================================

get_publication_stage <- function(
    completed_games_per_team
) {
  
  if (
    completed_games_per_team <=
    2
  ) {
    
    return(
      "Provisional"
    )
    
  } else if (
    completed_games_per_team <=
    4
  ) {
    
    return(
      "Early Season"
    )
    
  } else {
    
    return(
      "Standard"
    )
  }
}


# ============================================================
# 41. CREATE ONE END-OF-WEEK POWER TABLE
# ============================================================

create_weekly_power_table <- function(
    games,
    schedule,
    team_info,
    through_week,
    previous_table = NULL,
    hidden_true_path = NULL
) {
  
  league_teams <- team_info$team
  
  training_games <- games[
    games$week <=
      through_week,
  ]
  
  if (
    nrow(
      training_games
    ) ==
    0
  ) {
    
    stop(
      "No completed games are available through this week."
    )
  }
  
  completed_games_per_team <-
    get_completed_games_per_team(
      training_games =
        training_games,
      league_teams =
        league_teams
    )
  
  rating_model <-
    fit_final_power_model(
      training_games =
        training_games,
      league_teams =
        league_teams
    )
  
  records <- calculate_team_records(
    games = games,
    through_week = through_week,
    league_teams = league_teams
  )
  
  sos_table <- calculate_published_sos(
    schedule = schedule,
    current_ratings =
      rating_model$ratings,
    through_week =
      through_week,
    league_teams =
      league_teams,
    estimated_hfa =
      rating_model$hfa
  )
  
  uncertainty <-
    get_publication_uncertainty(
      completed_games_per_team =
        completed_games_per_team
    )
  
  power_table <- data.frame(
    week =
      through_week,
    team =
      league_teams,
    rating =
      as.numeric(
        rating_model$ratings[
          league_teams
        ]
      ),
    offense_rating =
      as.numeric(
        rating_model$offense_rating[
          league_teams
        ]
      ),
    defense_rating =
      as.numeric(
        rating_model$defense_rating[
          league_teams
        ]
      ),
    uncertainty_95_half_width =
      uncertainty$half_width,
    stringsAsFactors =
      FALSE
  )
  
  power_table$rating_lower_95 <- (
    power_table$rating -
      power_table$uncertainty_95_half_width
  )
  
  power_table$rating_upper_95 <- (
    power_table$rating +
      power_table$uncertainty_95_half_width
  )
  
  # ----------------------------------------------------------
  # SIMULATION-ONLY HIDDEN-TRUTH VALIDATION
  # ----------------------------------------------------------
  # These fields are available only in synthetic experiments.
  # They must not be expected in real-world production data.
  # ----------------------------------------------------------
  
  power_table$true_hidden_rating <-
    NA_real_
  
  power_table$rating_error_vs_truth <-
    NA_real_
  
  power_table$absolute_error_vs_truth <-
    NA_real_
  
  if (
    !is.null(
      hidden_true_path
    )
  ) {
    
    hidden_truth <- hidden_true_path[[through_week]]$rating[
      league_teams
    ]
    
    power_table$true_hidden_rating <-
      as.numeric(
        hidden_truth[
          power_table$team
        ]
      )
    
    power_table$rating_error_vs_truth <- (
      power_table$rating -
        power_table$true_hidden_rating
    )
    
    power_table$absolute_error_vs_truth <- abs(
      power_table$rating_error_vs_truth
    )
  }
  
  power_table <- merge(
    power_table,
    team_info[
      ,
      c(
        "team",
        "division"
      )
    ],
    by =
      "team",
    all.x =
      TRUE
  )
  
  power_table <- merge(
    power_table,
    records,
    by =
      "team",
    all.x =
      TRUE
  )
  
  power_table <- merge(
    power_table,
    sos_table,
    by =
      "team",
    all.x =
      TRUE
  )
  
  power_table$rank <- as.integer(
    rank(
      -power_table$rating,
      ties.method =
        "min"
    )
  )
  
  power_table$offense_rank <- as.integer(
    rank(
      -power_table$offense_rating,
      ties.method =
        "min"
    )
  )
  
  power_table$defense_rank <- as.integer(
    rank(
      -power_table$defense_rating,
      ties.method =
        "min"
    )
  )
  
  power_table$previous_rank <-
    NA_integer_
  
  power_table$rank_change <-
    NA_integer_
  
  power_table$previous_rating <-
    0
  
  power_table$rating_change <-
    power_table$rating
  
  power_table$previous_offense_rating <-
    0
  
  power_table$offense_rating_change <-
    power_table$offense_rating
  
  power_table$previous_defense_rating <-
    0
  
  power_table$defense_rating_change <-
    power_table$defense_rating
  
  if (
    !is.null(
      previous_table
    )
  ) {
    
    previous_lookup <- previous_table[
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
    
    power_table <- merge(
      power_table,
      previous_lookup,
      by =
        "team",
      all.x =
        TRUE
    )
    
    power_table$previous_rank <-
      power_table$previous_rank_lookup
    
    power_table$rank_change <- (
      power_table$previous_rank -
        power_table$rank
    )
    
    power_table$previous_rating <-
      power_table$previous_rating_lookup
    
    power_table$rating_change <- (
      power_table$rating -
        power_table$previous_rating
    )
    
    power_table$previous_offense_rating <-
      power_table$previous_offense_lookup
    
    power_table$offense_rating_change <- (
      power_table$offense_rating -
        power_table$previous_offense_rating
    )
    
    power_table$previous_defense_rating <-
      power_table$previous_defense_lookup
    
    power_table$defense_rating_change <- (
      power_table$defense_rating -
        power_table$previous_defense_rating
    )
    
    power_table$previous_rank_lookup <-
      NULL
    
    power_table$previous_rating_lookup <-
      NULL
    
    power_table$previous_offense_lookup <-
      NULL
    
    power_table$previous_defense_lookup <-
      NULL
  }
  
  power_table$estimated_HFA <-
    rating_model$hfa
  
  power_table$league_mean_points <-
    rating_model$league_mean_points
  
  power_table$component_lambda <-
    huber_component_lambda
  
  power_table$huber_k <-
    huber_k
  
  power_table$recency_half_life <-
    recency_half_life
  
  power_table$hfa_lambda <-
    rating_model$hfa_lambda
  
  power_table$completed_games_per_team <-
    completed_games_per_team
  
  power_table$uncertainty_method <-
    publication_uncertainty_mode
  
  power_table$uncertainty_holdout_coverage <-
    uncertainty$holdout_coverage
  
  power_table$uncertainty_min_drift_coverage <-
    uncertainty$min_drift_coverage
  
  power_table$publication_stage <-
    get_publication_stage(
      completed_games_per_team
    )
  
  power_table <- power_table[
    order(
      power_table$rank,
      -power_table$rating,
      power_table$team
    ),
  ]
  
  rownames(power_table) <- NULL
  
  power_table <- power_table[
    ,
    c(
      "week",
      "publication_stage",
      "rank",
      "team",
      "division",
      "rating",
      "rating_lower_95",
      "rating_upper_95",
      "uncertainty_95_half_width",
      "true_hidden_rating",
      "rating_error_vs_truth",
      "absolute_error_vs_truth",
      "offense_rating",
      "offense_rank",
      "defense_rating",
      "defense_rank",
      "record",
      "wins",
      "losses",
      "ties",
      "win_pct",
      "games_played",
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
      "component_lambda",
      "huber_k",
      "recency_half_life",
      "hfa_lambda",
      "completed_games_per_team",
      "uncertainty_method",
      "uncertainty_holdout_coverage",
      "uncertainty_min_drift_coverage"
    )
  ]
  
  validation_RMSE <- if (
    all(
      is.na(
        power_table$true_hidden_rating
      )
    )
  ) {
    
    NA_real_
    
  } else {
    
    sqrt(
      mean(
        power_table$rating_error_vs_truth^2,
        na.rm = TRUE
      )
    )
  }
  
  validation_MAE <- if (
    all(
      is.na(
        power_table$true_hidden_rating
      )
    )
  ) {
    
    NA_real_
    
  } else {
    
    mean(
      power_table$absolute_error_vs_truth,
      na.rm = TRUE
    )
  }
  
  validation_Pearson <- if (
    all(
      is.na(
        power_table$true_hidden_rating
      )
    )
  ) {
    
    NA_real_
    
  } else {
    
    safe_cor(
      power_table$rating,
      power_table$true_hidden_rating,
      method = "pearson"
    )
  }
  
  weekly_settings <- data.frame(
    week =
      through_week,
    publication_stage =
      get_publication_stage(
        completed_games_per_team
      ),
    completed_games_per_team =
      completed_games_per_team,
    estimated_HFA =
      rating_model$hfa,
    league_mean_points =
      rating_model$league_mean_points,
    component_lambda =
      huber_component_lambda,
    huber_k =
      huber_k,
    recency_half_life =
      recency_half_life,
    hfa_lambda =
      rating_model$hfa_lambda,
    uncertainty_mode =
      publication_uncertainty_mode,
    uncertainty_95_half_width =
      uncertainty$half_width,
    uncertainty_holdout_coverage =
      uncertainty$holdout_coverage,
    uncertainty_min_drift_coverage =
      uncertainty$min_drift_coverage,
    validation_RMSE =
      validation_RMSE,
    validation_MAE =
      validation_MAE,
    validation_Pearson =
      validation_Pearson,
    stringsAsFactors =
      FALSE
  )
  
  return(
    list(
      power_table =
        power_table,
      model_settings =
        weekly_settings,
      rating_model =
        rating_model
    )
  )
}


# ============================================================
# 42. CREATE COMPLETE WEEKLY HISTORY
# ============================================================

create_power_rating_history <- function(
    games,
    schedule,
    team_info,
    n_weeks,
    hidden_true_path = NULL
) {
  
  table_storage <- list()
  settings_storage <- list()
  
  previous_table <- NULL
  
  for (
    current_week in
    1:n_weeks
  ) {
    
    cat(
      "Publishing week",
      current_week,
      "of",
      n_weeks,
      "\n"
    )
    
    current_result <-
      create_weekly_power_table(
        games =
          games,
        schedule =
          schedule,
        team_info =
          team_info,
        through_week =
          current_week,
        previous_table =
          previous_table,
        hidden_true_path =
          hidden_true_path
      )
    
    table_storage[[current_week]] <-
      current_result$power_table
    
    settings_storage[[current_week]] <-
      current_result$model_settings
    
    previous_table <-
      current_result$power_table
  }
  
  history <- do.call(
    rbind,
    table_storage
  )
  
  settings_history <- do.call(
    rbind,
    settings_storage
  )
  
  rownames(history) <- NULL
  rownames(settings_history) <- NULL
  
  return(
    list(
      history =
        history,
      settings_history =
        settings_history
    )
  )
}


# ============================================================
# 43. TEAM COLORS
# ============================================================

team_colors <- setNames(
  grDevices::hcl.colors(
    length(
      teams
    ),
    palette =
      "Dark 3"
  ),
  teams
)

team_info$plot_color <-
  unname(
    team_colors[
      team_info$team
    ]
  )


# ============================================================
# 44. GENERATE EXAMPLE SEASON
# ============================================================
#
# Hidden strengths generate outcomes but are NEVER used by the
# weekly publication model.
# ============================================================

cat(
  "\nGenerating Version 9A-H example season...\n"
)

example_season <-
  generate_dynamic_experiment_season(
    season_seed =
      example_season_seed,
    requested_drift_sd =
      example_true_weekly_drift_sd
  )

example_schedule <-
  example_season$schedule

example_games <-
  example_season$games

example_true_path <-
  example_season$true_path

example_schedule_validation <-
  validate_schedule(
    schedule =
      example_schedule,
    team_info =
      team_info,
    n_weeks =
      n_weeks
  )

example_bye_weeks <- setdiff(
  1:n_weeks,
  sort(
    unique(
      example_schedule$week
    )
  )
)


# ============================================================
# 45. BUILD WEEKLY PUBLICATION TABLES
# ============================================================

power_history_result <-
  create_power_rating_history(
    games =
      example_games,
    schedule =
      example_schedule,
    team_info =
      team_info,
    n_weeks =
      n_weeks,
    hidden_true_path =
      example_true_path
  )

power_rating_history <-
  power_history_result$history

weekly_model_settings <-
  power_history_result$settings_history


# ============================================================
# 46. PUBLISHED DISPLAY TABLES
# ============================================================

published_columns <- c(
  "rank",
  "team",
  "rating",
  "rating_lower_95",
  "rating_upper_95",
  "true_hidden_rating",
  "rating_error_vs_truth",
  "absolute_error_vs_truth",
  "offense_rating",
  "offense_rank",
  "defense_rating",
  "defense_rank",
  "record",
  "rank_change",
  "rating_change",
  "played_SOS",
  "remaining_SOS",
  "full_SOS"
)

weekly_published_tables <- list()

for (
  current_week in
  1:n_weeks
) {
  
  current_table <-
    power_rating_history[
      power_rating_history$week ==
        current_week,
      published_columns
    ]
  
  current_table <- current_table[
    order(
      current_table$rank
    ),
  ]
  
  rownames(current_table) <- NULL
  
  weekly_published_tables[[current_week]] <-
    current_table
}


# ============================================================
# 47. HIDDEN ANSWER-KEY HISTORY
# ============================================================

answer_key_rows <- list()
answer_key_index <- 1

for (
  current_week in
  1:n_weeks
) {
  
  current_truth <-
    example_true_path[[current_week]]
  
  for (
    team_index in
    seq_along(
      teams
    )
  ) {
    
    current_team <- teams[
      team_index
    ]
    
    answer_key_rows[[answer_key_index]] <-
      data.frame(
        week =
          current_week,
        team =
          current_team,
        true_offense =
          current_truth$offense[
            current_team
          ],
        true_defense =
          current_truth$defense[
            current_team
          ],
        true_rating =
          current_truth$rating[
            current_team
          ],
        realized_weekly_drift_sd =
          current_truth$realized_drift_sd,
        stringsAsFactors =
          FALSE
      )
    
    answer_key_index <-
      answer_key_index + 1
  }
}

hidden_answer_key_history <- do.call(
  rbind,
  answer_key_rows
)

rownames(
  hidden_answer_key_history
) <- NULL


# ============================================================
# 48. PRINT SELECTED WEEKS
# ============================================================

weeks_to_print <- c(
  1,
  5,
  8,
  11,
  14
)

for (
  current_week in
  weeks_to_print
) {
  
  cat("\n\n========================================\n")
  cat(
    "VERSION 9A-H POWER RATINGS - END OF WEEK",
    current_week,
    "\n"
  )
  cat("========================================\n\n")
  
  current_stage <- unique(
    power_rating_history$publication_stage[
      power_rating_history$week ==
        current_week
    ]
  )
  
  cat(
    "Stage:",
    current_stage,
    "\n\n"
  )
  
  print(
    weekly_published_tables[[current_week]],
    row.names = FALSE
  )
}


# ============================================================
# 49. LATEST POWER TABLE
# ============================================================

latest_week <- max(
  power_rating_history$week
)

latest_power_table <-
  power_rating_history[
    power_rating_history$week ==
      latest_week,
  ]

latest_power_table <-
  latest_power_table[
    order(
      latest_power_table$rank
    ),
  ]

rownames(
  latest_power_table
) <- NULL


# ============================================================
# 50. PLOTS
# ============================================================

rating_history_plot <- ggplot(
  power_rating_history,
  aes(
    x = week,
    y = rating,
    color = team,
    group = team
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_color_manual(
    values =
      team_colors
  ) +
  scale_x_continuous(
    breaks =
      1:n_weeks
  ) +
  labs(
    title =
      "Version 9A-H Weekly Power-Rating History",
    subtitle =
      "Overall rating = offense + defense; neutral-field points above/below average",
    x =
      "Week",
    y =
      "Overall Power Rating",
    color =
      "Team"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_rating_history.png",
  plot =
    rating_history_plot,
  width =
    13,
  height =
    8,
  dpi =
    170
)


rank_history_plot <- ggplot(
  power_rating_history,
  aes(
    x = week,
    y = rank,
    color = team,
    group = team
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.7
  ) +
  scale_y_reverse(
    breaks =
      1:length(
        teams
      )
  ) +
  scale_x_continuous(
    breaks =
      1:n_weeks
  ) +
  scale_color_manual(
    values =
      team_colors
  ) +
  labs(
    title =
      "Version 9A-H Weekly Rank History",
    subtitle =
      "Rank 1 is the highest-rated team",
    x =
      "Week",
    y =
      "Power Rank",
    color =
      "Team"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_rank_history.png",
  plot =
    rank_history_plot,
  width =
    13,
  height =
    8,
  dpi =
    170
)


latest_plot_data <-
  latest_power_table

latest_plot_data$team <- factor(
  latest_plot_data$team,
  levels = rev(
    latest_plot_data$team[
      order(
        latest_plot_data$rank
      )
    ]
  )
)

latest_rating_plot <- ggplot(
  latest_plot_data,
  aes(
    x = team,
    y = rating,
    fill = team
  )
) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
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
      "Version 9A-H Power Ratings - End of Week ",
      latest_week
    ),
    subtitle =
      "Neutral-field points above/below league average",
    x =
      "Team",
    y =
      "Overall Power Rating"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_latest_ratings.png",
  plot =
    latest_rating_plot,
  width =
    10,
  height =
    8,
  dpi =
    170
)


latest_uncertainty_plot <- ggplot(
  latest_plot_data,
  aes(
    x = team,
    y = rating,
    color = team
  )
) +
  geom_errorbar(
    aes(
      ymin =
        rating_lower_95,
      ymax =
        rating_upper_95
    ),
    width =
      0.2
  ) +
  geom_point(
    size =
      3
  ) +
  coord_flip() +
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
  guides(
    color =
      "none"
  ) +
  labs(
    title = paste0(
      "Version 9A-H Rating Uncertainty - End of Week ",
      latest_week
    ),
    subtitle = paste0(
      "Simulation-calibrated ",
      publication_uncertainty_mode,
      " 95% ranges"
    ),
    x =
      "Team",
    y =
      "Overall Power Rating"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_latest_uncertainty.png",
  plot =
    latest_uncertainty_plot,
  width =
    10,
  height =
    8,
  dpi =
    170
)


latest_offense_plot <- ggplot(
  latest_plot_data,
  aes(
    x = reorder(
      team,
      offense_rating
    ),
    y = offense_rating,
    fill = team
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
    title =
      "Version 9A-H Latest Offense Ratings",
    subtitle =
      "Points of scoring strength relative to league average",
    x =
      "Team",
    y =
      "Offense Rating"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_latest_offense.png",
  plot =
    latest_offense_plot,
  width =
    10,
  height =
    8,
  dpi =
    170
)


latest_defense_plot <- ggplot(
  latest_plot_data,
  aes(
    x = reorder(
      team,
      defense_rating
    ),
    y = defense_rating,
    fill = team
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
    title =
      "Version 9A-H Latest Defense Ratings",
    subtitle =
      "Higher = more opponent points suppressed relative to league average",
    x =
      "Team",
    y =
      "Defense Rating"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_latest_defense.png",
  plot =
    latest_defense_plot,
  width =
    10,
  height =
    8,
  dpi =
    170
)



# ------------------------------------------------------------
# Simulation-only validation plot: estimated vs hidden truth
# ------------------------------------------------------------

latest_validation_plot <- ggplot(
  latest_power_table,
  aes(
    x = true_hidden_rating,
    y = rating,
    color = team
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_point(
    size = 3
  ) +
  scale_color_manual(
    values = team_colors
  ) +
  labs(
    title = paste0(
      "Version 9A-H Estimated vs Hidden True Ratings - Week ",
      latest_week
    ),
    subtitle =
      "Simulation-only validation; dashed line is perfect agreement",
    x =
      "Hidden True Rating",
    y =
      "Estimated Rating",
    color =
      "Team"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_latest_validation_vs_truth.png",
  plot =
    latest_validation_plot,
  width =
    9,
  height =
    7,
  dpi =
    170
)


# ------------------------------------------------------------
# Weekly validation error
# ------------------------------------------------------------

weekly_validation_plot <- ggplot(
  weekly_model_settings,
  aes(
    x = week
  )
) +
  geom_line(
    aes(
      y = validation_RMSE
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(
      y = validation_RMSE
    ),
    size = 2
  ) +
  geom_line(
    aes(
      y = validation_MAE
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(
    aes(
      y = validation_MAE
    ),
    size = 2,
    shape = 1
  ) +
  scale_x_continuous(
    breaks = 1:n_weeks
  ) +
  labs(
    title =
      "Version 9A-H Weekly Validation Error vs Hidden Truth",
    subtitle =
      "Solid = RMSE; dashed = MAE",
    x =
      "Week",
    y =
      "Rating Error"
  ) +
  theme_minimal()

ggsave(
  "version9A_H_weekly_validation_error.png",
  plot =
    weekly_validation_plot,
  width =
    10,
  height =
    7,
  dpi =
    170
)


# ============================================================
# 51. SAVE OUTPUTS
# ============================================================

write.csv(
  team_info,
  "version9A_H_team_info.csv",
  row.names = FALSE
)

write.csv(
  example_schedule,
  "version9A_H_schedule.csv",
  row.names = FALSE
)

write.csv(
  example_games,
  "version9A_H_simulated_results.csv",
  row.names = FALSE
)

write.csv(
  hidden_answer_key_history,
  "version9A_H_hidden_answer_key_history.csv",
  row.names = FALSE
)

write.csv(
  power_rating_history,
  "version9A_H_power_rating_history.csv",
  row.names = FALSE
)

write.csv(
  weekly_model_settings,
  "version9A_H_weekly_model_settings.csv",
  row.names = FALSE
)

write.csv(
  latest_power_table,
  "version9A_H_latest_power_table.csv",
  row.names = FALSE
)

write.csv(
  publication_uncertainty,
  "version9A_H_uncertainty_calibration_snapshot.csv",
  row.names = FALSE
)

for (
  current_week in
  1:n_weeks
) {
  
  write.csv(
    weekly_published_tables[[current_week]],
    paste0(
      "version9A_H_week_",
      current_week,
      "_power_rankings.csv"
    ),
    row.names = FALSE
  )
}


# ============================================================
# 52. FINAL SUMMARY
# ============================================================

cat("\n\n========================================\n")
cat("VERSION 9A-H2 COMPLETE\n")
cat("========================================\n\n")

cat("Finalized core model:\n")
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

cat("Rating interpretation:\n")
cat(
  "  Overall rating = neutral-field points above/below average.\n"
)
cat(
  "  Overall rating = offense rating + defense rating.\n"
)
cat(
  "  Higher offense = better scoring strength.\n"
)
cat(
  "  Higher defense = better opponent-points suppression.\n\n"
)

cat("Timing:\n")
cat(
  "  End-of-week rankings use only results through that week.\n"
)
cat(
  "  Future schedule is known only for SOS reporting.\n\n"
)

cat(
  "Uncertainty mode:",
  publication_uncertainty_mode,
  "\n"
)
cat(
  "Hidden-truth validation columns are SIMULATION-ONLY and unavailable in real deployment.\n"
)
cat(
  "Example hidden weekly drift SD:",
  example_true_weekly_drift_sd,
  "\n"
)
cat(
  "Schedule valid:",
  example_schedule_validation$valid,
  "\n"
)
cat(
  "Schedule connected:",
  example_schedule_validation$connected,
  "\n"
)
cat(
  "League-wide bye weeks:",
  paste(
    example_bye_weeks,
    collapse = ", "
  ),
  "\n\n"
)

cat("LATEST POWER RATINGS:\n\n")

print(
  latest_power_table[
    ,
    c(
      "rank",
      "team",
      "rating",
      "rating_lower_95",
      "rating_upper_95",
      "true_hidden_rating",
      "rating_error_vs_truth",
      "absolute_error_vs_truth",
      "offense_rating",
      "offense_rank",
      "defense_rating",
      "defense_rank",
      "record",
      "rank_change",
      "rating_change",
      "played_SOS",
      "remaining_SOS",
      "full_SOS"
    )
  ],
  row.names = FALSE
)

cat("\nWEEKLY MODEL SETTINGS:\n\n")

print(
  weekly_model_settings,
  row.names = FALSE
)

cat(
  "\nOutput CSVs and PNG plots were written to the current working directory.\n"
)

# ============================================================
# END VERSION 9A-H
# ============================================================


# ============================================================
# VERSION 9C
# NEXT-GAME FORECASTING + WALK-FORWARD OOS EVALUATION
# ============================================================
#
# This section extends Version 9A-H2 without changing its rating
# engine or weekly publication logic.
#
# FORECAST INFORMATION SET
# ------------------------
# To predict games in target week t:
#
#   1. use ONLY games with week < t
#   2. fit finalized Score Off/Def Huber + HL12 model
#   3. predict every scheduled game in week t
#
# Therefore every evaluated forecast is genuinely out-of-sample.
#
# Week 1 is not evaluated because there are no prior results from
# which to estimate team offense/defense ratings.
#
# EXPECTED SCORES
# ---------------
#
# Expected Home Score =
#   league mean points
#   + Home Offense
#   - Away Defense
#   + HFA / 2
#
# Expected Away Score =
#   league mean points
#   + Away Offense
#   - Home Defense
#   - HFA / 2
#
# Expected Margin =
#   Expected Home Score - Expected Away Score
#
# which is algebraically identical to:
#
#   Home Overall Rating - Away Overall Rating + HFA
#
# WIN PROBABILITY
# ---------------
# Margin error is modeled with a Student-t4 distribution.
#
# The predictive margin SD is estimated from residuals of games
# available BEFORE the target week and shrunk toward the generic
# structural prior of 8.5 points.
#
# This is NOT a team-strength preseason prior.
#
# OOS METRICS
# -----------
# Margin:
#   MAE, RMSE, bias, correlation
#
# Scores:
#   home-score MAE/RMSE
#   away-score MAE/RMSE
#   all-score MAE/RMSE
#   total-score MAE/RMSE
#
# Winner:
#   accuracy on non-tied games
#   Brier score (ties encoded as 0.5 outcome)
#   log loss on non-tied games
#
# Simulation-only latent validation:
#   expected-margin MAE/RMSE against the hidden game expectation
#   expected-score MAE/RMSE against hidden expected scores
#
# ============================================================


# ============================================================
# 53. 9C FORECAST SETTINGS
# ============================================================

win_probability_df <- 4

generic_margin_noise_prior_sd <- 8.5
margin_noise_prior_weight <- 20

probability_bin_breaks <- seq(
  0,
  1,
  by = 0.10
)


# ============================================================
# 54. PREDICT EXPECTED SCORES FOR GAMES
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
# 55. TRAINING RESIDUALS FOR PREDICTIVE NOISE
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
# 56. SHRUNK PREDICTIVE MARGIN SD
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
# 57. MARGIN -> WIN PROBABILITY
# ============================================================

margin_to_home_win_probability <- function(
    predicted_margin,
    predictive_margin_sd,
    degrees_freedom =
      win_probability_df
) {
  
  if (
    degrees_freedom <=
    2
  ) {
    
    stop(
      "Student-t degrees of freedom must exceed 2."
    )
  }
  
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
  
  probability <- pmin(
    pmax(
      probability,
      1e-6
    ),
    1 -
      1e-6
  )
  
  return(
    probability
  )
}


# ============================================================
# 58. FORECAST ONE FUTURE WEEK
# ============================================================

forecast_week <- function(
    games,
    schedule,
    target_week,
    team_info
) {
  
  league_teams <-
    team_info$team
  
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
      target_games
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  if (
    nrow(
      training_games
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  rating_model <-
    fit_final_power_model(
      training_games =
        training_games,
      league_teams =
        league_teams
    )
  
  completed_games_before_week <-
    get_completed_games_per_team(
      training_games =
        training_games,
      league_teams =
        league_teams
    )
  
  predictive_margin_sd <-
    estimate_predictive_margin_sd(
      rating_model =
        rating_model,
      training_games =
        training_games,
      league_teams =
        league_teams
    )
  
  predictions <-
    predict_games_from_rating_model(
      rating_model =
        rating_model,
      target_games =
        target_games,
      league_teams =
        league_teams
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
  
  predictions$predicted_winner_probability <-
    pmax(
      predictions$home_win_probability,
      predictions$away_win_probability
    )
  
  predictions$predictive_margin_sd <-
    predictive_margin_sd
  
  predictions$completed_games_per_team_before_week <-
    completed_games_before_week
  
  predictions$forecast_training_games <-
    nrow(
      training_games
    )
  
  predictions$forecast_available <-
    TRUE
  
  # ----------------------------------------------------------
  # ACTUAL OUTCOMES
  # ----------------------------------------------------------
  
  predictions$actual_total <- (
    predictions$home_score +
      predictions$away_score
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
      predictions$home_score
  )
  
  predictions$away_score_error <- (
    predictions$predicted_away_score -
      predictions$away_score
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
    predictions$actual_margin ==
      0,
    NA_real_,
    as.numeric(
      predictions$predicted_winner ==
        ifelse(
          predictions$actual_margin >
            0,
          predictions$home_team,
          predictions$away_team
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
  
  # ----------------------------------------------------------
  # SIMULATION-ONLY LATENT EXPECTATION VALIDATION
  # ----------------------------------------------------------
  
  if (
    "true_expected_margin" %in%
    names(
      predictions
    )
  ) {
    
    predictions$latent_margin_error <- (
      predictions$predicted_margin -
        predictions$true_expected_margin
    )
    
    predictions$absolute_latent_margin_error <- abs(
      predictions$latent_margin_error
    )
    
    predictions$squared_latent_margin_error <- (
      predictions$latent_margin_error^2
    )
    
  } else {
    
    predictions$latent_margin_error <-
      NA_real_
    
    predictions$absolute_latent_margin_error <-
      NA_real_
    
    predictions$squared_latent_margin_error <-
      NA_real_
  }
  
  if (
    all(
      c(
        "expected_home_score",
        "expected_away_score"
      ) %in%
      names(
        predictions
      )
    )
  ) {
    
    predictions$latent_home_score_error <- (
      predictions$predicted_home_score -
        predictions$expected_home_score
    )
    
    predictions$latent_away_score_error <- (
      predictions$predicted_away_score -
        predictions$expected_away_score
    )
    
  } else {
    
    predictions$latent_home_score_error <-
      NA_real_
    
    predictions$latent_away_score_error <-
      NA_real_
  }
  
  return(
    predictions
  )
}


# ============================================================
# 59. WALK-FORWARD OOS FORECASTS
# ============================================================

create_walk_forward_predictions <- function(
    games,
    schedule,
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
    
    cat(
      "Forecasting week",
      target_week,
      "using only prior results...\n"
    )
    
    current_predictions <-
      forecast_week(
        games =
          games,
        schedule =
          schedule,
        target_week =
          target_week,
        team_info =
          team_info
      )
    
    if (
      nrow(
        current_predictions
      ) >
      0
    ) {
      
      prediction_storage[[prediction_position]] <-
        current_predictions
      
      prediction_position <-
        prediction_position +
        1
    }
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
# 60. OOS FORECAST SUMMARY
# ============================================================

summarize_oos_forecasts <- function(
    predictions
) {
  
  non_tie_predictions <- predictions[
    !is.na(
      predictions$winner_correct
    ),
  ]
  
  all_score_errors <- c(
    predictions$home_score_error,
    predictions$away_score_error
  )
  
  latent_score_errors <- c(
    predictions$latent_home_score_error,
    predictions$latent_away_score_error
  )
  
  output <- data.frame(
    forecast_games =
      nrow(
        predictions
      ),
    non_tied_forecast_games =
      nrow(
        non_tie_predictions
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
    margin_Pearson =
      safe_cor(
        predictions$predicted_margin,
        predictions$actual_margin,
        method = "pearson"
      ),
    home_score_MAE =
      mean(
        abs(
          predictions$home_score_error
        )
      ),
    home_score_RMSE =
      sqrt(
        mean(
          predictions$home_score_error^2
        )
      ),
    away_score_MAE =
      mean(
        abs(
          predictions$away_score_error
        )
      ),
    away_score_RMSE =
      sqrt(
        mean(
          predictions$away_score_error^2
        )
      ),
    all_score_MAE =
      mean(
        abs(
          all_score_errors
        )
      ),
    all_score_RMSE =
      sqrt(
        mean(
          all_score_errors^2
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
      mean(
        non_tie_predictions$winner_correct
      ),
    brier_score =
      mean(
        predictions$brier_score
      ),
    log_loss =
      mean(
        predictions$log_loss,
        na.rm = TRUE
      ),
    mean_predicted_home_win_probability =
      mean(
        predictions$home_win_probability
      ),
    actual_home_win_rate_soft =
      mean(
        predictions$actual_home_win_soft
      ),
    latent_margin_MAE =
      mean(
        predictions$absolute_latent_margin_error,
        na.rm = TRUE
      ),
    latent_margin_RMSE =
      sqrt(
        mean(
          predictions$squared_latent_margin_error,
          na.rm = TRUE
        )
      ),
    latent_score_MAE =
      mean(
        abs(
          latent_score_errors
        ),
        na.rm = TRUE
      ),
    latent_score_RMSE =
      sqrt(
        mean(
          latent_score_errors^2,
          na.rm = TRUE
        )
      ),
    stringsAsFactors =
      FALSE
  )
  
  return(output)
}


# ============================================================
# 61. OOS METRICS BY TARGET WEEK
# ============================================================

summarize_oos_by_week <- function(
    predictions
) {
  
  target_weeks <- sort(
    unique(
      predictions$week
    )
  )
  
  weekly_rows <- list()
  
  for (
    i in
    seq_along(
      target_weeks
    )
  ) {
    
    current_week <- target_weeks[
      i
    ]
    
    current <- predictions[
      predictions$week ==
        current_week,
    ]
    
    non_tie <- current[
      !is.na(
        current$winner_correct
      ),
    ]
    
    weekly_rows[[i]] <-
      data.frame(
        week =
          current_week,
        completed_games_per_team_before_week =
          unique(
            current$completed_games_per_team_before_week
          )[1],
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
            na.rm = TRUE
          ),
        mean_predictive_margin_sd =
          mean(
            current$predictive_margin_sd
          ),
        latent_margin_RMSE =
          sqrt(
            mean(
              current$squared_latent_margin_error,
              na.rm = TRUE
            )
          ),
        stringsAsFactors =
          FALSE
      )
  }
  
  output <- do.call(
    rbind,
    weekly_rows
  )
  
  rownames(output) <- NULL
  
  return(output)
}


# ============================================================
# 62. WIN-PROBABILITY CALIBRATION TABLE
# ============================================================

build_probability_calibration_table <- function(
    predictions,
    bin_breaks =
      probability_bin_breaks
) {
  
  calibration_data <- predictions
  
  calibration_data$probability_bin <- cut(
    calibration_data$home_win_probability,
    breaks =
      bin_breaks,
    include.lowest =
      TRUE,
    right =
      TRUE
  )
  
  groups <- levels(
    calibration_data$probability_bin
  )
  
  output_rows <- list()
  
  for (
    i in
    seq_along(
      groups
    )
  ) {
    
    current_group <- groups[
      i
    ]
    
    current <- calibration_data[
      calibration_data$probability_bin ==
        current_group,
    ]
    
    if (
      nrow(
        current
      ) ==
      0
    ) {
      
      next
    }
    
    output_rows[[length(output_rows) + 1]] <-
      data.frame(
        probability_bin =
          current_group,
        games =
          nrow(
            current
          ),
        mean_predicted_home_win_probability =
          mean(
            current$home_win_probability
          ),
        actual_home_win_rate_soft =
          mean(
            current$actual_home_win_soft
          ),
        calibration_difference =
          mean(
            current$actual_home_win_soft
          ) -
          mean(
            current$home_win_probability
          ),
        stringsAsFactors =
          FALSE
      )
  }
  
  output <- do.call(
    rbind,
    output_rows
  )
  
  rownames(output) <- NULL
  
  return(output)
}


# ============================================================
# 63. CURRENT NEXT-SCHEDULED-GAME FORECAST FUNCTION
# ============================================================
#
# This function is intended for real use when the season is only
# partially complete.
#
# Give it the games that have ALREADY been played and the full
# schedule. It finds the next scheduled week and predicts it.
#
# In the fully simulated example season all games are complete,
# so there may be no remaining future week at script end.
# ============================================================

predict_next_scheduled_games <- function(
    completed_games,
    full_schedule,
    team_info
) {
  
  if (
    nrow(
      completed_games
    ) ==
    0
  ) {
    
    stop(
      "At least one completed game week is required for the finalized team-rating forecast."
    )
  }
  
  played_game_ids <- completed_games$game_id
  
  remaining_schedule <- full_schedule[
    !full_schedule$game_id %in%
      played_game_ids,
  ]
  
  if (
    nrow(
      remaining_schedule
    ) ==
    0
  ) {
    
    return(
      data.frame()
    )
  }
  
  next_week <- min(
    remaining_schedule$week
  )
  
  training_games <- completed_games
  
  rating_model <-
    fit_final_power_model(
      training_games =
        training_games,
      league_teams =
        team_info$team
    )
  
  target_schedule <- remaining_schedule[
    remaining_schedule$week ==
      next_week,
  ]
  
  # Join known completed-score columns only if this is a simulated
  # replay. For genuine future games, create an empty target frame
  # with the schedule identifiers and teams.
  target_games <- target_schedule
  
  if (
    !"home_score" %in%
    names(
      target_games
    )
  ) {
    
    target_games$home_score <-
      NA_real_
  }
  
  if (
    !"away_score" %in%
    names(
      target_games
    )
  ) {
    
    target_games$away_score <-
      NA_real_
  }
  
  if (
    !"actual_margin" %in%
    names(
      target_games
    )
  ) {
    
    target_games$actual_margin <-
      NA_real_
  }
  
  predictive_margin_sd <-
    estimate_predictive_margin_sd(
      rating_model =
        rating_model,
      training_games =
        training_games,
      league_teams =
        team_info$team
    )
  
  predictions <-
    predict_games_from_rating_model(
      rating_model =
        rating_model,
      target_games =
        target_games,
      league_teams =
        team_info$team
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
  
  predictions$predicted_winner_probability <-
    pmax(
      predictions$home_win_probability,
      predictions$away_win_probability
    )
  
  predictions$predictive_margin_sd <-
    predictive_margin_sd
  
  return(
    predictions[
      ,
      c(
        "week",
        "game_id",
        "home_team",
        "away_team",
        "predicted_home_score",
        "predicted_away_score",
        "predicted_margin",
        "home_win_probability",
        "away_win_probability",
        "predicted_winner",
        "predicted_winner_probability",
        "predictive_margin_sd",
        "pregame_home_rating",
        "pregame_away_rating",
        "pregame_home_offense",
        "pregame_away_offense",
        "pregame_home_defense",
        "pregame_away_defense",
        "pregame_estimated_HFA",
        "pregame_league_mean_points"
      )
    ]
  )
}


# ============================================================
# 64. RUN WALK-FORWARD OOS EVALUATION
# ============================================================

cat("\n\n========================================\n")
cat("VERSION 9C - WALK-FORWARD FORECASTING\n")
cat("========================================\n\n")

walk_forward_predictions <-
  create_walk_forward_predictions(
    games =
      example_games,
    schedule =
      example_schedule,
    team_info =
      team_info
  )

oos_forecast_summary <-
  summarize_oos_forecasts(
    predictions =
      walk_forward_predictions
  )

oos_forecast_by_week <-
  summarize_oos_by_week(
    predictions =
      walk_forward_predictions
  )

probability_calibration_table <-
  build_probability_calibration_table(
    predictions =
      walk_forward_predictions
  )


# ============================================================
# 65. COMPACT PREGAME FORECAST TABLE
# ============================================================

pregame_prediction_columns <- c(
  "week",
  "game_id",
  "home_team",
  "away_team",
  "pregame_home_rating",
  "pregame_away_rating",
  "predicted_home_score",
  "predicted_away_score",
  "predicted_margin",
  "home_win_probability",
  "away_win_probability",
  "predicted_winner",
  "predicted_winner_probability",
  "predictive_margin_sd",
  "home_score",
  "away_score",
  "actual_margin",
  "absolute_margin_error",
  "winner_correct",
  "brier_score",
  "true_expected_margin",
  "latent_margin_error"
)

available_prediction_columns <-
  pregame_prediction_columns[
    pregame_prediction_columns %in%
      names(
        walk_forward_predictions
      )
  ]

pregame_prediction_table <-
  walk_forward_predictions[
    ,
    available_prediction_columns
  ]


# ============================================================
# 66. WRITE WEEK-SPECIFIC PREGAME PREDICTION FILES
# ============================================================

forecast_weeks <- sort(
  unique(
    walk_forward_predictions$week
  )
)

for (
  target_week in
  forecast_weeks
) {
  
  current <- pregame_prediction_table[
    pregame_prediction_table$week ==
      target_week,
  ]
  
  write.csv(
    current,
    paste0(
      "version9C_week_",
      target_week,
      "_pregame_predictions.csv"
    ),
    row.names =
      FALSE
  )
}


# ============================================================
# 67. PREDICTION PLOTS
# ============================================================

margin_prediction_plot <- ggplot(
  walk_forward_predictions,
  aes(
    x = predicted_margin,
    y = actual_margin
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    title =
      "Version 9C: Out-of-Sample Predicted vs Actual Margin",
    subtitle =
      "Every point was forecast using only results from earlier weeks",
    x =
      "Predicted Home Margin",
    y =
      "Actual Home Margin"
  ) +
  theme_minimal()

ggsave(
  "version9C_predicted_vs_actual_margin.png",
  plot =
    margin_prediction_plot,
  width =
    9,
  height =
    7,
  dpi =
    170
)


weekly_forecast_error_plot <- ggplot(
  oos_forecast_by_week,
  aes(
    x = week
  )
) +
  geom_line(
    aes(
      y = margin_RMSE
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(
      y = margin_RMSE
    ),
    size = 2
  ) +
  geom_line(
    aes(
      y = margin_MAE
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(
    aes(
      y = margin_MAE
    ),
    size = 2,
    shape = 1
  ) +
  labs(
    title =
      "Version 9C: Walk-Forward Margin Error by Week",
    subtitle =
      "Solid = RMSE; dashed = MAE",
    x =
      "Target Week",
    y =
      "Margin Error"
  ) +
  theme_minimal()

ggsave(
  "version9C_margin_error_by_week.png",
  plot =
    weekly_forecast_error_plot,
  width =
    10,
  height =
    7,
  dpi =
    170
)


probability_calibration_plot <- ggplot(
  probability_calibration_table,
  aes(
    x =
      mean_predicted_home_win_probability,
    y =
      actual_home_win_rate_soft
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_point(
    aes(
      size = games
    )
  ) +
  geom_line() +
  coord_equal(
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0,
      1
    )
  ) +
  labs(
    title =
      "Version 9C: Home Win-Probability Calibration",
    subtitle =
      "Closer to the dashed 45-degree line is better",
    x =
      "Mean Predicted Home Win Probability",
    y =
      "Observed Home Win Rate",
    size =
      "Games"
  ) +
  theme_minimal()

ggsave(
  "version9C_win_probability_calibration.png",
  plot =
    probability_calibration_plot,
  width =
    8,
  height =
    8,
  dpi =
    170
)


latent_margin_plot <- ggplot(
  walk_forward_predictions,
  aes(
    x = true_expected_margin,
    y = predicted_margin
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    title =
      "Version 9C: Predicted Margin vs Hidden Expected Margin",
    subtitle =
      "Simulation-only: separates rating error from game-result randomness",
    x =
      "Hidden True Expected Margin",
    y =
      "Pregame Predicted Margin"
  ) +
  theme_minimal()

ggsave(
  "version9C_predicted_vs_hidden_expected_margin.png",
  plot =
    latent_margin_plot,
  width =
    9,
  height =
    7,
  dpi =
    170
)


# ============================================================
# 68. SAVE 9C OUTPUTS
# ============================================================

write.csv(
  pregame_prediction_table,
  "version9C_all_walk_forward_predictions.csv",
  row.names =
    FALSE
)

write.csv(
  oos_forecast_summary,
  "version9C_oos_forecast_summary.csv",
  row.names =
    FALSE
)

write.csv(
  oos_forecast_by_week,
  "version9C_oos_forecast_by_week.csv",
  row.names =
    FALSE
)

write.csv(
  probability_calibration_table,
  "version9C_win_probability_calibration.csv",
  row.names =
    FALSE
)


# ============================================================
# 69. EXAMPLE NEXT-GAME USAGE
# ============================================================
#
# The fully simulated season is complete, so there are no actual
# remaining games after Week 14.
#
# To demonstrate the production helper, pretend results are known
# only through Week 11 and predict the next scheduled week.
# ============================================================

example_completed_through_week <- 11

example_completed_games <- example_games[
  example_games$week <=
    example_completed_through_week,
]

example_next_game_predictions <-
  predict_next_scheduled_games(
    completed_games =
      example_completed_games,
    full_schedule =
      example_schedule,
    team_info =
      team_info
  )

write.csv(
  example_next_game_predictions,
  "version9C_example_next_scheduled_games.csv",
  row.names =
    FALSE
)


# ============================================================
# 70. PRINT 9C RESULTS
# ============================================================

cat("\n========================================\n")
cat("VERSION 9C OOS FORECAST SUMMARY\n")
cat("========================================\n\n")

print(
  oos_forecast_summary,
  row.names =
    FALSE
)

cat("\n========================================\n")
cat("OOS FORECAST PERFORMANCE BY WEEK\n")
cat("========================================\n\n")

print(
  oos_forecast_by_week,
  row.names =
    FALSE
)

cat("\n========================================\n")
cat("WIN-PROBABILITY CALIBRATION\n")
cat("========================================\n\n")

print(
  probability_calibration_table,
  row.names =
    FALSE
)

cat("\n========================================\n")
cat(
  "EXAMPLE NEXT SCHEDULED GAMES AFTER WEEK ",
  example_completed_through_week,
  "\n",
  sep = ""
)
cat("========================================\n\n")

print(
  example_next_game_predictions,
  row.names =
    FALSE
)

cat("\n========================================\n")
cat("VERSION 9C COMPLETE\n")
cat("========================================\n\n")

cat(
  "Core rating model: Score Offense/Defense Huber + HL12\n"
)
cat(
  "Win probability distribution: Student-t",
  win_probability_df,
  "\n",
  sep = ""
)
cat(
  "Generic predictive margin-noise prior SD:",
  generic_margin_noise_prior_sd,
  "\n"
)
cat(
  "Margin-noise prior weight:",
  margin_noise_prior_weight,
  "\n\n"
)

cat(
  "All OOS forecasts were generated before the target games were included in model fitting.\n"
)
cat(
  "Week 1 is intentionally excluded because the finalized team-rating model has no prior team results yet.\n"
)
cat(
  "Simulation-only latent metrics compare the forecast to the hidden expected game result rather than the noisy realized final score.\n\n"
)

cat(
  "9C CSVs and PNG plots were written to the current working directory.\n"
)

# ============================================================
# END VERSION 9C
# ============================================================

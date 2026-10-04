# ============================================================
# FOOTBALL POWER RATINGS - VERSION 8B
# UNBALANCED SCHEDULES + OPPONENT ADJUSTMENT VALIDATION
# ============================================================
#
# PURPOSE
# -------
# Version 8B tests whether the opponent-adjusted ridge/SRS model
# correctly handles the unbalanced 16-team schedules created in 8A.
#
# We compare two rating systems:
#
#   1. NAIVE POINT-DIFFERENTIAL RATING
#      - HL12 recency weighting
#      - HFA adjusted
#      - same margin cap and shrinkage as the adjusted model
#      - DOES NOT adjust for opponent quality
#
#   2. OPPONENT-ADJUSTED RIDGE RATING
#      - HL12 recency weighting
#      - simultaneous opponent adjustment
#      - HFA prior
#      - smooth parity-adaptive margin cap
#      - parity-adaptive shrinkage
#
# Hidden team strengths are STATIC in Version 8B.
#
# Main questions:
#
#   A. Does opponent adjustment recover hidden team strength better?
#   B. Does it predict future games better out of sample?
#   C. Does naive point differential systematically underrate teams
#      with difficult schedules and overrate teams with easy schedules?
#   D. Does the benefit of opponent adjustment increase as true SOS
#      imbalance becomes larger?
#
# No future leakage:
#   - target week t is predicted using only weeks < t
#   - first active round has no prediction
#   - parity calibration is built on a separate simulation sample
#
# ============================================================

library(ggplot2)


# ============================================================
# 1. GLOBAL SETTINGS
# ============================================================

n_weeks <- 14

games_per_team <- 12

recency_half_life <- 12

true_hfa <- 2.7

noise_sd <- 8.5

hfa_prior <- 2.5

lambda_parity_threshold <- 4.5

adaptive_start_games <- 4

true_sd_values <- c(
  2,
  3,
  4,
  5,
  6,
  7,
  8
)

n_parity_calibration_per_sd <- 300

n_evaluation_per_sd <- 500

validate_each_generated_schedule <- FALSE


# ============================================================
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

cat("\n========================================\n")
cat("VERSION 8B\n")
cat("========================================\n\n")
cat("Teams:", length(teams), "\n")
cat("Games per team:", games_per_team, "\n")
cat("Calendar weeks:", n_weeks, "\n")
cat("Fixed recency half-life:", recency_half_life, "weeks\n")
cat("Parity SD grid:", paste(true_sd_values, collapse = ", "), "\n\n")


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
# 16. GENERATE STATIC TRUE RATINGS WITH EXACT SD
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
  
  ratings <- ratings - mean(ratings)
  
  ratings <- ratings / sd(ratings)
  
  ratings <- ratings * target_sd
  
  names(ratings) <- league_teams
  
  return(ratings)
}


# ============================================================
# 17. SIMULATE ONE STATIC SEASON
# ============================================================

simulate_static_season <- function(
    schedule,
    true_ratings,
    true_hfa = 2.7,
    noise_sd = 8.5
) {
  
  games <- schedule
  
  games$true_home_rating <- true_ratings[
    games$home_team
  ]
  
  games$true_away_rating <- true_ratings[
    games$away_team
  ]
  
  games$true_expected_margin <- (
    games$true_home_rating -
      games$true_away_rating +
      true_hfa
  )
  
  games$actual_margin <- round(
    games$true_expected_margin +
      rnorm(
        nrow(games),
        mean = 0,
        sd = noise_sd
      )
  )
  
  tie_rows <- which(
    games$actual_margin == 0
  )
  
  if (length(tie_rows) > 0) {
    
    games$actual_margin[tie_rows] <- sample(
      c(-1, 1),
      size = length(tie_rows),
      replace = TRUE
    )
  }
  
  return(games)
}


# ============================================================
# 18. TRUE GAME-WEIGHTED STRENGTH OF SCHEDULE
# ============================================================

calculate_sos_from_ratings <- function(
    schedule,
    ratings,
    league_teams
) {
  
  sos <- numeric(
    length(league_teams)
  )
  
  names(sos) <- league_teams
  
  for (current_team in league_teams) {
    
    team_games <- schedule[
      schedule$home_team == current_team |
        schedule$away_team == current_team,
    ]
    
    opponents <- ifelse(
      team_games$home_team == current_team,
      team_games$away_team,
      team_games$home_team
    )
    
    sos[current_team] <- mean(
      ratings[
        opponents
      ]
    )
  }
  
  return(sos)
}


# ============================================================
# 19. HFA SHRINKAGE
# ============================================================

get_hfa_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <- (
    2 * nrow(data)
  ) / length(league_teams)
  
  if (avg_games_played <= 2) {
    
    return(10)
    
  } else if (avg_games_played <= 4) {
    
    return(8)
    
  } else if (avg_games_played <= 6) {
    
    return(5)
    
  } else {
    
    return(2)
  }
}


# ============================================================
# 20. LOW-PARITY TEAM SHRINKAGE
# ============================================================

get_dynamic_team_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <- (
    2 * nrow(data)
  ) / length(league_teams)
  
  if (avg_games_played <= 2) {
    
    return(5)
    
  } else if (avg_games_played <= 4) {
    
    return(4)
    
  } else if (avg_games_played <= 6) {
    
    return(3)
    
  } else {
    
    return(2)
  }
}


choose_team_lambda <- function(
    parity_sd,
    training_games,
    league_teams
) {
  
  if (parity_sd < lambda_parity_threshold) {
    
    return(
      get_dynamic_team_lambda(
        data = training_games,
        league_teams = league_teams
      )
    )
    
  } else {
    
    return(2)
  }
}


# ============================================================
# 21. SMOOTH PARITY-ADAPTIVE MARGIN CAP
# ============================================================

choose_cap_smooth <- function(
    parity_sd
) {
  
  cap <- 14 +
    5.25 * (
      parity_sd - 4
    )
  
  cap <- max(
    14,
    cap
  )
  
  cap <- min(
    35,
    cap
  )
  
  return(cap)
}


# ============================================================
# 22. HL12 RECENCY WEIGHTS
# ============================================================

make_recency_weights <- function(
    training_games,
    half_life = 12
) {
  
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
  
  return(normalized_weights)
}


# ============================================================
# 23. APPLY MARGIN CAP
# ============================================================

cap_margin <- function(
    margin,
    margin_cap
) {
  
  if (is.infinite(margin_cap)) {
    
    return(margin)
  }
  
  return(
    pmax(
      pmin(
        margin,
        margin_cap
      ),
      -margin_cap
    )
  )
}


# ============================================================
# 24. OPPONENT-ADJUSTED RIDGE MODEL
# ============================================================

fit_opponent_adjusted_ratings <- function(
    data,
    league_teams,
    margin_cap,
    team_lambda,
    hfa_lambda,
    hfa_prior = 2.5,
    game_weights = NULL
) {
  
  n_games <- nrow(data)
  
  n_teams <- length(league_teams)
  
  y <- cap_margin(
    data$actual_margin,
    margin_cap
  )
  
  X <- matrix(
    0,
    nrow = n_games,
    ncol = n_teams + 1
  )
  
  colnames(X) <- c(
    "HFA",
    league_teams
  )
  
  X[, "HFA"] <- 1
  
  for (i in seq_len(n_games)) {
    
    home <- data$home_team[i]
    
    away <- data$away_team[i]
    
    X[i, home] <- 1
    
    X[i, away] <- -1
  }
  
  if (is.null(game_weights)) {
    
    game_weights <- rep(
      1,
      n_games
    )
  }
  
  sqrt_weights <- sqrt(
    game_weights
  )
  
  X_weighted <- sweep(
    X,
    MARGIN = 1,
    STATS = sqrt_weights,
    FUN = "*"
  )
  
  y_weighted <- y * sqrt_weights
  
  penalty <- diag(
    c(
      hfa_lambda,
      rep(
        team_lambda,
        n_teams
      )
    )
  )
  
  prior_target <- c(
    hfa_prior,
    rep(
      0,
      n_teams
    )
  )
  
  beta_matrix <- solve(
    crossprod(X_weighted) +
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
  
  estimated_hfa <- beta["HFA"]
  
  ratings <- beta[
    league_teams
  ]
  
  ratings <- ratings -
    mean(ratings)
  
  names(ratings) <- league_teams
  
  return(
    list(
      ratings = ratings,
      home_field = as.numeric(estimated_hfa)
    )
  )
}


# ============================================================
# 25. NAIVE POINT-DIFFERENTIAL MODEL
# ============================================================
#
# This model gets the SAME:
#
#   - HL12 game weights
#   - margin cap
#   - HFA prior
#   - HFA lambda
#   - team shrinkage lambda
#
# as the opponent-adjusted model.
#
# The difference is that it does NOT account for who the
# opponent was.
#
# Each team's rating is its weighted, location-adjusted
# point differential shrunk toward zero.
# ============================================================

fit_naive_pd_ratings <- function(
    data,
    league_teams,
    margin_cap,
    team_lambda,
    hfa_lambda,
    hfa_prior = 2.5,
    game_weights = NULL
) {
  
  n_games <- nrow(data)
  
  if (is.null(game_weights)) {
    
    game_weights <- rep(
      1,
      n_games
    )
  }
  
  y <- cap_margin(
    data$actual_margin,
    margin_cap
  )
  
  estimated_hfa <- (
    sum(
      game_weights * y
    ) +
      hfa_lambda * hfa_prior
  ) / (
    sum(game_weights) +
      hfa_lambda
  )
  
  numerator <- numeric(
    length(league_teams)
  )
  
  denominator <- numeric(
    length(league_teams)
  )
  
  names(numerator) <- league_teams
  
  names(denominator) <- league_teams
  
  for (i in seq_len(n_games)) {
    
    home <- data$home_team[i]
    
    away <- data$away_team[i]
    
    home_performance <- (
      y[i] -
        estimated_hfa
    )
    
    away_performance <- (
      -y[i] +
        estimated_hfa
    )
    
    numerator[home] <- (
      numerator[home] +
        game_weights[i] *
        home_performance
    )
    
    numerator[away] <- (
      numerator[away] +
        game_weights[i] *
        away_performance
    )
    
    denominator[home] <- (
      denominator[home] +
        game_weights[i]
    )
    
    denominator[away] <- (
      denominator[away] +
        game_weights[i]
    )
  }
  
  ratings <- numerator / (
    denominator +
      team_lambda
  )
  
  ratings <- ratings -
    mean(ratings)
  
  names(ratings) <- league_teams
  
  return(
    list(
      ratings = ratings,
      home_field = as.numeric(estimated_hfa)
    )
  )
}


# ============================================================
# 26. RAW UNWEIGHTED PARITY FIT
# ============================================================

fit_raw_parity_model <- function(
    data,
    league_teams
) {
  
  current_hfa_lambda <- get_hfa_lambda(
    data = data,
    league_teams = league_teams
  )
  
  model <- fit_opponent_adjusted_ratings(
    data = data,
    league_teams = league_teams,
    margin_cap = Inf,
    team_lambda = 2,
    hfa_lambda = current_hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = rep(
      1,
      nrow(data)
    )
  )
  
  return(model)
}


# ============================================================
# 27. BUILD 16-TEAM PARITY CALIBRATION SAMPLE
# ============================================================
#
# Calibration is indexed by COMPLETED GAMES PER TEAM rather
# than calendar week because 8A contains league-wide byes.
#
# Every active round gives every team one game.
# ============================================================

cat("\n========================================\n")
cat("BUILDING VERSION 8B PARITY CALIBRATION\n")
cat("========================================\n\n")

set.seed(8201)

parity_calibration_rows <- list()

parity_index <- 1

for (current_true_sd in true_sd_values) {
  
  cat(
    "Calibration true SD:",
    current_true_sd,
    "\n"
  )
  
  for (simulation in 1:n_parity_calibration_per_sd) {
    
    schedule_result <- generate_unbalanced_schedule(
      team_info = team_info,
      n_weeks = n_weeks
    )
    
    schedule <- schedule_result$schedule
    
    if (validate_each_generated_schedule) {
      
      validate_schedule(
        schedule = schedule,
        team_info = team_info,
        n_weeks = n_weeks
      )
    }
    
    current_true_ratings <- generate_true_ratings(
      league_teams = teams,
      target_sd = current_true_sd
    )
    
    games <- simulate_static_season(
      schedule = schedule,
      true_ratings = current_true_ratings,
      true_hfa = true_hfa,
      noise_sd = noise_sd
    )
    
    active_weeks <- sort(
      unique(
        games$week
      )
    )
    
    for (completed_games in 2:games_per_team) {
      
      completed_weeks <- active_weeks[
        1:completed_games
      ]
      
      training_games <- games[
        games$week %in%
          completed_weeks,
      ]
      
      raw_model <- fit_raw_parity_model(
        data = training_games,
        league_teams = teams
      )
      
      observed_rating_sd <- sd(
        raw_model$ratings
      )
      
      parity_calibration_rows[[parity_index]] <- data.frame(
        true_rating_SD = current_true_sd,
        completed_games_per_team = completed_games,
        observed_rating_SD = observed_rating_sd
      )
      
      parity_index <- parity_index + 1
    }
  }
}

parity_calibration_data <- do.call(
  rbind,
  parity_calibration_rows
)

rownames(parity_calibration_data) <- NULL


# ============================================================
# 28. FIT PARITY CALIBRATION MODELS
# ============================================================

parity_calibration_coefficients <- data.frame(
  completed_games_per_team = 2:games_per_team,
  intercept = NA_real_,
  slope = NA_real_,
  r_squared = NA_real_,
  calibration_RMSE = NA_real_
)

for (i in seq_len(nrow(parity_calibration_coefficients))) {
  
  current_games <- parity_calibration_coefficients$completed_games_per_team[i]
  
  current_data <- parity_calibration_data[
    parity_calibration_data$completed_games_per_team ==
      current_games,
  ]
  
  calibration_model <- lm(
    true_rating_SD ~
      observed_rating_SD,
    data = current_data
  )
  
  predictions <- predict(
    calibration_model,
    newdata = current_data
  )
  
  parity_calibration_coefficients$intercept[i] <- coef(
    calibration_model
  )[1]
  
  parity_calibration_coefficients$slope[i] <- coef(
    calibration_model
  )[2]
  
  parity_calibration_coefficients$r_squared[i] <- summary(
    calibration_model
  )$r.squared
  
  parity_calibration_coefficients$calibration_RMSE[i] <- sqrt(
    mean(
      (
        current_data$true_rating_SD -
          predictions
      )^2
    )
  )
}

cat("\nPARITY CALIBRATION COEFFICIENTS:\n\n")

print(
  parity_calibration_coefficients
)


# ============================================================
# 29. APPLY PARITY CALIBRATION
# ============================================================

calibrate_parity_sd <- function(
    observed_sd,
    completed_games_per_team,
    coefficients
) {
  
  current_row <- coefficients[
    coefficients$completed_games_per_team ==
      completed_games_per_team,
  ]
  
  if (nrow(current_row) != 1) {
    
    stop(
      "Could not find unique parity calibration row."
    )
  }
  
  estimate <- (
    current_row$intercept +
      current_row$slope *
      observed_sd
  )
  
  estimate <- max(
    0,
    estimate
  )
  
  return(
    as.numeric(
      estimate
    )
  )
}


# ============================================================
# 30. GET CURRENT MODEL SETTINGS
# ============================================================

get_current_model_settings <- function(
    training_games,
    league_teams,
    parity_coefficients
) {
  
  completed_games_per_team <- round(
    (
      2 * nrow(training_games)
    ) /
      length(league_teams)
  )
  
  current_hfa_lambda <- get_hfa_lambda(
    data = training_games,
    league_teams = league_teams
  )
  
  if (
    completed_games_per_team <
    adaptive_start_games
  ) {
    
    return(
      list(
        completed_games_per_team = completed_games_per_team,
        estimated_parity_SD = NA_real_,
        margin_cap = 21,
        team_lambda = 2,
        hfa_lambda = current_hfa_lambda
      )
    )
  }
  
  raw_model <- fit_raw_parity_model(
    data = training_games,
    league_teams = league_teams
  )
  
  observed_sd <- sd(
    raw_model$ratings
  )
  
  estimated_parity <- calibrate_parity_sd(
    observed_sd = observed_sd,
    completed_games_per_team = completed_games_per_team,
    coefficients = parity_coefficients
  )
  
  current_cap <- choose_cap_smooth(
    estimated_parity
  )
  
  current_team_lambda <- choose_team_lambda(
    parity_sd = estimated_parity,
    training_games = training_games,
    league_teams = league_teams
  )
  
  return(
    list(
      completed_games_per_team = completed_games_per_team,
      estimated_parity_SD = estimated_parity,
      margin_cap = current_cap,
      team_lambda = current_team_lambda,
      hfa_lambda = current_hfa_lambda
    )
  )
}


# ============================================================
# 31. SAFE CORRELATION
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
# 32. PREDICT GAMES FROM RATING MODEL
# ============================================================

predict_games <- function(
    model,
    test_games
) {
  
  predictions <- (
    model$ratings[
      test_games$home_team
    ] -
      model$ratings[
        test_games$away_team
      ] +
      model$home_field
  )
  
  return(
    as.numeric(
      predictions
    )
  )
}


# ============================================================
# 33. RATING RECOVERY METRICS
# ============================================================

calculate_rating_metrics <- function(
    estimated_ratings,
    true_ratings
) {
  
  differences <- (
    estimated_ratings -
      true_ratings
  )
  
  return(
    list(
      RMSE = sqrt(
        mean(
          differences^2
        )
      ),
      
      MAE = mean(
        abs(
          differences
        )
      ),
      
      Pearson = safe_cor(
        estimated_ratings,
        true_ratings,
        method = "pearson"
      ),
      
      Spearman = safe_cor(
        estimated_ratings,
        true_ratings,
        method = "spearman"
      )
    )
  )
}


# ============================================================
# 34. PREDICTION METRICS
# ============================================================

calculate_prediction_metrics <- function(
    actual,
    predicted
) {
  
  error <- (
    actual -
      predicted
  )
  
  correct <- (
    actual > 0
  ) == (
    predicted > 0
  )
  
  return(
    list(
      MAE = mean(
        abs(error)
      ),
      
      RMSE = sqrt(
        mean(
          error^2
        )
      ),
      
      Accuracy = mean(correct),
      
      Correlation = safe_cor(
        predicted,
        actual
      ),
      
      Bias = mean(error)
    )
  )
}


# ============================================================
# 35. SOS-BIAS DIAGNOSTICS
# ============================================================
#
# rating_error = estimated rating - true rating
#
# If a naive model underrates difficult schedules, then
# the rating-error-vs-SOS slope should be NEGATIVE.
#
# An ideal opponent-adjusted model should push this slope
# and correlation much closer to zero.
# ============================================================

calculate_sos_bias <- function(
    estimated_ratings,
    true_ratings,
    true_sos
) {
  
  rating_error <- (
    estimated_ratings -
      true_ratings
  )
  
  bias_model <- lm(
    rating_error ~
      true_sos
  )
  
  return(
    list(
      slope = as.numeric(
        coef(bias_model)[2]
      ),
      
      correlation = safe_cor(
        rating_error,
        true_sos
      )
    )
  )
}


# ============================================================
# 36. WALK-FORWARD EVALUATION FOR ONE SEASON
# ============================================================

walk_forward_one_season <- function(
    games,
    league_teams,
    parity_coefficients
) {
  
  adjusted_prediction_rows <- list()
  
  naive_prediction_rows <- list()
  
  adjusted_index <- 1
  
  naive_index <- 1
  
  for (target_week in 1:n_weeks) {
    
    test_games <- games[
      games$week == target_week,
    ]
    
    if (nrow(test_games) == 0) {
      next
    }
    
    training_games <- games[
      games$week < target_week,
    ]
    
    if (nrow(training_games) == 0) {
      next
    }
    
    settings <- get_current_model_settings(
      training_games = training_games,
      league_teams = league_teams,
      parity_coefficients = parity_coefficients
    )
    
    game_weights <- make_recency_weights(
      training_games = training_games,
      half_life = recency_half_life
    )
    
    adjusted_model <- fit_opponent_adjusted_ratings(
      data = training_games,
      league_teams = league_teams,
      margin_cap = settings$margin_cap,
      team_lambda = settings$team_lambda,
      hfa_lambda = settings$hfa_lambda,
      hfa_prior = hfa_prior,
      game_weights = game_weights
    )
    
    naive_model <- fit_naive_pd_ratings(
      data = training_games,
      league_teams = league_teams,
      margin_cap = settings$margin_cap,
      team_lambda = settings$team_lambda,
      hfa_lambda = settings$hfa_lambda,
      hfa_prior = hfa_prior,
      game_weights = game_weights
    )
    
    adjusted_predictions <- predict_games(
      model = adjusted_model,
      test_games = test_games
    )
    
    naive_predictions <- predict_games(
      model = naive_model,
      test_games = test_games
    )
    
    adjusted_prediction_rows[[adjusted_index]] <- data.frame(
      target_week = target_week,
      actual_margin = test_games$actual_margin,
      predicted_margin = adjusted_predictions
    )
    
    naive_prediction_rows[[naive_index]] <- data.frame(
      target_week = target_week,
      actual_margin = test_games$actual_margin,
      predicted_margin = naive_predictions
    )
    
    adjusted_index <- adjusted_index + 1
    
    naive_index <- naive_index + 1
  }
  
  adjusted_predictions <- do.call(
    rbind,
    adjusted_prediction_rows
  )
  
  naive_predictions <- do.call(
    rbind,
    naive_prediction_rows
  )
  
  adjusted_metrics <- calculate_prediction_metrics(
    actual = adjusted_predictions$actual_margin,
    predicted = adjusted_predictions$predicted_margin
  )
  
  naive_metrics <- calculate_prediction_metrics(
    actual = naive_predictions$actual_margin,
    predicted = naive_predictions$predicted_margin
  )
  
  return(
    list(
      adjusted = adjusted_metrics,
      naive = naive_metrics,
      games_predicted = nrow(adjusted_predictions)
    )
  )
}


# ============================================================
# 37. FINAL EVALUATION
# ============================================================

cat("\n\n========================================\n")
cat("STARTING VERSION 8B HOLDOUT EVALUATION\n")
cat("========================================\n\n")

set.seed(8202)

evaluation_rows <- list()

evaluation_index <- 1

example_team_diagnostics <- NULL

for (current_true_sd in true_sd_values) {
  
  cat(
    "\nEvaluation true SD:",
    current_true_sd,
    "\n"
  )
  
  for (simulation in 1:n_evaluation_per_sd) {
    
    schedule_result <- generate_unbalanced_schedule(
      team_info = team_info,
      n_weeks = n_weeks
    )
    
    schedule <- schedule_result$schedule
    
    if (validate_each_generated_schedule) {
      
      validate_schedule(
        schedule = schedule,
        team_info = team_info,
        n_weeks = n_weeks
      )
    }
    
    current_true_ratings <- generate_true_ratings(
      league_teams = teams,
      target_sd = current_true_sd
    )
    
    games <- simulate_static_season(
      schedule = schedule,
      true_ratings = current_true_ratings,
      true_hfa = true_hfa,
      noise_sd = noise_sd
    )
    
    true_sos <- calculate_sos_from_ratings(
      schedule = schedule,
      ratings = current_true_ratings,
      league_teams = teams
    )
    
    true_sos_sd <- sd(
      true_sos
    )
    
    true_sos_range <- (
      max(true_sos) -
        min(true_sos)
    )
    
    # --------------------------------------------------------
    # FULL-SEASON RATINGS
    # --------------------------------------------------------
    
    full_settings <- get_current_model_settings(
      training_games = games,
      league_teams = teams,
      parity_coefficients = parity_calibration_coefficients
    )
    
    full_weights <- make_recency_weights(
      training_games = games,
      half_life = recency_half_life
    )
    
    adjusted_full <- fit_opponent_adjusted_ratings(
      data = games,
      league_teams = teams,
      margin_cap = full_settings$margin_cap,
      team_lambda = full_settings$team_lambda,
      hfa_lambda = full_settings$hfa_lambda,
      hfa_prior = hfa_prior,
      game_weights = full_weights
    )
    
    naive_full <- fit_naive_pd_ratings(
      data = games,
      league_teams = teams,
      margin_cap = full_settings$margin_cap,
      team_lambda = full_settings$team_lambda,
      hfa_lambda = full_settings$hfa_lambda,
      hfa_prior = hfa_prior,
      game_weights = full_weights
    )
    
    adjusted_rating_metrics <- calculate_rating_metrics(
      estimated_ratings = adjusted_full$ratings,
      true_ratings = current_true_ratings
    )
    
    naive_rating_metrics <- calculate_rating_metrics(
      estimated_ratings = naive_full$ratings,
      true_ratings = current_true_ratings
    )
    
    adjusted_sos_bias <- calculate_sos_bias(
      estimated_ratings = adjusted_full$ratings,
      true_ratings = current_true_ratings,
      true_sos = true_sos
    )
    
    naive_sos_bias <- calculate_sos_bias(
      estimated_ratings = naive_full$ratings,
      true_ratings = current_true_ratings,
      true_sos = true_sos
    )
    
    estimated_sos <- calculate_sos_from_ratings(
      schedule = schedule,
      ratings = adjusted_full$ratings,
      league_teams = teams
    )
    
    estimated_sos_error <- (
      estimated_sos -
        true_sos
    )
    
    estimated_sos_rmse <- sqrt(
      mean(
        estimated_sos_error^2
      )
    )
    
    estimated_sos_mae <- mean(
      abs(
        estimated_sos_error
      )
    )
    
    estimated_sos_correlation <- safe_cor(
      estimated_sos,
      true_sos
    )
    
    # --------------------------------------------------------
    # WALK-FORWARD GAME PREDICTION
    # --------------------------------------------------------
    
    walk_forward <- walk_forward_one_season(
      games = games,
      league_teams = teams,
      parity_coefficients = parity_calibration_coefficients
    )
    
    evaluation_rows[[evaluation_index]] <- data.frame(
      true_rating_SD = current_true_sd,
      simulation = simulation,
      
      true_SOS_SD = true_sos_sd,
      true_SOS_range = true_sos_range,
      
      adjusted_rating_RMSE = adjusted_rating_metrics$RMSE,
      naive_rating_RMSE = naive_rating_metrics$RMSE,
      rating_RMSE_improvement = (
        naive_rating_metrics$RMSE -
          adjusted_rating_metrics$RMSE
      ),
      
      adjusted_rating_MAE = adjusted_rating_metrics$MAE,
      naive_rating_MAE = naive_rating_metrics$MAE,
      rating_MAE_improvement = (
        naive_rating_metrics$MAE -
          adjusted_rating_metrics$MAE
      ),
      
      adjusted_rating_Pearson = adjusted_rating_metrics$Pearson,
      naive_rating_Pearson = naive_rating_metrics$Pearson,
      
      adjusted_rating_Spearman = adjusted_rating_metrics$Spearman,
      naive_rating_Spearman = naive_rating_metrics$Spearman,
      
      adjusted_SOS_error_slope = adjusted_sos_bias$slope,
      naive_SOS_error_slope = naive_sos_bias$slope,
      
      adjusted_SOS_error_correlation = adjusted_sos_bias$correlation,
      naive_SOS_error_correlation = naive_sos_bias$correlation,
      
      estimated_SOS_RMSE = estimated_sos_rmse,
      estimated_SOS_MAE = estimated_sos_mae,
      estimated_SOS_correlation = estimated_sos_correlation,
      
      adjusted_HFA_error = (
        adjusted_full$home_field -
          true_hfa
      ),
      
      naive_HFA_error = (
        naive_full$home_field -
          true_hfa
      ),
      
      adjusted_OOS_MAE = walk_forward$adjusted$MAE,
      naive_OOS_MAE = walk_forward$naive$MAE,
      OOS_MAE_improvement = (
        walk_forward$naive$MAE -
          walk_forward$adjusted$MAE
      ),
      
      adjusted_OOS_RMSE = walk_forward$adjusted$RMSE,
      naive_OOS_RMSE = walk_forward$naive$RMSE,
      OOS_RMSE_improvement = (
        walk_forward$naive$RMSE -
          walk_forward$adjusted$RMSE
      ),
      
      adjusted_OOS_Accuracy = walk_forward$adjusted$Accuracy,
      naive_OOS_Accuracy = walk_forward$naive$Accuracy,
      
      adjusted_OOS_Correlation = walk_forward$adjusted$Correlation,
      naive_OOS_Correlation = walk_forward$naive$Correlation,
      
      games_predicted = walk_forward$games_predicted,
      
      estimated_full_parity_SD = full_settings$estimated_parity_SD,
      full_margin_cap = full_settings$margin_cap,
      full_team_lambda = full_settings$team_lambda,
      
      stringsAsFactors = FALSE
    )
    
    if (
      is.null(example_team_diagnostics) &&
      current_true_sd == 5
    ) {
      
      example_team_diagnostics <- data.frame(
        team = teams,
        division = team_info$division[
          match(
            teams,
            team_info$team
          )
        ],
        true_rating = as.numeric(
          current_true_ratings[
            teams
          ]
        ),
        true_SOS = as.numeric(
          true_sos[
            teams
          ]
        ),
        naive_rating = as.numeric(
          naive_full$ratings[
            teams
          ]
        ),
        adjusted_rating = as.numeric(
          adjusted_full$ratings[
            teams
          ]
        ),
        estimated_SOS = as.numeric(
          estimated_sos[
            teams
          ]
        ),
        stringsAsFactors = FALSE
      )
      
      example_team_diagnostics$naive_error <- (
        example_team_diagnostics$naive_rating -
          example_team_diagnostics$true_rating
      )
      
      example_team_diagnostics$adjusted_error <- (
        example_team_diagnostics$adjusted_rating -
          example_team_diagnostics$true_rating
      )
      
      example_team_diagnostics <- example_team_diagnostics[
        order(
          -example_team_diagnostics$true_SOS
        ),
      ]
      
      rownames(example_team_diagnostics) <- NULL
    }
    
    evaluation_index <- evaluation_index + 1
    
    if (simulation %% 100 == 0) {
      
      cat(
        "Completed",
        simulation,
        "of",
        n_evaluation_per_sd,
        "\n"
      )
    }
  }
}

results <- do.call(
  rbind,
  evaluation_rows
)

rownames(results) <- NULL


# ============================================================
# 38. OVERALL MODEL COMPARISON
# ============================================================

model_summary <- data.frame(
  model = c(
    "Opponent Adjusted",
    "Naive Point Differential"
  ),
  
  Rating_RMSE = c(
    mean(results$adjusted_rating_RMSE),
    mean(results$naive_rating_RMSE)
  ),
  
  Rating_MAE = c(
    mean(results$adjusted_rating_MAE),
    mean(results$naive_rating_MAE)
  ),
  
  Rating_Pearson = c(
    mean(results$adjusted_rating_Pearson),
    mean(results$naive_rating_Pearson)
  ),
  
  Rating_Spearman = c(
    mean(results$adjusted_rating_Spearman),
    mean(results$naive_rating_Spearman)
  ),
  
  OOS_MAE = c(
    mean(results$adjusted_OOS_MAE),
    mean(results$naive_OOS_MAE)
  ),
  
  OOS_RMSE = c(
    mean(results$adjusted_OOS_RMSE),
    mean(results$naive_OOS_RMSE)
  ),
  
  OOS_Accuracy = c(
    mean(results$adjusted_OOS_Accuracy),
    mean(results$naive_OOS_Accuracy)
  ),
  
  OOS_Correlation = c(
    mean(results$adjusted_OOS_Correlation),
    mean(results$naive_OOS_Correlation)
  ),
  
  SOS_Error_Slope = c(
    mean(results$adjusted_SOS_error_slope),
    mean(results$naive_SOS_error_slope)
  ),
  
  SOS_Error_Correlation = c(
    mean(results$adjusted_SOS_error_correlation),
    mean(results$naive_SOS_error_correlation)
  )
)

cat("\n\n========================================\n")
cat("OVERALL VERSION 8B MODEL COMPARISON\n")
cat("========================================\n\n")

print(model_summary)


# ============================================================
# 39. RESULTS BY TRUE PARITY SD
# ============================================================

summary_by_true_sd <- aggregate(
  cbind(
    adjusted_rating_RMSE,
    naive_rating_RMSE,
    rating_RMSE_improvement,
    adjusted_rating_MAE,
    naive_rating_MAE,
    adjusted_OOS_MAE,
    naive_OOS_MAE,
    OOS_MAE_improvement,
    adjusted_OOS_RMSE,
    naive_OOS_RMSE,
    adjusted_OOS_Accuracy,
    naive_OOS_Accuracy,
    adjusted_SOS_error_slope,
    naive_SOS_error_slope,
    estimated_SOS_RMSE,
    estimated_SOS_correlation,
    true_SOS_SD,
    true_SOS_range
  ) ~
    true_rating_SD,
  data = results,
  FUN = mean
)

cat("\n========================================\n")
cat("RESULTS BY TRUE RATING SD\n")
cat("========================================\n\n")

print(summary_by_true_sd)


# ============================================================
# 40. PAIRED IMPROVEMENT SUMMARY
# ============================================================

paired_metric_summary <- function(
    difference,
    metric_name
) {
  
  current_mean <- mean(difference)
  
  current_se <- sd(difference) /
    sqrt(
      length(difference)
    )
  
  return(
    data.frame(
      metric = metric_name,
      mean_improvement = current_mean,
      MC_SE = current_se,
      lower_95 = (
        current_mean -
          1.96 * current_se
      ),
      upper_95 = (
        current_mean +
          1.96 * current_se
      ),
      adjusted_better_percent = (
        100 *
          mean(
            difference > 0
          )
      ),
      stringsAsFactors = FALSE
    )
  )
}

paired_summary <- rbind(
  paired_metric_summary(
    results$rating_RMSE_improvement,
    "Hidden Rating RMSE"
  ),
  
  paired_metric_summary(
    results$rating_MAE_improvement,
    "Hidden Rating MAE"
  ),
  
  paired_metric_summary(
    results$OOS_MAE_improvement,
    "Walk-Forward MAE"
  ),
  
  paired_metric_summary(
    results$OOS_RMSE_improvement,
    "Walk-Forward RMSE"
  )
)

cat("\n========================================\n")
cat("PAIRED OPPONENT-ADJUSTMENT IMPROVEMENT\n")
cat("========================================\n\n")

print(paired_summary)


# ============================================================
# 41. SOS-BIAS SUMMARY
# ============================================================

sos_bias_summary <- data.frame(
  model = c(
    "Opponent Adjusted",
    "Naive Point Differential"
  ),
  
  mean_error_vs_SOS_slope = c(
    mean(results$adjusted_SOS_error_slope),
    mean(results$naive_SOS_error_slope)
  ),
  
  mean_error_vs_SOS_correlation = c(
    mean(results$adjusted_SOS_error_correlation),
    mean(results$naive_SOS_error_correlation)
  ),
  
  mean_absolute_slope = c(
    mean(abs(results$adjusted_SOS_error_slope)),
    mean(abs(results$naive_SOS_error_slope))
  ),
  
  mean_absolute_correlation = c(
    mean(abs(results$adjusted_SOS_error_correlation)),
    mean(abs(results$naive_SOS_error_correlation))
  )
)

cat("\n========================================\n")
cat("RATING ERROR VS TRUE SOS\n")
cat("========================================\n\n")

print(sos_bias_summary)


# ============================================================
# 42. ESTIMATED SOS RECOVERY
# ============================================================

estimated_sos_summary <- data.frame(
  RMSE = mean(
    results$estimated_SOS_RMSE
  ),
  
  MAE = mean(
    results$estimated_SOS_MAE
  ),
  
  Correlation = mean(
    results$estimated_SOS_correlation
  )
)

cat("\n========================================\n")
cat("ESTIMATED SOS RECOVERY\n")
cat("========================================\n\n")

print(estimated_sos_summary)


# ============================================================
# 43. SCHEDULE-IMBALANCE QUARTILES
# ============================================================
#
# This is descriptive only.
# It does NOT affect fitting or predictions.
#
# IMPORTANT:
# Quartiles are calculated WITHIN each true-rating SD level.
# That prevents high-parity-spread leagues from automatically
# being labeled more schedule-imbalanced simply because team
# ratings themselves have a larger scale.
#
# Q1 = most balanced realized schedules within that SD level
# Q4 = most uneven realized schedules within that SD level
# ============================================================

results$normalized_SOS_SD <- (
  results$true_SOS_SD /
    results$true_rating_SD
)

results$normalized_SOS_range <- (
  results$true_SOS_range /
    results$true_rating_SD
)

results$SOS_imbalance_quartile_number <- NA_integer_

for (current_true_sd in true_sd_values) {
  
  current_rows <- which(
    results$true_rating_SD ==
      current_true_sd
  )
  
  current_rank <- rank(
    results$true_SOS_SD[current_rows],
    ties.method = "first"
  )
  
  results$SOS_imbalance_quartile_number[current_rows] <- ceiling(
    4 *
      current_rank /
      length(current_rows)
  )
}

results$SOS_imbalance_quartile <- factor(
  results$SOS_imbalance_quartile_number,
  levels = 1:4,
  labels = c(
    "Q1 Most Balanced",
    "Q2",
    "Q3",
    "Q4 Most Uneven"
  )
)

imbalance_summary <- aggregate(
  cbind(
    true_SOS_SD,
    true_SOS_range,
    normalized_SOS_SD,
    normalized_SOS_range,
    rating_RMSE_improvement,
    rating_MAE_improvement,
    OOS_MAE_improvement,
    OOS_RMSE_improvement
  ) ~
    SOS_imbalance_quartile,
  data = results,
  FUN = mean
)

cat("\n========================================\n")
cat("OPPONENT-ADJUSTMENT GAIN BY SOS IMBALANCE\n")
cat("========================================\n\n")

print(imbalance_summary)


# ============================================================
# 44. IMPROVEMENT VS SOS IMBALANCE REGRESSIONS
# ============================================================

rating_improvement_model <- lm(
  rating_RMSE_improvement ~
    true_SOS_SD +
    factor(true_rating_SD),
  data = results
)

oos_improvement_model <- lm(
  OOS_MAE_improvement ~
    true_SOS_SD +
    factor(true_rating_SD),
  data = results
)

imbalance_regression_summary <- data.frame(
  outcome = c(
    "Rating RMSE Improvement",
    "OOS MAE Improvement"
  ),
  
  intercept = c(
    coef(rating_improvement_model)[1],
    coef(oos_improvement_model)[1]
  ),
  
  SOS_SD_slope = c(
    coef(rating_improvement_model)[2],
    coef(oos_improvement_model)[2]
  ),
  
  R_squared = c(
    summary(rating_improvement_model)$r.squared,
    summary(oos_improvement_model)$r.squared
  )
)

cat("\n========================================\n")
cat("DOES OPPONENT ADJUSTMENT HELP MORE WHEN SOS IS UNEVEN?\n")
cat("========================================\n\n")

print(imbalance_regression_summary)


# ============================================================
# 45. EXAMPLE TEAM-LEVEL DIAGNOSTICS
# ============================================================

cat("\n========================================\n")
cat("EXAMPLE TRUE SD = 5 SEASON\n")
cat("========================================\n\n")

print(example_team_diagnostics)


# ============================================================
# 46. PLOT - RATING RMSE BY TRUE SD
# ============================================================

rating_plot_data <- rbind(
  data.frame(
    true_rating_SD = summary_by_true_sd$true_rating_SD,
    model = "Opponent Adjusted",
    RMSE = summary_by_true_sd$adjusted_rating_RMSE
  ),
  
  data.frame(
    true_rating_SD = summary_by_true_sd$true_rating_SD,
    model = "Naive Point Differential",
    RMSE = summary_by_true_sd$naive_rating_RMSE
  )
)

rating_rmse_plot <- ggplot(
  rating_plot_data,
  aes(
    x = true_rating_SD,
    y = RMSE,
    group = model,
    linetype = model
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2.5
  ) +
  labs(
    title = "Version 8B: Hidden Rating Recovery",
    subtitle = "Lower RMSE is better",
    x = "True League Rating SD",
    y = "Hidden Rating RMSE",
    linetype = "Model"
  ) +
  theme_minimal()

print(rating_rmse_plot)


# ============================================================
# 47. PLOT - WALK-FORWARD MAE BY TRUE SD
# ============================================================

oos_plot_data <- rbind(
  data.frame(
    true_rating_SD = summary_by_true_sd$true_rating_SD,
    model = "Opponent Adjusted",
    MAE = summary_by_true_sd$adjusted_OOS_MAE
  ),
  
  data.frame(
    true_rating_SD = summary_by_true_sd$true_rating_SD,
    model = "Naive Point Differential",
    MAE = summary_by_true_sd$naive_OOS_MAE
  )
)

oos_mae_plot <- ggplot(
  oos_plot_data,
  aes(
    x = true_rating_SD,
    y = MAE,
    group = model,
    linetype = model
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2.5
  ) +
  labs(
    title = "Version 8B: Walk-Forward Prediction",
    subtitle = "Lower raw-margin MAE is better",
    x = "True League Rating SD",
    y = "Walk-Forward MAE",
    linetype = "Model"
  ) +
  theme_minimal()

print(oos_mae_plot)


# ============================================================
# 48. PLOT - SOS IMBALANCE VS RATING GAIN
# ============================================================

imbalance_rating_plot <- ggplot(
  results,
  aes(
    x = true_SOS_SD,
    y = rating_RMSE_improvement
  )
) +
  geom_point(
    alpha = 0.18
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Opponent Adjustment vs Schedule Imbalance",
    subtitle = "Positive values mean opponent adjustment recovered ratings better",
    x = "True SOS Standard Deviation",
    y = "Naive RMSE - Adjusted RMSE"
  ) +
  theme_minimal()

print(imbalance_rating_plot)


# ============================================================
# 49. PLOT - EXAMPLE RATING ERROR VS SOS
# ============================================================

example_error_plot_data <- rbind(
  data.frame(
    team = example_team_diagnostics$team,
    true_SOS = example_team_diagnostics$true_SOS,
    model = "Naive Point Differential",
    rating_error = example_team_diagnostics$naive_error
  ),
  
  data.frame(
    team = example_team_diagnostics$team,
    true_SOS = example_team_diagnostics$true_SOS,
    model = "Opponent Adjusted",
    rating_error = example_team_diagnostics$adjusted_error
  )
)

example_error_plot <- ggplot(
  example_error_plot_data,
  aes(
    x = true_SOS,
    y = rating_error,
    shape = model
  )
) +
  geom_point(
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Example Season: Rating Error vs True SOS",
    subtitle = "A negative naive slope means hard schedules are being underrated",
    x = "True Strength of Schedule",
    y = "Estimated Rating - True Rating",
    shape = "Model"
  ) +
  theme_minimal()

print(example_error_plot)


# ============================================================
# 50. PLOT - TRUE VS ESTIMATED SOS EXAMPLE
# ============================================================

example_sos_plot <- ggplot(
  example_team_diagnostics,
  aes(
    x = true_SOS,
    y = estimated_SOS
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Example Season: Estimated vs True SOS",
    x = "True SOS",
    y = "Estimated SOS"
  ) +
  theme_minimal()

print(example_sos_plot)


# ============================================================
# 51. SAVE OUTPUTS
# ============================================================

write.csv(
  parity_calibration_coefficients,
  "version8B_parity_calibration.csv",
  row.names = FALSE
)

write.csv(
  results,
  "version8B_all_season_results.csv",
  row.names = FALSE
)

write.csv(
  model_summary,
  "version8B_overall_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  summary_by_true_sd,
  "version8B_results_by_true_sd.csv",
  row.names = FALSE
)

write.csv(
  paired_summary,
  "version8B_paired_improvement.csv",
  row.names = FALSE
)

write.csv(
  sos_bias_summary,
  "version8B_sos_bias_summary.csv",
  row.names = FALSE
)

write.csv(
  estimated_sos_summary,
  "version8B_estimated_sos_recovery.csv",
  row.names = FALSE
)

write.csv(
  imbalance_summary,
  "version8B_gain_by_sos_imbalance.csv",
  row.names = FALSE
)

write.csv(
  imbalance_regression_summary,
  "version8B_imbalance_regressions.csv",
  row.names = FALSE
)

write.csv(
  example_team_diagnostics,
  "version8B_example_team_diagnostics.csv",
  row.names = FALSE
)


# ============================================================
# 52. FINAL SUMMARY
# ============================================================

cat("\n\n========================================\n")
cat("VERSION 8B COMPLETE\n")
cat("========================================\n\n")

cat("Models compared:\n")
cat("  1. Opponent Adjusted Ridge + HL12\n")
cat("  2. Naive Point Differential + HL12\n\n")

cat(
  "Parity calibration seasons:",
  length(true_sd_values) *
    n_parity_calibration_per_sd,
  "\n"
)

cat(
  "Independent evaluation seasons:",
  length(true_sd_values) *
    n_evaluation_per_sd,
  "\n\n"
)

cat("OVERALL MODEL COMPARISON:\n\n")
print(model_summary)

cat("\nPAIRED IMPROVEMENT:\n\n")
print(paired_summary)

cat("\nSOS BIAS:\n\n")
print(sos_bias_summary)

cat("\nESTIMATED SOS RECOVERY:\n\n")
print(estimated_sos_summary)

cat("\nGAIN BY SOS IMBALANCE:\n\n")
print(imbalance_summary)

cat("\nIMBALANCE REGRESSIONS:\n\n")
print(imbalance_regression_summary)

cat("\nINTERPRETATION GUIDE:\n\n")
cat("1. rating_RMSE_improvement > 0 favors opponent adjustment.\n")
cat("2. OOS_MAE_improvement > 0 favors opponent adjustment.\n")
cat("3. Naive SOS-error slope should be negative if hard schedules are underrated.\n")
cat("4. Adjusted SOS-error slope should be closer to zero.\n")
cat("5. Positive SOS_SD slope in the improvement regressions means opponent\n")
cat("   adjustment becomes more valuable as schedules become more unequal.\n")
cat("6. Estimated SOS correlation shows whether model-derived opponent ratings\n")
cat("   correctly identify which teams actually had difficult schedules.\n")


# ============================================================
# END VERSION 8B
# ============================================================

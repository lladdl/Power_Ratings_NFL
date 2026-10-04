# ============================================================
# FOOTBALL POWER RATINGS - VERSION 9A
# WEEKLY PUBLISHED POWER-RATING TABLE
# ============================================================
#
# PURPOSE
# -------
# Version 9A uses the validated results-based rating engine as a
# weekly publishing system rather than another model comparison.
#
# After each calendar week, it fits ratings using ONLY games that
# have already been completed through that week and publishes:
#
#   - power rank
#   - neutral-field rating in points above/below average
#   - record
#   - previous rank and rank change
#   - previous rating and rating change
#   - played SOS
#   - remaining SOS
#   - full-schedule SOS
#   - played / remaining location-adjusted difficulty
#   - estimated HFA
#   - estimated league parity
#   - current margin cap / shrinkage settings
#
# The statistical engine remains:
#
#   Margin = HFA + R_home - R_away + error
#
# with:
#
#   - Version 8 unbalanced 16-team schedule
#   - fixed HL12 recency weighting
#   - HFA prior
#   - smooth parity-adaptive margin cap
#   - parity-adaptive team shrinkage
#   - all teams equal before games are played
#
# SOS is descriptive output derived from opponent ratings. It is
# NOT added as another predictor to the rating regression.
#
# IMPORTANT TIMING CONVENTION
# ---------------------------
# A Week t published table uses results through Week t. Therefore:
#
#   played games    = week <= t
#   remaining games = week > t
#
# This differs from the pregame walk-forward convention used in
# Versions 8B/8C, where Week t predictions used weeks < t.
#
# Hidden true ratings exist only to simulate the example season.
# They are NEVER used in the published power-rating table.
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

reuse_version8C_parity_calibration <- TRUE


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
cat("VERSION 9A\n")
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
# 27. LOAD OR BUILD 16-TEAM PARITY CALIBRATION
# ============================================================
#
# Version 9A uses the SAME 16-team parity calibration developed
# in Version 8C. If the successful 8C calibration CSV exists in
# the current working directory, load it directly. Otherwise,
# rebuild the calibration from simulation.
# ============================================================

version8C_calibration_file <- "version8C_parity_calibration.csv"

if (
  reuse_version8C_parity_calibration &&
  file.exists(version8C_calibration_file)
) {
  
  cat("\n========================================\n")
  cat("LOADING VERSION 8C PARITY CALIBRATION\n")
  cat("========================================\n\n")
  
  parity_calibration_coefficients <- read.csv(
    version8C_calibration_file,
    stringsAsFactors = FALSE
  )
  
  required_calibration_columns <- c(
    "completed_games_per_team",
    "intercept",
    "slope",
    "r_squared",
    "calibration_RMSE"
  )
  
  missing_calibration_columns <- setdiff(
    required_calibration_columns,
    names(parity_calibration_coefficients)
  )
  
  if (
    length(missing_calibration_columns) > 0
  ) {
    
    stop(
      paste(
        "Saved 8C parity calibration is missing columns:",
        paste(
          missing_calibration_columns,
          collapse = ", "
        )
      )
    )
  }
  
  expected_completed_games <- 2:games_per_team
  
  if (
    !all(
      expected_completed_games %in%
      parity_calibration_coefficients$completed_games_per_team
    )
  ) {
    
    stop(
      "Saved 8C parity calibration does not contain all completed-game levels 2 through 12."
    )
  }
  
  parity_calibration_coefficients <- parity_calibration_coefficients[
    parity_calibration_coefficients$completed_games_per_team %in%
      expected_completed_games,
  ]
  
  parity_calibration_coefficients <- parity_calibration_coefficients[
    order(
      parity_calibration_coefficients$completed_games_per_team
    ),
  ]
  
  rownames(parity_calibration_coefficients) <- NULL
  
  cat(
    "Loaded:",
    version8C_calibration_file,
    "\n\n"
  )
  
  print(
    parity_calibration_coefficients
  )
  
} else {
  
  cat("\n========================================\n")
  cat("BUILDING VERSION 9A PARITY CALIBRATION\n")
  cat("========================================\n\n")
  
  set.seed(9001)
  
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
  
  
  # ==========================================================
  # 28. FIT PARITY CALIBRATION MODELS
  # ==========================================================
  
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
}


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
# 32. TEAM RECORDS THROUGH A PUBLISHED WEEK
# ============================================================

calculate_team_records <- function(
    games,
    through_week,
    league_teams
) {
  
  completed_games <- games[
    games$week <= through_week,
  ]
  
  record_rows <- list()
  
  for (i in seq_along(league_teams)) {
    
    current_team <- league_teams[i]
    
    team_games <- completed_games[
      completed_games$home_team == current_team |
        completed_games$away_team == current_team,
    ]
    
    if (nrow(team_games) == 0) {
      
      wins <- 0
      losses <- 0
      
    } else {
      
      home_wins <- (
        team_games$home_team == current_team &
          team_games$actual_margin > 0
      )
      
      away_wins <- (
        team_games$away_team == current_team &
          team_games$actual_margin < 0
      )
      
      wins <- sum(
        home_wins |
          away_wins
      )
      
      losses <- nrow(team_games) - wins
    }
    
    total_games <- wins + losses
    
    win_pct <- if (
      total_games > 0
    ) {
      
      wins / total_games
      
    } else {
      
      NA_real_
    }
    
    record_rows[[i]] <- data.frame(
      team = current_team,
      wins = wins,
      losses = losses,
      games_played = total_games,
      win_pct = win_pct,
      record = paste0(
        wins,
        "-",
        losses
      ),
      stringsAsFactors = FALSE
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
# 33. END-OF-WEEK SOS TABLE
# ============================================================
#
# Ratings are current ratings estimated through through_week.
# Future opponents are known from the schedule, but future
# results are never used.
#
# Repeated opponents count repeatedly, so SOS is game-weighted.
# ============================================================

calculate_published_sos <- function(
    schedule,
    current_ratings,
    through_week,
    league_teams,
    estimated_hfa
) {
  
  sos_rows <- list()
  
  for (i in seq_along(league_teams)) {
    
    current_team <- league_teams[i]
    
    team_schedule <- schedule[
      schedule$home_team == current_team |
        schedule$away_team == current_team,
    ]
    
    team_schedule$opponent <- ifelse(
      team_schedule$home_team == current_team,
      team_schedule$away_team,
      team_schedule$home_team
    )
    
    team_schedule$is_home <- (
      team_schedule$home_team == current_team
    )
    
    played_games <- team_schedule[
      team_schedule$week <= through_week,
    ]
    
    remaining_games <- team_schedule[
      team_schedule$week > through_week,
    ]
    
    played_sos <- if (
      nrow(played_games) > 0
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
      nrow(remaining_games) > 0
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
      nrow(played_games) > 0
    ) {
      
      played_location_values <- ifelse(
        played_games$is_home,
        current_ratings[
          played_games$opponent
        ] - estimated_hfa,
        current_ratings[
          played_games$opponent
        ] + estimated_hfa
      )
      
      mean(
        played_location_values
      )
      
    } else {
      
      NA_real_
    }
    
    remaining_location_difficulty <- if (
      nrow(remaining_games) > 0
    ) {
      
      remaining_location_values <- ifelse(
        remaining_games$is_home,
        current_ratings[
          remaining_games$opponent
        ] - estimated_hfa,
        current_ratings[
          remaining_games$opponent
        ] + estimated_hfa
      )
      
      mean(
        remaining_location_values
      )
      
    } else {
      
      NA_real_
    }
    
    sos_rows[[i]] <- data.frame(
      team = current_team,
      played_SOS = played_sos,
      remaining_SOS = remaining_sos,
      full_SOS = full_sos,
      played_location_difficulty = played_location_difficulty,
      remaining_location_difficulty = remaining_location_difficulty,
      stringsAsFactors = FALSE
    )
  }
  
  sos_table <- do.call(
    rbind,
    sos_rows
  )
  
  rownames(sos_table) <- NULL
  
  return(sos_table)
}


# ============================================================
# 34. PUBLICATION STAGE LABEL
# ============================================================

get_publication_stage <- function(
    completed_games_per_team
) {
  
  if (
    completed_games_per_team <= 2
  ) {
    
    return("Provisional")
    
  } else if (
    completed_games_per_team <= 4
  ) {
    
    return("Early Season")
    
  } else {
    
    return("Standard")
  }
}


# ============================================================
# 35. CREATE ONE END-OF-WEEK POWER TABLE
# ============================================================

create_weekly_power_table <- function(
    games,
    schedule,
    team_info,
    through_week,
    parity_coefficients,
    previous_table = NULL
) {
  
  league_teams <- team_info$team
  
  training_games <- games[
    games$week <= through_week,
  ]
  
  if (
    nrow(training_games) == 0
  ) {
    
    stop(
      "No completed games are available through this week."
    )
  }
  
  model_settings <- get_current_model_settings(
    training_games = training_games,
    league_teams = league_teams,
    parity_coefficients = parity_coefficients
  )
  
  game_weights <- make_recency_weights(
    training_games = training_games,
    half_life = recency_half_life
  )
  
  rating_model <- fit_opponent_adjusted_ratings(
    data = training_games,
    league_teams = league_teams,
    margin_cap = model_settings$margin_cap,
    team_lambda = model_settings$team_lambda,
    hfa_lambda = model_settings$hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = game_weights
  )
  
  records <- calculate_team_records(
    games = games,
    through_week = through_week,
    league_teams = league_teams
  )
  
  sos_table <- calculate_published_sos(
    schedule = schedule,
    current_ratings = rating_model$ratings,
    through_week = through_week,
    league_teams = league_teams,
    estimated_hfa = rating_model$home_field
  )
  
  power_table <- data.frame(
    week = through_week,
    team = league_teams,
    rating = as.numeric(
      rating_model$ratings[
        league_teams
      ]
    ),
    stringsAsFactors = FALSE
  )
  
  power_table <- merge(
    power_table,
    team_info[
      ,
      c(
        "team",
        "division"
      )
    ],
    by = "team",
    all.x = TRUE
  )
  
  power_table <- merge(
    power_table,
    records,
    by = "team",
    all.x = TRUE
  )
  
  power_table <- merge(
    power_table,
    sos_table,
    by = "team",
    all.x = TRUE
  )
  
  power_table$rank <- rank(
    -power_table$rating,
    ties.method = "min"
  )
  
  power_table$rank <- as.integer(
    power_table$rank
  )
  
  power_table$previous_rank <- NA_integer_
  
  power_table$rank_change <- NA_integer_
  
  power_table$previous_rating <- 0
  
  power_table$rating_change <- power_table$rating
  
  if (
    !is.null(previous_table)
  ) {
    
    previous_lookup <- previous_table[
      ,
      c(
        "team",
        "rank",
        "rating"
      )
    ]
    
    names(previous_lookup) <- c(
      "team",
      "previous_rank_lookup",
      "previous_rating_lookup"
    )
    
    power_table <- merge(
      power_table,
      previous_lookup,
      by = "team",
      all.x = TRUE
    )
    
    power_table$previous_rank <- power_table$previous_rank_lookup
    
    power_table$rank_change <- (
      power_table$previous_rank -
        power_table$rank
    )
    
    power_table$previous_rating <- power_table$previous_rating_lookup
    
    power_table$rating_change <- (
      power_table$rating -
        power_table$previous_rating
    )
    
    power_table$previous_rank_lookup <- NULL
    
    power_table$previous_rating_lookup <- NULL
  }
  
  power_table$estimated_HFA <- rating_model$home_field
  
  power_table$estimated_parity_SD <- model_settings$estimated_parity_SD
  
  power_table$margin_cap <- model_settings$margin_cap
  
  power_table$team_lambda <- model_settings$team_lambda
  
  power_table$hfa_lambda <- model_settings$hfa_lambda
  
  power_table$completed_games_per_team <- model_settings$completed_games_per_team
  
  power_table$publication_stage <- get_publication_stage(
    model_settings$completed_games_per_team
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
      "record",
      "wins",
      "losses",
      "win_pct",
      "games_played",
      "previous_rank",
      "rank_change",
      "previous_rating",
      "rating_change",
      "played_SOS",
      "remaining_SOS",
      "full_SOS",
      "played_location_difficulty",
      "remaining_location_difficulty",
      "estimated_HFA",
      "estimated_parity_SD",
      "margin_cap",
      "team_lambda",
      "hfa_lambda",
      "completed_games_per_team"
    )
  ]
  
  weekly_settings <- data.frame(
    week = through_week,
    publication_stage = get_publication_stage(
      model_settings$completed_games_per_team
    ),
    completed_games_per_team = model_settings$completed_games_per_team,
    estimated_HFA = rating_model$home_field,
    estimated_parity_SD = model_settings$estimated_parity_SD,
    margin_cap = model_settings$margin_cap,
    team_lambda = model_settings$team_lambda,
    hfa_lambda = model_settings$hfa_lambda,
    stringsAsFactors = FALSE
  )
  
  return(
    list(
      power_table = power_table,
      model_settings = weekly_settings,
      rating_model = rating_model
    )
  )
}


# ============================================================
# 36. CREATE FULL WEEKLY POWER-RATING HISTORY
# ============================================================

create_power_rating_history <- function(
    games,
    schedule,
    team_info,
    parity_coefficients,
    n_weeks
) {
  
  table_storage <- list()
  
  settings_storage <- list()
  
  previous_table <- NULL
  
  for (current_week in 1:n_weeks) {
    
    current_result <- create_weekly_power_table(
      games = games,
      schedule = schedule,
      team_info = team_info,
      through_week = current_week,
      parity_coefficients = parity_coefficients,
      previous_table = previous_table
    )
    
    table_storage[[current_week]] <- current_result$power_table
    
    settings_storage[[current_week]] <- current_result$model_settings
    
    previous_table <- current_result$power_table
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
      history = history,
      settings_history = settings_history
    )
  )
}


# ============================================================
# 37. GENERATE ONE VERSION 9A EXAMPLE SEASON
# ============================================================
#
# Hidden ratings generate the outcomes, but the power-rating
# tables below never use them.
# ============================================================

example_true_sd <- 5

set.seed(9002)

example_schedule_result <- generate_unbalanced_schedule(
  team_info = team_info,
  n_weeks = n_weeks
)

example_schedule <- example_schedule_result$schedule

example_schedule_validation <- validate_schedule(
  schedule = example_schedule,
  team_info = team_info,
  n_weeks = n_weeks
)

example_true_ratings <- generate_true_ratings(
  league_teams = teams,
  target_sd = example_true_sd
)

example_games <- simulate_static_season(
  schedule = example_schedule,
  true_ratings = example_true_ratings,
  true_hfa = true_hfa,
  noise_sd = noise_sd
)

power_history_result <- create_power_rating_history(
  games = example_games,
  schedule = example_schedule,
  team_info = team_info,
  parity_coefficients = parity_calibration_coefficients,
  n_weeks = n_weeks
)

power_rating_history <- power_history_result$history

weekly_model_settings <- power_history_result$settings_history


# ============================================================
# 38. PUBLISHED DISPLAY TABLES
# ============================================================

published_columns <- c(
  "rank",
  "team",
  "rating",
  "record",
  "rank_change",
  "rating_change",
  "played_SOS",
  "remaining_SOS",
  "full_SOS"
)

weekly_published_tables <- list()

for (current_week in 1:n_weeks) {
  
  current_table <- power_rating_history[
    power_rating_history$week == current_week,
    published_columns
  ]
  
  current_table <- current_table[
    order(
      current_table$rank
    ),
  ]
  
  rownames(current_table) <- NULL
  
  weekly_published_tables[[current_week]] <- current_table
}


# ============================================================
# 39. PRINT SELECTED WEEKLY TABLES
# ============================================================

weeks_to_print <- c(
  1,
  5,
  8,
  11,
  14
)

for (current_week in weeks_to_print) {
  
  cat("\n\n========================================\n")
  cat(
    "VERSION 9A POWER RATINGS - END OF WEEK",
    current_week,
    "\n"
  )
  cat("========================================\n\n")
  
  current_stage <- unique(
    power_rating_history$publication_stage[
      power_rating_history$week == current_week
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
# 40. LATEST POWER TABLE
# ============================================================

latest_week <- max(
  power_rating_history$week
)

latest_power_table <- power_rating_history[
  power_rating_history$week == latest_week,
]

latest_power_table <- latest_power_table[
  order(
    latest_power_table$rank
  ),
]

rownames(latest_power_table) <- NULL


# ============================================================
# 41. POWER-RATING TRAJECTORY PLOT
# ============================================================

rating_history_plot <- ggplot(
  power_rating_history,
  aes(
    x = week,
    y = rating,
    group = team
  )
) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Version 9A Weekly Power-Rating History",
    subtitle = "Rating = estimated neutral-field points above/below league average",
    x = "Week",
    y = "Power Rating"
  ) +
  theme_minimal()

print(
  rating_history_plot
)


# ============================================================
# 42. RANK TRAJECTORY PLOT
# ============================================================

rank_history_plot <- ggplot(
  power_rating_history,
  aes(
    x = week,
    y = rank,
    group = team
  )
) +
  geom_line() +
  geom_point() +
  scale_y_reverse(
    breaks = 1:length(teams)
  ) +
  labs(
    title = "Version 9A Weekly Rank History",
    subtitle = "Rank 1 is the highest-rated team",
    x = "Week",
    y = "Power Rank"
  ) +
  theme_minimal()

print(
  rank_history_plot
)


# ============================================================
# 43. LATEST POWER-RATING PLOT
# ============================================================

latest_plot_data <- latest_power_table

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
    y = rating
  )
) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = paste0(
      "Version 9A Power Ratings - End of Week ",
      latest_week
    ),
    subtitle = "Neutral-field points above/below league average",
    x = "Team",
    y = "Power Rating"
  ) +
  theme_minimal()

print(
  latest_rating_plot
)


# ============================================================
# 44. SAVE VERSION 9A OUTPUTS
# ============================================================

write.csv(
  parity_calibration_coefficients,
  "version9A_parity_calibration_coefficients.csv",
  row.names = FALSE
)

write.csv(
  team_info,
  "version9A_team_info.csv",
  row.names = FALSE
)

write.csv(
  example_schedule,
  "version9A_schedule.csv",
  row.names = FALSE
)

write.csv(
  example_games,
  "version9A_simulated_results.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    team = names(example_true_ratings),
    true_rating = as.numeric(example_true_ratings),
    stringsAsFactors = FALSE
  ),
  "version9A_hidden_answer_key.csv",
  row.names = FALSE
)

write.csv(
  power_rating_history,
  "version9A_power_rating_history.csv",
  row.names = FALSE
)

write.csv(
  weekly_model_settings,
  "version9A_weekly_model_settings.csv",
  row.names = FALSE
)

write.csv(
  latest_power_table,
  "version9A_latest_power_table.csv",
  row.names = FALSE
)

for (current_week in 1:n_weeks) {
  
  write.csv(
    weekly_published_tables[[current_week]],
    paste0(
      "version9A_week_",
      current_week,
      "_power_rankings.csv"
    ),
    row.names = FALSE
  )
}


# ============================================================
# 45. FINAL SUMMARY
# ============================================================

cat("\n\n========================================\n")
cat("VERSION 9A COMPLETE\n")
cat("========================================\n\n")

cat("Core model:\n")
cat("  Opponent-Adjusted Ridge + HL12\n\n")

cat("Interpretation:\n")
cat("  Rating = estimated points above/below the average team\n")
cat("  on a neutral field.\n\n")

cat("Timing:\n")
cat("  End-of-week rankings use all results through that week.\n")
cat("  Future schedule is known; future results are never used.\n\n")

cat("Example hidden true SD:", example_true_sd, "\n")
cat("Schedule valid:", example_schedule_validation$valid, "\n")
cat("Schedule connected:", example_schedule_validation$connected, "\n")
cat(
  "League-wide bye weeks:",
  paste(
    example_schedule_result$bye_weeks,
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

cat("\nOutput files written to the current working directory.\n")

# ============================================================
# END VERSION 9A
# ============================================================

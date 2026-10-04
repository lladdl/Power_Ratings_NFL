# ============================================================
# FOOTBALL POWER RATINGS - VERSION 9B-MC
# MONTE CARLO RATING-ACCURACY + UNCERTAINTY CALIBRATION
# ============================================================
#
# PURPOSE
# -------
# Version 9B-MC stress-tests the Version 9A/9B rating engine
# across many independent simulated seasons and calibrates uncertainty.
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

# Number of residual-bootstrap refits per published week.
# For a quick code test, temporarily reduce this to 50 or 100.
n_bootstrap_reps <- 300

bootstrap_confidence_level <- 0.95
bootstrap_seed_base <- 9100


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

# Consistent team colors used in every team-level plot.
team_colors <- setNames(
  grDevices::hcl.colors(
    length(teams),
    palette = "Dark 3"
  ),
  teams
)

cat("\n========================================\n")
cat("VERSION 9B-MC\n")
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
# Version 9B-MC uses the SAME 16-team parity calibration developed
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
  cat("BUILDING VERSION 9B-MC PARITY CALIBRATION\n")
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


# ============================================================
# 32. VERSION 9B-MC EXPERIMENT SETTINGS
# ============================================================
#
# Full experiment defaults:
#   - 1,000 independent seasons for rating-accuracy measurement
#   - 100 independent seasons to learn uncertainty multipliers
#   - 100 separate holdout seasons to test calibrated coverage
#   - 100 residual-bootstrap refits per snapshot
#
# Set quick_test <- TRUE for a fast code check before the full run.
# ============================================================

quick_test <- FALSE

target_true_sd <- 5

if (quick_test) {
  
  n_accuracy_seasons <- 50
  n_uncertainty_calibration_seasons <- 10
  n_uncertainty_holdout_seasons <- 10
  n_bootstrap_reps_mc <- 30
  
} else {
  
  n_accuracy_seasons <- 1000
  n_uncertainty_calibration_seasons <- 100
  n_uncertainty_holdout_seasons <- 100
  n_bootstrap_reps_mc <- 100
}

accuracy_seed_base <- 920100
uncertainty_calibration_seed_base <- 920200
uncertainty_holdout_seed_base <- 920300
bootstrap_seed_base_mc <- 930000

nominal_confidence_level <- 0.95
nominal_z <- qnorm(
  1 -
    (1 - nominal_confidence_level) / 2
)

cat("\n========================================\n")
cat("VERSION 9B-MC EXPERIMENT SETTINGS\n")
cat("========================================\n\n")
cat("Target hidden true SD:", target_true_sd, "\n")
cat("Accuracy seasons:", n_accuracy_seasons, "\n")
cat(
  "Uncertainty calibration seasons:",
  n_uncertainty_calibration_seasons,
  "\n"
)
cat(
  "Uncertainty holdout seasons:",
  n_uncertainty_holdout_seasons,
  "\n"
)
cat("Bootstrap refits per snapshot:", n_bootstrap_reps_mc, "\n")
cat(
  "Nominal interval coverage:",
  100 * nominal_confidence_level,
  "%\n\n"
)


# ============================================================
# 33. FIT ONE RATING SNAPSHOT AFTER N COMPLETED GAMES / TEAM
# ============================================================
#
# Because Version 8/9 schedules contain two league-wide bye weeks,
# completed games per team is a cleaner information index than raw
# calendar week. The returned calendar_week tells us when that active
# round occurred in the simulated season.
# ============================================================

fit_rating_snapshot <- function(
    games,
    completed_games_per_team,
    league_teams,
    parity_coefficients
) {
  
  active_weeks <- sort(
    unique(
      games$week
    )
  )
  
  if (
    completed_games_per_team < 1 ||
    completed_games_per_team > length(active_weeks)
  ) {
    
    stop(
      "completed_games_per_team is outside the available active rounds."
    )
  }
  
  current_calendar_week <- active_weeks[
    completed_games_per_team
  ]
  
  training_games <- games[
    games$week <= current_calendar_week,
  ]
  
  model_settings <- get_current_model_settings(
    training_games = training_games,
    league_teams = league_teams,
    parity_coefficients = parity_coefficients
  )
  
  game_weights <- make_recency_weights(
    training_games = training_games,
    half_life = recency_half_life
  )
  
  rating_fit <- fit_opponent_adjusted_ratings(
    data = training_games,
    league_teams = league_teams,
    margin_cap = model_settings$margin_cap,
    team_lambda = model_settings$team_lambda,
    hfa_lambda = model_settings$hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = game_weights
  )
  
  return(
    list(
      calendar_week = current_calendar_week,
      training_games = training_games,
      model_settings = model_settings,
      game_weights = game_weights,
      rating_fit = rating_fit
    )
  )
}


# ============================================================
# 34. GENERATE ONE INDEPENDENT VERSION-9 STYLE SEASON
# ============================================================

generate_mc_season <- function(
    season_seed,
    target_sd
) {
  
  set.seed(
    season_seed
  )
  
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
  
  true_ratings <- generate_true_ratings(
    league_teams = teams,
    target_sd = target_sd
  )
  
  games <- simulate_static_season(
    schedule = schedule,
    true_ratings = true_ratings,
    true_hfa = true_hfa,
    noise_sd = noise_sd
  )
  
  return(
    list(
      schedule = schedule,
      true_ratings = true_ratings,
      games = games
    )
  )
}


# ============================================================
# 35. ONE-SEASON RATING ACCURACY THROUGH ALL ACTIVE ROUNDS
# ============================================================

measure_one_season_accuracy <- function(
    games,
    true_ratings,
    league_teams,
    parity_coefficients,
    simulation_id
) {
  
  true_ranks <- as.integer(
    rank(
      -true_ratings[
        league_teams
      ],
      ties.method = "min"
    )
  )
  
  names(true_ranks) <- league_teams
  
  season_rows <- list()
  
  for (completed_games in 1:games_per_team) {
    
    snapshot <- fit_rating_snapshot(
      games = games,
      completed_games_per_team = completed_games,
      league_teams = league_teams,
      parity_coefficients = parity_coefficients
    )
    
    estimated_ratings <- snapshot$rating_fit$ratings[
      league_teams
    ]
    
    estimated_ranks <- as.integer(
      rank(
        -estimated_ratings,
        ties.method = "min"
      )
    )
    
    names(estimated_ranks) <- league_teams
    
    errors <- (
      estimated_ratings -
        true_ratings[
          league_teams
        ]
    )
    
    absolute_errors <- abs(
      errors
    )
    
    estimated_top4 <- league_teams[
      estimated_ranks <= 4
    ]
    
    true_top4 <- league_teams[
      true_ranks <= 4
    ]
    
    estimated_number_one <- league_teams[
      which.min(
        estimated_ranks
      )
    ]
    
    true_number_one <- league_teams[
      which.min(
        true_ranks
      )
    ]
    
    season_rows[[completed_games]] <- data.frame(
      simulation = simulation_id,
      completed_games_per_team = completed_games,
      calendar_week = snapshot$calendar_week,
      Rating_MSE = mean(
        errors^2
      ),
      Rating_RMSE = sqrt(
        mean(
          errors^2
        )
      ),
      Rating_MAE = mean(
        absolute_errors
      ),
      Rating_Bias = mean(
        errors
      ),
      Rating_Pearson = safe_cor(
        estimated_ratings,
        true_ratings[
          league_teams
        ],
        method = "pearson"
      ),
      Rating_Spearman = safe_cor(
        estimated_ratings,
        true_ratings[
          league_teams
        ],
        method = "spearman"
      ),
      Mean_Absolute_Rank_Error = mean(
        abs(
          estimated_ranks -
            true_ranks
        )
      ),
      Top4_Overlap = length(
        intersect(
          estimated_top4,
          true_top4
        )
      ) / 4,
      Number1_Correct = as.integer(
        estimated_number_one ==
          true_number_one
      ),
      Within_1_Point = mean(
        absolute_errors <= 1
      ),
      Within_2_Points = mean(
        absolute_errors <= 2
      ),
      Within_3_Points = mean(
        absolute_errors <= 3
      ),
      Estimated_HFA = snapshot$rating_fit$home_field,
      Estimated_Parity_SD = snapshot$model_settings$estimated_parity_SD,
      Margin_Cap = snapshot$model_settings$margin_cap,
      Team_Lambda = snapshot$model_settings$team_lambda,
      HFA_Lambda = snapshot$model_settings$hfa_lambda,
      stringsAsFactors = FALSE
    )
  }
  
  season_accuracy <- do.call(
    rbind,
    season_rows
  )
  
  rownames(season_accuracy) <- NULL
  
  return(
    season_accuracy
  )
}


# ============================================================
# 36. RUN 1,000-SEASON RATING-ACCURACY MONTE CARLO
# ============================================================

cat("\n========================================\n")
cat("RATING-ACCURACY MONTE CARLO\n")
cat("========================================\n\n")

accuracy_storage <- list()

for (simulation in 1:n_accuracy_seasons) {
  
  if (
    simulation == 1 ||
    simulation %% 100 == 0 ||
    simulation == n_accuracy_seasons
  ) {
    
    cat(
      "Accuracy simulation",
      simulation,
      "of",
      n_accuracy_seasons,
      "\n"
    )
  }
  
  season <- generate_mc_season(
    season_seed = accuracy_seed_base + simulation,
    target_sd = target_true_sd
  )
  
  accuracy_storage[[simulation]] <- measure_one_season_accuracy(
    games = season$games,
    true_ratings = season$true_ratings,
    league_teams = teams,
    parity_coefficients = parity_calibration_coefficients,
    simulation_id = simulation
  )
}

accuracy_season_level <- do.call(
  rbind,
  accuracy_storage
)

rownames(accuracy_season_level) <- NULL


# ============================================================
# 37. SUMMARIZE RATING ACCURACY BY COMPLETED GAMES
# ============================================================

accuracy_summary_rows <- list()

for (completed_games in 1:games_per_team) {
  
  current_data <- accuracy_season_level[
    accuracy_season_level$completed_games_per_team ==
      completed_games,
  ]
  
  accuracy_summary_rows[[completed_games]] <- data.frame(
    completed_games_per_team = completed_games,
    mean_calendar_week = mean(
      current_data$calendar_week
    ),
    Rating_RMSE = sqrt(
      mean(
        current_data$Rating_MSE
      )
    ),
    Mean_Season_RMSE = mean(
      current_data$Rating_RMSE
    ),
    Median_Season_RMSE = median(
      current_data$Rating_RMSE
    ),
    Rating_MAE = mean(
      current_data$Rating_MAE
    ),
    Rating_Bias = mean(
      current_data$Rating_Bias
    ),
    Mean_Pearson = mean(
      current_data$Rating_Pearson,
      na.rm = TRUE
    ),
    Mean_Spearman = mean(
      current_data$Rating_Spearman,
      na.rm = TRUE
    ),
    Mean_Absolute_Rank_Error = mean(
      current_data$Mean_Absolute_Rank_Error
    ),
    Mean_Top4_Overlap = mean(
      current_data$Top4_Overlap
    ),
    Probability_Number1_Correct = mean(
      current_data$Number1_Correct
    ),
    Probability_Within_1_Point = mean(
      current_data$Within_1_Point
    ),
    Probability_Within_2_Points = mean(
      current_data$Within_2_Points
    ),
    Probability_Within_3_Points = mean(
      current_data$Within_3_Points
    ),
    Mean_Estimated_HFA = mean(
      current_data$Estimated_HFA
    ),
    Mean_Absolute_HFA_Error = mean(
      abs(
        current_data$Estimated_HFA -
          true_hfa
      )
    ),
    stringsAsFactors = FALSE
  )
}

accuracy_by_games <- do.call(
  rbind,
  accuracy_summary_rows
)

rownames(accuracy_by_games) <- NULL

cat("\nRATING ACCURACY BY COMPLETED GAMES / TEAM:\n\n")
print(
  accuracy_by_games
)


# ============================================================
# 38. FAST RESIDUAL BOOTSTRAP FOR ONE SNAPSHOT
# ============================================================
#
# This reproduces the Version 9B residual-bootstrap idea but avoids
# repeatedly rebuilding the same design matrix. The ridge solver is
# fixed within a snapshot, so only the bootstrapped response vector
# changes from replicate to replicate.
# ============================================================

bootstrap_snapshot_fast <- function(
    games,
    true_ratings,
    completed_games_per_team,
    league_teams,
    parity_coefficients,
    n_bootstrap_reps,
    bootstrap_seed
) {
  
  snapshot <- fit_rating_snapshot(
    games = games,
    completed_games_per_team = completed_games_per_team,
    league_teams = league_teams,
    parity_coefficients = parity_coefficients
  )
  
  training_games <- snapshot$training_games
  model_settings <- snapshot$model_settings
  game_weights <- snapshot$game_weights
  official_fit <- snapshot$rating_fit
  
  n_games <- nrow(
    training_games
  )
  
  n_teams <- length(
    league_teams
  )
  
  capped_observed_margin <- cap_margin(
    training_games$actual_margin,
    model_settings$margin_cap
  )
  
  fitted_margin <- (
    official_fit$home_field +
      official_fit$ratings[
        training_games$home_team
      ] -
      official_fit$ratings[
        training_games$away_team
      ]
  )
  
  residuals <- (
    capped_observed_margin -
      fitted_margin
  )
  
  residuals <- residuals - weighted.mean(
    residuals,
    w = game_weights
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
  
  for (game_index in seq_len(n_games)) {
    
    home_team <- training_games$home_team[game_index]
    away_team <- training_games$away_team[game_index]
    
    X[game_index, home_team] <- 1
    X[game_index, away_team] <- -1
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
  
  penalty <- diag(
    c(
      model_settings$hfa_lambda,
      rep(
        model_settings$team_lambda,
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
  
  ridge_inverse <- solve(
    crossprod(
      X_weighted
    ) +
      penalty
  )
  
  fixed_prior_rhs <- penalty %*%
    prior_target
  
  set.seed(
    bootstrap_seed
  )
  
  bootstrap_ratings <- matrix(
    NA_real_,
    nrow = n_bootstrap_reps,
    ncol = n_teams
  )
  
  colnames(bootstrap_ratings) <- league_teams
  
  for (bootstrap_rep in seq_len(n_bootstrap_reps)) {
    
    sampled_positions <- sample.int(
      length(residuals),
      size = length(residuals),
      replace = TRUE
    )
    
    bootstrap_margin <- (
      fitted_margin +
        residuals[
          sampled_positions
        ]
    )
    
    bootstrap_margin <- cap_margin(
      bootstrap_margin,
      model_settings$margin_cap
    )
    
    bootstrap_y_weighted <- (
      bootstrap_margin *
        sqrt_weights
    )
    
    beta_matrix <- ridge_inverse %*% (
      crossprod(
        X_weighted,
        bootstrap_y_weighted
      ) +
        fixed_prior_rhs
    )
    
    beta <- as.numeric(
      beta_matrix
    )
    
    names(beta) <- colnames(X)
    
    current_ratings <- beta[
      league_teams
    ]
    
    current_ratings <- current_ratings -
      mean(
        current_ratings
      )
    
    bootstrap_ratings[
      bootstrap_rep,
    ] <- current_ratings
  }
  
  alpha <- (
    1 - nominal_confidence_level
  ) / 2
  
  output_rows <- list()
  
  for (team_index in seq_along(league_teams)) {
    
    current_team <- league_teams[team_index]
    
    rating_samples <- bootstrap_ratings[
      ,
      team_index
    ]
    
    current_sd <- sd(
      rating_samples
    )
    
    percentile_interval <- quantile(
      rating_samples,
      probs = c(
        alpha,
        1 - alpha
      ),
      names = FALSE,
      type = 7
    )
    
    official_rating <- official_fit$ratings[
      current_team
    ]
    
    true_rating <- true_ratings[
      current_team
    ]
    
    absolute_error <- abs(
      official_rating -
        true_rating
    )
    
    standardized_absolute_error <- if (
      is.finite(current_sd) &&
      current_sd > 1e-10
    ) {
      absolute_error / current_sd
    } else {
      NA_real_
    }
    
    normal_lower <- (
      official_rating -
        nominal_z *
        current_sd
    )
    
    normal_upper <- (
      official_rating +
        nominal_z *
        current_sd
    )
    
    output_rows[[team_index]] <- data.frame(
      completed_games_per_team = completed_games_per_team,
      calendar_week = snapshot$calendar_week,
      team = current_team,
      official_rating = as.numeric(
        official_rating
      ),
      true_rating = as.numeric(
        true_rating
      ),
      rating_error = as.numeric(
        official_rating -
          true_rating
      ),
      absolute_rating_error = as.numeric(
        absolute_error
      ),
      bootstrap_rating_SD = current_sd,
      raw_percentile_lower = percentile_interval[1],
      raw_percentile_upper = percentile_interval[2],
      raw_percentile_covered = as.integer(
        true_rating >= percentile_interval[1] &&
          true_rating <= percentile_interval[2]
      ),
      raw_normal_lower = as.numeric(
        normal_lower
      ),
      raw_normal_upper = as.numeric(
        normal_upper
      ),
      raw_normal_covered = as.integer(
        true_rating >= normal_lower &&
          true_rating <= normal_upper
      ),
      standardized_absolute_error = standardized_absolute_error,
      stringsAsFactors = FALSE
    )
  }
  
  output <- do.call(
    rbind,
    output_rows
  )
  
  rownames(output) <- NULL
  
  return(
    output
  )
}


# ============================================================
# 39. RUN UNCERTAINTY CALIBRATION SAMPLE
# ============================================================

cat("\n========================================\n")
cat("UNCERTAINTY CALIBRATION SAMPLE\n")
cat("========================================\n\n")

uncertainty_calibration_storage <- list()
uncertainty_calibration_index <- 1

for (simulation in 1:n_uncertainty_calibration_seasons) {
  
  if (
    simulation == 1 ||
    simulation %% 20 == 0 ||
    simulation == n_uncertainty_calibration_seasons
  ) {
    
    cat(
      "Calibration season",
      simulation,
      "of",
      n_uncertainty_calibration_seasons,
      "\n"
    )
  }
  
  season <- generate_mc_season(
    season_seed = uncertainty_calibration_seed_base + simulation,
    target_sd = target_true_sd
  )
  
  for (completed_games in 1:games_per_team) {
    
    current_uncertainty <- bootstrap_snapshot_fast(
      games = season$games,
      true_ratings = season$true_ratings,
      completed_games_per_team = completed_games,
      league_teams = teams,
      parity_coefficients = parity_calibration_coefficients,
      n_bootstrap_reps = n_bootstrap_reps_mc,
      bootstrap_seed = (
        bootstrap_seed_base_mc +
          100000 +
          simulation * 100 +
          completed_games
      )
    )
    
    current_uncertainty$simulation <- simulation
    current_uncertainty$sample <- "Calibration"
    
    uncertainty_calibration_storage[[uncertainty_calibration_index]] <-
      current_uncertainty
    
    uncertainty_calibration_index <-
      uncertainty_calibration_index + 1
  }
}

uncertainty_calibration_detail <- do.call(
  rbind,
  uncertainty_calibration_storage
)

rownames(uncertainty_calibration_detail) <- NULL


# ============================================================
# 40. LEARN EMPIRICAL COVERAGE MULTIPLIERS
# ============================================================
#
# For each amount of information, calculate the 95th percentile of:
#
#     |official rating - true rating| / bootstrap SD
#
# A calibrated interval is then:
#
#     official rating +/- k * bootstrap SD
#
# k is learned ONLY on the calibration seasons and will be judged
# on a separate holdout set below.
# ============================================================

calibration_multiplier_rows <- list()

for (completed_games in 1:games_per_team) {
  
  current_data <- uncertainty_calibration_detail[
    uncertainty_calibration_detail$completed_games_per_team ==
      completed_games &
      is.finite(
        uncertainty_calibration_detail$standardized_absolute_error
      ),
  ]
  
  empirical_multiplier <- as.numeric(
    quantile(
      current_data$standardized_absolute_error,
      probs = nominal_confidence_level,
      names = FALSE,
      type = 1
    )
  )
  
  calibration_multiplier_rows[[completed_games]] <- data.frame(
    completed_games_per_team = completed_games,
    n_team_season_observations = nrow(
      current_data
    ),
    raw_percentile_coverage = mean(
      current_data$raw_percentile_covered
    ),
    raw_normal_coverage = mean(
      current_data$raw_normal_covered
    ),
    mean_bootstrap_SD = mean(
      current_data$bootstrap_rating_SD
    ),
    mean_absolute_rating_error = mean(
      current_data$absolute_rating_error
    ),
    empirical_95_multiplier = empirical_multiplier,
    nominal_normal_multiplier = nominal_z,
    multiplier_inflation_vs_normal = (
      empirical_multiplier /
        nominal_z
    ),
    stringsAsFactors = FALSE
  )
}

uncertainty_multipliers <- do.call(
  rbind,
  calibration_multiplier_rows
)

rownames(uncertainty_multipliers) <- NULL

cat("\nEMPIRICAL UNCERTAINTY MULTIPLIERS:\n\n")
print(
  uncertainty_multipliers
)


# ============================================================
# 41. RUN INDEPENDENT UNCERTAINTY HOLDOUT SAMPLE
# ============================================================

cat("\n========================================\n")
cat("UNCERTAINTY HOLDOUT SAMPLE\n")
cat("========================================\n\n")

uncertainty_holdout_storage <- list()
uncertainty_holdout_index <- 1

for (simulation in 1:n_uncertainty_holdout_seasons) {
  
  if (
    simulation == 1 ||
    simulation %% 20 == 0 ||
    simulation == n_uncertainty_holdout_seasons
  ) {
    
    cat(
      "Holdout season",
      simulation,
      "of",
      n_uncertainty_holdout_seasons,
      "\n"
    )
  }
  
  season <- generate_mc_season(
    season_seed = uncertainty_holdout_seed_base + simulation,
    target_sd = target_true_sd
  )
  
  for (completed_games in 1:games_per_team) {
    
    current_uncertainty <- bootstrap_snapshot_fast(
      games = season$games,
      true_ratings = season$true_ratings,
      completed_games_per_team = completed_games,
      league_teams = teams,
      parity_coefficients = parity_calibration_coefficients,
      n_bootstrap_reps = n_bootstrap_reps_mc,
      bootstrap_seed = (
        bootstrap_seed_base_mc +
          200000 +
          simulation * 100 +
          completed_games
      )
    )
    
    current_uncertainty$simulation <- simulation
    current_uncertainty$sample <- "Holdout"
    
    uncertainty_holdout_storage[[uncertainty_holdout_index]] <-
      current_uncertainty
    
    uncertainty_holdout_index <-
      uncertainty_holdout_index + 1
  }
}

uncertainty_holdout_detail <- do.call(
  rbind,
  uncertainty_holdout_storage
)

rownames(uncertainty_holdout_detail) <- NULL

uncertainty_holdout_detail <- merge(
  uncertainty_holdout_detail,
  uncertainty_multipliers[
    ,
    c(
      "completed_games_per_team",
      "empirical_95_multiplier"
    )
  ],
  by = "completed_games_per_team",
  all.x = TRUE
)

uncertainty_holdout_detail$calibrated_lower <- (
  uncertainty_holdout_detail$official_rating -
    uncertainty_holdout_detail$empirical_95_multiplier *
    uncertainty_holdout_detail$bootstrap_rating_SD
)

uncertainty_holdout_detail$calibrated_upper <- (
  uncertainty_holdout_detail$official_rating +
    uncertainty_holdout_detail$empirical_95_multiplier *
    uncertainty_holdout_detail$bootstrap_rating_SD
)

uncertainty_holdout_detail$calibrated_covered <- as.integer(
  uncertainty_holdout_detail$true_rating >=
    uncertainty_holdout_detail$calibrated_lower &
    uncertainty_holdout_detail$true_rating <=
    uncertainty_holdout_detail$calibrated_upper
)

uncertainty_holdout_detail$raw_percentile_width <- (
  uncertainty_holdout_detail$raw_percentile_upper -
    uncertainty_holdout_detail$raw_percentile_lower
)

uncertainty_holdout_detail$raw_normal_width <- (
  uncertainty_holdout_detail$raw_normal_upper -
    uncertainty_holdout_detail$raw_normal_lower
)

uncertainty_holdout_detail$calibrated_width <- (
  uncertainty_holdout_detail$calibrated_upper -
    uncertainty_holdout_detail$calibrated_lower
)


# ============================================================
# 42. HOLDOUT COVERAGE SUMMARY
# ============================================================

holdout_coverage_rows <- list()

for (completed_games in 1:games_per_team) {
  
  current_data <- uncertainty_holdout_detail[
    uncertainty_holdout_detail$completed_games_per_team ==
      completed_games,
  ]
  
  holdout_coverage_rows[[completed_games]] <- data.frame(
    completed_games_per_team = completed_games,
    n_team_season_observations = nrow(
      current_data
    ),
    raw_percentile_coverage = mean(
      current_data$raw_percentile_covered
    ),
    raw_normal_coverage = mean(
      current_data$raw_normal_covered
    ),
    calibrated_coverage = mean(
      current_data$calibrated_covered
    ),
    empirical_95_multiplier = unique(
      current_data$empirical_95_multiplier
    )[1],
    mean_raw_percentile_width = mean(
      current_data$raw_percentile_width
    ),
    mean_raw_normal_width = mean(
      current_data$raw_normal_width
    ),
    mean_calibrated_width = mean(
      current_data$calibrated_width
    ),
    mean_bootstrap_SD = mean(
      current_data$bootstrap_rating_SD
    ),
    mean_absolute_rating_error = mean(
      current_data$absolute_rating_error
    ),
    stringsAsFactors = FALSE
  )
}

uncertainty_holdout_coverage <- do.call(
  rbind,
  holdout_coverage_rows
)

rownames(uncertainty_holdout_coverage) <- NULL

cat("\nINDEPENDENT HOLDOUT COVERAGE:\n\n")
print(
  uncertainty_holdout_coverage
)


# ============================================================
# 43. OVERALL HOLDOUT COVERAGE
# ============================================================

overall_holdout_coverage <- data.frame(
  nominal_target = nominal_confidence_level,
  raw_percentile_coverage = mean(
    uncertainty_holdout_detail$raw_percentile_covered
  ),
  raw_normal_coverage = mean(
    uncertainty_holdout_detail$raw_normal_covered
  ),
  calibrated_coverage = mean(
    uncertainty_holdout_detail$calibrated_covered
  ),
  mean_raw_percentile_width = mean(
    uncertainty_holdout_detail$raw_percentile_width
  ),
  mean_raw_normal_width = mean(
    uncertainty_holdout_detail$raw_normal_width
  ),
  mean_calibrated_width = mean(
    uncertainty_holdout_detail$calibrated_width
  ),
  stringsAsFactors = FALSE
)

cat("\nOVERALL INDEPENDENT HOLDOUT COVERAGE:\n\n")
print(
  overall_holdout_coverage
)


# ============================================================
# 44. PLOT - RATING RMSE AND MAE VS GAMES PLAYED
# ============================================================

accuracy_error_plot_data <- rbind(
  data.frame(
    completed_games_per_team = accuracy_by_games$completed_games_per_team,
    metric = "RMSE",
    value = accuracy_by_games$Rating_RMSE
  ),
  data.frame(
    completed_games_per_team = accuracy_by_games$completed_games_per_team,
    metric = "MAE",
    value = accuracy_by_games$Rating_MAE
  )
)

accuracy_error_plot <- ggplot(
  accuracy_error_plot_data,
  aes(
    x = completed_games_per_team,
    y = value,
    color = metric
  )
) +
  geom_line(
    linewidth = 1.1
  ) +
  geom_point(
    size = 2.4
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  labs(
    title = "Version 9B-MC Rating Error vs Information",
    subtitle = paste0(
      n_accuracy_seasons,
      " independent seasons; hidden rating SD = ",
      target_true_sd
    ),
    x = "Completed Games per Team",
    y = "Rating Error (Points)",
    color = "Metric"
  ) +
  theme_minimal()

print(
  accuracy_error_plot
)


# ============================================================
# 45. PLOT - PROBABILITY RATING IS WITHIN 1 / 2 / 3 POINTS
# ============================================================

within_plot_data <- rbind(
  data.frame(
    completed_games_per_team = accuracy_by_games$completed_games_per_team,
    threshold = "Within 1 Point",
    probability = accuracy_by_games$Probability_Within_1_Point
  ),
  data.frame(
    completed_games_per_team = accuracy_by_games$completed_games_per_team,
    threshold = "Within 2 Points",
    probability = accuracy_by_games$Probability_Within_2_Points
  ),
  data.frame(
    completed_games_per_team = accuracy_by_games$completed_games_per_team,
    threshold = "Within 3 Points",
    probability = accuracy_by_games$Probability_Within_3_Points
  )
)

within_plot <- ggplot(
  within_plot_data,
  aes(
    x = completed_games_per_team,
    y = probability,
    color = threshold
  )
) +
  geom_line(
    linewidth = 1.1
  ) +
  geom_point(
    size = 2.4
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  scale_y_continuous(
    limits = c(
      0,
      1
    )
  ) +
  labs(
    title = "Version 9B-MC Rating Accuracy Thresholds",
    subtitle = "Probability an estimated rating is close to hidden truth",
    x = "Completed Games per Team",
    y = "Probability",
    color = "Accuracy Threshold"
  ) +
  theme_minimal()

print(
  within_plot
)


# ============================================================
# 46. PLOT - RATING CORRELATION VS GAMES PLAYED
# ============================================================

correlation_plot_data <- rbind(
  data.frame(
    completed_games_per_team = accuracy_by_games$completed_games_per_team,
    metric = "Pearson",
    value = accuracy_by_games$Mean_Pearson
  ),
  data.frame(
    completed_games_per_team = accuracy_by_games$completed_games_per_team,
    metric = "Spearman",
    value = accuracy_by_games$Mean_Spearman
  )
)

correlation_plot <- ggplot(
  correlation_plot_data,
  aes(
    x = completed_games_per_team,
    y = value,
    color = metric
  )
) +
  geom_line(
    linewidth = 1.1
  ) +
  geom_point(
    size = 2.4
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  scale_y_continuous(
    limits = c(
      0,
      1
    )
  ) +
  labs(
    title = "Version 9B-MC Estimated vs True Rating Correlation",
    x = "Completed Games per Team",
    y = "Mean Correlation",
    color = "Correlation"
  ) +
  theme_minimal()

print(
  correlation_plot
)


# ============================================================
# 47. PLOT - RAW VS CALIBRATED HOLDOUT COVERAGE
# ============================================================

coverage_plot_data <- rbind(
  data.frame(
    completed_games_per_team = uncertainty_holdout_coverage$completed_games_per_team,
    interval = "Raw Percentile",
    coverage = uncertainty_holdout_coverage$raw_percentile_coverage
  ),
  data.frame(
    completed_games_per_team = uncertainty_holdout_coverage$completed_games_per_team,
    interval = "Raw Normal",
    coverage = uncertainty_holdout_coverage$raw_normal_coverage
  ),
  data.frame(
    completed_games_per_team = uncertainty_holdout_coverage$completed_games_per_team,
    interval = "Empirically Calibrated",
    coverage = uncertainty_holdout_coverage$calibrated_coverage
  )
)

coverage_plot <- ggplot(
  coverage_plot_data,
  aes(
    x = completed_games_per_team,
    y = coverage,
    color = interval
  )
) +
  geom_hline(
    yintercept = nominal_confidence_level,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1.1
  ) +
  geom_point(
    size = 2.4
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  scale_y_continuous(
    limits = c(
      0,
      1
    )
  ) +
  labs(
    title = "Version 9B-MC Holdout Interval Coverage",
    subtitle = "Calibrated multipliers are learned on separate seasons",
    x = "Completed Games per Team",
    y = "Empirical Coverage",
    color = "Interval"
  ) +
  theme_minimal()

print(
  coverage_plot
)


# ============================================================
# 48. PLOT - EMPIRICAL UNCERTAINTY MULTIPLIER
# ============================================================

multiplier_plot <- ggplot(
  uncertainty_multipliers,
  aes(
    x = completed_games_per_team,
    y = empirical_95_multiplier
  )
) +
  geom_hline(
    yintercept = nominal_z,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1.1
  ) +
  geom_point(
    size = 2.4
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  labs(
    title = "Version 9B-MC Empirical 95% Uncertainty Multiplier",
    subtitle = "Dashed line = ordinary normal-theory 1.96 multiplier",
    x = "Completed Games per Team",
    y = "Multiplier k"
  ) +
  theme_minimal()

print(
  multiplier_plot
)


# ============================================================
# 49. SAVE OUTPUT TABLES
# ============================================================

write.csv(
  accuracy_season_level,
  "version9B_MC_accuracy_season_level.csv",
  row.names = FALSE
)

write.csv(
  accuracy_by_games,
  "version9B_MC_accuracy_by_games.csv",
  row.names = FALSE
)

write.csv(
  uncertainty_calibration_detail,
  "version9B_MC_uncertainty_calibration_detail.csv",
  row.names = FALSE
)

write.csv(
  uncertainty_multipliers,
  "version9B_MC_uncertainty_multipliers.csv",
  row.names = FALSE
)

write.csv(
  uncertainty_holdout_detail,
  "version9B_MC_uncertainty_holdout_detail.csv",
  row.names = FALSE
)

write.csv(
  uncertainty_holdout_coverage,
  "version9B_MC_uncertainty_holdout_coverage.csv",
  row.names = FALSE
)

write.csv(
  overall_holdout_coverage,
  "version9B_MC_overall_holdout_coverage.csv",
  row.names = FALSE
)


# ============================================================
# 50. SAVE PLOTS
# ============================================================

ggsave(
  filename = "version9B_MC_rating_error_vs_games.png",
  plot = accuracy_error_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "version9B_MC_within_thresholds.png",
  plot = within_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "version9B_MC_rating_correlation.png",
  plot = correlation_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "version9B_MC_holdout_coverage.png",
  plot = coverage_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "version9B_MC_uncertainty_multiplier.png",
  plot = multiplier_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 51. FINAL SUMMARY
# ============================================================

cat("\n========================================\n")
cat("VERSION 9B-MC COMPLETE\n")
cat("========================================\n\n")

cat("Core rating model:\n")
cat("  Opponent-Adjusted Ridge + HL12\n\n")

cat("Simulation design:\n")
cat("  Hidden true rating SD:", target_true_sd, "\n")
cat("  Rating-accuracy seasons:", n_accuracy_seasons, "\n")
cat(
  "  Uncertainty calibration seasons:",
  n_uncertainty_calibration_seasons,
  "\n"
)
cat(
  "  Independent uncertainty holdout seasons:",
  n_uncertainty_holdout_seasons,
  "\n"
)
cat("  Bootstrap refits per snapshot:", n_bootstrap_reps_mc, "\n\n")

cat("FINAL 12-GAME RATING ACCURACY:\n\n")
print(
  accuracy_by_games[
    accuracy_by_games$completed_games_per_team == games_per_team,
  ]
)

cat("\nFINAL 12-GAME UNCERTAINTY CALIBRATION:\n\n")
print(
  uncertainty_holdout_coverage[
    uncertainty_holdout_coverage$completed_games_per_team ==
      games_per_team,
  ]
)

cat("\nOVERALL HOLDOUT COVERAGE:\n\n")
print(
  overall_holdout_coverage
)

cat("\nInterpretation:\n")
cat("  1. accuracy_by_games measures the information limit of the\n")
cat("     current results-only rating engine across many seasons.\n")
cat("  2. uncertainty_multipliers tells us how much the residual\n")
cat("     bootstrap SD must be inflated to obtain 95% empirical\n")
cat("     rating coverage on calibration seasons.\n")
cat("  3. uncertainty_holdout_coverage tells us whether that\n")
cat("     calibration generalizes to completely independent seasons.\n")
cat("  4. The calibrated interval should only be adopted for future\n")
cat("     published rankings if holdout coverage is close to 95%.\n\n")

cat("Output CSV files and PNG plots written to the current working directory.\n")

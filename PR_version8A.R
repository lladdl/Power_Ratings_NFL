# ============================================================
# FOOTBALL POWER RATINGS - VERSION 8A
# UNBALANCED SCHEDULE + TRUE STRENGTH OF SCHEDULE SIMULATOR
# ============================================================
#
# PURPOSE
# -------
# Version 8A creates a new simulation environment designed to
# stress-test opponent adjustment and strength of schedule.
#
# League structure:
#   - 16 teams
#   - 4 divisions of 4 teams
#   - 14 calendar weeks
#   - 12 games per team
#   - 2 bye weeks per team
#   - 96 total games
#   - 6 home and 6 away games per team
#
# Opponent structure per team:
#   - 1 protected divisional rival played 3 times
#   - 2 other divisional opponents played 2 times each
#   - 5 interdivision opponents played once each
#   - 7 league opponents not played
#
# Therefore each team plays:
#   3 + 2 + 2 + 1 + 1 + 1 + 1 + 1 = 12 games
#
# Pair-frequency distribution across all 120 possible pairs:
#   - 56 pairs play 0 times
#   - 40 pairs play 1 time
#   - 16 pairs play 2 times
#   - 8 pairs play 3 times
#
# Version 8A intentionally keeps hidden team strength stable.
# The purpose is to isolate schedule imbalance.
#
# HL12 is now the fixed recency rule for future model fitting.
# It is stored here for continuity but is not needed to generate
# the simulated outcomes themselves.
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

example_true_sd <- 5

n_schedule_checks <- 100


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

divisions <- unique(team_info$division)

cat("\nNumber of teams:", length(teams), "\n")
cat("Number of divisions:", length(divisions), "\n")
cat("Calendar weeks:", n_weeks, "\n")
cat("Games per team:", games_per_team, "\n")
cat("Fixed recency half-life:", recency_half_life, "weeks\n\n")

print(team_info)


# ============================================================
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
# 16. GENERATE THE VERSION 8A EXAMPLE SCHEDULE
# ============================================================

set.seed(8001)

schedule_result <- generate_unbalanced_schedule(
  team_info = team_info,
  n_weeks = n_weeks
)

schedule_8A <- schedule_result$schedule

league_bye_weeks <- schedule_result$bye_weeks

schedule_validation <- validate_schedule(
  schedule = schedule_8A,
  team_info = team_info,
  n_weeks = n_weeks
)

cat("\n\n========================================\n")
cat("VERSION 8A SCHEDULE VALIDATION\n")
cat("========================================\n\n")

cat("Valid schedule:", schedule_validation$valid, "\n")
cat("Connected schedule graph:", schedule_validation$connected, "\n")
cat("Total games:", nrow(schedule_8A), "\n")
cat("League-wide bye weeks:", paste(league_bye_weeks, collapse = ", "), "\n\n")

cat("PAIR FREQUENCY DISTRIBUTION:\n\n")
print(schedule_validation$frequency_distribution)

cat("\nTEAM SCHEDULE SUMMARY:\n\n")
print(schedule_validation$team_summary)


# ============================================================
# 17. WEEK SUMMARY
# ============================================================

week_summary <- data.frame(
  week = 1:n_weeks,
  games = as.integer(schedule_validation$week_game_counts),
  stringsAsFactors = FALSE
)

week_summary$teams_playing <- 2 * week_summary$games

week_summary$teams_on_bye <- length(teams) - week_summary$teams_playing

cat("\nWEEK SUMMARY:\n\n")
print(week_summary)


# ============================================================
# 18. GENERATE STABLE HIDDEN TRUE RATINGS
# ============================================================
#
# Version 8A sets hidden drift to zero. Each team has one true
# rating for the entire season.
# ============================================================

generate_true_ratings <- function(
    teams,
    target_sd
) {
  
  ratings <- rnorm(
    length(teams),
    mean = 0,
    sd = 1
  )
  
  ratings <- ratings - mean(ratings)
  
  ratings <- ratings / sd(ratings)
  
  ratings <- ratings * target_sd
  
  names(ratings) <- teams
  
  return(ratings)
}


set.seed(8003)

true_ratings <- generate_true_ratings(
  teams = teams,
  target_sd = example_true_sd
)

true_rating_table <- data.frame(
  team = names(true_ratings),
  true_rating = as.numeric(true_ratings),
  stringsAsFactors = FALSE
)

true_rating_table <- merge(
  true_rating_table,
  team_info[, c("team", "division")],
  by = "team"
)

true_rating_table <- true_rating_table[
  order(-true_rating_table$true_rating),
]

rownames(true_rating_table) <- NULL

cat("\n========================================\n")
cat("HIDDEN TRUE RATINGS\n")
cat("========================================\n\n")

print(true_rating_table)


# ============================================================
# 19. CALCULATE TRUE FULL-SCHEDULE SOS
# ============================================================
#
# Main SOS:
#   mean hidden rating of all scheduled opponents.
#
# Repeated opponents count repeatedly because they represent
# repeated games on the actual schedule.
#
# We also calculate:
#   - distinct-opponent SOS
#   - location-adjusted schedule difficulty
#
# Location adjustment from the team's perspective:
#   home game difficulty = opponent rating - HFA
#   away game difficulty = opponent rating + HFA
# ============================================================

calculate_true_sos <- function(
    schedule,
    true_ratings,
    teams,
    true_hfa
) {
  
  sos_rows <- list()
  
  for (i in seq_along(teams)) {
    
    current_team <- teams[i]
    
    team_games <- schedule[
      schedule$home_team == current_team |
        schedule$away_team == current_team,
    ]
    
    is_home <- team_games$home_team == current_team
    
    opponents <- ifelse(
      is_home,
      team_games$away_team,
      team_games$home_team
    )
    
    opponent_ratings <- as.numeric(
      true_ratings[opponents]
    )
    
    location_adjusted_difficulty <- ifelse(
      is_home,
      opponent_ratings - true_hfa,
      opponent_ratings + true_hfa
    )
    
    distinct_opponents <- unique(opponents)
    
    distinct_opponent_ratings <- as.numeric(
      true_ratings[distinct_opponents]
    )
    
    sos_rows[[i]] <- data.frame(
      team = current_team,
      games = nrow(team_games),
      distinct_opponents = length(distinct_opponents),
      true_SOS_game_weighted = mean(opponent_ratings),
      true_SOS_distinct_opponents = mean(distinct_opponent_ratings),
      true_schedule_difficulty_location_adjusted = mean(location_adjusted_difficulty),
      hardest_opponent_rating = max(opponent_ratings),
      easiest_opponent_rating = min(opponent_ratings),
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


true_sos <- calculate_true_sos(
  schedule = schedule_8A,
  true_ratings = true_ratings,
  teams = teams,
  true_hfa = true_hfa
)

true_sos <- merge(
  true_sos,
  true_rating_table[, c("team", "true_rating", "division")],
  by = "team"
)

true_sos <- true_sos[
  order(-true_sos$true_SOS_game_weighted),
]

rownames(true_sos) <- NULL

cat("\n========================================\n")
cat("TRUE FULL-SEASON STRENGTH OF SCHEDULE\n")
cat("========================================\n\n")

print(true_sos)


# ============================================================
# 20. TRUE SOS SUMMARY
# ============================================================

sos_summary <- data.frame(
  mean_SOS = mean(true_sos$true_SOS_game_weighted),
  SD_SOS = sd(true_sos$true_SOS_game_weighted),
  easiest_SOS = min(true_sos$true_SOS_game_weighted),
  hardest_SOS = max(true_sos$true_SOS_game_weighted),
  SOS_range = max(true_sos$true_SOS_game_weighted) -
    min(true_sos$true_SOS_game_weighted)
)

cat("\n========================================\n")
cat("TRUE SOS SPREAD\n")
cat("========================================\n\n")

print(sos_summary)


# ============================================================
# 21. TRUE PLAYED / REMAINING SOS BY TARGET WEEK
# ============================================================
#
# This is a simulation benchmark for future Versions 8B/8C.
# Before target week t:
#   played SOS    = true mean rating of opponents already faced
#   remaining SOS = true mean rating of opponents still scheduled
#
# No future information will be used in the eventual real model;
# here these are hidden-truth diagnostics only.
# ============================================================

calculate_true_sos_by_week <- function(
    schedule,
    true_ratings,
    teams,
    n_weeks
) {
  
  output_rows <- list()
  
  output_index <- 1
  
  for (target_week in 1:n_weeks) {
    
    for (current_team in teams) {
      
      team_games <- schedule[
        schedule$home_team == current_team |
          schedule$away_team == current_team,
      ]
      
      team_games$opponent <- ifelse(
        team_games$home_team == current_team,
        team_games$away_team,
        team_games$home_team
      )
      
      played_games <- team_games[
        team_games$week < target_week,
      ]
      
      remaining_games <- team_games[
        team_games$week >= target_week,
      ]
      
      played_sos <- if (
        nrow(played_games) > 0
      ) {
        mean(
          true_ratings[played_games$opponent]
        )
      } else {
        NA_real_
      }
      
      remaining_sos <- if (
        nrow(remaining_games) > 0
      ) {
        mean(
          true_ratings[remaining_games$opponent]
        )
      } else {
        NA_real_
      }
      
      output_rows[[output_index]] <- data.frame(
        target_week = target_week,
        team = current_team,
        games_played = nrow(played_games),
        games_remaining = nrow(remaining_games),
        true_played_SOS = as.numeric(played_sos),
        true_remaining_SOS = as.numeric(remaining_sos),
        stringsAsFactors = FALSE
      )
      
      output_index <- output_index + 1
    }
  }
  
  output <- do.call(
    rbind,
    output_rows
  )
  
  rownames(output) <- NULL
  
  return(output)
}


true_sos_by_week <- calculate_true_sos_by_week(
  schedule = schedule_8A,
  true_ratings = true_ratings,
  teams = teams,
  n_weeks = n_weeks
)


# ============================================================
# 22. SIMULATE GAME RESULTS
# ============================================================

simulate_scores <- function(
    schedule,
    true_ratings,
    true_hfa = 2.7,
    noise_sd = 8.5
) {
  
  games <- schedule
  
  games$true_home_rating <- as.numeric(
    true_ratings[games$home_team]
  )
  
  games$true_away_rating <- as.numeric(
    true_ratings[games$away_team]
  )
  
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
  
  losing_score <- pmax(
    0,
    round(
      rnorm(
        nrow(games),
        mean = 17,
        sd = 6
      )
    )
  )
  
  games$home_score <- NA_integer_
  games$away_score <- NA_integer_
  
  home_win <- games$actual_margin > 0
  
  games$away_score[home_win] <- losing_score[home_win]
  
  games$home_score[home_win] <- (
    games$away_score[home_win] +
      games$actual_margin[home_win]
  )
  
  away_win <- games$actual_margin < 0
  
  games$home_score[away_win] <- losing_score[away_win]
  
  games$away_score[away_win] <- (
    games$home_score[away_win] -
      games$actual_margin[away_win]
  )
  
  games <- games[
    order(
      games$week,
      games$game_id
    ),
  ]
  
  rownames(games) <- NULL
  
  return(games)
}


set.seed(8004)

simulated_games <- simulate_scores(
  schedule = schedule_8A,
  true_ratings = true_ratings,
  true_hfa = true_hfa,
  noise_sd = noise_sd
)

cat("\n========================================\n")
cat("FIRST 20 SIMULATED GAMES\n")
cat("========================================\n\n")

print(
  head(
    simulated_games,
    20
  )
)


# ============================================================
# 23. MATCHUP FREQUENCY MATRIX
# ============================================================

matchup_matrix <- matrix(
  0,
  nrow = length(teams),
  ncol = length(teams)
)

rownames(matchup_matrix) <- teams
colnames(matchup_matrix) <- teams

for (i in seq_len(nrow(schedule_8A))) {
  
  home <- schedule_8A$home_team[i]
  away <- schedule_8A$away_team[i]
  
  matchup_matrix[home, away] <- matchup_matrix[home, away] + 1
  matchup_matrix[away, home] <- matchup_matrix[away, home] + 1
}


# ============================================================
# 24. MATCHUP FREQUENCY HEATMAP
# ============================================================

matchup_plot_data <- expand.grid(
  team1 = teams,
  team2 = teams,
  stringsAsFactors = FALSE
)

matchup_plot_data$games <- mapply(
  function(team1, team2) {
    matchup_matrix[team1, team2]
  },
  matchup_plot_data$team1,
  matchup_plot_data$team2
)

matchup_heatmap <- ggplot(
  matchup_plot_data,
  aes(
    x = team1,
    y = team2,
    fill = games
  )
) +
  geom_tile() +
  geom_text(
    aes(label = games),
    size = 3
  ) +
  labs(
    title = "Version 8A Matchup Frequency Matrix",
    subtitle = "Every pair meets 0, 1, 2, or 3 times",
    x = "Team",
    y = "Opponent",
    fill = "Games"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1
    )
  )

print(matchup_heatmap)


# ============================================================
# 25. TRUE SOS PLOT
# ============================================================

sos_plot_data <- true_sos

sos_plot_data$team <- factor(
  sos_plot_data$team,
  levels = sos_plot_data$team[
    order(sos_plot_data$true_SOS_game_weighted)
  ]
)

sos_plot <- ggplot(
  sos_plot_data,
  aes(
    x = team,
    y = true_SOS_game_weighted
  )
) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Version 8A True Strength of Schedule",
    subtitle = "Mean hidden rating of all scheduled opponents",
    x = "Team",
    y = "True SOS"
  ) +
  theme_minimal()

print(sos_plot)


# ============================================================
# 26. MULTIPLE-SCHEDULE VALIDATION
# ============================================================
#
# Repeatedly generate schedules to confirm that the structural
# constraints always hold and to measure the amount of true SOS
# variation induced by random scheduling plus random team strength.
# ============================================================

set.seed(8005)

schedule_check_rows <- list()

for (simulation in 1:n_schedule_checks) {
  
  current_schedule_result <- generate_unbalanced_schedule(
    team_info = team_info,
    n_weeks = n_weeks
  )
  
  current_schedule <- current_schedule_result$schedule
  
  current_validation <- validate_schedule(
    schedule = current_schedule,
    team_info = team_info,
    n_weeks = n_weeks
  )
  
  current_ratings <- generate_true_ratings(
    teams = teams,
    target_sd = example_true_sd
  )
  
  current_sos <- calculate_true_sos(
    schedule = current_schedule,
    true_ratings = current_ratings,
    teams = teams,
    true_hfa = true_hfa
  )
  
  schedule_check_rows[[simulation]] <- data.frame(
    simulation = simulation,
    valid = current_validation$valid,
    connected = current_validation$connected,
    SOS_SD = sd(current_sos$true_SOS_game_weighted),
    SOS_range = max(current_sos$true_SOS_game_weighted) -
      min(current_sos$true_SOS_game_weighted),
    hardest_SOS = max(current_sos$true_SOS_game_weighted),
    easiest_SOS = min(current_sos$true_SOS_game_weighted),
    stringsAsFactors = FALSE
  )
}

schedule_check_results <- do.call(
  rbind,
  schedule_check_rows
)

schedule_check_summary <- data.frame(
  schedules_tested = n_schedule_checks,
  valid_percent = 100 * mean(schedule_check_results$valid),
  connected_percent = 100 * mean(schedule_check_results$connected),
  mean_SOS_SD = mean(schedule_check_results$SOS_SD),
  mean_SOS_range = mean(schedule_check_results$SOS_range),
  minimum_SOS_range = min(schedule_check_results$SOS_range),
  maximum_SOS_range = max(schedule_check_results$SOS_range)
)

cat("\n========================================\n")
cat("MULTIPLE-SCHEDULE VALIDATION\n")
cat("========================================\n\n")

print(schedule_check_summary)


# ============================================================
# 27. SAVE OUTPUTS
# ============================================================

write.csv(
  team_info,
  "version8A_team_info.csv",
  row.names = FALSE
)

write.csv(
  schedule_8A,
  "version8A_unbalanced_schedule.csv",
  row.names = FALSE
)

write.csv(
  schedule_validation$team_summary,
  "version8A_team_schedule_summary.csv",
  row.names = FALSE
)

write.csv(
  schedule_validation$pair_frequency,
  "version8A_pair_frequency.csv",
  row.names = FALSE
)

write.csv(
  week_summary,
  "version8A_week_summary.csv",
  row.names = FALSE
)

write.csv(
  true_rating_table,
  "version8A_true_ratings.csv",
  row.names = FALSE
)

write.csv(
  true_sos,
  "version8A_true_strength_of_schedule.csv",
  row.names = FALSE
)

write.csv(
  true_sos_by_week,
  "version8A_true_sos_by_week.csv",
  row.names = FALSE
)

write.csv(
  simulated_games,
  "version8A_simulated_games.csv",
  row.names = FALSE
)

write.csv(
  schedule_check_results,
  "version8A_schedule_validation_simulations.csv",
  row.names = FALSE
)

write.csv(
  schedule_check_summary,
  "version8A_schedule_validation_summary.csv",
  row.names = FALSE
)


# ============================================================
# 28. FINAL SUMMARY
# ============================================================

cat("\n\n========================================\n")
cat("VERSION 8A COMPLETE\n")
cat("========================================\n\n")

cat("Teams:", length(teams), "\n")
cat("Divisions:", length(divisions), "\n")
cat("Calendar weeks:", n_weeks, "\n")
cat("Games per team:", games_per_team, "\n")
cat("Total games:", nrow(schedule_8A), "\n")
cat("Home games per team: 6\n")
cat("Away games per team: 6\n")
cat("Distinct opponents per team: 8\n")
cat("Opponents not played per team: 7\n")
cat("Fixed recency half-life:", recency_half_life, "weeks\n")
cat("Hidden strength drift: 0\n")
cat("Schedule connected:", schedule_validation$connected, "\n")
cat("League-wide bye weeks:", paste(league_bye_weeks, collapse = ", "), "\n\n")

cat("PAIR FREQUENCY DISTRIBUTION:\n\n")
print(schedule_validation$frequency_distribution)

cat("\nTEAM SCHEDULE SUMMARY:\n\n")
print(schedule_validation$team_summary)

cat("\nTRUE SOS:\n\n")
print(true_sos)

cat("\nTRUE SOS SUMMARY:\n\n")
print(sos_summary)

cat("\nMULTIPLE-SCHEDULE VALIDATION SUMMARY:\n\n")
print(schedule_check_summary)

# ============================================================
# END VERSION 8A
# ============================================================

# ============================================================
# FOOTBALL POWER RATINGS - VERSION 9B-MC4
# FOUR-MODEL RESULTS-ONLY COMPARISON
# ============================================================
#
# PURPOSE
# -------
# Compare four rating models using only schedule + final scores:
#
#   Model 1: Current margin model + HL12 recency
#   Model 2: Current margin model + no recency
#   Model 3: Separate offense/defense score ridge model + HL12
#   Model 4: Separate offense/defense robust Huber model + HL12
#
# All four models are evaluated on the EXACT SAME simulated
# schedules and seasons within each noise scenario.
#
# The simulator generates latent offense and defense components
# but defines true overall team strength as:
#
#   R_i = Offense_i + Defense_i
#
# where positive Defense means better defense.
#
# Expected scoring is:
#
#   Home Score =
#     league_mean_points +
#     Offense_home -
#     Defense_away +
#     HFA / 2
#
#   Away Score =
#     league_mean_points +
#     Offense_away -
#     Defense_home -
#     HFA / 2
#
# Therefore:
#
#   E(Home Margin) =
#     R_home - R_away + HFA
#
# This means the current margin model is still correctly targeted,
# while Models 3/4 are allowed to use the extra structure contained
# in the two final scores.
#
# Two score-noise environments are tested:
#
#   1. Gaussian
#   2. Student-t with 4 df, scaled to the SAME score-error SD
#
# The heavy-tail scenario determines whether robust score fitting
# adds value when occasional extreme scores occur.
#
# IMPORTANT:
# No EPA, play-by-play, players, betting markets, recruiting,
# injuries, preseason priors, or external football information
# are used anywhere in this experiment.
#
# UNCERTAINTY COMPARISON
# ----------------------
# For each model and completed-games level:
#
#   - first half of evaluation seasons learns the empirical
#     95th percentile of |estimated rating - true rating|
#   - second half is an independent holdout test
#
# The resulting symmetric interval:
#
#   rating +/- empirical_95_half_width
#
# is judged by:
#
#   - actual holdout coverage
#   - full interval width
#
# This directly compares how narrow each model can be while
# remaining genuinely near 95% calibrated.
# ============================================================

library(ggplot2)


# ============================================================
# 1. GLOBAL SETTINGS
# ============================================================

n_weeks <- 14
games_per_team <- 12

true_hfa <- 2.7
hfa_prior <- 2.5

# The Gaussian score-error SD is chosen so independent home/away
# score errors imply an approximately 8.5-point margin-error SD.
target_margin_noise_sd <- 8.5
score_noise_sd <- target_margin_noise_sd / sqrt(2)

league_mean_points <- 24

target_true_rating_sd <- 5

# Modest unequal offense/defense variation. This lets game totals
# potentially contain some information about overall strength.
# Set both to 1.0 for a balanced-component sensitivity check.
offense_component_scale <- 1.15
defense_component_scale <- 0.85

recency_half_life <- 12

lambda_parity_threshold <- 4.5
adaptive_start_games <- 4

validate_each_generated_schedule <- FALSE

# ------------------------------------------------------------
# Experiment size
# ------------------------------------------------------------

quick_test <- FALSE

if (quick_test) {
  
  n_tuning_seasons <- 30
  n_evaluation_seasons_per_scenario <- 50
  
} else {
  
  n_tuning_seasons <- 200
  n_evaluation_seasons_per_scenario <- 500
}

# Tuning grids for Models 3 and 4.
score_lambda_grid <- c(
  0.5,
  1,
  2,
  3,
  4,
  6,
  8
)

huber_k_grid <- c(
  1.0,
  1.345,
  1.5,
  2.0
)

# Tune score-model hyperparameters across early/mid/late season
# rather than optimizing only the final week.
tuning_completed_games <- c(
  4,
  8,
  12
)

max_huber_iterations <- 40
huber_tolerance <- 1e-6

noise_scenarios <- c(
  "Gaussian",
  "Student_t4"
)

model_names <- c(
  "Margin HL12",
  "Margin No Decay",
  "Score OffDef Ridge",
  "Score OffDef Huber"
)

model_colors <- setNames(
  grDevices::hcl.colors(
    length(model_names),
    palette = "Dark 3"
  ),
  model_names
)

# Separate seeds keep tuning and evaluation independent.
tuning_seed_base <- 940100
evaluation_seed_base <- 940500


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
# 16. EMBED THE VALIDATED VERSION 8C PARITY CALIBRATION
# ============================================================
#
# These coefficients are the successful 16-team unbalanced-schedule
# calibration already used in Versions 8C/9A/9B.
# This keeps Models 1 and 2 identical to the current rating engine
# without rerunning 2,100 parity-calibration seasons first.
# ============================================================

parity_calibration_coefficients <- data.frame(
  completed_games_per_team = 2:12,
  
  intercept = c(
    -0.1479266,
    -0.8059229,
    -0.9604493,
    -0.9826548,
    -0.9488747,
    -0.8797497,
    -0.8485264,
    -0.8273144,
    -0.8110207,
    -0.7374405,
    -0.6808297
  ),
  
  slope = c(
    1.434181,
    1.440457,
    1.390703,
    1.350912,
    1.309762,
    1.272636,
    1.251089,
    1.234739,
    1.219218,
    1.198353,
    1.181486
  ),
  
  r_squared = c(
    0.4844963,
    0.6059096,
    0.6939443,
    0.7442485,
    0.7825732,
    0.8165527,
    0.8382563,
    0.8592568,
    0.8750468,
    0.8865816,
    0.8964938
  ),
  
  calibration_RMSE = c(
    1.4359718,
    1.2555325,
    1.1064461,
    1.0114375,
    0.9325809,
    0.8566149,
    0.8043475,
    0.7503153,
    0.7069745,
    0.6735529,
    0.6434476
  )
)


# ============================================================
# 17. GENERATE LATENT OFFENSE / DEFENSE WITH EXACT OVERALL SD
# ============================================================

generate_true_components <- function(
    league_teams,
    target_sd,
    offense_scale = 1.15,
    defense_scale = 0.85
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
  
  true_offense <- true_offense - mean(true_offense)
  true_defense <- true_defense - mean(true_defense)
  
  raw_rating <- true_offense + true_defense
  
  scaling_factor <- target_sd / sd(raw_rating)
  
  true_offense <- true_offense * scaling_factor
  true_defense <- true_defense * scaling_factor
  
  true_rating <- true_offense + true_defense
  true_rating <- true_rating - mean(true_rating)
  
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
# 18. SCORE-ERROR GENERATOR
# ============================================================

generate_score_errors <- function(
    n,
    noise_scenario,
    target_sd
) {
  
  if (noise_scenario == "Gaussian") {
    
    return(
      rnorm(
        n,
        mean = 0,
        sd = target_sd
      )
    )
  }
  
  if (noise_scenario == "Student_t4") {
    
    degrees_freedom <- 4
    
    # Standard t(df=4) variance is 4 / (4 - 2) = 2.
    # Scale it so the resulting SD matches target_sd.
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
# 19. SIMULATE ONE SCORE-BASED SEASON
# ============================================================

simulate_score_season <- function(
    schedule,
    true_components,
    noise_scenario,
    true_hfa = 2.7,
    mean_points = 24,
    score_error_sd = score_noise_sd
) {
  
  games <- schedule
  
  true_offense <- true_components$offense
  true_defense <- true_components$defense
  true_ratings <- true_components$rating
  
  games$true_home_rating <- true_ratings[
    games$home_team
  ]
  
  games$true_away_rating <- true_ratings[
    games$away_team
  ]
  
  games$expected_home_score <- (
    mean_points +
      true_offense[
        games$home_team
      ] -
      true_defense[
        games$away_team
      ] +
      true_hfa / 2
  )
  
  games$expected_away_score <- (
    mean_points +
      true_offense[
        games$away_team
      ] -
      true_defense[
        games$home_team
      ] -
      true_hfa / 2
  )
  
  home_error <- generate_score_errors(
    n = nrow(games),
    noise_scenario = noise_scenario,
    target_sd = score_error_sd
  )
  
  away_error <- generate_score_errors(
    n = nrow(games),
    noise_scenario = noise_scenario,
    target_sd = score_error_sd
  )
  
  games$home_score <- round(
    pmax(
      0,
      games$expected_home_score +
        home_error
    )
  )
  
  games$away_score <- round(
    pmax(
      0,
      games$expected_away_score +
        away_error
    )
  )
  
  games$actual_margin <- (
    games$home_score -
      games$away_score
  )
  
  games$true_expected_margin <- (
    games$true_home_rating -
      games$true_away_rating +
      true_hfa
  )
  
  return(games)
}


# ============================================================
# 20. CURRENT MODEL HELPERS
# ============================================================

get_hfa_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <- (
    2 * nrow(data)
  ) /
    length(league_teams)
  
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


get_dynamic_team_lambda <- function(
    data,
    league_teams
) {
  
  avg_games_played <- (
    2 * nrow(data)
  ) /
    length(league_teams)
  
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
  }
  
  return(2)
}


choose_cap_smooth <- function(
    parity_sd
) {
  
  cap <- 14 +
    5.25 *
    (
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
  
  y_weighted <- y *
    sqrt_weights
  
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
  
  ratings <- beta[
    league_teams
  ]
  
  ratings <- ratings -
    mean(ratings)
  
  names(ratings) <- league_teams
  
  return(
    list(
      ratings = ratings,
      home_field = as.numeric(
        beta["HFA"]
      )
    )
  )
}


fit_raw_parity_model <- function(
    data,
    league_teams
) {
  
  current_hfa_lambda <- get_hfa_lambda(
    data = data,
    league_teams = league_teams
  )
  
  return(
    fit_opponent_adjusted_ratings(
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
  )
}


calibrate_parity_sd <- function(
    observed_sd,
    completed_games_per_team,
    coefficients = parity_calibration_coefficients
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
  
  return(
    max(
      0,
      as.numeric(
        estimate
      )
    )
  )
}


get_current_model_settings <- function(
    training_games,
    league_teams
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
    completed_games_per_team = completed_games_per_team
  )
  
  return(
    list(
      completed_games_per_team = completed_games_per_team,
      estimated_parity_SD = estimated_parity,
      margin_cap = choose_cap_smooth(
        estimated_parity
      ),
      team_lambda = choose_team_lambda(
        parity_sd = estimated_parity,
        training_games = training_games,
        league_teams = league_teams
      ),
      hfa_lambda = current_hfa_lambda
    )
  )
}


# ============================================================
# 21. SCORE OFFENSE / DEFENSE DESIGN MATRIX
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
# 22. WEIGHTED RIDGE SOLVER
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
# 23. NORMAL OFFENSE / DEFENSE SCORE RIDGE MODEL
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
# 24. ROBUST HUBER OFFENSE / DEFENSE SCORE MODEL
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
# 25. SAFE CORRELATION
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
# 26. GENERATE ONE INDEPENDENT SEASON
# ============================================================

generate_experiment_season <- function(
    season_seed,
    noise_scenario
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
  
  true_components <- generate_true_components(
    league_teams = teams,
    target_sd = target_true_rating_sd,
    offense_scale = offense_component_scale,
    defense_scale = defense_component_scale
  )
  
  games <- simulate_score_season(
    schedule = schedule,
    true_components = true_components,
    noise_scenario = noise_scenario,
    true_hfa = true_hfa,
    mean_points = league_mean_points,
    score_error_sd = score_noise_sd
  )
  
  return(
    list(
      schedule = schedule,
      games = games,
      true_components = true_components
    )
  )
}


# ============================================================
# 27. SELECT TRAINING DATA AFTER N COMPLETED GAMES / TEAM
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
# 28. FIT ALL FOUR MODELS TO ONE SNAPSHOT
# ============================================================

fit_four_models <- function(
    training_games,
    league_teams,
    score_component_lambda,
    robust_component_lambda,
    robust_huber_k
) {
  
  settings <- get_current_model_settings(
    training_games = training_games,
    league_teams = league_teams
  )
  
  hl12_weights <- make_recency_weights(
    training_games = training_games,
    half_life = recency_half_life
  )
  
  no_decay_weights <- rep(
    1,
    nrow(training_games)
  )
  
  model_1 <- fit_opponent_adjusted_ratings(
    data = training_games,
    league_teams = league_teams,
    margin_cap = settings$margin_cap,
    team_lambda = settings$team_lambda,
    hfa_lambda = settings$hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = hl12_weights
  )
  
  model_2 <- fit_opponent_adjusted_ratings(
    data = training_games,
    league_teams = league_teams,
    margin_cap = settings$margin_cap,
    team_lambda = settings$team_lambda,
    hfa_lambda = settings$hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = no_decay_weights
  )
  
  model_3 <- fit_score_offdef_ridge(
    data = training_games,
    league_teams = league_teams,
    component_lambda = score_component_lambda,
    hfa_lambda = settings$hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = hl12_weights
  )
  
  model_4 <- fit_score_offdef_huber(
    data = training_games,
    league_teams = league_teams,
    component_lambda = robust_component_lambda,
    huber_k = robust_huber_k,
    hfa_lambda = settings$hfa_lambda,
    hfa_prior = hfa_prior,
    game_weights = hl12_weights,
    max_iterations = max_huber_iterations,
    tolerance = huber_tolerance
  )
  
  return(
    list(
      "Margin HL12" = model_1,
      "Margin No Decay" = model_2,
      "Score OffDef Ridge" = model_3,
      "Score OffDef Huber" = model_4
    )
  )
}


# ============================================================
# 29. TUNE SCORE-MODEL HYPERPARAMETERS ON INDEPENDENT SEASONS
# ============================================================
#
# One common production setting is learned from a 50/50 mixture
# of Gaussian and Student-t4 tuning seasons.
#
# Tuning minimizes pooled rating RMSE across 4, 8, and 12
# completed games per team. Models 1 and 2 are already locked
# and are not retuned.
# ============================================================

cat("\n========================================\n")
cat("VERSION 9B-MC4 FOUR-MODEL COMPARISON\n")
cat("========================================\n\n")

cat("Teams:", length(teams), "\n")
cat("Games per team:", games_per_team, "\n")
cat("True overall rating SD:", target_true_rating_sd, "\n")
cat("True HFA:", true_hfa, "\n")
cat("Target Gaussian margin-noise SD:", target_margin_noise_sd, "\n")
cat(
  "Latent offense/defense scales:",
  offense_component_scale,
  "/",
  defense_component_scale,
  "\n"
)
cat("Tuning seasons:", n_tuning_seasons, "\n")
cat(
  "Evaluation seasons per noise scenario:",
  n_evaluation_seasons_per_scenario,
  "\n\n"
)


ridge_tuning_rows <- list()
robust_tuning_rows <- list()

ridge_tuning_index <- 1
robust_tuning_index <- 1

for (simulation in seq_len(n_tuning_seasons)) {
  
  if (
    simulation == 1 ||
    simulation %% max(
      1,
      floor(
        n_tuning_seasons / 5
      )
    ) == 0
  ) {
    
    cat(
      "Tuning season",
      simulation,
      "of",
      n_tuning_seasons,
      "\n"
    )
  }
  
  noise_scenario <- noise_scenarios[
    1 +
      (
        simulation - 1
      ) %%
      length(noise_scenarios)
  ]
  
  season <- generate_experiment_season(
    season_seed = tuning_seed_base +
      simulation,
    noise_scenario = noise_scenario
  )
  
  truth <- season$true_components$rating[
    teams
  ]
  
  for (
    tuning_games in tuning_completed_games
  ) {
    
    snapshot <- get_training_snapshot(
      games = season$games,
      completed_games_per_team = tuning_games
    )
    
    training_games <- snapshot$training_games
    
    hfa_lambda <- get_hfa_lambda(
      data = training_games,
      league_teams = teams
    )
    
    game_weights <- make_recency_weights(
      training_games = training_games,
      half_life = recency_half_life
    )
    
    for (current_lambda in score_lambda_grid) {
      
      ridge_fit <- fit_score_offdef_ridge(
        data = training_games,
        league_teams = teams,
        component_lambda = current_lambda,
        hfa_lambda = hfa_lambda,
        hfa_prior = hfa_prior,
        game_weights = game_weights
      )
      
      ridge_errors <- ridge_fit$ratings[
        teams
      ] -
        truth
      
      ridge_tuning_rows[[ridge_tuning_index]] <- data.frame(
        simulation = simulation,
        noise_scenario = noise_scenario,
        completed_games_per_team = tuning_games,
        component_lambda = current_lambda,
        squared_error_sum = sum(
          ridge_errors^2
        ),
        absolute_error_sum = sum(
          abs(
            ridge_errors
          )
        ),
        n_teams = length(teams)
      )
      
      ridge_tuning_index <- ridge_tuning_index + 1
    }
    
    for (current_lambda in score_lambda_grid) {
      
      for (current_k in huber_k_grid) {
        
        robust_fit <- fit_score_offdef_huber(
          data = training_games,
          league_teams = teams,
          component_lambda = current_lambda,
          huber_k = current_k,
          hfa_lambda = hfa_lambda,
          hfa_prior = hfa_prior,
          game_weights = game_weights,
          max_iterations = max_huber_iterations,
          tolerance = huber_tolerance
        )
        
        robust_errors <- robust_fit$ratings[
          teams
        ] -
          truth
        
        robust_tuning_rows[[robust_tuning_index]] <- data.frame(
          simulation = simulation,
          noise_scenario = noise_scenario,
          completed_games_per_team = tuning_games,
          component_lambda = current_lambda,
          huber_k = current_k,
          squared_error_sum = sum(
            robust_errors^2
          ),
          absolute_error_sum = sum(
            abs(
              robust_errors
            )
          ),
          n_teams = length(teams)
        )
        
        robust_tuning_index <- robust_tuning_index + 1
      }
    }
  }
}


ridge_tuning_raw <- do.call(
  rbind,
  ridge_tuning_rows
)

robust_tuning_raw <- do.call(
  rbind,
  robust_tuning_rows
)

ridge_tuning_summary <- aggregate(
  cbind(
    squared_error_sum,
    absolute_error_sum,
    n_teams
  ) ~
    component_lambda,
  data = ridge_tuning_raw,
  FUN = sum
)

ridge_tuning_summary$RMSE <- sqrt(
  ridge_tuning_summary$squared_error_sum /
    ridge_tuning_summary$n_teams
)

ridge_tuning_summary$MAE <- (
  ridge_tuning_summary$absolute_error_sum /
    ridge_tuning_summary$n_teams
)

ridge_tuning_summary <- ridge_tuning_summary[
  order(
    ridge_tuning_summary$RMSE
  ),
]

best_score_lambda <- ridge_tuning_summary$component_lambda[1]


robust_tuning_summary <- aggregate(
  cbind(
    squared_error_sum,
    absolute_error_sum,
    n_teams
  ) ~
    component_lambda +
    huber_k,
  data = robust_tuning_raw,
  FUN = sum
)

robust_tuning_summary$RMSE <- sqrt(
  robust_tuning_summary$squared_error_sum /
    robust_tuning_summary$n_teams
)

robust_tuning_summary$MAE <- (
  robust_tuning_summary$absolute_error_sum /
    robust_tuning_summary$n_teams
)

robust_tuning_summary <- robust_tuning_summary[
  order(
    robust_tuning_summary$RMSE
  ),
]

best_robust_lambda <- robust_tuning_summary$component_lambda[1]
best_huber_k <- robust_tuning_summary$huber_k[1]


cat("\nBEST NORMAL SCORE MODEL:\n\n")

print(
  ridge_tuning_summary[
    seq_len(
      min(
        7,
        nrow(
          ridge_tuning_summary
        )
      )
    ),
  ]
)

cat(
  "\nSelected component lambda:",
  best_score_lambda,
  "\n"
)


cat("\nBEST ROBUST SCORE MODEL:\n\n")

print(
  robust_tuning_summary[
    seq_len(
      min(
        10,
        nrow(
          robust_tuning_summary
        )
      )
    ),
  ]
)

cat(
  "\nSelected robust component lambda:",
  best_robust_lambda,
  "\n"
)

cat(
  "Selected Huber k:",
  best_huber_k,
  "\n\n"
)


write.csv(
  ridge_tuning_summary,
  "version9B_MC4_score_ridge_tuning.csv",
  row.names = FALSE
)

write.csv(
  robust_tuning_summary,
  "version9B_MC4_score_huber_tuning.csv",
  row.names = FALSE
)


# ============================================================
# 30. PAIRED FOUR-MODEL EVALUATION
# ============================================================

evaluation_rows <- list()
season_metric_rows <- list()

evaluation_index <- 1
season_metric_index <- 1

for (scenario_index in seq_along(noise_scenarios)) {
  
  noise_scenario <- noise_scenarios[
    scenario_index
  ]
  
  cat("\n========================================\n")
  cat(
    "EVALUATING NOISE SCENARIO:",
    noise_scenario,
    "\n"
  )
  cat("========================================\n\n")
  
  for (
    simulation in seq_len(
      n_evaluation_seasons_per_scenario
    )
  ) {
    
    if (
      simulation == 1 ||
      simulation %% max(
        1,
        floor(
          n_evaluation_seasons_per_scenario / 5
        )
      ) == 0
    ) {
      
      cat(
        noise_scenario,
        "season",
        simulation,
        "of",
        n_evaluation_seasons_per_scenario,
        "\n"
      )
    }
    
    season_seed <- (
      evaluation_seed_base +
        scenario_index * 100000 +
        simulation
    )
    
    season <- generate_experiment_season(
      season_seed = season_seed,
      noise_scenario = noise_scenario
    )
    
    true_ratings <- season$true_components$rating[
      teams
    ]
    
    true_ranks <- as.integer(
      rank(
        -true_ratings,
        ties.method = "min"
      )
    )
    
    names(true_ranks) <- teams
    
    for (
      completed_games in seq_len(
        games_per_team
      )
    ) {
      
      snapshot <- get_training_snapshot(
        games = season$games,
        completed_games_per_team = completed_games
      )
      
      model_fits <- fit_four_models(
        training_games = snapshot$training_games,
        league_teams = teams,
        score_component_lambda = best_score_lambda,
        robust_component_lambda = best_robust_lambda,
        robust_huber_k = best_huber_k
      )
      
      for (current_model in model_names) {
        
        estimated_ratings <- model_fits[[current_model]]$ratings[
          teams
        ]
        
        rating_error <- (
          estimated_ratings -
            true_ratings
        )
        
        estimated_ranks <- as.integer(
          rank(
            -estimated_ratings,
            ties.method = "min"
          )
        )
        
        names(
          estimated_ranks
        ) <- teams
        
        rank_error <- (
          estimated_ranks -
            true_ranks
        )
        
        true_top4 <- names(
          sort(
            true_ranks
          )
        )[1:4]
        
        estimated_top4 <- names(
          sort(
            setNames(
              estimated_ranks,
              teams
            )
          )
        )[1:4]
        
        number1_correct <- as.numeric(
          names(
            estimated_ranks
          )[
            which.min(
              estimated_ranks
            )
          ] ==
            names(
              true_ranks
            )[
              which.min(
                true_ranks
              )
            ]
        )
        
        for (team_index in seq_along(teams)) {
          
          current_team <- teams[
            team_index
          ]
          
          evaluation_rows[[evaluation_index]] <- data.frame(
            noise_scenario = noise_scenario,
            simulation = simulation,
            completed_games_per_team = completed_games,
            calendar_week = snapshot$calendar_week,
            model = current_model,
            team = current_team,
            true_rating = true_ratings[
              current_team
            ],
            estimated_rating = estimated_ratings[
              current_team
            ],
            rating_error = rating_error[
              current_team
            ],
            absolute_rating_error = abs(
              rating_error[
                current_team
              ]
            ),
            squared_rating_error = rating_error[
              current_team
            ]^2,
            true_rank = true_ranks[
              current_team
            ],
            estimated_rank = estimated_ranks[
              team_index
            ],
            absolute_rank_error = abs(
              rank_error[
                team_index
              ]
            ),
            stringsAsFactors = FALSE
          )
          
          evaluation_index <- evaluation_index + 1
        }
        
        season_metric_rows[[season_metric_index]] <- data.frame(
          noise_scenario = noise_scenario,
          simulation = simulation,
          completed_games_per_team = completed_games,
          calendar_week = snapshot$calendar_week,
          model = current_model,
          season_RMSE = sqrt(
            mean(
              rating_error^2
            )
          ),
          season_MAE = mean(
            abs(
              rating_error
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
          ),
          mean_absolute_rank_error = mean(
            abs(
              rank_error
            )
          ),
          top4_overlap = length(
            intersect(
              true_top4,
              estimated_top4
            )
          ) /
            4,
          number1_correct = number1_correct,
          estimated_HFA = model_fits[[current_model]]$home_field,
          stringsAsFactors = FALSE
        )
        
        season_metric_index <- season_metric_index + 1
      }
    }
  }
}


evaluation_team_level <- do.call(
  rbind,
  evaluation_rows
)

evaluation_season_level <- do.call(
  rbind,
  season_metric_rows
)

rownames(
  evaluation_team_level
) <- NULL

rownames(
  evaluation_season_level
) <- NULL


# ============================================================
# 31. AGGREGATE ACCURACY BY MODEL / SCENARIO / GAMES
# ============================================================

accuracy_keys <- unique(
  evaluation_team_level[
    ,
    c(
      "noise_scenario",
      "completed_games_per_team",
      "model"
    )
  ]
)

accuracy_rows <- list()

for (i in seq_len(nrow(accuracy_keys))) {
  
  current_scenario <- accuracy_keys$noise_scenario[i]
  current_games <- accuracy_keys$completed_games_per_team[i]
  current_model <- accuracy_keys$model[i]
  
  team_subset <- evaluation_team_level[
    evaluation_team_level$noise_scenario ==
      current_scenario &
      evaluation_team_level$completed_games_per_team ==
      current_games &
      evaluation_team_level$model ==
      current_model,
  ]
  
  season_subset <- evaluation_season_level[
    evaluation_season_level$noise_scenario ==
      current_scenario &
      evaluation_season_level$completed_games_per_team ==
      current_games &
      evaluation_season_level$model ==
      current_model,
  ]
  
  accuracy_rows[[i]] <- data.frame(
    noise_scenario = current_scenario,
    completed_games_per_team = current_games,
    mean_calendar_week = mean(
      team_subset$calendar_week
    ),
    model = current_model,
    
    Rating_RMSE = sqrt(
      mean(
        team_subset$squared_rating_error
      )
    ),
    
    Rating_MAE = mean(
      team_subset$absolute_rating_error
    ),
    
    Rating_Bias = mean(
      team_subset$rating_error
    ),
    
    Mean_Season_RMSE = mean(
      season_subset$season_RMSE
    ),
    
    Median_Season_RMSE = median(
      season_subset$season_RMSE
    ),
    
    Mean_Season_MAE = mean(
      season_subset$season_MAE
    ),
    
    Mean_Pearson = mean(
      season_subset$Pearson,
      na.rm = TRUE
    ),
    
    Mean_Spearman = mean(
      season_subset$Spearman,
      na.rm = TRUE
    ),
    
    Mean_Absolute_Rank_Error = mean(
      season_subset$mean_absolute_rank_error
    ),
    
    Mean_Top4_Overlap = mean(
      season_subset$top4_overlap
    ),
    
    Probability_Number1_Correct = mean(
      season_subset$number1_correct
    ),
    
    Probability_Within_1_Point = mean(
      team_subset$absolute_rating_error <=
        1
    ),
    
    Probability_Within_2_Points = mean(
      team_subset$absolute_rating_error <=
        2
    ),
    
    Probability_Within_3_Points = mean(
      team_subset$absolute_rating_error <=
        3
    ),
    
    Mean_Estimated_HFA = mean(
      season_subset$estimated_HFA
    ),
    
    Mean_Absolute_HFA_Error = mean(
      abs(
        season_subset$estimated_HFA -
          true_hfa
      )
    ),
    
    stringsAsFactors = FALSE
  )
}

accuracy_summary <- do.call(
  rbind,
  accuracy_rows
)

accuracy_summary <- accuracy_summary[
  order(
    accuracy_summary$noise_scenario,
    accuracy_summary$completed_games_per_team,
    match(
      accuracy_summary$model,
      model_names
    )
  ),
]

rownames(
  accuracy_summary
) <- NULL


# ============================================================
# 32. EMPIRICALLY CALIBRATED 95% ERROR INTERVALS
# ============================================================
#
# Evaluation seasons are split in half:
#
#   first half  -> learn 95th percentile absolute error
#   second half -> independently test coverage
#
# This is done separately for every:
#
#   noise scenario x model x games played
# ============================================================

calibration_cutoff <- floor(
  n_evaluation_seasons_per_scenario /
    2
)

interval_rows <- list()
interval_index <- 1

for (noise_scenario in noise_scenarios) {
  
  for (completed_games in seq_len(games_per_team)) {
    
    for (current_model in model_names) {
      
      calibration_data <- evaluation_team_level[
        evaluation_team_level$noise_scenario ==
          noise_scenario &
          evaluation_team_level$completed_games_per_team ==
          completed_games &
          evaluation_team_level$model ==
          current_model &
          evaluation_team_level$simulation <=
          calibration_cutoff,
      ]
      
      holdout_data <- evaluation_team_level[
        evaluation_team_level$noise_scenario ==
          noise_scenario &
          evaluation_team_level$completed_games_per_team ==
          completed_games &
          evaluation_team_level$model ==
          current_model &
          evaluation_team_level$simulation >
          calibration_cutoff,
      ]
      
      empirical_half_width <- as.numeric(
        quantile(
          calibration_data$absolute_rating_error,
          probs = 0.95,
          names = FALSE,
          type = 8
        )
      )
      
      holdout_coverage <- mean(
        holdout_data$absolute_rating_error <=
          empirical_half_width
      )
      
      interval_rows[[interval_index]] <- data.frame(
        noise_scenario = noise_scenario,
        completed_games_per_team = completed_games,
        model = current_model,
        calibration_team_observations = nrow(
          calibration_data
        ),
        holdout_team_observations = nrow(
          holdout_data
        ),
        empirical_95_half_width = empirical_half_width,
        empirical_95_full_width = 2 *
          empirical_half_width,
        holdout_coverage = holdout_coverage,
        holdout_RMSE = sqrt(
          mean(
            holdout_data$squared_rating_error
          )
        ),
        holdout_MAE = mean(
          holdout_data$absolute_rating_error
        ),
        stringsAsFactors = FALSE
      )
      
      interval_index <- interval_index + 1
    }
  }
}

interval_summary <- do.call(
  rbind,
  interval_rows
)

interval_summary <- interval_summary[
  order(
    interval_summary$noise_scenario,
    interval_summary$completed_games_per_team,
    match(
      interval_summary$model,
      model_names
    )
  ),
]

rownames(
  interval_summary
) <- NULL


# ============================================================
# 33. FINAL-WEEK PAIRED DIFFERENCES VS CURRENT MODEL
# ============================================================

final_season_metrics <- evaluation_season_level[
  evaluation_season_level$completed_games_per_team ==
    games_per_team,
]

paired_rows <- list()
paired_index <- 1

for (noise_scenario in noise_scenarios) {
  
  baseline <- final_season_metrics[
    final_season_metrics$noise_scenario ==
      noise_scenario &
      final_season_metrics$model ==
      "Margin HL12",
  ]
  
  baseline <- baseline[
    order(
      baseline$simulation
    ),
  ]
  
  for (current_model in model_names) {
    
    comparison <- final_season_metrics[
      final_season_metrics$noise_scenario ==
        noise_scenario &
        final_season_metrics$model ==
        current_model,
    ]
    
    comparison <- comparison[
      order(
        comparison$simulation
      ),
    ]
    
    if (
      !identical(
        baseline$simulation,
        comparison$simulation
      )
    ) {
      
      stop(
        "Paired simulation IDs do not match."
      )
    }
    
    rmse_difference <- (
      comparison$season_RMSE -
        baseline$season_RMSE
    )
    
    mae_difference <- (
      comparison$season_MAE -
        baseline$season_MAE
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
    
    paired_rows[[paired_index]] <- data.frame(
      noise_scenario = noise_scenario,
      model = current_model,
      
      mean_RMSE_difference_vs_Margin_HL12 = mean(
        rmse_difference
      ),
      
      RMSE_MCSE = rmse_mcse,
      
      RMSE_CI_lower = mean(
        rmse_difference
      ) -
        1.96 *
        rmse_mcse,
      
      RMSE_CI_upper = mean(
        rmse_difference
      ) +
        1.96 *
        rmse_mcse,
      
      probability_lower_RMSE_than_Margin_HL12 = mean(
        rmse_difference <
          0
      ),
      
      mean_MAE_difference_vs_Margin_HL12 = mean(
        mae_difference
      ),
      
      MAE_MCSE = mae_mcse,
      
      MAE_CI_lower = mean(
        mae_difference
      ) -
        1.96 *
        mae_mcse,
      
      MAE_CI_upper = mean(
        mae_difference
      ) +
        1.96 *
        mae_mcse,
      
      probability_lower_MAE_than_Margin_HL12 = mean(
        mae_difference <
          0
      ),
      
      stringsAsFactors = FALSE
    )
    
    paired_index <- paired_index + 1
  }
}

paired_final_comparison <- do.call(
  rbind,
  paired_rows
)

rownames(
  paired_final_comparison
) <- NULL


# ============================================================
# 34. FINAL 12-GAME MODEL SUMMARY
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
      "noise_scenario",
      "model",
      "empirical_95_half_width",
      "empirical_95_full_width",
      "holdout_coverage"
    )
  ],
  by = c(
    "noise_scenario",
    "model"
  ),
  all.x = TRUE
)

final_summary <- final_summary[
  order(
    final_summary$noise_scenario,
    final_summary$Rating_RMSE
  ),
]

rownames(
  final_summary
) <- NULL


# ============================================================
# 35. PRINT RESULTS
# ============================================================

cat("\n========================================\n")
cat("ACCURACY BY COMPLETED GAMES / TEAM\n")
cat("========================================\n\n")

print(
  accuracy_summary
)


cat("\n========================================\n")
cat("EMPIRICALLY CALIBRATED 95% INTERVALS\n")
cat("========================================\n\n")

print(
  interval_summary
)


cat("\n========================================\n")
cat("FINAL-WEEK PAIRED DIFFERENCES VS MARGIN HL12\n")
cat("========================================\n\n")

print(
  paired_final_comparison
)


cat("\n========================================\n")
cat("FINAL 12-GAME MODEL COMPARISON\n")
cat("========================================\n\n")

print(
  final_summary[
    ,
    c(
      "noise_scenario",
      "model",
      "Rating_RMSE",
      "Rating_MAE",
      "Mean_Pearson",
      "Mean_Spearman",
      "Mean_Absolute_Rank_Error",
      "Mean_Top4_Overlap",
      "Probability_Number1_Correct",
      "Probability_Within_1_Point",
      "Probability_Within_2_Points",
      "Probability_Within_3_Points",
      "empirical_95_full_width",
      "holdout_coverage"
    )
  ]
)


# ============================================================
# 36. IDENTIFY WINNERS
# ============================================================

winner_rows <- list()

for (i in seq_along(noise_scenarios)) {
  
  current_scenario <- noise_scenarios[
    i
  ]
  
  current <- final_summary[
    final_summary$noise_scenario ==
      current_scenario,
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
  
  best_width_row <- current[
    which.min(
      current$empirical_95_full_width
    ),
  ]
  
  winner_rows[[i]] <- data.frame(
    noise_scenario = current_scenario,
    best_RMSE_model = best_rmse_row$model,
    best_RMSE = best_rmse_row$Rating_RMSE,
    best_MAE_model = best_mae_row$model,
    best_MAE = best_mae_row$Rating_MAE,
    narrowest_calibrated_95_model = best_width_row$model,
    calibrated_95_width = best_width_row$empirical_95_full_width,
    calibrated_95_coverage = best_width_row$holdout_coverage,
    stringsAsFactors = FALSE
  )
}

winner_summary <- do.call(
  rbind,
  winner_rows
)

cat("\n========================================\n")
cat("MODEL WINNERS\n")
cat("========================================\n\n")

print(
  winner_summary
)


# ============================================================
# 37. SAVE CSV OUTPUTS
# ============================================================

write.csv(
  evaluation_team_level,
  "version9B_MC4_team_level_results.csv",
  row.names = FALSE
)

write.csv(
  evaluation_season_level,
  "version9B_MC4_season_level_results.csv",
  row.names = FALSE
)

write.csv(
  accuracy_summary,
  "version9B_MC4_accuracy_by_games.csv",
  row.names = FALSE
)

write.csv(
  interval_summary,
  "version9B_MC4_calibrated_interval_comparison.csv",
  row.names = FALSE
)

write.csv(
  paired_final_comparison,
  "version9B_MC4_paired_final_comparison.csv",
  row.names = FALSE
)

write.csv(
  final_summary,
  "version9B_MC4_final_model_summary.csv",
  row.names = FALSE
)

write.csv(
  winner_summary,
  "version9B_MC4_winner_summary.csv",
  row.names = FALSE
)


# ============================================================
# 38. PLOTS
# ============================================================

rmse_plot <- ggplot(
  accuracy_summary,
  aes(
    x = completed_games_per_team,
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
  facet_wrap(
    ~ noise_scenario
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  labs(
    title = "Four-Model Rating RMSE",
    subtitle = "Lower is better; paired simulated seasons",
    x = "Completed Games per Team",
    y = "Rating RMSE",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC4_rating_RMSE.png",
  plot = rmse_plot,
  width = 11,
  height = 6,
  dpi = 160
)


mae_plot <- ggplot(
  accuracy_summary,
  aes(
    x = completed_games_per_team,
    y = Rating_MAE,
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
  facet_wrap(
    ~ noise_scenario
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  labs(
    title = "Four-Model Rating MAE",
    subtitle = "Lower is better",
    x = "Completed Games per Team",
    y = "Rating MAE",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC4_rating_MAE.png",
  plot = mae_plot,
  width = 11,
  height = 6,
  dpi = 160
)


correlation_plot <- ggplot(
  accuracy_summary,
  aes(
    x = completed_games_per_team,
    y = Mean_Pearson,
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
  facet_wrap(
    ~ noise_scenario
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  labs(
    title = "Estimated vs True Rating Correlation",
    subtitle = "Higher is better",
    x = "Completed Games per Team",
    y = "Mean Pearson Correlation",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC4_rating_correlation.png",
  plot = correlation_plot,
  width = 11,
  height = 6,
  dpi = 160
)


interval_plot <- ggplot(
  interval_summary,
  aes(
    x = completed_games_per_team,
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
  facet_wrap(
    ~ noise_scenario
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  labs(
    title = "Empirically Calibrated 95% Rating-Interval Width",
    subtitle = "Lower is better when holdout coverage stays near 95%",
    x = "Completed Games per Team",
    y = "Full 95% Interval Width",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC4_calibrated_interval_width.png",
  plot = interval_plot,
  width = 11,
  height = 6,
  dpi = 160
)


coverage_plot <- ggplot(
  interval_summary,
  aes(
    x = completed_games_per_team,
    y = holdout_coverage,
    color = model,
    group = model
  )
) +
  geom_hline(
    yintercept = 0.95,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  facet_wrap(
    ~ noise_scenario
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  scale_x_continuous(
    breaks = 1:games_per_team
  ) +
  labs(
    title = "Independent Holdout Coverage",
    subtitle = "Intervals were calibrated to 95% using separate seasons",
    x = "Completed Games per Team",
    y = "Holdout Coverage",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC4_holdout_coverage.png",
  plot = coverage_plot,
  width = 11,
  height = 6,
  dpi = 160
)


final_rmse_plot <- ggplot(
  final_summary,
  aes(
    x = model,
    y = Rating_RMSE,
    fill = model
  )
) +
  geom_col() +
  facet_wrap(
    ~ noise_scenario
  ) +
  scale_fill_manual(
    values = model_colors
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Final 12-Game Rating RMSE",
    subtitle = "Lower is better",
    x = "Model",
    y = "Rating RMSE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

ggsave(
  "version9B_MC4_final_RMSE.png",
  plot = final_rmse_plot,
  width = 11,
  height = 6,
  dpi = 160
)


# ============================================================
# 39. FINAL CONSOLE SUMMARY
# ============================================================

cat("\n========================================\n")
cat("VERSION 9B-MC4 COMPLETE\n")
cat("========================================\n\n")

cat("Models compared:\n")
cat("  1. Margin HL12\n")
cat("  2. Margin No Decay\n")
cat("  3. Score OffDef Ridge\n")
cat("  4. Score OffDef Huber\n\n")

cat("Selected score-model tuning:\n")
cat(
  "  Ridge component lambda:",
  best_score_lambda,
  "\n"
)
cat(
  "  Robust component lambda:",
  best_robust_lambda,
  "\n"
)
cat(
  "  Robust Huber k:",
  best_huber_k,
  "\n\n"
)

cat(
  "Evaluation seasons per scenario:",
  n_evaluation_seasons_per_scenario,
  "\n"
)

cat(
  "Independent interval-calibration seasons per scenario:",
  calibration_cutoff,
  "\n"
)

cat(
  "Independent interval-holdout seasons per scenario:",
  n_evaluation_seasons_per_scenario -
    calibration_cutoff,
  "\n\n"
)

cat("FINAL 12-GAME SUMMARY:\n\n")

print(
  final_summary[
    ,
    c(
      "noise_scenario",
      "model",
      "Rating_RMSE",
      "Rating_MAE",
      "Mean_Pearson",
      "Mean_Spearman",
      "Probability_Within_2_Points",
      "Probability_Within_3_Points",
      "empirical_95_full_width",
      "holdout_coverage"
    )
  ]
)

cat("\nWINNERS:\n\n")

print(
  winner_summary
)

cat(
  "\nInterpretation rule:\n",
  "A genuine improvement should lower rating RMSE/MAE AND ideally\n",
  "produce a narrower empirically calibrated 95% interval while\n",
  "keeping independent holdout coverage near 0.95.\n\n",
  sep = ""
)

cat(
  "If Score OffDef Ridge does not beat the margin model under\n",
  "Gaussian noise, that means final-score totals are adding little\n",
  "independent information about overall team strength under this\n",
  "data-generating structure. If Huber only wins under Student_t4,\n",
  "that means robustness is useful specifically for heavy-tailed\n",
  "score environments rather than as a universal replacement.\n\n",
  sep = ""
)

cat(
  "Output CSVs and PNG plots were written to the current working directory.\n"
)

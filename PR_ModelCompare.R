# ============================================================
# FOOTBALL POWER RATINGS - VERSION 9B-MC4S
# OFFENSE/DEFENSE + NOISE SENSITIVITY EXPERIMENT
# ============================================================
#
# PURPOSE
# -------
# Stress-test the four results-only models from Version 9B-MC4
# across multiple latent offense/defense structures and multiple
# score-noise tail environments.
#
# MODELS -- LOCKED FROM THE PREVIOUS EXPERIMENT
# ------------------------------------------------------------
#   1. Margin HL12
#   2. Margin No Decay
#   3. Score OffDef Ridge
#        component lambda = 3
#   4. Score OffDef Huber
#        component lambda = 2
#        Huber k = 1.5
#
# IMPORTANT:
# The score-model hyperparameters are NOT retuned separately for
# each sensitivity scenario. This tests whether the improvements
# found in 9B-MC4 GENERALIZE rather than letting each model tune
# itself to the simulated answer key.
#
# LATENT COMPONENT SCENARIOS
# ------------------------------------------------------------
# Equal:           offense scale 1.00, defense scale 1.00
# Offense Heavy:   offense scale 1.15, defense scale 0.85
# Defense Heavy:   offense scale 0.85, defense scale 1.15
# Strong Offense:  offense scale 1.30, defense scale 0.70
# Strong Defense:  offense scale 0.70, defense scale 1.30
#
# In every case, the combined hidden rating:
#
#   True Rating = True Offense + True Defense
#
# is rescaled to have SD = 5 exactly.
#
# SCORE-NOISE SCENARIOS
# ------------------------------------------------------------
#   Gaussian
#   Student-t with 8 df
#   Student-t with 4 df
#   Student-t with 3 df
#
# Every Student-t distribution is variance-scaled so its marginal
# score-error SD equals the Gaussian score-error SD. Thus the
# scenarios differ in tail shape, not nominal variance.
#
# PRIMARY QUESTIONS
# ------------------------------------------------------------
# 1. Does the Score OffDef Ridge improvement survive when offense
#    and defense contribute differently to overall strength?
#
# 2. At what degree of tail-heaviness does Huber become preferable?
#
# 3. Does removing recency still help in a static-strength world?
#
# 4. Which model has the best combination of:
#      - rating RMSE
#      - rating MAE
#      - rank recovery
#      - within-1/2/3-point accuracy
#      - narrow empirically calibrated 95% uncertainty
#
# NO EXTRA FOOTBALL INFORMATION IS USED.
# Only schedule and final scores are available to the models.
# ============================================================

library(ggplot2)


# ============================================================
# 1. GLOBAL SETTINGS
# ============================================================

n_weeks <- 14
games_per_team <- 12

true_hfa <- 2.7
hfa_prior <- 2.5

target_margin_noise_sd <- 8.5
score_noise_sd <- target_margin_noise_sd / sqrt(2)

league_mean_points <- 24
target_true_rating_sd <- 5

recency_half_life <- 12

lambda_parity_threshold <- 4.5
adaptive_start_games <- 4

validate_each_generated_schedule <- FALSE

# Locked score-model settings from the prior MC4 experiment.
locked_ridge_component_lambda <- 3
locked_huber_component_lambda <- 2
locked_huber_k <- 1.5

max_huber_iterations <- 40
huber_tolerance <- 1e-6

# ------------------------------------------------------------
# Experiment size
# ------------------------------------------------------------

quick_test <- FALSE

if (quick_test) {
  
  n_evaluation_seasons_per_cell <- 20
  
} else {
  
  n_evaluation_seasons_per_cell <- 200
}

# Accuracy is summarized after every completed game count.
evaluation_completed_games <- 1:12

# First half of seasons within each scenario calibrates the
# absolute-error 95% interval. Second half is independent holdout.
interval_calibration_fraction <- 0.50

component_scenarios <- data.frame(
  component_scenario = c(
    "Equal",
    "Offense Heavy",
    "Defense Heavy",
    "Strong Offense",
    "Strong Defense"
  ),
  scenario_offense_scale = c(
    1.00,
    1.15,
    0.85,
    1.30,
    0.70
  ),
  scenario_defense_scale = c(
    1.00,
    0.85,
    1.15,
    0.70,
    1.30
  ),
  stringsAsFactors = FALSE
)

required_component_columns <- c(
  "component_scenario",
  "scenario_offense_scale",
  "scenario_defense_scale"
)

if (
  !all(
    required_component_columns %in%
    names(component_scenarios)
  )
) {
  
  stop(
    paste(
      "component_scenarios is missing required columns.",
      "Found:",
      paste(
        names(component_scenarios),
        collapse = ", "
      )
    )
  )
}

if (
  nrow(component_scenarios) == 0L
) {
  
  stop(
    "component_scenarios has zero rows."
  )
}


noise_scenarios <- c(
  "Gaussian",
  "Student_t8",
  "Student_t4",
  "Student_t3"
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

evaluation_seed_base <- 951000


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
    
    if (degrees_freedom <= 2) {
      
      stop(
        "Student-t df must exceed 2 to have finite variance."
      )
    }
    
    # Var(t_df) = df / (df - 2).
    # Scale to exactly target_sd.
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
      hfa = as.numeric(
        beta["HFA"]
      ),
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
    noise_scenario,
    offense_scale,
    defense_scale
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
    offense_scale = offense_scale,
    defense_scale = defense_scale
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
# 27B. STANDARDIZE MODEL OUTPUTS
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
  
  model_1 <- standardize_model_output(
    fit = model_1,
    model_name = "Margin HL12",
    league_teams = league_teams
  )
  
  model_2 <- standardize_model_output(
    fit = model_2,
    model_name = "Margin No Decay",
    league_teams = league_teams
  )
  
  model_3 <- standardize_model_output(
    fit = model_3,
    model_name = "Score OffDef Ridge",
    league_teams = league_teams
  )
  
  model_4 <- standardize_model_output(
    fit = model_4,
    model_name = "Score OffDef Huber",
    league_teams = league_teams
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
# 29. SENSITIVITY EXPERIMENT
# ============================================================

cat("\n========================================\n")
cat("VERSION 9B-MC4S SENSITIVITY EXPERIMENT\n")
cat("========================================\n\n")

cat("Teams:", length(teams), "\n")
cat("Games per team:", games_per_team, "\n")
cat("True overall rating SD:", target_true_rating_sd, "\n")
cat("True HFA:", true_hfa, "\n")
cat("Margin-noise SD target:", target_margin_noise_sd, "\n")
cat(
  "Evaluation seasons per sensitivity cell:",
  n_evaluation_seasons_per_cell,
  "\n"
)
cat(
  "Component scenarios:",
  nrow(component_scenarios),
  "\n"
)
cat(
  "Noise scenarios:",
  length(noise_scenarios),
  "\n"
)
cat(
  "Total simulated seasons:",
  n_evaluation_seasons_per_cell *
    nrow(component_scenarios) *
    length(noise_scenarios),
  "\n\n"
)

cat("Locked score-model parameters:\n")
cat(
  "  Score OffDef Ridge lambda:",
  locked_ridge_component_lambda,
  "\n"
)
cat(
  "  Score OffDef Huber lambda:",
  locked_huber_component_lambda,
  "\n"
)
cat(
  "  Score OffDef Huber k:",
  locked_huber_k,
  "\n\n"
)


# ============================================================
# 30. RUN ALL SCENARIOS
# ============================================================

team_level_rows <- list()
season_level_rows <- list()

team_row_index <- 1
season_row_index <- 1

total_cells <- (
  nrow(component_scenarios) *
    length(noise_scenarios)
)

cell_index <- 0

for (
  component_index in
  seq_len(
    nrow(component_scenarios)
  )
) {
  
  component_name <- component_scenarios[["component_scenario"]][
    component_index
  ]
  
  current_offense_scale <- component_scenarios[["scenario_offense_scale"]][
    component_index
  ]
  
  current_defense_scale <- component_scenarios[["scenario_defense_scale"]][
    component_index
  ]
  
  if (
    length(component_name) != 1L ||
    length(current_offense_scale) != 1L ||
    length(current_defense_scale) != 1L ||
    is.na(component_name) ||
    is.na(current_offense_scale) ||
    is.na(current_defense_scale)
  ) {
    
    stop(
      paste0(
        "Invalid component scenario row at component_index = ",
        component_index,
        ". Check component_scenarios."
      )
    )
  }
  
  for (
    noise_index in
    seq_along(noise_scenarios)
  ) {
    
    noise_scenario <- noise_scenarios[
      noise_index
    ]
    
    cell_index <- cell_index + 1
    
    cat("\n========================================\n")
    cat(
      "CELL",
      cell_index,
      "OF",
      total_cells,
      "\n"
    )
    cat("========================================\n")
    cat("Component scenario:", component_name, "\n")
    cat(
      "Offense / defense scales:",
      current_offense_scale,
      "/",
      current_defense_scale,
      "\n"
    )
    cat("Noise:", noise_scenario, "\n\n")
    
    for (
      simulation in
      seq_len(
        n_evaluation_seasons_per_cell
      )
    ) {
      
      if (
        simulation == 1 ||
        simulation %% 50 == 0 ||
        simulation ==
        n_evaluation_seasons_per_cell
      ) {
        
        cat(
          "  Season",
          simulation,
          "of",
          n_evaluation_seasons_per_cell,
          "\n"
        )
      }
      
      # The cell-specific offset prevents accidental overlap while
      # retaining exact pairing of all four models within a season.
      season_seed <- (
        evaluation_seed_base +
          component_index * 1000000 +
          noise_index * 100000 +
          simulation
      )
      
      season_data <- generate_experiment_season(
        season_seed = season_seed,
        noise_scenario = noise_scenario,
        offense_scale = current_offense_scale,
        defense_scale = current_defense_scale
      )
      
      games <- season_data$games
      true_ratings <- season_data$true_components$rating
      
      true_rank <- rank(
        -true_ratings,
        ties.method = "average"
      )
      
      for (
        completed_games in
        evaluation_completed_games
      ) {
        
        snapshot <- get_training_snapshot(
          games = games,
          completed_games_per_team =
            completed_games
        )
        
        model_fits <- fit_four_models(
          training_games =
            snapshot$training_games,
          league_teams = teams,
          score_component_lambda =
            locked_ridge_component_lambda,
          robust_component_lambda =
            locked_huber_component_lambda,
          robust_huber_k =
            locked_huber_k
        )
        
        for (
          model_name in
          model_names
        ) {
          
          current_fit <- model_fits[[model_name]]
          
          if (
            is.null(current_fit$ratings) ||
            length(current_fit$ratings) != length(teams) ||
            is.null(current_fit$hfa) ||
            length(current_fit$hfa) != 1L
          ) {
            
            stop(
              paste0(
                "Invalid model output for ",
                model_name,
                " in simulation ",
                simulation,
                ", completed_games = ",
                completed_games,
                ". ratings length = ",
                length(current_fit$ratings),
                ", hfa length = ",
                length(current_fit$hfa)
              )
            )
          }
          
          estimated_rating <- current_fit$ratings[
            teams
          ]
          
          rating_error <- (
            estimated_rating -
              true_ratings[
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
              true_ratings,
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
              true_ratings
            )
          )
          
          estimated_number1 <- names(
            which.max(
              estimated_rating
            )
          )
          
          team_level_rows[[team_row_index]] <-
            data.frame(
              component_scenario =
                component_name,
              offense_scale =
                current_offense_scale,
              defense_scale =
                current_defense_scale,
              noise_scenario =
                noise_scenario,
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
                  true_ratings[
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
          
          team_row_index <- team_row_index + 1
          
          season_level_rows[[season_row_index]] <-
            data.frame(
              component_scenario =
                component_name,
              offense_scale =
                current_offense_scale,
              defense_scale =
                current_defense_scale,
              noise_scenario =
                noise_scenario,
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
                  true_ratings[
                    teams
                  ],
                  method = "pearson"
                ),
              Spearman =
                safe_cor(
                  estimated_rating,
                  true_ratings[
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
          
          season_row_index <- season_row_index + 1
        }
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

rownames(team_level) <- NULL
rownames(season_level) <- NULL


# ============================================================
# 31. ACCURACY SUMMARY BY SCENARIO / GAMES / MODEL
# ============================================================

summary_keys <- unique(
  team_level[
    ,
    c(
      "component_scenario",
      "offense_scale",
      "defense_scale",
      "noise_scenario",
      "completed_games_per_team",
      "model"
    )
  ]
)

summary_rows <- list()

for (
  i in
  seq_len(
    nrow(summary_keys)
  )
) {
  
  key <- summary_keys[i, ]
  
  team_subset <- team_level[
    team_level$component_scenario ==
      key$component_scenario &
      team_level$noise_scenario ==
      key$noise_scenario &
      team_level$completed_games_per_team ==
      key$completed_games_per_team &
      team_level$model ==
      key$model,
  ]
  
  season_subset <- season_level[
    season_level$component_scenario ==
      key$component_scenario &
      season_level$noise_scenario ==
      key$noise_scenario &
      season_level$completed_games_per_team ==
      key$completed_games_per_team &
      season_level$model ==
      key$model,
  ]
  
  summary_rows[[i]] <- data.frame(
    component_scenario =
      key$component_scenario,
    offense_scale =
      key$offense_scale,
    defense_scale =
      key$defense_scale,
    noise_scenario =
      key$noise_scenario,
    completed_games_per_team =
      key$completed_games_per_team,
    mean_calendar_week =
      mean(
        season_subset$calendar_week
      ),
    model =
      key$model,
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
}

accuracy_summary <- do.call(
  rbind,
  summary_rows
)

rownames(accuracy_summary) <- NULL


# ============================================================
# 32. EMPIRICALLY CALIBRATED 95% UNCERTAINTY
# ============================================================
#
# Within every component/noise/model/games cell:
#
#   first half of seasons:
#       learn 95th percentile of |rating error|
#
#   second half:
#       test coverage and width
#
# This interval is:
#
#   estimated rating +/- empirical half-width
#
# It is deliberately model-agnostic and gives a fair comparison
# of how narrow each model can be at genuine ~95% coverage.
# ============================================================

interval_rows <- list()
interval_index <- 1

for (
  component_name in
  component_scenarios$component_scenario
) {
  
  for (
    noise_scenario in
    noise_scenarios
  ) {
    
    for (
      completed_games in
      evaluation_completed_games
    ) {
      
      for (
        model_name in
        model_names
      ) {
        
        current <- team_level[
          team_level$component_scenario ==
            component_name &
            team_level$noise_scenario ==
            noise_scenario &
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
        
        n_calibration_seasons <- floor(
          length(season_ids) *
            interval_calibration_fraction
        )
        
        calibration_ids <- season_ids[
          seq_len(
            n_calibration_seasons
          )
        ]
        
        holdout_ids <- season_ids[
          (
            n_calibration_seasons + 1
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
        
        holdout_coverage <- mean(
          holdout_data$absolute_rating_error <=
            half_width
        )
        
        interval_rows[[interval_index]] <-
          data.frame(
            component_scenario =
              component_name,
            noise_scenario =
              noise_scenario,
            completed_games_per_team =
              completed_games,
            model =
              model_name,
            calibration_team_observations =
              nrow(
                calibration_data
              ),
            holdout_team_observations =
              nrow(
                holdout_data
              ),
            empirical_95_half_width =
              half_width,
            empirical_95_full_width =
              2 *
              half_width,
            holdout_coverage =
              holdout_coverage,
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
        
        interval_index <- interval_index + 1
      }
    }
  }
}

interval_summary <- do.call(
  rbind,
  interval_rows
)

rownames(interval_summary) <- NULL


# ============================================================
# 33. FINAL-WEEK SUMMARY
# ============================================================

final_accuracy <- accuracy_summary[
  accuracy_summary$completed_games_per_team ==
    12,
]

final_intervals <- interval_summary[
  interval_summary$completed_games_per_team ==
    12,
]

final_summary <- merge(
  final_accuracy,
  final_intervals[
    ,
    c(
      "component_scenario",
      "noise_scenario",
      "model",
      "empirical_95_half_width",
      "empirical_95_full_width",
      "holdout_coverage"
    )
  ],
  by = c(
    "component_scenario",
    "noise_scenario",
    "model"
  ),
  all.x = TRUE,
  sort = FALSE
)

final_summary <- final_summary[
  order(
    final_summary$noise_scenario,
    final_summary$component_scenario,
    final_summary$Rating_RMSE
  ),
]

rownames(final_summary) <- NULL


# ============================================================
# 34. PAIRED FINAL-WEEK DIFFERENCES VS MARGIN HL12
# ============================================================

paired_rows <- list()
paired_index <- 1

for (
  component_name in
  component_scenarios$component_scenario
) {
  
  for (
    noise_scenario in
    noise_scenarios
  ) {
    
    reference <- season_level[
      season_level$component_scenario ==
        component_name &
        season_level$noise_scenario ==
        noise_scenario &
        season_level$completed_games_per_team ==
        12 &
        season_level$model ==
        "Margin HL12",
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
        season_level$component_scenario ==
          component_name &
          season_level$noise_scenario ==
          noise_scenario &
          season_level$completed_games_per_team ==
          12 &
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
      
      rmse_mcse <- if (
        length(rmse_difference) > 1
      ) {
        
        sd(rmse_difference) /
          sqrt(
            length(rmse_difference)
          )
        
      } else {
        
        NA_real_
      }
      
      mae_mcse <- if (
        length(mae_difference) > 1
      ) {
        
        sd(mae_difference) /
          sqrt(
            length(mae_difference)
          )
        
      } else {
        
        NA_real_
      }
      
      paired_rows[[paired_index]] <-
        data.frame(
          component_scenario =
            component_name,
          noise_scenario =
            noise_scenario,
          model =
            model_name,
          mean_RMSE_difference_vs_Margin_HL12 =
            mean(
              rmse_difference
            ),
          RMSE_MCSE =
            rmse_mcse,
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
          probability_lower_RMSE_than_Margin_HL12 =
            mean(
              rmse_difference < 0
            ),
          mean_MAE_difference_vs_Margin_HL12 =
            mean(
              mae_difference
            ),
          MAE_MCSE =
            mae_mcse,
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
          probability_lower_MAE_than_Margin_HL12 =
            mean(
              mae_difference < 0
            ),
          stringsAsFactors =
            FALSE
        )
      
      paired_index <- paired_index + 1
    }
  }
}

paired_final <- do.call(
  rbind,
  paired_rows
)

rownames(paired_final) <- NULL


# ============================================================
# 35. WINNER BY SENSITIVITY CELL
# ============================================================

scenario_cells <- unique(
  final_summary[
    ,
    c(
      "component_scenario",
      "noise_scenario"
    )
  ]
)

winner_rows <- list()

for (
  i in
  seq_len(
    nrow(scenario_cells)
  )
) {
  
  component_name <- scenario_cells$component_scenario[i]
  noise_scenario <- scenario_cells$noise_scenario[i]
  
  current <- final_summary[
    final_summary$component_scenario ==
      component_name &
      final_summary$noise_scenario ==
      noise_scenario,
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
  
  winner_rows[[i]] <- data.frame(
    component_scenario =
      component_name,
    noise_scenario =
      noise_scenario,
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

rownames(winner_summary) <- NULL


# ============================================================
# 36. ROBUSTNESS SUMMARY ACROSS ALL 20 CELLS
# ============================================================
#
# RMSE regret:
#
#   model RMSE - best RMSE in that scenario
#
# Lower mean/worst regret means a model is safer when the true
# offense/defense and tail environment are unknown.
# ============================================================

regret_rows <- list()
regret_index <- 1

for (
  i in
  seq_len(
    nrow(scenario_cells)
  )
) {
  
  component_name <- scenario_cells$component_scenario[i]
  noise_scenario <- scenario_cells$noise_scenario[i]
  
  current <- final_summary[
    final_summary$component_scenario ==
      component_name &
      final_summary$noise_scenario ==
      noise_scenario,
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
    
    regret_rows[[regret_index]] <- data.frame(
      component_scenario =
        component_name,
      noise_scenario =
        noise_scenario,
      model =
        current$model[row_index],
      Rating_RMSE =
        current$Rating_RMSE[row_index],
      Rating_MAE =
        current$Rating_MAE[row_index],
      interval_width =
        current$empirical_95_full_width[row_index],
      interval_coverage =
        current$holdout_coverage[row_index],
      RMSE_regret =
        current$Rating_RMSE[row_index] -
        best_rmse,
      interval_width_regret =
        current$empirical_95_full_width[row_index] -
        best_width,
      stringsAsFactors =
        FALSE
    )
    
    regret_index <- regret_index + 1
  }
}

regret_table <- do.call(
  rbind,
  regret_rows
)

robustness_rows <- list()

for (
  model_index in
  seq_along(model_names)
) {
  
  model_name <- model_names[
    model_index
  ]
  
  current <- regret_table[
    regret_table$model ==
      model_name,
  ]
  
  robustness_rows[[model_index]] <- data.frame(
    model =
      model_name,
    mean_RMSE =
      mean(
        current$Rating_RMSE
      ),
    worst_cell_RMSE =
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
    RMSE_cell_wins =
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
    width_cell_wins =
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

rownames(robustness_summary) <- NULL


# ============================================================
# 37. SUMMARY BY NOISE SCENARIO
# ============================================================

noise_summary_rows <- list()
noise_summary_index <- 1

for (
  noise_scenario in
  noise_scenarios
) {
  
  for (
    model_name in
    model_names
  ) {
    
    current <- final_summary[
      final_summary$noise_scenario ==
        noise_scenario &
        final_summary$model ==
        model_name,
    ]
    
    noise_summary_rows[[noise_summary_index]] <-
      data.frame(
        noise_scenario =
          noise_scenario,
        model =
          model_name,
        mean_RMSE_across_component_splits =
          mean(
            current$Rating_RMSE
          ),
        mean_MAE_across_component_splits =
          mean(
            current$Rating_MAE
          ),
        mean_95_width_across_component_splits =
          mean(
            current$empirical_95_full_width
          ),
        mean_coverage_across_component_splits =
          mean(
            current$holdout_coverage
          ),
        stringsAsFactors =
          FALSE
      )
    
    noise_summary_index <- noise_summary_index + 1
  }
}

noise_summary <- do.call(
  rbind,
  noise_summary_rows
)

rownames(noise_summary) <- NULL


# ============================================================
# 38. SUMMARY BY COMPONENT STRUCTURE
# ============================================================

component_summary_rows <- list()
component_summary_index <- 1

for (
  component_name in
  component_scenarios$component_scenario
) {
  
  for (
    model_name in
    model_names
  ) {
    
    current <- final_summary[
      final_summary$component_scenario ==
        component_name &
        final_summary$model ==
        model_name,
    ]
    
    component_summary_rows[[component_summary_index]] <-
      data.frame(
        component_scenario =
          component_name,
        model =
          model_name,
        mean_RMSE_across_noise_scenarios =
          mean(
            current$Rating_RMSE
          ),
        mean_MAE_across_noise_scenarios =
          mean(
            current$Rating_MAE
          ),
        mean_95_width_across_noise_scenarios =
          mean(
            current$empirical_95_full_width
          ),
        mean_coverage_across_noise_scenarios =
          mean(
            current$holdout_coverage
          ),
        stringsAsFactors =
          FALSE
      )
    
    component_summary_index <- component_summary_index + 1
  }
}

component_summary <- do.call(
  rbind,
  component_summary_rows
)

rownames(component_summary) <- NULL


# ============================================================
# 39. PRINT CORE RESULTS
# ============================================================

cat("\n========================================\n")
cat("FINAL 12-GAME SENSITIVITY RESULTS\n")
cat("========================================\n\n")

print(
  final_summary[
    ,
    c(
      "component_scenario",
      "noise_scenario",
      "model",
      "Rating_RMSE",
      "Rating_MAE",
      "Mean_Pearson",
      "Mean_Spearman",
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
cat("WINNER BY SENSITIVITY CELL\n")
cat("========================================\n\n")

print(
  winner_summary,
  row.names = FALSE
)

cat("\n========================================\n")
cat("ROBUSTNESS ACROSS ALL SENSITIVITY CELLS\n")
cat("========================================\n\n")

print(
  robustness_summary,
  row.names = FALSE
)

cat("\n========================================\n")
cat("AVERAGE PERFORMANCE BY NOISE DISTRIBUTION\n")
cat("========================================\n\n")

print(
  noise_summary[
    order(
      noise_summary$noise_scenario,
      noise_summary$mean_RMSE_across_component_splits
    ),
  ],
  row.names = FALSE
)

cat("\n========================================\n")
cat("AVERAGE PERFORMANCE BY COMPONENT STRUCTURE\n")
cat("========================================\n\n")

print(
  component_summary[
    order(
      component_summary$component_scenario,
      component_summary$mean_RMSE_across_noise_scenarios
    ),
  ],
  row.names = FALSE
)


# ============================================================
# 40. WRITE CSV OUTPUTS
# ============================================================

write.csv(
  accuracy_summary,
  "version9B_MC4S_accuracy_by_games.csv",
  row.names = FALSE
)

write.csv(
  interval_summary,
  "version9B_MC4S_calibrated_intervals.csv",
  row.names = FALSE
)

write.csv(
  final_summary,
  "version9B_MC4S_final_model_summary.csv",
  row.names = FALSE
)

write.csv(
  paired_final,
  "version9B_MC4S_paired_final_comparison.csv",
  row.names = FALSE
)

write.csv(
  winner_summary,
  "version9B_MC4S_winner_by_scenario.csv",
  row.names = FALSE
)

write.csv(
  robustness_summary,
  "version9B_MC4S_robustness_summary.csv",
  row.names = FALSE
)

write.csv(
  noise_summary,
  "version9B_MC4S_summary_by_noise.csv",
  row.names = FALSE
)

write.csv(
  component_summary,
  "version9B_MC4S_summary_by_component.csv",
  row.names = FALSE
)


# ============================================================
# 41. PLOTS
# ============================================================

# ------------------------------------------------------------
# Plot 1: Final RMSE by noise scenario, faceted by component split
# ------------------------------------------------------------

plot_rmse <- ggplot(
  final_summary,
  aes(
    x = model,
    y = Rating_RMSE,
    fill = model
  )
) +
  geom_col() +
  facet_grid(
    component_scenario ~
      noise_scenario
  ) +
  scale_fill_manual(
    values = model_colors
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Version 9B-MC4S: Final Rating RMSE",
    subtitle = "Lower is better; 12 games per team",
    x = NULL,
    y = "Rating RMSE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "version9B_MC4S_final_RMSE.png",
  plot = plot_rmse,
  width = 14,
  height = 12,
  dpi = 160
)


# ------------------------------------------------------------
# Plot 2: Final calibrated 95% interval width
# ------------------------------------------------------------

plot_width <- ggplot(
  final_summary,
  aes(
    x = model,
    y = empirical_95_full_width,
    fill = model
  )
) +
  geom_col() +
  facet_grid(
    component_scenario ~
      noise_scenario
  ) +
  scale_fill_manual(
    values = model_colors
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Version 9B-MC4S: Calibrated 95% Interval Width",
    subtitle = "Lower is better if holdout coverage remains near 95%",
    x = NULL,
    y = "Full 95% interval width"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "version9B_MC4S_final_interval_width.png",
  plot = plot_width,
  width = 14,
  height = 12,
  dpi = 160
)


# ------------------------------------------------------------
# Plot 3: Accuracy learning curve averaged over component splits
# ------------------------------------------------------------

learning_curve <- aggregate(
  Rating_RMSE ~
    noise_scenario +
    completed_games_per_team +
    model,
  data = accuracy_summary,
  FUN = mean
)

plot_learning <- ggplot(
  learning_curve,
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
    size = 1.8
  ) +
  facet_wrap(
    ~noise_scenario
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  scale_x_continuous(
    breaks = 1:12
  ) +
  labs(
    title = "Version 9B-MC4S: Rating Error as Games Accumulate",
    subtitle = "Averaged across offense/defense component structures",
    x = "Completed games per team",
    y = "Rating RMSE",
    color = "Model"
  ) +
  theme_minimal()

ggsave(
  "version9B_MC4S_learning_curve.png",
  plot = plot_learning,
  width = 13,
  height = 8,
  dpi = 160
)


# ------------------------------------------------------------
# Plot 4: RMSE regret heatmap-like point plot
# ------------------------------------------------------------

plot_regret <- ggplot(
  regret_table,
  aes(
    x = noise_scenario,
    y = RMSE_regret,
    color = model,
    group = model
  )
) +
  geom_point(
    position = position_dodge(
      width = 0.45
    ),
    size = 2
  ) +
  facet_wrap(
    ~component_scenario
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_color_manual(
    values = model_colors
  ) +
  labs(
    title = "Version 9B-MC4S: RMSE Regret vs Best Model in Each Cell",
    subtitle = "Zero means the model won that sensitivity scenario",
    x = "Noise scenario",
    y = "RMSE above best model",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

ggsave(
  "version9B_MC4S_RMSE_regret.png",
  plot = plot_regret,
  width = 13,
  height = 9,
  dpi = 160
)


# ============================================================
# 42. FINAL CONSOLE SUMMARY
# ============================================================

cat("\n========================================\n")
cat("VERSION 9B-MC4S COMPLETE\n")
cat("========================================\n\n")

cat("Sensitivity cells:", total_cells, "\n")
cat(
  "Evaluation seasons per cell:",
  n_evaluation_seasons_per_cell,
  "\n"
)
cat(
  "Total evaluation seasons:",
  total_cells *
    n_evaluation_seasons_per_cell,
  "\n\n"
)

cat("Locked score-model settings:\n")
cat(
  "  Ridge component lambda:",
  locked_ridge_component_lambda,
  "\n"
)
cat(
  "  Huber component lambda:",
  locked_huber_component_lambda,
  "\n"
)
cat(
  "  Huber k:",
  locked_huber_k,
  "\n\n"
)

cat("ROBUSTNESS SUMMARY:\n\n")
print(
  robustness_summary,
  row.names = FALSE
)

cat("\nInterpretation:\n")
cat(
  "A score model is ready to replace the margin-only engine only if\n"
)
cat(
  "its advantage remains reasonably consistent across component\n"
)
cat(
  "splits and plausible tail environments, rather than winning only\n"
)
cat(
  "in the exact simulator structure used to motivate it.\n\n"
)
cat(
  "RMSE regret is especially useful: the safest production model\n"
)
cat(
  "should have low average and low worst-case regret when the true\n"
)
cat(
  "football scoring environment is unknown.\n\n"
)

cat(
  "Output CSVs and PNG plots were written to the current working directory.\n"
)

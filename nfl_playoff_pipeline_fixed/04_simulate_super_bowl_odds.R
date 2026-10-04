#!/usr/bin/env Rscript

# ============================================================
# 04 - SIMULATE SUPER BOWL ODDS AFTER EACH REGULAR SEASON
# Retrospective + walk-forward calibrations
# ============================================================

source("00_config.R")
source(file.path("R", "playoff_helpers.R"))

games_file <- file.path(
  output_directory,
  "04_playoff_games_with_predictions.csv"
)

seed_file <- file.path(
  output_directory,
  "03_playoff_seeds_and_brackets_1970_2025.csv"
)

outcome_file <- file.path(
  output_directory,
  "02_playoff_team_outcomes_1970_2025.csv"
)

model_file <- file.path(
  output_directory,
  "playoff_probability_model.rds"
)

required_files <- c(
  games_file,
  seed_file,
  outcome_file,
  model_file
)

if (!all(file.exists(required_files))) {
  stop(
    paste0(
      "Missing prerequisite files. Run scripts 01 and 02 first.\n",
      paste(
        required_files[!file.exists(required_files)],
        collapse = "\n"
      )
    )
  )
}

games <- read_csv_base(games_file)
seed_table <- read_csv_base(seed_file)
outcomes <- read_csv_base(outcome_file)
master <- read_csv_base(master_ratings_file)

games$home_team <- normalize_team_code(games$home_team)
games$away_team <- normalize_team_code(games$away_team)
seed_table$team <- normalize_team_code(seed_table$team)
outcomes$team <- normalize_team_code(outcomes$team)
master$team <- normalize_team_code(master$team)

retrospective_model <- readRDS(
  model_file
)

retrospective_coef <- coef(
  retrospective_model
)

set.seed(
  simulation_seed
)

simulate_probability_set <- function(
    season,
    model_coef,
    label,
    n_sims
) {
  
  season_outcomes <- outcomes[
    outcomes$season == season,
    ,
    drop = FALSE
  ]
  
  teams <- sort(
    unique(
      season_outcomes$team
    )
  )
  
  if (
    length(
      teams
    ) ==
    0
  ) {
    
    return(
      NULL
    )
  }
  
  conf_final_count <- setNames(
    integer(
      length(
        teams
      )
    ),
    teams
  )
  
  sb_app_count <- conf_final_count
  sb_win_count <- conf_final_count
  
  for (
    sim in
    seq_len(
      n_sims
    )
  ) {
    
    result <- simulate_season_playoffs_once(
      season = season,
      playoff_games = games,
      seed_table = seed_table,
      master_rows = master,
      model_coef = model_coef
    )
    
    conf_final_count[
      result$conference_finalists
    ] <- (
      conf_final_count[
        result$conference_finalists
      ] +
        1L
    )
    
    sb_app_count[
      result$super_bowl_teams
    ] <- (
      sb_app_count[
        result$super_bowl_teams
      ] +
        1L
    )
    
    sb_win_count[
      result$champion
    ] <- (
      sb_win_count[
        result$champion
      ] +
        1L
    )
  }
  
  data.frame(
    season = season,
    team = teams,
    calibration = label,
    simulations = n_sims,
    conference_championship_probability =
      as.numeric(
        conf_final_count[
          teams
        ]
      ) /
      n_sims,
    super_bowl_appearance_probability =
      as.numeric(
        sb_app_count[
          teams
        ]
      ) /
      n_sims,
    super_bowl_win_probability =
      as.numeric(
        sb_win_count[
          teams
        ]
      ) /
      n_sims,
    stringsAsFactors = FALSE
  )
}

all_storage <- list()
k <- 1L

season_values <- sort(
  unique(
    games$season
  )
)

for (
  season in
  season_values
) {
  
  cat(
    "Simulating",
    season,
    "retrospective odds...\n"
  )
  
  retro <- simulate_probability_set(
    season = season,
    model_coef = retrospective_coef,
    label = "retrospective_all_1970_2025_results",
    n_sims = n_simulations
  )
  
  all_storage[[k]] <- retro
  k <- k + 1L
  
  train <- games[
    games$season <
      season &
      is.finite(
        games$expected_home_margin
      ) &
      !is.na(
        games$home_win
      ),
    ,
    drop = FALSE
  ]
  
  if (
    nrow(
      train
    ) >=
    walkforward_min_games
  ) {
    
    cat(
      "Simulating",
      season,
      "walk-forward odds...\n"
    )
    
    wf_model <- glm(
      home_win ~ 0 + expected_home_margin,
      data = train,
      family = binomial()
    )
    
    wf <- simulate_probability_set(
      season = season,
      model_coef = coef(
        wf_model
      ),
      label = "walkforward_prior_playoffs_only",
      n_sims = n_simulations
    )
    
    all_storage[[k]] <- wf
    k <- k + 1L
  }
}

odds_long <- do.call(
  rbind,
  all_storage
)

# ------------------------------------------------------------
# Add frozen regular-season context and actual outcomes.
# ------------------------------------------------------------

context_cols <- c(
  "season",
  "team",
  "rank",
  "rating",
  "offense_rating",
  "defense_rating",
  "record",
  "win_pct",
  "played_SOS",
  "season_rating_z"
)

context_cols <- intersect(
  context_cols,
  names(
    master
  )
)

odds_long <- merge(
  odds_long,
  master[
    ,
    context_cols,
    drop = FALSE
  ],
  by = c(
    "season",
    "team"
  ),
  all.x = TRUE
)

outcome_cols <- c(
  "season",
  "team",
  "conference",
  "division",
  "playoff_finish",
  "champion",
  "super_bowl_appearance",
  "playoff_wins",
  "playoff_losses"
)

outcome_cols <- intersect(
  outcome_cols,
  names(
    outcomes
  )
)

odds_long <- merge(
  odds_long,
  outcomes[
    ,
    outcome_cols,
    drop = FALSE
  ],
  by = c(
    "season",
    "team"
  ),
  all.x = TRUE
)

# Seeds are not required for 1970-1977; preserve NA there.
seed_keep <- seed_table[
  ,
  c(
    "season",
    "team",
    "seed",
    "seed_inference_status",
    "playoff_format"
  ),
  drop = FALSE
]

odds_long <- merge(
  odds_long,
  seed_keep,
  by = c(
    "season",
    "team"
  ),
  all.x = TRUE
)

odds_long$playoff_format[
  is.na(
    odds_long$playoff_format
  )
] <- vapply(
  odds_long$season[
    is.na(
      odds_long$playoff_format
    )
  ],
  season_playoff_format,
  character(1)
)

# Rank each season/calibration by championship probability.
group_key <- interaction(
  odds_long$season,
  odds_long$calibration,
  drop = TRUE
)

odds_long$super_bowl_probability_rank <- ave(
  -odds_long$super_bowl_win_probability,
  group_key,
  FUN = function(x) rank(
    x,
    ties.method = "min"
  )
)

odds_long <- odds_long[
  order(
    odds_long$season,
    odds_long$calibration,
    odds_long$super_bowl_probability_rank
  ),
  ,
  drop = FALSE
]

# Wide convenience file with retrospective and walk-forward SB odds.
retro <- odds_long[
  odds_long$calibration ==
    "retrospective_all_1970_2025_results",
  ,
  drop = FALSE
]

wf <- odds_long[
  odds_long$calibration ==
    "walkforward_prior_playoffs_only",
  c(
    "season",
    "team",
    "conference_championship_probability",
    "super_bowl_appearance_probability",
    "super_bowl_win_probability"
  ),
  drop = FALSE
]

names(
  wf
)[
  3:5
] <- c(
  "conference_championship_probability_walkforward",
  "super_bowl_appearance_probability_walkforward",
  "super_bowl_win_probability_walkforward"
)

wide <- merge(
  retro,
  wf,
  by = c(
    "season",
    "team"
  ),
  all.x = TRUE
)

names(
  wide
)[
  names(
    wide
  ) ==
    "conference_championship_probability"
] <- "conference_championship_probability_retrospective"

names(
  wide
)[
  names(
    wide
  ) ==
    "super_bowl_appearance_probability"
] <- "super_bowl_appearance_probability_retrospective"

names(
  wide
)[
  names(
    wide
  ) ==
    "super_bowl_win_probability"
] <- "super_bowl_win_probability_retrospective"

# ------------------------------------------------------------
# Actual champions vs their pre-playoff championship odds
# ------------------------------------------------------------

champions <- wide[
  wide$champion ==
    1,
  ,
  drop = FALSE
]

champions <- champions[
  order(
    champions$super_bowl_win_probability_retrospective
  ),
  ,
  drop = FALSE
]

champions$champion_surprise_rank <- seq_len(
  nrow(
    champions
  )
)

write_csv_base(
  wide,
  file.path(
    output_directory,
    "08_super_bowl_probabilities_by_season.csv"
  )
)

write_csv_base(
  odds_long,
  file.path(
    output_directory,
    "08_super_bowl_probabilities_long.csv"
  )
)

write_csv_base(
  champions,
  file.path(
    output_directory,
    "08_super_bowl_champion_preplayoff_odds.csv"
  )
)

cat("\nSuper Bowl simulations complete.\n")
cat("Simulations per season/calibration:", n_simulations, "\n")
cat("\nMost surprising champions by retrospective pre-playoff odds:\n")
print(
  head(
    champions[
      ,
      c(
        "season",
        "team",
        "record",
        "rating",
        "super_bowl_win_probability_retrospective"
      ),
      drop = FALSE
    ],
    15
  )
)

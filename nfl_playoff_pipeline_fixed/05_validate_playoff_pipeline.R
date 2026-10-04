#!/usr/bin/env Rscript

# ============================================================
# 05 - VALIDATE COMPLETE PLAYOFF PIPELINE
# ============================================================

source("00_config.R")
source(file.path("R", "playoff_helpers.R"))

files <- list(
  games = file.path(
    output_directory,
    "01_playoff_games_1970_2025.csv"
  ),
  outcomes = file.path(
    output_directory,
    "02_playoff_team_outcomes_1970_2025.csv"
  ),
  seeds = file.path(
    output_directory,
    "03_playoff_seeds_and_brackets_1970_2025.csv"
  ),
  predictions = file.path(
    output_directory,
    "04_playoff_games_with_predictions.csv"
  ),
  upsets = file.path(
    output_directory,
    "05_playoff_upsets_ranked.csv"
  ),
  pre_post = file.path(
    output_directory,
    "06_pre_vs_post_playoff_ratings.csv"
  ),
  trajectories = file.path(
    output_directory,
    "07_playoff_rating_trajectories.csv"
  ),
  odds = file.path(
    output_directory,
    "08_super_bowl_probabilities_by_season.csv"
  )
)

missing <- names(files)[
  !file.exists(
    unlist(
      files
    )
  )
]

if (length(missing) > 0) {
  stop(
    "Missing pipeline outputs: ",
    paste(
      missing,
      collapse = ", "
    )
  )
}

games <- read_csv_base(files$games)
outcomes <- read_csv_base(files$outcomes)
seeds <- read_csv_base(files$seeds)
predictions <- read_csv_base(files$predictions)
upsets <- read_csv_base(files$upsets)
pre_post <- read_csv_base(files$pre_post)
trajectories <- read_csv_base(files$trajectories)
odds <- read_csv_base(files$odds)

validation <- list()
k <- 1L

add_check <- function(check_name, passed, value = NA_character_, note = "") {
  data.frame(
    check = check_name,
    passed = as.logical(passed),
    value = as.character(value),
    note = note,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Coverage
# ------------------------------------------------------------

seasons_present <- sort(
  unique(
    games$season
  )
)

expected_seasons <- analysis_start_season:analysis_end_season

validation[[k]] <- add_check(
  "all_seasons_present",
  identical(
    seasons_present,
    expected_seasons
  ),
  paste(
    min(seasons_present),
    max(seasons_present),
    sep = "-"
  ),
  "Expected every season from 1970 through 2025."
)
k <- k + 1L

sb_counts <- table(
  games$season[
    games$round ==
      "SB"
  ]
)

validation[[k]] <- add_check(
  "exactly_one_super_bowl_per_season",
  length(sb_counts) ==
    length(expected_seasons) &&
    all(sb_counts == 1),
  paste(
    range(
      as.integer(
        sb_counts
      )
    ),
    collapse = " to "
  )
)
k <- k + 1L

# ------------------------------------------------------------
# Seeds
# ------------------------------------------------------------

seed_required <- seeds[
  seeds$season >=
    1978,
  ,
  drop = FALSE
]

validation[[k]] <- add_check(
  "no_missing_required_seeds",
  all(
    !is.na(
      seed_required$seed
    )
  ),
  sum(
    is.na(
      seed_required$seed
    )
  ),
  "1970-1977 uses the actual opening bracket and does not require numeric seeds."
)
k <- k + 1L

duplicate_seed_groups <- 0L

for (season in unique(seed_required$season)) {
  for (conf in c("AFC", "NFC")) {
    x <- seed_required[
      seed_required$season ==
        season &
        seed_required$conference ==
          conf,
      ,
      drop = FALSE
    ]
    if (
      nrow(
        x
      ) >
        0 &&
        any(
          duplicated(
            x$seed
          )
        )
    ) {
      duplicate_seed_groups <- duplicate_seed_groups + 1L
    }
  }
}

validation[[k]] <- add_check(
  "unique_seeds_within_conference",
  duplicate_seed_groups ==
    0,
  duplicate_seed_groups
)
k <- k + 1L

# ------------------------------------------------------------
# Prediction probabilities
# ------------------------------------------------------------

validation[[k]] <- add_check(
  "retrospective_game_probabilities_valid",
  all(
    predictions$home_win_probability_retrospective >
      0 &
      predictions$home_win_probability_retrospective <
        1,
    na.rm = TRUE
  ),
  paste(
    round(
      range(
        predictions$home_win_probability_retrospective,
        na.rm = TRUE
      ),
      4
    ),
    collapse = " to "
  )
)
k <- k + 1L

validation[[k]] <- add_check(
  "upsets_are_sub_50_percent_winners",
  all(
    upsets$winner_probability_retrospective <
      0.5,
    na.rm = TRUE
  ),
  nrow(
    upsets
  )
)
k <- k + 1L

# ------------------------------------------------------------
# Super Bowl simulation probability sums
# ------------------------------------------------------------

sum_checks <- aggregate(
  cbind(
    conference_championship_probability_retrospective,
    super_bowl_appearance_probability_retrospective,
    super_bowl_win_probability_retrospective
  ) ~ season,
  data = odds,
  FUN = sum
)

max_conf_error <- max(
  abs(
    sum_checks$conference_championship_probability_retrospective -
      4
  ),
  na.rm = TRUE
)

max_sb_app_error <- max(
  abs(
    sum_checks$super_bowl_appearance_probability_retrospective -
      2
  ),
  na.rm = TRUE
)

max_sb_win_error <- max(
  abs(
    sum_checks$super_bowl_win_probability_retrospective -
      1
  ),
  na.rm = TRUE
)

simulation_tolerance <- max(
  0.02,
  8 /
    sqrt(
      n_simulations
    )
)

validation[[k]] <- add_check(
  "conference_finalist_probabilities_sum_to_four",
  max_conf_error <
    simulation_tolerance,
  round(
    max_conf_error,
    5
  )
)
k <- k + 1L

validation[[k]] <- add_check(
  "super_bowl_appearance_probabilities_sum_to_two",
  max_sb_app_error <
    simulation_tolerance,
  round(
    max_sb_app_error,
    5
  )
)
k <- k + 1L

validation[[k]] <- add_check(
  "super_bowl_win_probabilities_sum_to_one",
  max_sb_win_error <
    simulation_tolerance,
  round(
    max_sb_win_error,
    5
  )
)
k <- k + 1L

# ------------------------------------------------------------
# Champions + ratings
# ------------------------------------------------------------

champion_rows <- odds[
  odds$champion ==
    1,
  ,
  drop = FALSE
]

validation[[k]] <- add_check(
  "one_actual_champion_in_odds_each_year",
  nrow(champion_rows) ==
    length(expected_seasons),
  nrow(
    champion_rows
  )
)
k <- k + 1L

validation[[k]] <- add_check(
  "pre_post_rating_rows_exist",
  nrow(pre_post) >
    0 &&
    all(
      is.finite(
        pre_post$pre_playoff_rating
      )
    ) &&
    all(
      is.finite(
        pre_post$post_playoff_rating
      )
    ),
  nrow(
    pre_post
  )
)
k <- k + 1L

rating_validation_file <- file.path(
  output_directory,
  "03_post_playoff_rating_validation.csv"
)

if (
  file.exists(
    rating_validation_file
  )
) {

  rv <- read_csv_base(
    rating_validation_file
  )

  max_refit_difference <- max(
    rv$max_abs_regular_refit_minus_master,
    na.rm = TRUE
  )

  validation[[k]] <- add_check(
    "regular_refit_close_to_master_archive",
    max_refit_difference <
      0.25,
    round(
      max_refit_difference,
      4
    ),
    paste0(
      "This is a diagnostic, not a hard identity. ",
      "Larger differences can indicate schedule/team-code preprocessing differences."
    )
  )

  k <- k + 1L
}

validation_table <- do.call(
  rbind,
  validation
)

write_csv_base(
  validation_table,
  file.path(
    output_directory,
    "09_complete_playoff_pipeline_validation.csv"
  )
)

cat("\n========================================\n")
cat("PLAYOFF PIPELINE VALIDATION\n")
cat("========================================\n\n")

print(
  validation_table,
  row.names = FALSE
)

if (
  any(
    !validation_table$passed
  )
) {

  cat(
    "\nOne or more checks failed. Review the validation CSV before using the ",
    "historical playoff probabilities in the Shiny app.\n",
    sep = ""
  )

} else {

  cat(
    "\nAll validation checks passed.\n"
  )
}

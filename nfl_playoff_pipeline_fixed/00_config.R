# ============================================================
# PLAYOFF PIPELINE CONFIGURATION
# 1970-2025
# ============================================================

analysis_start_season <- 1970L
analysis_end_season <- 2025L

# Historical game file created by the user's 1970-1998 rebuild script.
historical_games_file <- file.path(
  path.expand("~/Downloads"),
  "historical_nfl_1970_1998",
  "historical_nfl_1970_1998_games_COMPLETE.csv"
)

# Enriched final regular-season ratings.
# The script first checks this project folder, then the user's Downloads area.
master_rating_candidates <- c(
  file.path(
    getwd(),
    "data",
    "version12_1_1970_2025_all_season_team_summaries_enriched.csv"
  ),
  file.path(
    path.expand("~/Downloads"),
    "version12_1_1970_2025_historical_analysis",
    "version12_1_1970_2025_all_season_team_summaries_enriched.csv"
  ),
  file.path(
    path.expand("~/Downloads"),
    "version12_1_1970_2025_all_season_team_summaries_enriched.csv"
  )
)

master_ratings_file <- master_rating_candidates[
  file.exists(master_rating_candidates)
][1]

if (length(master_ratings_file) == 0 || is.na(master_ratings_file)) {
  stop(
    paste0(
      "Could not find the enriched 1970-2025 regular-season rating archive.\n",
      "Put version12_1_1970_2025_all_season_team_summaries_enriched.csv in ",
      "this project's data/ folder or change master_rating_candidates in 00_config.R."
    )
  )
}

output_directory <- file.path(
  path.expand("~/Downloads"),
  "nfl_playoff_analysis_1970_2025"
)

if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# Optional manual corrections for inferred historical seeds.
seed_override_file <- file.path(
  output_directory,
  "playoff_seed_overrides.csv"
)

# Simulation settings.
n_simulations <- 50000L
simulation_seed <- 20260927L

# Minimum prior postseason games required before a walk-forward
# logistic calibration is reported.
walkforward_min_games <- 30L

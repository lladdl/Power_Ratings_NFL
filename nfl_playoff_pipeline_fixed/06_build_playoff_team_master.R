#!/usr/bin/env Rscript

# ============================================================
# 06 - BUILD ONE PLAYOFF TEAM-SEASON MASTER TABLE FOR SHINY
# ============================================================

source("00_config.R")
source(file.path("R", "playoff_helpers.R"))

files <- list(
  outcomes = file.path(
    output_directory,
    "02_playoff_team_outcomes_1970_2025.csv"
  ),
  ratings = file.path(
    output_directory,
    "06_pre_vs_post_playoff_ratings.csv"
  ),
  odds = file.path(
    output_directory,
    "08_super_bowl_probabilities_by_season.csv"
  ),
  performance = file.path(
    output_directory,
    "10_playoff_overperformance.csv"
  )
)

if (!all(file.exists(unlist(files)))) {
  stop("Run scripts 01-04 before building the playoff team master.")
}

outcomes <- read_csv_base(files$outcomes)
ratings <- read_csv_base(files$ratings)
odds <- read_csv_base(files$odds)
performance <- read_csv_base(files$performance)

# Keep one row per team-season from the odds file.
odds_keep <- odds

master_playoff <- merge(
  outcomes,
  ratings,
  by = c("season", "team"),
  all.x = TRUE,
  suffixes = c("", "_ratings")
)

master_playoff <- merge(
  master_playoff,
  odds_keep,
  by = c("season", "team"),
  all.x = TRUE,
  suffixes = c("", "_odds")
)

master_playoff <- merge(
  master_playoff,
  performance,
  by = c("season", "team"),
  all.x = TRUE,
  suffixes = c("", "_performance")
)

master_playoff <- master_playoff[
  order(
    master_playoff$season,
    -master_playoff$champion,
    master_playoff$team
  ),
  ,
  drop = FALSE
]

write_csv_base(
  master_playoff,
  file.path(
    output_directory,
    "11_playoff_team_season_master_1970_2025.csv"
  )
)

cat(
  "\nPlayoff team-season master created:",
  nrow(master_playoff),
  "rows.\n"
)

#!/usr/bin/env Rscript

# ============================================================
# RUN COMPLETE PLAYOFF PIPELINE
# ============================================================

scripts <- c(
  "01_build_playoff_archive.R",
  "02_fit_playoff_model_and_upsets.R",
  "03_post_playoff_ratings.R",
  "04_simulate_super_bowl_odds.R",
  "06_build_playoff_team_master.R",
  "05_validate_playoff_pipeline.R"
)

for (script in scripts) {

  cat("\n\n")
  cat("============================================================\n")
  cat("RUNNING:", script, "\n")
  cat("============================================================\n\n")

  source(
    script,
    local = new.env(
      parent = globalenv()
    )
  )
}

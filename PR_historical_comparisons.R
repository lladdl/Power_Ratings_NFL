#!/usr/bin/env Rscript

# ============================================================
# VERSION 12.1 — COMPLETE POST-MERGER HISTORICAL ANALYSIS
# 1970-2025
# ============================================================
#
# PURPOSE
# -------
# 1. Combine:
#      1970-1998 Version 12.1 historical season summaries
#      1999-2025 Version 12.1 modern season summaries
#
# 2. Analyze:
#      - season ratings
#      - historical raw rankings
#      - rating vs point differential
#      - strength of schedule
#      - standardized all-era / era rankings
#
# 3. Produce:
#      - one combined 1970-2025 archive
#      - analysis CSVs
#      - publication-ready PNG plots
#      - console summary of key findings
#
# IMPORTANT
# ---------
# - This script analyzes regular-season final snapshots only.
# - It does NOT use postseason games.
# - Raw Version 12.1 rating remains the primary point-based rating.
# - Standardized ratings are reporting tools only and do not
#   replace or alter the production rating model.
#
# PRIMARY STANDARDIZATION
# -----------------------
# season_rating_z:
#
#   (rating - season mean rating) / season rating SD
#
# Since Version 12.1 centers each season near zero, this is
# effectively rating / within-season SD.
#
# Interpretation:
#   +2.0 = about two within-season SD above league average.
#
# This is the preferred metric for comparing relative dominance
# across eras with different league/scoring environments.
#
# SECONDARY ERA STANDARDIZATION
# -----------------------------
# era_rating_z:
#
#   (rating - era mean rating) / era rating SD
#
# This describes a team's raw-rating extremity relative to all
# team-seasons in its decade-style era.
#
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "ggplot2",
  "tidyr",
  "stringr",
  "purrr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (
  length(
    missing_packages
  ) >
  0
) {
  
  stop(
    paste0(
      "Install these packages first: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(stringr)
  library(purrr)
})


# ============================================================
# 1. PATH SETTINGS
# ============================================================

downloads_directory <- path.expand(
  "~/Downloads"
)

historical_directory <- file.path(
  downloads_directory,
  "historical_nfl_1970_1998"
)

historical_file <- file.path(
  historical_directory,
  "version12_1_1970_1998_all_season_team_summaries.csv"
)

# OPTIONAL:
# If you know the exact path of your existing 1999-2025 combined
# Version 12.1 archive, put it here.
#
# Example:
#
# modern_combined_file <- file.path(
#   downloads_directory,
#   "version12_1_all_season_team_summaries.csv"
# )
#
# Leave as NA to auto-detect the modern files under ~/Downloads.

modern_combined_file <- NA_character_

analysis_directory <- file.path(
  downloads_directory,
  "version12_1_1970_2025_historical_analysis"
)

plot_directory <- file.path(
  analysis_directory,
  "plots"
)

if (
  !dir.exists(
    analysis_directory
  )
) {
  
  dir.create(
    analysis_directory,
    recursive = TRUE
  )
}

if (
  !dir.exists(
    plot_directory
  )
) {
  
  dir.create(
    plot_directory,
    recursive = TRUE
  )
}


# ============================================================
# 2. REQUIRED COMMON COLUMNS
# ============================================================

required_columns <- c(
  "season",
  "through_week",
  "team",
  "rank",
  "rating",
  "offense_rank",
  "offense_rating",
  "defense_rank",
  "defense_rating",
  "record",
  "wins",
  "losses",
  "ties",
  "win_pct",
  "scheduled_games",
  "games_played",
  "remaining_games",
  "points_for",
  "points_against",
  "point_differential",
  "points_for_per_game",
  "points_against_per_game",
  "point_differential_per_game",
  "rating_minus_point_diff_per_game",
  "played_SOS",
  "played_SOS_rank",
  "remaining_SOS",
  "remaining_SOS_rank",
  "full_SOS",
  "full_SOS_rank",
  "played_location_difficulty",
  "played_location_difficulty_rank",
  "remaining_location_difficulty",
  "remaining_location_difficulty_rank",
  "full_location_difficulty",
  "full_location_difficulty_rank",
  "estimated_HFA",
  "league_mean_points",
  "predictive_margin_sd",
  "average_games_per_team"
)


# ============================================================
# 3. HELPERS
# ============================================================

validate_summary_schema <- function(
    data,
    source_name
) {
  
  missing_columns <- setdiff(
    required_columns,
    names(
      data
    )
  )
  
  if (
    length(
      missing_columns
    ) >
    0
  ) {
    
    stop(
      paste0(
        source_name,
        " is missing required columns:\n",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
  }
  
  invisible(
    TRUE
  )
}


read_summary_file <- function(
    file_path
) {
  
  suppressMessages(
    readr::read_csv(
      file_path,
      show_col_types = FALSE,
      progress = FALSE
    )
  )
}


make_era <- function(
    season
) {
  
  dplyr::case_when(
    season >= 1970 &
      season <= 1979 ~
      "1970s",
    season >= 1980 &
      season <= 1989 ~
      "1980s",
    season >= 1990 &
      season <= 1999 ~
      "1990s",
    season >= 2000 &
      season <= 2009 ~
      "2000s",
    season >= 2010 &
      season <= 2019 ~
      "2010s",
    season >= 2020 &
      season <= 2025 ~
      "2020s",
    TRUE ~
      NA_character_
  )
}


safe_cor <- function(
    x,
    y
) {
  
  valid <- (
    is.finite(
      x
    ) &
      is.finite(
        y
      )
  )
  
  if (
    sum(
      valid
    ) <
    3
  ) {
    
    return(
      NA_real_
    )
  }
  
  cor(
    x[
      valid
    ],
    y[
      valid
    ]
  )
}


safe_lm_summary <- function(
    formula,
    data
) {
  
  fit <- lm(
    formula,
    data =
      data
  )
  
  coefficients <- summary(
    fit
  )$coefficients
  
  data.frame(
    intercept =
      unname(
        coefficients[
          1,
          "Estimate"
        ]
      ),
    slope =
      if (
        nrow(
          coefficients
        ) >=
        2
      ) {
        unname(
          coefficients[
            2,
            "Estimate"
          ]
        )
      } else {
        NA_real_
      },
    r_squared =
      summary(
        fit
      )$r.squared,
    adjusted_r_squared =
      summary(
        fit
      )$adj.r.squared,
    residual_sd =
      sigma(
        fit
      ),
    stringsAsFactors =
      FALSE
  )
}


save_plot <- function(
    plot_object,
    filename,
    width = 10,
    height = 7
) {
  
  ggplot2::ggsave(
    filename =
      file.path(
        plot_directory,
        filename
      ),
    plot =
      plot_object,
    width =
      width,
    height =
      height,
    dpi =
      300
  )
}


# ============================================================
# 4. LOAD 1970-1998 ARCHIVE
# ============================================================

if (
  !file.exists(
    historical_file
  )
) {
  
  stop(
    paste0(
      "Could not find the 1970-1998 combined archive:\n",
      historical_file
    )
  )
}

historical <- read_summary_file(
  historical_file
)

validate_summary_schema(
  historical,
  "1970-1998 archive"
)

historical <- historical |>
  filter(
    season >=
      1970,
    season <=
      1998
  )


# ============================================================
# 5. LOAD / AUTO-DETECT 1999-2025 ARCHIVE
# ============================================================

load_modern_archive <- function() {
  
  # ----------------------------------------------------------
  # 5A. User supplied exact combined file
  # ----------------------------------------------------------
  
  if (
    !is.na(
      modern_combined_file
    ) &&
    file.exists(
      modern_combined_file
    )
  ) {
    
    candidate <- read_summary_file(
      modern_combined_file
    )
    
    validate_summary_schema(
      candidate,
      "Specified modern combined archive"
    )
    
    candidate <- candidate |>
      filter(
        season >=
          1999,
        season <=
          2025
      )
    
    if (
      length(
        unique(
          candidate$season
        )
      ) ==
      27
    ) {
      
      cat(
        "Using specified modern combined archive:\n",
        modern_combined_file,
        "\n\n",
        sep = ""
      )
      
      return(
        candidate
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # 5B. Search for combined Version 12.1 archive
  # ----------------------------------------------------------
  
  combined_candidates <- list.files(
    downloads_directory,
    pattern =
      "version12_1.*all_season_team_summaries\\.csv$",
    recursive =
      TRUE,
    full.names =
      TRUE,
    ignore.case =
      TRUE
  )
  
  if (
    length(
      combined_candidates
    ) >
    0
  ) {
    
    candidate_info <- file.info(
      combined_candidates
    )
    
    combined_candidates <- combined_candidates[
      order(
        candidate_info$mtime,
        decreasing =
          TRUE
      )
    ]
    
    for (
      file_path in
      combined_candidates
    ) {
      
      candidate <- tryCatch(
        read_summary_file(
          file_path
        ),
        error = function(
    e
        ) {
          NULL
        }
      )
      
      if (
        is.null(
          candidate
        )
      ) {
        
        next
      }
      
      if (
        !all(
          required_columns %in%
          names(
            candidate
          )
        )
      ) {
        
        next
      }
      
      candidate <- candidate |>
        filter(
          season >=
            1999,
          season <=
            2025
        )
      
      seasons_found <- sort(
        unique(
          candidate$season
        )
      )
      
      if (
        identical(
          seasons_found,
          1999:2025
        )
      ) {
        
        cat(
          "Auto-detected modern combined archive:\n",
          file_path,
          "\n\n",
          sep = ""
        )
        
        return(
          candidate
        )
      }
    }
  }
  
  
  # ----------------------------------------------------------
  # 5C. Fall back to individual season summary CSVs
  # ----------------------------------------------------------
  
  individual_candidates <- list.files(
    downloads_directory,
    pattern =
      "^version12_1_[0-9]{4}_team_summary.*\\.csv$",
    recursive =
      TRUE,
    full.names =
      TRUE,
    ignore.case =
      TRUE
  )
  
  if (
    length(
      individual_candidates
    ) ==
    0
  ) {
    
    stop(
      paste0(
        "Could not locate the 1999-2025 Version 12.1 archive.\n\n",
        "Either:\n",
        "1. Set modern_combined_file near the top of this script, or\n",
        "2. Put the 1999-2025 version12_1_<year>_team_summary.csv ",
        "files somewhere under ~/Downloads."
      )
    )
  }
  
  individual_info <- data.frame(
    file =
      individual_candidates,
    mtime =
      file.info(
        individual_candidates
      )$mtime,
    stringsAsFactors =
      FALSE
  )
  
  individual_info <- individual_info[
    order(
      individual_info$mtime,
      decreasing =
        TRUE
    ),
  ]
  
  season_storage <- list()
  storage_position <- 1
  
  for (
    file_path in
    individual_info$file
  ) {
    
    candidate <- tryCatch(
      read_summary_file(
        file_path
      ),
      error = function(
    e
      ) {
        NULL
      }
    )
    
    if (
      is.null(
        candidate
      ) ||
      nrow(
        candidate
      ) ==
      0
    ) {
      
      next
    }
    
    if (
      !all(
        required_columns %in%
        names(
          candidate
        )
      )
    ) {
      
      next
    }
    
    candidate <- candidate |>
      filter(
        season >=
          1999,
        season <=
          2025
      )
    
    if (
      nrow(
        candidate
      ) ==
      0
    ) {
      
      next
    }
    
    candidate$source_file <- file_path
    
    season_storage[[storage_position]] <- candidate
    storage_position <- storage_position + 1
  }
  
  modern_candidates <- bind_rows(
    season_storage
  )
  
  if (
    nrow(
      modern_candidates
    ) ==
    0
  ) {
    
    stop(
      "Individual modern files were found, but none had the required Version 12.1 schema."
    )
  }
  
  # If duplicate season/team rows exist, prefer the latest final
  # snapshot by through_week, then the most recently modified file.
  modern_candidates <- modern_candidates |>
    mutate(
      source_mtime =
        file.info(
          source_file
        )$mtime
    ) |>
    arrange(
      season,
      team,
      desc(
        through_week
      ),
      desc(
        source_mtime
      )
    ) |>
    group_by(
      season,
      team
    ) |>
    slice(
      1
    ) |>
    ungroup() |>
    select(
      -source_file,
      -source_mtime
    )
  
  seasons_found <- sort(
    unique(
      modern_candidates$season
    )
  )
  
  missing_seasons <- setdiff(
    1999:2025,
    seasons_found
  )
  
  if (
    length(
      missing_seasons
    ) >
    0
  ) {
    
    stop(
      paste0(
        "Could not assemble every modern season. Missing:\n",
        paste(
          missing_seasons,
          collapse = ", "
        )
      )
    )
  }
  
  cat(
    "Built modern archive from individual season CSVs.\n\n"
  )
  
  modern_candidates
}


modern <- load_modern_archive()

validate_summary_schema(
  modern,
  "1999-2025 archive"
)


# ============================================================
# 6. COMBINE 1970-2025
# ============================================================

historical <- historical[
  ,
  required_columns
]

modern <- modern[
  ,
  required_columns
]

combined <- bind_rows(
  historical,
  modern
) |>
  arrange(
    season,
    rank,
    team
  )


# ============================================================
# 7. CORE VALIDATION
# ============================================================

expected_seasons <- as.integer(
  1970:2025
)

combined$season <- as.integer(
  combined$season
)

seasons_found <- sort(
  unique(
    combined$season
  )
)

if (
  !setequal(
    seasons_found,
    expected_seasons
  ) ||
  length(
    seasons_found
  ) !=
  length(
    expected_seasons
  )
) {
  
  stop(
    paste0(
      "Combined archive does not contain exactly 1970-2025.\n",
      "Found: ",
      paste(
        seasons_found,
        collapse = ", "
      )
    )
  )
}


duplicate_rows <- combined |>
  count(
    season,
    team
  ) |>
  filter(
    n >
      1
  )

if (
  nrow(
    duplicate_rows
  ) >
  0
) {
  
  print(
    duplicate_rows
  )
  
  stop(
    "Duplicate season/team rows found after combining."
  )
}


expected_rows <- (
  nrow(
    historical
  ) +
    nrow(
      modern
    )
)

if (
  nrow(
    combined
  ) !=
  expected_rows
) {
  
  stop(
    "Unexpected row-count change during combine."
  )
}


if (
  any(
    combined$remaining_games !=
    0,
    na.rm = TRUE
  )
) {
  
  warning(
    "At least one final historical row has remaining_games != 0."
  )
}


# ============================================================
# 8. ADD STANDARDIZED / HISTORICAL RANKING VARIABLES
# ============================================================

combined <- combined |>
  mutate(
    era =
      make_era(
        season
      )
  ) |>
  group_by(
    season
  ) |>
  mutate(
    season_rating_mean =
      mean(
        rating,
        na.rm = TRUE
      ),
    season_rating_sd =
      sd(
        rating,
        na.rm = TRUE
      ),
    season_rating_z =
      (
        rating -
          season_rating_mean
      ) /
      season_rating_sd,
    season_offense_z =
      (
        offense_rating -
          mean(
            offense_rating,
            na.rm = TRUE
          )
      ) /
      sd(
        offense_rating,
        na.rm = TRUE
      ),
    season_defense_z =
      (
        defense_rating -
          mean(
            defense_rating,
            na.rm = TRUE
          )
      ) /
      sd(
        defense_rating,
        na.rm = TRUE
      ),
    season_rating_percentile =
      100 *
      dplyr::percent_rank(
        rating
      ),
    record_rank =
      as.integer(
        rank(
          -win_pct,
          ties.method = "min"
        )
      ),
    record_rating_gap =
      record_rank -
      rank
  ) |>
  ungroup() |>
  group_by(
    era
  ) |>
  mutate(
    era_rating_z =
      (
        rating -
          mean(
            rating,
            na.rm = TRUE
          )
      ) /
      sd(
        rating,
        na.rm = TRUE
      ),
    era_raw_rank =
      as.integer(
        rank(
          -rating,
          ties.method = "min"
        )
      ),
    era_standardized_rank =
      as.integer(
        rank(
          -season_rating_z,
          ties.method = "min"
        )
      )
  ) |>
  ungroup() |>
  mutate(
    historical_raw_rank =
      as.integer(
        rank(
          -rating,
          ties.method = "min"
        )
      ),
    historical_standardized_rank =
      as.integer(
        rank(
          -season_rating_z,
          ties.method = "min"
        )
      ),
    historical_offense_rank =
      as.integer(
        rank(
          -offense_rating,
          ties.method = "min"
        )
      ),
    historical_defense_rank =
      as.integer(
        rank(
          -defense_rating,
          ties.method = "min"
        )
      ),
    schedule_adjustment =
      rating -
      point_differential_per_game
  )


# ============================================================
# 9. SAVE COMBINED MASTER ARCHIVE
# ============================================================

combined_output <- file.path(
  analysis_directory,
  "version12_1_1970_2025_all_season_team_summaries_enriched.csv"
)

readr::write_csv(
  combined,
  combined_output
)


# ============================================================
# 10. SEASON-LEVEL RATING ANALYSIS
# ============================================================

season_analysis <- combined |>
  group_by(
    season,
    era
  ) |>
  summarise(
    teams =
      n(),
    average_games_per_team =
      first(
        average_games_per_team
      ),
    rating_mean =
      mean(
        rating
      ),
    rating_sd =
      sd(
        rating
      ),
    rating_min =
      min(
        rating
      ),
    rating_max =
      max(
        rating
      ),
    rating_range =
      rating_max -
      rating_min,
    top_team =
      team[
        which.max(
          rating
        )
      ],
    top_rating =
      max(
        rating
      ),
    bottom_team =
      team[
        which.min(
          rating
        )
      ],
    bottom_rating =
      min(
        rating
      ),
    league_mean_points =
      first(
        league_mean_points
      ),
    estimated_HFA =
      first(
        estimated_HFA
      ),
    predictive_margin_sd =
      first(
        predictive_margin_sd
      ),
    rating_win_pct_correlation =
      safe_cor(
        rating,
        win_pct
      ),
    rating_point_diff_correlation =
      safe_cor(
        rating,
        point_differential_per_game
      ),
    schedule_adjustment_SOS_correlation =
      safe_cor(
        schedule_adjustment,
        played_SOS
      ),
    .groups =
      "drop"
  )


readr::write_csv(
  season_analysis,
  file.path(
    analysis_directory,
    "01_season_rating_analysis.csv"
  )
)


# ============================================================
# 11. HISTORICAL RAW RANKINGS
# ============================================================

historical_raw_rankings <- combined |>
  arrange(
    historical_raw_rank,
    season,
    team
  ) |>
  select(
    historical_raw_rank,
    season,
    era,
    team,
    record,
    rank,
    rating,
    offense_rank,
    offense_rating,
    defense_rank,
    defense_rating,
    win_pct,
    point_differential_per_game,
    played_SOS,
    played_SOS_rank,
    season_rating_z,
    record_rank,
    record_rating_gap
  )


readr::write_csv(
  historical_raw_rankings,
  file.path(
    analysis_directory,
    "02_historical_raw_rating_rankings.csv"
  )
)


readr::write_csv(
  head(
    historical_raw_rankings,
    100
  ),
  file.path(
    analysis_directory,
    "03_top_100_raw_ratings.csv"
  )
)


# ============================================================
# 12. HISTORICAL OFFENSE / DEFENSE RANKINGS
# ============================================================

historical_offense_rankings <- combined |>
  arrange(
    historical_offense_rank,
    season,
    team
  ) |>
  select(
    historical_offense_rank,
    season,
    era,
    team,
    record,
    offense_rating,
    offense_rank,
    season_offense_z,
    rating,
    historical_raw_rank
  )


historical_defense_rankings <- combined |>
  arrange(
    historical_defense_rank,
    season,
    team
  ) |>
  select(
    historical_defense_rank,
    season,
    era,
    team,
    record,
    defense_rating,
    defense_rank,
    season_defense_z,
    rating,
    historical_raw_rank
  )


readr::write_csv(
  historical_offense_rankings,
  file.path(
    analysis_directory,
    "04_historical_offense_rankings.csv"
  )
)


readr::write_csv(
  historical_defense_rankings,
  file.path(
    analysis_directory,
    "05_historical_defense_rankings.csv"
  )
)


# ============================================================
# 13. RATING VS POINT DIFFERENTIAL
# ============================================================

rating_pd_overall <- safe_lm_summary(
  rating ~ point_differential_per_game,
  combined
)

rating_pd_overall$n <- nrow(
  combined
)

rating_pd_overall$correlation <- safe_cor(
  combined$rating,
  combined$point_differential_per_game
)

rating_pd_overall <- rating_pd_overall |>
  select(
    n,
    correlation,
    everything()
  )


rating_pd_by_era <- combined |>
  group_by(
    era
  ) |>
  group_modify(
    function(
    .x,
    .y
    ) {
      
      model_summary <- safe_lm_summary(
        rating ~ point_differential_per_game,
        .x
      )
      
      model_summary$n <- nrow(
        .x
      )
      
      model_summary$correlation <- safe_cor(
        .x$rating,
        .x$point_differential_per_game
      )
      
      model_summary |>
        select(
          n,
          correlation,
          everything()
        )
    }
  ) |>
  ungroup()


rating_pd_by_season <- combined |>
  group_by(
    season,
    era
  ) |>
  summarise(
    n =
      n(),
    correlation =
      safe_cor(
        rating,
        point_differential_per_game
      ),
    mean_absolute_rating_minus_pd =
      mean(
        abs(
          schedule_adjustment
        )
      ),
    .groups =
      "drop"
  )


readr::write_csv(
  rating_pd_overall,
  file.path(
    analysis_directory,
    "06_rating_vs_point_differential_overall.csv"
  )
)


readr::write_csv(
  rating_pd_by_era,
  file.path(
    analysis_directory,
    "07_rating_vs_point_differential_by_era.csv"
  )
)


readr::write_csv(
  rating_pd_by_season,
  file.path(
    analysis_directory,
    "08_rating_vs_point_differential_by_season.csv"
  )
)


# ============================================================
# 14. BIGGEST RATING VS POINT-DIFFERENTIAL DIFFERENCES
# ============================================================

rating_pd_adjustments <- combined |>
  arrange(
    desc(
      schedule_adjustment
    )
  ) |>
  select(
    season,
    era,
    team,
    record,
    rating,
    point_differential_per_game,
    schedule_adjustment,
    played_SOS,
    played_SOS_rank,
    rank,
    historical_raw_rank
  )


readr::write_csv(
  rating_pd_adjustments,
  file.path(
    analysis_directory,
    "09_rating_minus_point_differential_adjustments.csv"
  )
)


# ============================================================
# 15. SOS ANALYSIS
# ============================================================

sos_valid <- combined |>
  filter(
    is.finite(
      played_SOS
    ),
    is.finite(
      schedule_adjustment
    )
  )


sos_overall <- safe_lm_summary(
  schedule_adjustment ~ played_SOS,
  sos_valid
)

sos_overall$n <- nrow(
  sos_valid
)

sos_overall$correlation <- safe_cor(
  sos_valid$schedule_adjustment,
  sos_valid$played_SOS
)

sos_overall <- sos_overall |>
  select(
    n,
    correlation,
    everything()
  )


sos_by_era <- sos_valid |>
  group_by(
    era
  ) |>
  group_modify(
    function(
    .x,
    .y
    ) {
      
      model_summary <- safe_lm_summary(
        schedule_adjustment ~ played_SOS,
        .x
      )
      
      model_summary$n <- nrow(
        .x
      )
      
      model_summary$correlation <- safe_cor(
        .x$schedule_adjustment,
        .x$played_SOS
      )
      
      model_summary |>
        select(
          n,
          correlation,
          everything()
        )
    }
  ) |>
  ungroup()


hardest_schedules <- combined |>
  arrange(
    desc(
      played_SOS
    )
  ) |>
  select(
    season,
    era,
    team,
    record,
    played_SOS,
    played_SOS_rank,
    rating,
    point_differential_per_game,
    schedule_adjustment,
    rank,
    historical_raw_rank
  )


easiest_schedules <- combined |>
  arrange(
    played_SOS
  ) |>
  select(
    season,
    era,
    team,
    record,
    played_SOS,
    played_SOS_rank,
    rating,
    point_differential_per_game,
    schedule_adjustment,
    rank,
    historical_raw_rank
  )


readr::write_csv(
  sos_overall,
  file.path(
    analysis_directory,
    "10_SOS_analysis_overall.csv"
  )
)


readr::write_csv(
  sos_by_era,
  file.path(
    analysis_directory,
    "11_SOS_analysis_by_era.csv"
  )
)


readr::write_csv(
  hardest_schedules,
  file.path(
    analysis_directory,
    "12_hardest_historical_schedules.csv"
  )
)


readr::write_csv(
  easiest_schedules,
  file.path(
    analysis_directory,
    "13_easiest_historical_schedules.csv"
  )
)


# ============================================================
# 16. STANDARDIZED HISTORICAL RANKINGS
# ============================================================

standardized_rankings <- combined |>
  arrange(
    historical_standardized_rank,
    season,
    team
  ) |>
  select(
    historical_standardized_rank,
    historical_raw_rank,
    season,
    era,
    team,
    record,
    rating,
    season_rating_z,
    season_rating_percentile,
    era_rating_z,
    era_standardized_rank,
    era_raw_rank,
    offense_rating,
    defense_rating,
    point_differential_per_game,
    played_SOS,
    rank
  )


readr::write_csv(
  standardized_rankings,
  file.path(
    analysis_directory,
    "14_historical_standardized_rankings.csv"
  )
)


readr::write_csv(
  head(
    standardized_rankings,
    100
  ),
  file.path(
    analysis_directory,
    "15_top_100_standardized_ratings.csv"
  )
)


# ============================================================
# 17. ERA STANDARDIZED RANKINGS
# ============================================================

era_standardized_rankings <- combined |>
  arrange(
    era,
    era_standardized_rank,
    season,
    team
  ) |>
  select(
    era,
    era_standardized_rank,
    season,
    team,
    record,
    rating,
    season_rating_z,
    era_rating_z,
    historical_standardized_rank,
    historical_raw_rank,
    offense_rating,
    defense_rating,
    point_differential_per_game,
    played_SOS
  )


top_teams_per_era <- era_standardized_rankings |>
  group_by(
    era
  ) |>
  slice_head(
    n =
      25
  ) |>
  ungroup()


readr::write_csv(
  era_standardized_rankings,
  file.path(
    analysis_directory,
    "16_era_standardized_rankings.csv"
  )
)


readr::write_csv(
  top_teams_per_era,
  file.path(
    analysis_directory,
    "17_top_25_teams_per_era_standardized.csv"
  )
)


# ============================================================
# 18. ERA SUMMARY
# ============================================================

era_summary <- combined |>
  group_by(
    era
  ) |>
  summarise(
    seasons =
      n_distinct(
        season
      ),
    team_seasons =
      n(),
    average_rating_sd =
      mean(
        season_rating_sd
      ),
    average_league_points =
      mean(
        league_mean_points
      ),
    average_HFA =
      mean(
        estimated_HFA
      ),
    average_predictive_margin_sd =
      mean(
        predictive_margin_sd
      ),
    rating_win_pct_correlation =
      safe_cor(
        rating,
        win_pct
      ),
    rating_point_diff_correlation =
      safe_cor(
        rating,
        point_differential_per_game
      ),
    adjustment_SOS_correlation =
      safe_cor(
        schedule_adjustment,
        played_SOS
      ),
    .groups =
      "drop"
  )


readr::write_csv(
  era_summary,
  file.path(
    analysis_directory,
    "18_era_summary.csv"
  )
)


# ============================================================
# 19. RECORD VS RATING GAP
# ============================================================

record_rating_gap_rankings <- combined |>
  arrange(
    desc(
      record_rating_gap
    )
  ) |>
  select(
    season,
    era,
    team,
    record,
    win_pct,
    record_rank,
    rank,
    record_rating_gap,
    rating,
    point_differential_per_game,
    played_SOS,
    historical_raw_rank,
    historical_standardized_rank
  )


readr::write_csv(
  record_rating_gap_rankings,
  file.path(
    analysis_directory,
    "19_record_vs_rating_rank_gap.csv"
  )
)


# ============================================================
# 20. RATING BANDS
# ============================================================

rating_bands <- combined |>
  mutate(
    rating_band =
      cut(
        rating,
        breaks =
          c(
            -Inf,
            -7,
            -3,
            0,
            3,
            7,
            Inf
          ),
        labels =
          c(
            "< -7",
            "-7 to -3",
            "-3 to 0",
            "0 to 3",
            "3 to 7",
            "> 7"
          ),
        right =
          FALSE
      )
  ) |>
  group_by(
    rating_band
  ) |>
  summarise(
    team_seasons =
      n(),
    average_win_pct =
      mean(
        win_pct
      ),
    average_point_diff_per_game =
      mean(
        point_differential_per_game
      ),
    average_SOS =
      mean(
        played_SOS
      ),
    .groups =
      "drop"
  )


readr::write_csv(
  rating_bands,
  file.path(
    analysis_directory,
    "20_rating_band_outcomes.csv"
  )
)


# ============================================================
# 21. PLOTS — TOP RAW RATINGS
# ============================================================

top_raw_plot_data <- historical_raw_rankings |>
  slice_head(
    n =
      30
  ) |>
  mutate(
    label =
      paste(
        season,
        team
      ),
    label =
      factor(
        label,
        levels =
          rev(
            label
          )
      )
  )


plot_top_raw <- ggplot(
  top_raw_plot_data,
  aes(
    x =
      rating,
    y =
      label
  )
) +
  geom_col() +
  labs(
    title =
      "Top 30 Version 12.1 Regular-Season Ratings, 1970-2025",
    subtitle =
      "Raw rating is estimated neutral-field points above league average",
    x =
      "Version 12.1 rating",
    y =
      NULL
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_top_raw,
  "01_top_30_raw_ratings.png",
  width =
    10,
  height =
    9
)


# ============================================================
# 22. PLOTS — TOP STANDARDIZED RATINGS
# ============================================================

top_standardized_plot_data <- standardized_rankings |>
  slice_head(
    n =
      30
  ) |>
  mutate(
    label =
      paste(
        season,
        team
      ),
    label =
      factor(
        label,
        levels =
          rev(
            label
          )
      )
  )


plot_top_standardized <- ggplot(
  top_standardized_plot_data,
  aes(
    x =
      season_rating_z,
    y =
      label
  )
) +
  geom_col() +
  labs(
    title =
      "Top 30 Era-Comparable Team Seasons, 1970-2025",
    subtitle =
      "Season z-score measures dominance relative to the team's contemporaries",
    x =
      "Within-season rating z-score",
    y =
      NULL
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_top_standardized,
  "02_top_30_standardized_ratings.png",
  width =
    10,
  height =
    9
)


# ============================================================
# 23. PLOT — RATING VS POINT DIFFERENTIAL
# ============================================================

plot_rating_pd <- ggplot(
  combined,
  aes(
    x =
      point_differential_per_game,
    y =
      rating
  )
) +
  geom_point(
    alpha =
      0.35
  ) +
  geom_smooth(
    method =
      "lm",
    se =
      FALSE
  ) +
  labs(
    title =
      "Version 12.1 Rating vs Point Differential",
    subtitle =
      "Each point is one regular-season team-season, 1970-2025",
    x =
      "Point differential per game",
    y =
      "Version 12.1 rating"
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_rating_pd,
  "03_rating_vs_point_differential.png"
)


# ============================================================
# 24. PLOT — SOS VS RATING ADJUSTMENT
# ============================================================

plot_sos_adjustment <- ggplot(
  sos_valid,
  aes(
    x =
      played_SOS,
    y =
      schedule_adjustment
  )
) +
  geom_point(
    alpha =
      0.35
  ) +
  geom_smooth(
    method =
      "lm",
    se =
      FALSE
  ) +
  labs(
    title =
      "Strength of Schedule vs Rating Adjustment",
    subtitle =
      "Adjustment = Version 12.1 rating minus raw point differential per game",
    x =
      "Played SOS",
    y =
      "Rating minus point differential per game"
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_sos_adjustment,
  "04_SOS_vs_rating_adjustment.png"
)


# ============================================================
# 25. PLOT — RATING SPREAD BY SEASON
# ============================================================

plot_rating_sd <- ggplot(
  season_analysis,
  aes(
    x =
      season,
    y =
      rating_sd
  )
) +
  geom_line() +
  labs(
    title =
      "Within-Season Rating Spread, 1970-2025",
    subtitle =
      "Higher SD means the season produced a wider separation between teams",
    x =
      "Season",
    y =
      "Rating standard deviation"
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_rating_sd,
  "05_rating_standard_deviation_by_season.png"
)


# ============================================================
# 26. PLOT — LEAGUE SCORING ENVIRONMENT
# ============================================================

plot_scoring <- ggplot(
  season_analysis,
  aes(
    x =
      season,
    y =
      league_mean_points
  )
) +
  geom_line() +
  labs(
    title =
      "Estimated League Scoring Environment, 1970-2025",
    x =
      "Season",
    y =
      "Estimated points per team per game"
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_scoring,
  "06_league_scoring_environment.png"
)


# ============================================================
# 27. PLOT — HOME-FIELD ADVANTAGE
# ============================================================

plot_hfa <- ggplot(
  season_analysis,
  aes(
    x =
      season,
    y =
      estimated_HFA
  )
) +
  geom_line() +
  geom_hline(
    yintercept =
      2.5,
    linetype =
      "dashed"
  ) +
  labs(
    title =
      "Estimated Home-Field Advantage, 1970-2025",
    subtitle =
      "Dashed line is the generic 2.5-point structural prior",
    x =
      "Season",
    y =
      "Estimated HFA in points"
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_hfa,
  "07_estimated_HFA_by_season.png"
)


# ============================================================
# 28. PLOT — ERA DISTRIBUTIONS OF SEASON Z-SCORES
# ============================================================

plot_era_standardized <- ggplot(
  combined,
  aes(
    x =
      era,
    y =
      season_rating_z
  )
) +
  geom_boxplot(
    outlier.alpha =
      0.25
  ) +
  labs(
    title =
      "Standardized Team Strength by Era",
    subtitle =
      "Within-season z-scores put relative dominance on a common scale",
    x =
      "Era",
    y =
      "Within-season rating z-score"
  ) +
  theme_minimal(
    base_size =
      12
  )


save_plot(
  plot_era_standardized,
  "08_standardized_rating_distribution_by_era.png"
)


# ============================================================
# 29. VALIDATION TABLE
# ============================================================

validation <- data.frame(
  item =
    c(
      "first_season",
      "last_season",
      "number_of_seasons",
      "historical_1970_1998_rows",
      "modern_1999_2025_rows",
      "combined_team_seasons",
      "duplicate_season_team_rows",
      "all_seasons_present",
      "all_ratings_finite",
      "all_season_z_finite"
    ),
  value =
    c(
      min(
        combined$season
      ),
      max(
        combined$season
      ),
      n_distinct(
        combined$season
      ),
      nrow(
        historical
      ),
      nrow(
        modern
      ),
      nrow(
        combined
      ),
      nrow(
        duplicate_rows
      ),
      setequal(
        seasons_found,
        expected_seasons
      ) &&
        length(
          seasons_found
        ) ==
        length(
          expected_seasons
        ),
      all(
        is.finite(
          combined$rating
        )
      ),
      all(
        is.finite(
          combined$season_rating_z
        )
      )
    ),
  stringsAsFactors =
    FALSE
)


readr::write_csv(
  validation,
  file.path(
    analysis_directory,
    "21_validation.csv"
  )
)


# ============================================================
# 30. CONSOLE SUMMARY
# ============================================================

top_raw_console <- historical_raw_rankings |>
  slice_head(
    n =
      20
  ) |>
  select(
    historical_raw_rank,
    season,
    team,
    record,
    rating,
    offense_rating,
    defense_rating,
    point_differential_per_game,
    played_SOS
  )


top_standardized_console <- standardized_rankings |>
  slice_head(
    n =
      20
  ) |>
  select(
    historical_standardized_rank,
    season,
    team,
    record,
    rating,
    season_rating_z,
    historical_raw_rank
  )


hardest_console <- hardest_schedules |>
  slice_head(
    n =
      15
  ) |>
  select(
    season,
    team,
    record,
    played_SOS,
    rating,
    point_differential_per_game,
    schedule_adjustment
  )


cat(
  "\n========================================\n"
)

cat(
  "VERSION 12.1 HISTORICAL ANALYSIS\n"
)

cat(
  "1970-2025\n"
)

cat(
  "========================================\n\n"
)

cat(
  "Seasons: ",
  n_distinct(
    combined$season
  ),
  "\n",
  sep = ""
)

cat(
  "Team-seasons: ",
  nrow(
    combined
  ),
  "\n\n",
  sep = ""
)


cat(
  "Overall rating vs win percentage correlation: ",
  round(
    safe_cor(
      combined$rating,
      combined$win_pct
    ),
    4
  ),
  "\n",
  sep = ""
)

cat(
  "Overall rating vs point differential/game correlation: ",
  round(
    safe_cor(
      combined$rating,
      combined$point_differential_per_game
    ),
    4
  ),
  "\n",
  sep = ""
)

cat(
  "Rating adjustment vs played SOS correlation: ",
  round(
    safe_cor(
      combined$schedule_adjustment,
      combined$played_SOS
    ),
    4
  ),
  "\n\n",
  sep = ""
)


cat(
  "Rating ~ point differential/game regression:\n"
)

print(
  rating_pd_overall
)


cat(
  "\nRating adjustment ~ SOS regression:\n"
)

print(
  sos_overall
)


cat(
  "\nTop 20 raw historical ratings:\n"
)

print(
  top_raw_console,
  n =
    nrow(
      top_raw_console
    )
)


cat(
  "\nTop 20 standardized historical ratings:\n"
)

print(
  top_standardized_console,
  n =
    nrow(
      top_standardized_console
    )
)


cat(
  "\n15 hardest historical schedules:\n"
)

print(
  hardest_console,
  n =
    nrow(
      hardest_console
    )
)


cat(
  "\nEra summary:\n"
)

print(
  era_summary,
  n =
    nrow(
      era_summary
    )
)


cat(
  "\nRating bands:\n"
)

print(
  rating_bands,
  n =
    nrow(
      rating_bands
    )
)


cat(
  "\n========================================\n"
)

cat(
  "FILES WRITTEN TO\n"
)

cat(
  "========================================\n"
)

cat(
  analysis_directory,
  "\n\n",
  sep = ""
)

cat(
  "Combined master archive:\n",
  combined_output,
  "\n\n",
  sep = ""
)

cat(
  "Plots directory:\n",
  plot_directory,
  "\n",
  sep = ""
)

# ============================================================
# NFL VERSION 12.1
# MODEL BENCHMARK COMPARISON
# ============================================================
#
# PURPOSE
# -------
# Compare Version 12.1 against progressively stronger
# benchmarks:
#
#   1. HFA-only
#   2. Simple point-differential rating
#   3. Basic Elo
#   4. Raw Version 12.1
#   5. Properly calibrated Version 12.1
#   6. Vegas closing spread
#
#
# IMPORTANT
# ---------
#
# All non-market models are walk-forward.
#
# A target game is never used to produce its own prediction.
#
# Version 12.1 forecast calibration is learned using
# PRIOR SEASONS ONLY.
#
# Elo's mapping from Elo difference -> point margin is also
# learned using PRIOR SEASONS ONLY.
#
# ============================================================



# ============================================================
# 1. PACKAGES
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(nflreadr)



# ============================================================
# 2. SETTINGS
# ============================================================

MODEL_FILE <-
  "version12_1_historical_oos_predictions.csv"


# ------------------------------------------------------------
# Seasons available in Version 12.1 history
# ------------------------------------------------------------

FIRST_SEASON <-
  1999


LAST_SEASON <-
  2025


# ------------------------------------------------------------
# HFA baseline
# ------------------------------------------------------------

FIXED_HFA <-
  2.5


# ------------------------------------------------------------
# Basic Elo parameters
#
# Elo is deliberately kept SIMPLE.
#
# We are benchmarking against a reasonable basic system,
# not trying to optimize Elo until it becomes another
# complicated model.
# ------------------------------------------------------------

ELO_START <-
  1500


ELO_K <-
  20


ELO_HOME_ADVANTAGE <-
  55



# ============================================================
# 3. LOAD VERSION 12.1 OOS PREDICTIONS
# ============================================================

v12_raw <-
  readr::read_csv(
    MODEL_FILE,
    show_col_types = FALSE
  )


cat("\n")
cat("========================================\n")
cat("VERSION 12.1 DATA LOADED\n")
cat("========================================\n\n")


cat(
  "Rows:",
  nrow(v12_raw),
  "\n"
)


cat(
  "Seasons:",
  min(
    v12_raw$season,
    na.rm = TRUE
  ),
  "-",
  max(
    v12_raw$season,
    na.rm = TRUE
  ),
  "\n"
)



# ============================================================
# 4. VERIFY REQUIRED COLUMNS
# ============================================================

required_columns <-
  c(
    "season",
    "week",
    "game_id",
    "home_team",
    "away_team",
    "actual_margin",
    "predicted_margin",
    "baseline_margin",
    "games_played_bucket",
    "spread_line"
  )


missing_columns <-
  setdiff(
    required_columns,
    names(v12_raw)
  )


if (
  length(
    missing_columns
  ) >
  0
) {
  
  stop(
    paste0(
      "Missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}



# ============================================================
# 5. CLEAN VERSION 12.1 DATA
# ============================================================

v12_data <-
  v12_raw %>%
  
  transmute(
    
    season =
      as.integer(
        season
      ),
    
    week =
      as.integer(
        week
      ),
    
    game_id =
      as.character(
        game_id
      ),
    
    home_team =
      as.character(
        home_team
      ),
    
    away_team =
      as.character(
        away_team
      ),
    
    actual_margin =
      as.numeric(
        actual_margin
      ),
    
    raw_v12_margin =
      as.numeric(
        predicted_margin
      ),
    
    baseline_margin =
      as.numeric(
        baseline_margin
      ),
    
    games_played_bucket =
      as.integer(
        games_played_bucket
      ),
    
    vegas_margin =
      as.numeric(
        spread_line
      )
  ) %>%
  
  filter(
    
    is.finite(
      actual_margin
    ),
    
    is.finite(
      raw_v12_margin
    ),
    
    is.finite(
      baseline_margin
    ),
    
    games_played_bucket %in%
      1:5
  ) %>%
  
  arrange(
    season,
    week,
    game_id
  )



# ============================================================
# 6. VERSION 12.1 PROPER SHRINKAGE CALIBRATION
# ============================================================

get_fixed_blend_weight <- function(
    average_games_per_team
) {
  
  pmin(
    1,
    pmax(
      0,
      average_games_per_team /
        4
    )
  )
}



learn_v12_shrinkage <- function(
    calibration_data
) {
  
  purrr::map_dfr(
    
    1:5,
    
    function(
    bucket
    ) {
      
      current <-
        calibration_data %>%
        
        filter(
          games_played_bucket ==
            bucket
        )
      
      
      fallback <-
        get_fixed_blend_weight(
          
          if (
            bucket <
            5
          ) {
            
            bucket
            
          } else {
            
            5
          }
        )
      
      
      if (
        nrow(
          current
        ) ==
        0
      ) {
        
        learned_weight <-
          fallback
        
      } else {
        
        delta <- (
          current$raw_v12_margin -
            current$baseline_margin
        )
        
        
        target <- (
          current$actual_margin -
            current$baseline_margin
        )
        
        
        denominator <-
          sum(
            delta^2
          )
        
        
        if (
          !is.finite(
            denominator
          ) ||
          denominator <=
          1e-12
        ) {
          
          learned_weight <-
            fallback
          
        } else {
          
          learned_weight <- (
            sum(
              delta *
                target
            ) /
              denominator
          )
          
          
          learned_weight <-
            pmin(
              1,
              pmax(
                0,
                learned_weight
              )
            )
        }
      }
      
      
      tibble(
        
        games_played_bucket =
          bucket,
        
        learned_v12_weight =
          learned_weight
      )
    }
  )
}



build_proper_v12 <- function(
    data
) {
  
  seasons <-
    sort(
      unique(
        data$season
      )
    )
  
  
  output <-
    list()
  
  
  position <-
    1
  
  
  for (
    target_season in seasons
  ) {
    
    prior <-
      data %>%
      
      filter(
        season <
          target_season
      )
    
    
    target <-
      data %>%
      
      filter(
        season ==
          target_season
      )
    
    
    # First season has no earlier calibration season
    if (
      nrow(
        prior
      ) ==
      0
    ) {
      
      next
    }
    
    
    weights <-
      learn_v12_shrinkage(
        prior
      )
    
    
    target <-
      target %>%
      
      left_join(
        weights,
        by =
          "games_played_bucket"
      ) %>%
      
      mutate(
        
        calibrated_v12_margin = (
          
          baseline_margin +
            
            learned_v12_weight *
            
            (
              raw_v12_margin -
                baseline_margin
            )
        )
      )
    
    
    output[[position]] <- target
    
    
    position <-
      position +
      1
  }
  
  
  bind_rows(
    output
  )
}



benchmark_base <-
  build_proper_v12(
    v12_data
  )



# ============================================================
# 7. LOAD FULL NFL SCHEDULE
# ============================================================
#
# We need the full schedule because Version 12.1's historical
# OOS file generally begins at Week 2.
#
# The simple benchmarks still need Week 1 results when
# forecasting Week 2.
#
# ============================================================

cat("\n")
cat("========================================\n")
cat("LOADING NFL SCHEDULES\n")
cat("========================================\n\n")


schedule_raw <-
  nflreadr::load_schedules(
    FIRST_SEASON:
      LAST_SEASON
  )



schedule <-
  schedule_raw %>%
  
  filter(
    game_type ==
      "REG"
  ) %>%
  
  mutate(
    
    season =
      as.integer(
        season
      ),
    
    week =
      as.integer(
        week
      ),
    
    game_id =
      as.character(
        game_id
      ),
    
    home_team =
      as.character(
        home_team
      ),
    
    away_team =
      as.character(
        away_team
      ),
    
    home_score =
      as.numeric(
        home_score
      ),
    
    away_score =
      as.numeric(
        away_score
      ),
    
    actual_margin = (
      home_score -
        away_score
    )
  ) %>%
  
  filter(
    
    !is.na(
      home_score
    ),
    
    !is.na(
      away_score
    )
  )



# ------------------------------------------------------------
# Neutral-site field
# ------------------------------------------------------------

if (
  "neutral_site" %in%
  names(
    schedule_raw
  )
) {
  
  schedule$neutral_site <-
    schedule_raw$neutral_site[
      match(
        schedule$game_id,
        schedule_raw$game_id
      )
    ]
  
  
  schedule$neutral_site[
    is.na(
      schedule$neutral_site
    )
  ] <-
    FALSE
  
} else {
  
  schedule$neutral_site <-
    FALSE
}



cat(
  "Completed regular-season games loaded:",
  nrow(
    schedule
  ),
  "\n\n"
)



# ############################################################
#
# BENCHMARK 1
#
# HFA-ONLY
#
# ############################################################
#
# This is already represented by baseline_margin:
#
# normal:
#
#   Home by 2.5
#
# neutral:
#
#   0
#
# ============================================================

benchmark_base <-
  benchmark_base %>%
  
  mutate(
    
    hfa_only_margin =
      baseline_margin
  )



# ############################################################
#
# BENCHMARK 2
#
# SIMPLE POINT-DIFFERENTIAL RATING
#
# ############################################################
#
# This is deliberately simple.
#
# For every team:
#
#   1. Look only at games before target week.
#
#   2. Convert each result into a team-perspective margin.
#
#   3. Remove fixed 2.5-point home advantage.
#
#   4. Take average adjusted point differential.
#
#
# Prediction:
#
#   Home Team Rating
#       -
#   Away Team Rating
#       +
#   HFA
#
#
# NO:
#
#   opponent adjustment
#   recency weighting
#   robustness
#   offense/defense separation
#   regularization
#
# This makes it a useful simple benchmark.
#
# ============================================================

build_simple_pd_predictions <- function(
    schedule_data,
    target_games
) {
  
  output <-
    list()
  
  
  position <-
    1
  
  
  target_seasons <-
    sort(
      unique(
        target_games$season
      )
    )
  
  
  for (
    current_season in target_seasons
  ) {
    
    season_schedule <-
      schedule_data %>%
      
      filter(
        season ==
          current_season
      )
    
    
    season_targets <-
      target_games %>%
      
      filter(
        season ==
          current_season
      )
    
    
    target_weeks <-
      sort(
        unique(
          season_targets$week
        )
      )
    
    
    for (
      target_week in target_weeks
    ) {
      
      training <-
        season_schedule %>%
        
        filter(
          week <
            target_week
        )
      
      
      targets <-
        season_targets %>%
        
        filter(
          week ==
            target_week
        )
      
      
      if (
        nrow(
          training
        ) ==
        0
      ) {
        
        next
      }
      
      
      # ------------------------------------------------------
      # Team-perspective performance
      # ------------------------------------------------------
      
      team_performances <-
        bind_rows(
          
          training %>%
            
            transmute(
              
              team =
                home_team,
              
              adjusted_margin = (
                
                actual_margin -
                  
                  ifelse(
                    neutral_site,
                    0,
                    FIXED_HFA
                  )
              )
            ),
          
          
          training %>%
            
            transmute(
              
              team =
                away_team,
              
              adjusted_margin = (
                
                -actual_margin +
                  
                  ifelse(
                    neutral_site,
                    0,
                    FIXED_HFA
                  )
              )
            )
        )
      
      
      team_ratings <-
        team_performances %>%
        
        group_by(
          team
        ) %>%
        
        summarise(
          
          simple_pd_rating =
            mean(
              adjusted_margin
            ),
          
          .groups =
            "drop"
        )
      
      
      home_lookup <-
        team_ratings$simple_pd_rating[
          match(
            targets$home_team,
            team_ratings$team
          )
        ]
      
      
      away_lookup <-
        team_ratings$simple_pd_rating[
          match(
            targets$away_team,
            team_ratings$team
          )
        ]
      
      
      # If somehow a team has not played yet, league average = 0
      home_lookup[
        is.na(
          home_lookup
        )
      ] <-
        0
      
      
      away_lookup[
        is.na(
          away_lookup
        )
      ] <-
        0
      
      
      output[[position]] <- tibble(
          
          game_id =
            targets$game_id,
          
          simple_pd_margin = (
            
            home_lookup -
              away_lookup +
              targets$baseline_margin
          )
        )
      
      
      position <-
        position +
        1
    }
  }
  
  
  bind_rows(
    output
  )
}



simple_pd_predictions <-
  build_simple_pd_predictions(
    
    schedule_data =
      schedule,
    
    target_games =
      benchmark_base
  )



benchmark_base <-
  benchmark_base %>%
  
  left_join(
    simple_pd_predictions,
    by =
      "game_id"
  )



# ############################################################
#
# BENCHMARK 3
#
# BASIC ELO
#
# ############################################################
#
# Standard Elo structure:
#
# All teams begin every season:
#
#   1500
#
# No preseason ranking advantage.
#
#
# Pregame Elo difference:
#
#   Home Elo
#      -
#   Away Elo
#      +
#   Home-field Elo bonus
#
#
# Win probability:
#
#   1 /
#   (1 + 10^(-difference / 400))
#
#
# Update:
#
#   new rating =
#     old rating +
#     K * (actual result - expected result)
#
#
# IMPORTANT:
#
# Elo produces a win-strength rating, not a point-margin
# forecast.
#
# Therefore we learn:
#
#   actual_margin ~ pregame_elo_difference
#
# using PRIOR SEASONS ONLY.
#
# ============================================================



# ============================================================
# 8. GENERATE WALK-FORWARD ELO DIFFERENCES
# ============================================================

build_elo_history <- function(
    schedule_data
) {
  
  output <-
    list()
  
  
  position <-
    1
  
  
  seasons <-
    sort(
      unique(
        schedule_data$season
      )
    )
  
  
  for (
    current_season in seasons
  ) {
    
    games <-
      schedule_data %>%
      
      filter(
        season ==
          current_season
      ) %>%
      
      arrange(
        week,
        game_id
      )
    
    
    teams <-
      sort(
        unique(
          c(
            games$home_team,
            games$away_team
          )
        )
      )
    
    
    ratings <-
      setNames(
        rep(
          ELO_START,
          length(
            teams
          )
        ),
        teams
      )
    
    
    for (
      i in seq_len(
        nrow(
          games
        )
      )
    ) {
      
      game <-
        games[
          i,
        ]
      
      
      home <-
        game$home_team
      
      
      away <-
        game$away_team
      
      
      hfa_elo <-
        if (
          isTRUE(
            game$neutral_site
          )
        ) {
          
          0
          
        } else {
          
          ELO_HOME_ADVANTAGE
        }
      
      
      elo_difference <- (
        
        ratings[
          home
        ] -
          
          ratings[
            away
          ] +
          
          hfa_elo
      )
      
      
      expected_home <-
        1 /
        (
          1 +
            10^(
              -elo_difference /
                400
            )
        )
      
      
      if (
        game$actual_margin >
        0
      ) {
        
        actual_home <-
          1
        
      } else if (
        game$actual_margin <
        0
      ) {
        
        actual_home <-
          0
        
      } else {
        
        actual_home <-
          0.5
      }
      
      
      output[[position]] <- tibble(
          
          season =
            current_season,
          
          week =
            game$week,
          
          game_id =
            game$game_id,
          
          home_team =
            home,
          
          away_team =
            away,
          
          actual_margin =
            game$actual_margin,
          
          pregame_elo_difference =
            as.numeric(
              elo_difference
            ),
          
          elo_home_win_probability =
            as.numeric(
              expected_home
            )
        )
      
      
      rating_change <- (
        ELO_K *
          (
            actual_home -
              expected_home
          )
      )
      
      
      ratings[
        home
      ] <-
        ratings[
          home
        ] +
        rating_change
      
      
      ratings[
        away
      ] <-
        ratings[
          away
        ] -
        rating_change
      
      
      position <-
        position +
        1
    }
  }
  
  
  bind_rows(
    output
  )
}



elo_history <-
  build_elo_history(
    schedule
  )



# ============================================================
# 9. EXPANDING-SEASON ELO -> MARGIN CALIBRATION
# ============================================================
#
# Example:
#
# 2005 Elo margin conversion is learned from
# 1999-2004 only.
#
# ============================================================

build_elo_margin_predictions <- function(
    elo_data,
    target_games
) {
  
  output <-
    list()
  
  
  coefficients <-
    list()
  
  
  position <-
    1
  
  
  coefficient_position <-
    1
  
  
  seasons <-
    sort(
      unique(
        target_games$season
      )
    )
  
  
  for (
    target_season in seasons
  ) {
    
    calibration <-
      elo_data %>%
      
      filter(
        season <
          target_season
      )
    
    
    target_elo <-
      elo_data %>%
      
      filter(
        season ==
          target_season,
        game_id %in%
          target_games$game_id
      )
    
    
    if (
      nrow(
        calibration
      ) ==
      0 ||
      nrow(
        target_elo
      ) ==
      0
    ) {
      
      next
    }
    
    
    elo_margin_model <-
      lm(
        
        actual_margin ~
          pregame_elo_difference,
        
        data =
          calibration
      )
    
    
    target_elo$elo_margin <-
      as.numeric(
        predict(
          elo_margin_model,
          newdata =
            target_elo
        )
      )
    
    
    output[[position]] <- target_elo %>%
      
      select(
        game_id,
        elo_margin,
        elo_home_win_probability,
        pregame_elo_difference
      )
    
    
    coefficients[[coefficient_position]] <- tibble(
        
        target_season =
          target_season,
        
        calibration_start =
          min(
            calibration$season
          ),
        
        calibration_end =
          max(
            calibration$season
          ),
        
        calibration_games =
          nrow(
            calibration
          ),
        
        intercept =
          coef(
            elo_margin_model
          )[1],
        
        elo_difference_slope =
          coef(
            elo_margin_model
          )[2]
      )
    
    
    position <-
      position +
      1
    
    
    coefficient_position <-
      coefficient_position +
      1
  }
  
  
  list(
    
    predictions =
      bind_rows(
        output
      ),
    
    coefficients =
      bind_rows(
        coefficients
      )
  )
}



elo_results <-
  build_elo_margin_predictions(
    
    elo_data =
      elo_history,
    
    target_games =
      benchmark_base
  )



benchmark_base <-
  benchmark_base %>%
  
  left_join(
    elo_results$predictions,
    by =
      "game_id"
  )



# ============================================================
# 10. COMMON EVALUATION SAMPLE
# ============================================================
#
# Every model must have a valid prediction.
#
# This keeps comparisons apples-to-apples.
#
# ============================================================

benchmark_games <-
  benchmark_base %>%
  
  filter(
    
    is.finite(
      actual_margin
    ),
    
    is.finite(
      hfa_only_margin
    ),
    
    is.finite(
      simple_pd_margin
    ),
    
    is.finite(
      elo_margin
    ),
    
    is.finite(
      raw_v12_margin
    ),
    
    is.finite(
      calibrated_v12_margin
    ),
    
    is.finite(
      vegas_margin
    )
  )



cat("\n")
cat("========================================\n")
cat("COMMON BENCHMARK SAMPLE\n")
cat("========================================\n\n")


cat(
  "Games:",
  nrow(
    benchmark_games
  ),
  "\n"
)


cat(
  "Seasons:",
  min(
    benchmark_games$season
  ),
  "-",
  max(
    benchmark_games$season
  ),
  "\n\n"
)



# ============================================================
# 11. CONVERT TO LONG FORMAT
# ============================================================

benchmark_long <-
  benchmark_games %>%
  
  select(
    
    season,
    week,
    game_id,
    home_team,
    away_team,
    actual_margin,
    
    `HFA Only` =
      hfa_only_margin,
    
    `Simple Point Differential` =
      simple_pd_margin,
    
    `Basic Elo` =
      elo_margin,
    
    `Raw Version 12.1` =
      raw_v12_margin,
    
    `Calibrated Version 12.1` =
      calibrated_v12_margin,
    
    `Vegas Closing Spread` =
      vegas_margin
  ) %>%
  
  pivot_longer(
    
    cols =
      -c(
        season,
        week,
        game_id,
        home_team,
        away_team,
        actual_margin
      ),
    
    names_to =
      "model",
    
    values_to =
      "predicted_margin"
  ) %>%
  
  mutate(
    
    error = (
      predicted_margin -
        actual_margin
    ),
    
    abs_error =
      abs(
        error
      ),
    
    squared_error =
      error^2,
    
    correct_winner =
      case_when(
        
        actual_margin >
          0 &
          predicted_margin >
          0 ~
          TRUE,
        
        actual_margin <
          0 &
          predicted_margin <
          0 ~
          TRUE,
        
        actual_margin ==
          0 ~
          NA,
        
        TRUE ~
          FALSE
      )
  )



# ============================================================
# 12. OVERALL BENCHMARK SUMMARY
# ============================================================

overall_benchmark <-
  benchmark_long %>%
  
  group_by(
    model
  ) %>%
  
  summarise(
    
    games =
      n(),
    
    MAE =
      mean(
        abs_error
      ),
    
    RMSE =
      sqrt(
        mean(
          squared_error
        )
      ),
    
    mean_bias =
      mean(
        error
      ),
    
    median_abs_error =
      median(
        abs_error
      ),
    
    margin_correlation =
      cor(
        predicted_margin,
        actual_margin
      ),
    
    winner_accuracy_pct =
      100 *
      mean(
        correct_winner,
        na.rm =
          TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    MAE
  ) %>%
  
  mutate(
    
    MAE_rank =
      row_number()
  )



# ============================================================
# 13. ADD IMPROVEMENT RELATIVE TO HFA-ONLY
# ============================================================

hfa_mae <-
  overall_benchmark$MAE[
    overall_benchmark$model ==
      "HFA Only"
  ]


overall_benchmark <-
  overall_benchmark %>%
  
  mutate(
    
    MAE_improvement_vs_HFA = (
      hfa_mae -
        MAE
    ),
    
    pct_MAE_improvement_vs_HFA =
      100 *
      (
        hfa_mae -
          MAE
      ) /
      hfa_mae
  )



# ============================================================
# 14. BENCHMARK BY SEASON
# ============================================================

benchmark_by_season <-
  benchmark_long %>%
  
  group_by(
    season,
    model
  ) %>%
  
  summarise(
    
    games =
      n(),
    
    MAE =
      mean(
        abs_error
      ),
    
    RMSE =
      sqrt(
        mean(
          squared_error
        )
      ),
    
    mean_bias =
      mean(
        error
      ),
    
    winner_accuracy_pct =
      100 *
      mean(
        correct_winner,
        na.rm =
          TRUE
      ),
    
    .groups =
      "drop"
  )



# ============================================================
# 15. BENCHMARK BY WEEK
# ============================================================

benchmark_by_week <-
  benchmark_long %>%
  
  group_by(
    week,
    model
  ) %>%
  
  summarise(
    
    games =
      n(),
    
    MAE =
      mean(
        abs_error
      ),
    
    RMSE =
      sqrt(
        mean(
          squared_error
        )
      ),
    
    .groups =
      "drop"
  )



# ============================================================
# 16. ERA COMPARISON
# ============================================================

benchmark_by_era <-
  benchmark_long %>%
  
  mutate(
    
    era =
      case_when(
        
        season <=
          2009 ~
          "2000-2009",
        
        season <=
          2019 ~
          "2010-2019",
        
        TRUE ~
          "2020-2025"
      )
  ) %>%
  
  group_by(
    era,
    model
  ) %>%
  
  summarise(
    
    games =
      n(),
    
    MAE =
      mean(
        abs_error
      ),
    
    RMSE =
      sqrt(
        mean(
          squared_error
        )
      ),
    
    .groups =
      "drop"
  )



# ============================================================
# 17. HEAD-TO-HEAD:
#     CALIBRATED V12.1 VS RAW V12.1
# ============================================================

v12_pair <-
  benchmark_games %>%
  
  transmute(
    
    game_id,
    
    raw_error =
      abs(
        raw_v12_margin -
          actual_margin
      ),
    
    calibrated_error =
      abs(
        calibrated_v12_margin -
          actual_margin
      )
  )


v12_test <-
  t.test(
    
    v12_pair$calibrated_error,
    
    v12_pair$raw_error,
    
    paired =
      TRUE
  )


v12_comparison <-
  tibble(
    
    comparison =
      "Calibrated V12.1 minus Raw V12.1",
    
    mean_MAE_difference =
      mean(
        v12_pair$calibrated_error -
          v12_pair$raw_error
      ),
    
    ci_low =
      v12_test$conf.int[1],
    
    ci_high =
      v12_test$conf.int[2],
    
    p_value =
      v12_test$p.value
  )



# ============================================================
# 18. HEAD-TO-HEAD:
#     CALIBRATED V12.1 VS SIMPLE PD
# ============================================================

pd_pair <-
  benchmark_games %>%
  
  transmute(
    
    game_id,
    
    v12_error =
      abs(
        calibrated_v12_margin -
          actual_margin
      ),
    
    pd_error =
      abs(
        simple_pd_margin -
          actual_margin
      )
  )


pd_test <-
  t.test(
    
    pd_pair$v12_error,
    
    pd_pair$pd_error,
    
    paired =
      TRUE
  )


pd_comparison <-
  tibble(
    
    comparison =
      "Calibrated V12.1 minus Simple PD",
    
    mean_MAE_difference =
      mean(
        pd_pair$v12_error -
          pd_pair$pd_error
      ),
    
    ci_low =
      pd_test$conf.int[1],
    
    ci_high =
      pd_test$conf.int[2],
    
    p_value =
      pd_test$p.value
  )



# ============================================================
# 19. HEAD-TO-HEAD:
#     CALIBRATED V12.1 VS ELO
# ============================================================

elo_pair <-
  benchmark_games %>%
  
  transmute(
    
    game_id,
    
    v12_error =
      abs(
        calibrated_v12_margin -
          actual_margin
      ),
    
    elo_error =
      abs(
        elo_margin -
          actual_margin
      )
  )


elo_test <-
  t.test(
    
    elo_pair$v12_error,
    
    elo_pair$elo_error,
    
    paired =
      TRUE
  )


elo_comparison <-
  tibble(
    
    comparison =
      "Calibrated V12.1 minus Basic Elo",
    
    mean_MAE_difference =
      mean(
        elo_pair$v12_error -
          elo_pair$elo_error
      ),
    
    ci_low =
      elo_test$conf.int[1],
    
    ci_high =
      elo_test$conf.int[2],
    
    p_value =
      elo_test$p.value
  )



# ============================================================
# 20. HEAD-TO-HEAD:
#     CALIBRATED V12.1 VS VEGAS
# ============================================================

vegas_pair <-
  benchmark_games %>%
  
  transmute(
    
    game_id,
    
    v12_error =
      abs(
        calibrated_v12_margin -
          actual_margin
      ),
    
    vegas_error =
      abs(
        vegas_margin -
          actual_margin
      )
  )


vegas_test <-
  t.test(
    
    vegas_pair$v12_error,
    
    vegas_pair$vegas_error,
    
    paired =
      TRUE
  )


vegas_comparison <-
  tibble(
    
    comparison =
      "Calibrated V12.1 minus Vegas",
    
    mean_MAE_difference =
      mean(
        vegas_pair$v12_error -
          vegas_pair$vegas_error
      ),
    
    ci_low =
      vegas_test$conf.int[1],
    
    ci_high =
      vegas_test$conf.int[2],
    
    p_value =
      vegas_test$p.value
  )



pairwise_tests <-
  bind_rows(
    
    v12_comparison,
    
    pd_comparison,
    
    elo_comparison,
    
    vegas_comparison
  )



# ============================================================
# 21. HOW OFTEN DOES EACH MODEL BEAT V12.1?
# ============================================================

game_level_comparison <-
  benchmark_games %>%
  
  mutate(
    
    hfa_error =
      abs(
        hfa_only_margin -
          actual_margin
      ),
    
    simple_pd_error =
      abs(
        simple_pd_margin -
          actual_margin
      ),
    
    elo_error =
      abs(
        elo_margin -
          actual_margin
      ),
    
    raw_v12_error =
      abs(
        raw_v12_margin -
          actual_margin
      ),
    
    calibrated_v12_error =
      abs(
        calibrated_v12_margin -
          actual_margin
      ),
    
    vegas_error =
      abs(
        vegas_margin -
          actual_margin
      )
  )



closer_to_actual_summary <-
  tibble(
    
    comparison =
      c(
        "V12.1 vs HFA Only",
        "V12.1 vs Simple PD",
        "V12.1 vs Basic Elo",
        "V12.1 vs Raw V12.1",
        "V12.1 vs Vegas"
      ),
    
    v12_closer_pct =
      c(
        
        100 *
          mean(
            game_level_comparison$calibrated_v12_error <
              game_level_comparison$hfa_error
          ),
        
        100 *
          mean(
            game_level_comparison$calibrated_v12_error <
              game_level_comparison$simple_pd_error
          ),
        
        100 *
          mean(
            game_level_comparison$calibrated_v12_error <
              game_level_comparison$elo_error
          ),
        
        100 *
          mean(
            game_level_comparison$calibrated_v12_error <
              game_level_comparison$raw_v12_error
          ),
        
        100 *
          mean(
            game_level_comparison$calibrated_v12_error <
              game_level_comparison$vegas_error
          )
      )
  )



# ============================================================
# 22. PRINT OVERALL BENCHMARK
# ============================================================

cat("\n")
cat("################################################\n")
cat("OVERALL MODEL BENCHMARK\n")
cat("################################################\n\n")


print(
  overall_benchmark,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 23. PRINT PAIRWISE TESTS
# ============================================================

cat("\n")
cat("========================================\n")
cat("PAIRWISE MAE TESTS\n")
cat("========================================\n\n")


cat(
  "Negative mean_MAE_difference means ",
  "Calibrated Version 12.1 is better.\n\n"
)


print(
  pairwise_tests,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 24. PRINT CLOSER-GAME PERCENTAGES
# ============================================================

cat("\n")
cat("========================================\n")
cat("HOW OFTEN IS VERSION 12.1 CLOSER?\n")
cat("========================================\n\n")


print(
  closer_to_actual_summary,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 25. PRINT ERA RESULTS
# ============================================================

cat("\n")
cat("========================================\n")
cat("BENCHMARK BY ERA\n")
cat("========================================\n\n")


print(
  benchmark_by_era,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 26. MAE BAR PLOT
# ============================================================

p_overall <-
  ggplot(
    
    overall_benchmark,
    
    aes(
      x =
        reorder(
          model,
          MAE
        ),
      y =
        MAE
    )
  ) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    
    title =
      "NFL Margin Forecast Benchmark",
    
    subtitle =
      "Lower Mean Absolute Error is better",
    
    x =
      NULL,
    
    y =
      "Mean Absolute Error (Points)"
  ) +
  
  theme_minimal()



# ============================================================
# 27. MAE BY SEASON PLOT
# ============================================================

p_season <-
  ggplot(
    
    benchmark_by_season,
    
    aes(
      x =
        season,
      y =
        MAE,
      group =
        model,
      linetype =
        model
    )
  ) +
  
  geom_line(
    linewidth =
      0.8
  ) +
  
  labs(
    
    title =
      "Forecast MAE by NFL Season",
    
    x =
      "Season",
    
    y =
      "Mean Absolute Error",
    
    linetype =
      NULL
  ) +
  
  theme_minimal()



# ============================================================
# 28. VERSION 12.1 IMPROVEMENT OVER SIMPLE MODELS
# ============================================================

simple_models <-
  overall_benchmark %>%
  
  filter(
    model %in%
      c(
        "HFA Only",
        "Simple Point Differential",
        "Basic Elo",
        "Raw Version 12.1",
        "Calibrated Version 12.1"
      )
  )


p_progression <-
  ggplot(
    
    simple_models,
    
    aes(
      x =
        reorder(
          model,
          MAE
        ),
      y =
        MAE
    )
  ) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    
    title =
      "How Much Does Version 12.1 Improve on Simpler Models?",
    
    subtitle =
      "Closing market removed to focus on model-development progression",
    
    x =
      NULL,
    
    y =
      "Mean Absolute Error (Points)"
  ) +
  
  theme_minimal()



print(
  p_overall
)

print(
  p_season
)

print(
  p_progression
)



# ============================================================
# 29. SAVE RESULTS
# ============================================================

write_csv(
  
  benchmark_games,
  
  "benchmark_game_level_predictions.csv"
)


write_csv(
  
  overall_benchmark,
  
  "benchmark_overall_results.csv"
)


write_csv(
  
  benchmark_by_season,
  
  "benchmark_by_season.csv"
)


write_csv(
  
  benchmark_by_week,
  
  "benchmark_by_week.csv"
)


write_csv(
  
  benchmark_by_era,
  
  "benchmark_by_era.csv"
)


write_csv(
  
  pairwise_tests,
  
  "benchmark_pairwise_tests.csv"
)


write_csv(
  
  closer_to_actual_summary,
  
  "benchmark_v12_closer_percentage.csv"
)


write_csv(
  
  elo_results$coefficients,
  
  "benchmark_elo_margin_calibration.csv"
)



# ============================================================
# 30. SAVE PLOTS
# ============================================================

ggsave(
  
  "benchmark_overall_MAE.png",
  
  p_overall,
  
  width =
    9,
  
  height =
    6,
  
  dpi =
    300
)


ggsave(
  
  "benchmark_MAE_by_season.png",
  
  p_season,
  
  width =
    10,
  
  height =
    6,
  
  dpi =
    300
)


ggsave(
  
  "benchmark_model_progression.png",
  
  p_progression,
  
  width =
    9,
  
  height =
    6,
  
  dpi =
    300
)



# ============================================================
# 31. FINISHED
# ============================================================

cat("\n")
cat("========================================\n")
cat("BENCHMARK COMPLETE\n")
cat("========================================\n\n")


cat(
  "Most important outputs:\n\n"
)


cat(
  "  benchmark_overall_results.csv\n"
)


cat(
  "  benchmark_pairwise_tests.csv\n"
)


cat(
  "  benchmark_by_era.csv\n"
)


cat(
  "  benchmark_by_season.csv\n\n"
)
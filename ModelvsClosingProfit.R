# ============================================================
# NFL VERSION 12.1
# PROFITABILITY VS CLOSING SPREAD
# PROPER EXPANDING-SEASON CALIBRATION
# ============================================================
#
# PURPOSE
# -------
# Test whether Version 12.1 forecasts would have produced
# positive hypothetical returns against historical NFL
# closing spreads.
#
# IMPORTANT:
#
# We DO NOT use the existing forecast_margin column from
# version12_1_historical_oos_predictions.csv.
#
# Instead, for every target season:
#
#   1. Use ONLY prior seasons to learn forecast shrinkage.
#   2. Apply those weights to the target season.
#   3. Compare the resulting forecast to spread_line.
#   4. Grade the preferred side ATS.
#   5. Calculate hypothetical ROI at standardized -110.
#
# Example:
#
#   2015 forecasts:
#       calibration = 1999-2014
#
#   2016 forecasts:
#       calibration = 1999-2015
#
# etc.
#
# This removes future-season leakage from the forecast
# shrinkage calibration.
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


# ============================================================
# 2. USER SETTINGS
# ============================================================

INPUT_FILE <-
  "version12_1_historical_oos_predictions.csv"


# ------------------------------------------------------------
# Main hypothesis
# ------------------------------------------------------------
#
# A qualifying game occurs when our properly calibrated
# forecast differs from the closing line by at least 3 points.
#
# Keep this at 3 for the primary test because that was the
# threshold we were interested in BEFORE looking at results.
#
PRIMARY_EDGE <- 3


# ------------------------------------------------------------
# Standard spread price
# ------------------------------------------------------------
#
# Risk 1 unit:
#
#   win at -110 = +0.90909 units
#   loss        = -1 unit
#   push        = 0
#
STANDARD_PRICE <- -110


# ------------------------------------------------------------
# Calibration warm-up
# ------------------------------------------------------------
#
# Minimum number of PRIOR seasons required before a season
# enters the profitability evaluation.
#
# 1:
#   1999 calibrates 2000
#   1999-2000 calibrates 2001
#   etc.
#
# This gives the largest leakage-free sample.
#
# You could change this to 5 later as a robustness check.
#
MIN_PRIOR_SEASONS <- 1


# ------------------------------------------------------------
# Other thresholds to inspect
# ------------------------------------------------------------
#
# These are exploratory.
#
# The 3-point threshold remains our PRIMARY test.
#
EDGE_THRESHOLDS <- seq(
  from = 0.5,
  to = 7,
  by = 0.5
)



# ============================================================
# 3. LOAD HISTORICAL OOS PREDICTIONS
# ============================================================

raw <- readr::read_csv(
  INPUT_FILE,
  show_col_types = FALSE
)


cat("\n")
cat("========================================\n")
cat("LOADED HISTORICAL OOS DATA\n")
cat("========================================\n\n")

cat(
  "Rows:",
  nrow(raw),
  "\n"
)

cat(
  "Seasons:",
  min(raw$season, na.rm = TRUE),
  "-",
  max(raw$season, na.rm = TRUE),
  "\n"
)



# ============================================================
# 4. CHECK REQUIRED COLUMNS
# ============================================================

required_columns <- c(
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


missing_columns <- setdiff(
  required_columns,
  names(raw)
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
# 5. CLEAN MODEL DATA
# ============================================================
#
# VERY IMPORTANT:
#
# We use:
#
#   predicted_margin
#
# which is the RAW historical point-margin prediction.
#
# We intentionally DO NOT use:
#
#   forecast_margin
#
# because that historical column used shrinkage learned using
# the complete historical calibration sample.
#
# We are going to recreate forecast_margin properly.
#
# ============================================================

model_data <- raw %>%
  
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
    
    raw_predicted_margin =
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
    
    spread_line =
      as.numeric(
        spread_line
      )
  ) %>%
  
  filter(
    
    is.finite(
      actual_margin
    ),
    
    is.finite(
      raw_predicted_margin
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
# 6. CHECK FOR DUPLICATE GAMES
# ============================================================

duplicates <- model_data %>%
  
  count(
    game_id
  ) %>%
  
  filter(
    n >
      1
  )


if (
  nrow(
    duplicates
  ) >
  0
) {
  
  stop(
    "Duplicate game_id values found."
  )
}



# ============================================================
# 7. VERSION 12.1 FIXED FALLBACK WEIGHT
# ============================================================
#
# Same rule used in Version 12.1:
#
# weight =
#
#   average games/team / 4
#
# capped between 0 and 1.
#
# For buckets:
#
#   1 -> .25
#   2 -> .50
#   3 -> .75
#   4 -> 1.00
#   5 -> 1.00
#
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



# ============================================================
# 8. LEARN VERSION 12.1 SHRINKAGE WEIGHTS
# ============================================================
#
# Version 12.1 estimates:
#
# delta =
#
#   raw model margin - baseline margin
#
#
# target =
#
#   actual margin - baseline margin
#
#
# learned weight =
#
#   sum(delta * target) /
#   sum(delta^2)
#
#
# Then restrict weight to:
#
#   0 <= weight <= 1
#
# ============================================================

learn_shrinkage_weights <- function(
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
      
      
      fallback_weight <-
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
          fallback_weight
        
      } else {
        
        delta <- (
          current$raw_predicted_margin -
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
            fallback_weight
          
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
        
        bucket_calibration_games =
          nrow(
            current
          ),
        
        learned_forecast_weight =
          learned_weight,
        
        fallback_weight =
          fallback_weight
      )
    }
  )
}



# ============================================================
# 9. EXPANDING-SEASON CALIBRATION
# ============================================================
#
# This is the key correction.
#
# For target season S:
#
# calibration data:
#
#       season < S
#
# Never:
#
#       season == S
#
# Never:
#
#       season > S
#
# ============================================================

build_expanding_forecasts <- function(
    data,
    min_prior_seasons = 1
) {
  
  seasons <-
    sort(
      unique(
        data$season
      )
    )
  
  
  prediction_storage <-
    list()
  
  
  weight_storage <-
    list()
  
  
  prediction_position <-
    1
  
  
  weight_position <-
    1
  
  
  for (
    target_season in
    seasons
  ) {
    
    prior_seasons <-
      seasons[
        seasons <
          target_season
      ]
    
    
    if (
      length(
        prior_seasons
      ) <
      min_prior_seasons
    ) {
      
      next
    }
    
    
    # ----------------------------------------------------------
    # ONLY PREVIOUS SEASONS
    # ----------------------------------------------------------
    
    calibration_data <-
      data %>%
      
      filter(
        season <
          target_season
      )
    
    
    # ----------------------------------------------------------
    # Current target season
    # ----------------------------------------------------------
    
    target_data <-
      data %>%
      
      filter(
        season ==
          target_season
      )
    
    
    if (
      nrow(
        calibration_data
      ) ==
      0 ||
      nrow(
        target_data
      ) ==
      0
    ) {
      
      next
    }
    
    
    # ----------------------------------------------------------
    # Learn weights using prior seasons only
    # ----------------------------------------------------------
    
    weights <-
      learn_shrinkage_weights(
        calibration_data
      ) %>%
      
      mutate(
        
        target_season =
          target_season,
        
        calibration_start_season =
          min(
            calibration_data$season
          ),
        
        calibration_end_season =
          max(
            calibration_data$season
          ),
        
        total_calibration_games =
          nrow(
            calibration_data
          ),
        
        .before =
          1
      )
    
    
    # ----------------------------------------------------------
    # Apply those weights to target season
    # ----------------------------------------------------------
    
    calibrated_target <-
      target_data %>%
      
      left_join(
        
        weights %>%
          
          select(
            games_played_bucket,
            bucket_calibration_games,
            learned_forecast_weight,
            fallback_weight
          ),
        
        by =
          "games_played_bucket"
      ) %>%
      
      mutate(
        
        calibration_start_season =
          min(
            calibration_data$season
          ),
        
        calibration_end_season =
          max(
            calibration_data$season
          ),
        
        total_calibration_games =
          nrow(
            calibration_data
          ),
        
        
        # ------------------------------------------------------
        # FINAL PROPERLY CALIBRATED MARGIN
        # ------------------------------------------------------
        
        proper_forecast_margin = (
          
          baseline_margin +
            
            learned_forecast_weight *
            
            (
              raw_predicted_margin -
                baseline_margin
            )
        )
      )
    
    
    prediction_storage[[prediction_position]] <- calibrated_target
    
    
    weight_storage[[weight_position]] <-weights
    
    
    prediction_position <-
      prediction_position +
      1
    
    
    weight_position <-
      weight_position +
      1
  }
  
  
  if (
    length(
      prediction_storage
    ) ==
    0
  ) {
    
    stop(
      "No seasons had enough prior seasons for calibration."
    )
  }
  
  
  list(
    
    predictions =
      bind_rows(
        prediction_storage
      ),
    
    weights =
      bind_rows(
        weight_storage
      )
  )
}



# ============================================================
# 10. BUILD PROPER HISTORICAL FORECASTS
# ============================================================

expanding <-
  build_expanding_forecasts(
    
    model_data,
    
    min_prior_seasons =
      MIN_PRIOR_SEASONS
  )


calibrated_predictions <-
  expanding$predictions


calibration_weights <-
  expanding$weights



# ============================================================
# 11. STANDARD -110 PROFITABILITY SETTINGS
# ============================================================

profit_per_win <- if (
  STANDARD_PRICE <
  0
) {
  
  100 /
    abs(
      STANDARD_PRICE
    )
  
} else {
  
  STANDARD_PRICE /
    100
}



break_even_rate <- if (
  STANDARD_PRICE <
  0
) {
  
  abs(
    STANDARD_PRICE
  ) /
    (
      abs(
        STANDARD_PRICE
      ) +
        100
    )
  
} else {
  
  100 /
    (
      STANDARD_PRICE +
        100
    )
}



# ============================================================
# 12. COMPARE MODEL WITH CLOSING SPREAD
# ============================================================
#
# spread_line:
#
#   positive = home favored
#   negative = away favored
#
#
# Examples:
#
# Market:
#
#   HOME -3.5
#
# represented as:
#
#   spread_line = +3.5
#
#
# Model:
#
#   HOME by 7
#
# model_edge =
#
#   7 - 3.5
#   = +3.5
#
# Positive:
#   prefer HOME ATS
#
#
# If:
#
# Model HOME by 1
# Market HOME by 4
#
# edge:
#
#   1 - 4
#   = -3
#
# Model prefers AWAY ATS.
#
# ============================================================

bet_data <-
  calibrated_predictions %>%
  
  filter(
    is.finite(
      spread_line
    )
  ) %>%
  
  mutate(
    
    market_margin =
      spread_line,
    
    
    # --------------------------------------------------------
    # MODEL vs MARKET
    # --------------------------------------------------------
    
    model_edge = (
      proper_forecast_margin -
        market_margin
    ),
    
    
    abs_edge =
      abs(
        model_edge
      ),
    
    
    # --------------------------------------------------------
    # Which side does the model prefer?
    # --------------------------------------------------------
    
    preferred_side =
      case_when(
        
        model_edge >
          0 ~
          "HOME",
        
        model_edge <
          0 ~
          "AWAY",
        
        TRUE ~
          "NONE"
      ),
    
    
    preferred_team =
      case_when(
        
        preferred_side ==
          "HOME" ~
          home_team,
        
        preferred_side ==
          "AWAY" ~
          away_team,
        
        TRUE ~
          NA_character_
      ),
    
    
    # --------------------------------------------------------
    # Did the home team beat the spread?
    # --------------------------------------------------------
    
    home_cover_margin = (
      actual_margin -
        market_margin
    ),
    
    
    # --------------------------------------------------------
    # Convert result to OUR preferred side
    # --------------------------------------------------------
    
    preferred_side_cover_margin =
      case_when(
        
        preferred_side ==
          "HOME" ~
          home_cover_margin,
        
        preferred_side ==
          "AWAY" ~
          -home_cover_margin,
        
        TRUE ~
          NA_real_
      ),
    
    
    # --------------------------------------------------------
    # ATS result
    # --------------------------------------------------------
    
    ats_result =
      case_when(
        
        preferred_side_cover_margin >
          0 ~
          "Win",
        
        preferred_side_cover_margin <
          0 ~
          "Loss",
        
        preferred_side_cover_margin ==
          0 ~
          "Push",
        
        TRUE ~
          NA_character_
      ),
    
    
    # --------------------------------------------------------
    # Hypothetical profit
    #
    # Risk exactly 1 unit per qualifying game.
    # --------------------------------------------------------
    
    profit_units =
      case_when(
        
        ats_result ==
          "Win" ~
          profit_per_win,
        
        ats_result ==
          "Loss" ~
          -1,
        
        ats_result ==
          "Push" ~
          0,
        
        TRUE ~
          NA_real_
      )
  ) %>%
  
  filter(
    preferred_side !=
      "NONE"
  )



# ============================================================
# 13. PROFITABILITY SUMMARY FUNCTION
# ============================================================

summarize_profitability <- function(
    data
) {
  
  bets <-
    nrow(
      data
    )
  
  
  wins <-
    sum(
      data$ats_result ==
        "Win",
      na.rm =
        TRUE
    )
  
  
  losses <-
    sum(
      data$ats_result ==
        "Loss",
      na.rm =
        TRUE
    )
  
  
  pushes <-
    sum(
      data$ats_result ==
        "Push",
      na.rm =
        TRUE
    )
  
  
  decisions <- (
    wins +
      losses
  )
  
  
  win_rate <- if (
    decisions >
    0
  ) {
    
    wins /
      decisions
    
  } else {
    
    NA_real_
  }
  
  
  # ----------------------------------------------------------
  # Approximate 95% CI for ATS win percentage
  # ----------------------------------------------------------
  
  if (
    decisions >
    0
  ) {
    
    se <- sqrt(
      win_rate *
        (
          1 -
            win_rate
        ) /
        decisions
    )
    
    
    ci_low <- max(
      0,
      win_rate -
        1.96 *
        se
    )
    
    
    ci_high <- min(
      1,
      win_rate +
        1.96 *
        se
    )
    
  } else {
    
    ci_low <-
      NA_real_
    
    
    ci_high <-
      NA_real_
  }
  
  
  # ----------------------------------------------------------
  # Test against -110 break-even rate
  # ----------------------------------------------------------
  
  p_above_break_even <- if (
    decisions >
    0
  ) {
    
    stats::binom.test(
      
      wins,
      
      decisions,
      
      p =
        break_even_rate,
      
      alternative =
        "greater"
      
    )$p.value
    
  } else {
    
    NA_real_
  }
  
  
  total_profit <-
    sum(
      data$profit_units,
      na.rm =
        TRUE
    )
  
  
  tibble(
    
    bets =
      bets,
    
    wins =
      wins,
    
    losses =
      losses,
    
    pushes =
      pushes,
    
    ats_win_pct =
      100 *
      win_rate,
    
    ats_ci_low_pct =
      100 *
      ci_low,
    
    ats_ci_high_pct =
      100 *
      ci_high,
    
    break_even_pct =
      100 *
      break_even_rate,
    
    profit_units =
      total_profit,
    
    roi_pct =
      if (
        bets >
        0
      ) {
        
        100 *
          total_profit /
          bets
        
      } else {
        
        NA_real_
      },
    
    avg_abs_edge =
      if (
        bets >
        0
      ) {
        
        mean(
          data$abs_edge
        )
        
      } else {
        
        NA_real_
      },
    
    median_abs_edge =
      if (
        bets >
        0
      ) {
        
        median(
          data$abs_edge
        )
        
      } else {
        
        NA_real_
      },
    
    p_value_above_break_even =
      p_above_break_even
  )
}



# ============================================================
# 14. PRIMARY TEST:
#     MODEL DIFFERS FROM VEGAS BY >= 3 POINTS
# ============================================================

primary_games <-
  bet_data %>%
  
  filter(
    abs_edge >=
      PRIMARY_EDGE
  )


primary_result <-
  summarize_profitability(
    primary_games
  ) %>%
  
  mutate(
    
    edge_threshold =
      PRIMARY_EDGE,
    
    evaluation_start_season =
      min(
        calibrated_predictions$season
      ),
    
    evaluation_end_season =
      max(
        calibrated_predictions$season
      ),
    
    .before =
      1
  )



# ============================================================
# 15. PROFITABILITY BY EDGE THRESHOLD
# ============================================================
#
# Examples:
#
# >= 0.5
# >= 1.0
# >= 1.5
# ...
# >= 7.0
#
# These overlap.
#
# ============================================================

threshold_results <-
  purrr::map_dfr(
    
    EDGE_THRESHOLDS,
    
    function(
    threshold
    ) {
      
      bet_data %>%
        
        filter(
          abs_edge >=
            threshold
        ) %>%
        
        summarize_profitability() %>%
        
        mutate(
          
          edge_threshold =
            threshold,
          
          .before =
            1
        )
    }
  )



# ============================================================
# 16. PROFITABILITY BY STARTING WEEK
# ============================================================
#
# THIS IS THE MAIN ANSWER TO:
#
# "By what week does the model become profitable?"
#
#
# Example:
#
# start_week = 5
#
# means:
#
#   ignore Weeks 1-4
#   evaluate Week 5 onward
#
# while requiring:
#
#   abs(model edge) >= 3
#
# ============================================================

start_weeks <- seq(
  
  min(
    bet_data$week,
    na.rm =
      TRUE
  ),
  
  max(
    bet_data$week,
    na.rm =
      TRUE
  )
)



start_week_results <-
  purrr::map_dfr(
    
    start_weeks,
    
    function(
    start_week
    ) {
      
      bet_data %>%
        
        filter(
          
          week >=
            start_week,
          
          abs_edge >=
            PRIMARY_EDGE
        ) %>%
        
        summarize_profitability() %>%
        
        mutate(
          
          start_week =
            start_week,
          
          .before =
            1
        )
    }
  )



# ============================================================
# 17. PROFITABILITY BY INDIVIDUAL WEEK
# ============================================================

individual_week_results <-
  bet_data %>%
  
  filter(
    abs_edge >=
      PRIMARY_EDGE
  ) %>%
  
  group_by(
    week
  ) %>%
  
  group_modify(
    ~ summarize_profitability(
      .x
    )
  ) %>%
  
  ungroup()



# ============================================================
# 18. PROFITABILITY BY SEASON
# ============================================================
#
# Important because we do not want one unusually good season
# generating the entire historical result.
#
# ============================================================

season_results <-
  bet_data %>%
  
  filter(
    abs_edge >=
      PRIMARY_EDGE
  ) %>%
  
  group_by(
    season
  ) %>%
  
  group_modify(
    ~ summarize_profitability(
      .x
    )
  ) %>%
  
  ungroup()



# ============================================================
# 19. WEEK x EDGE GRID
# ============================================================
#
# Exploratory analysis.
#
# Example:
#
# start Week 4 + >= 2 points
# start Week 4 + >= 3 points
# start Week 5 + >= 3 points
# start Week 6 + >= 4 points
#
# etc.
#
# Do not simply select the maximum historical ROI and call
# that the optimal model. That would overfit this backtest.
#
# ============================================================

week_edge_grid <-
  tidyr::expand_grid(
    
    start_week =
      start_weeks,
    
    edge_threshold =
      EDGE_THRESHOLDS
  )



week_edge_results <-
  purrr::pmap_dfr(
    
    week_edge_grid,
    
    function(
    start_week,
    edge_threshold
    ) {
      
      bet_data %>%
        
        filter(
          
          week >=
            start_week,
          
          abs_edge >=
            edge_threshold
        ) %>%
        
        summarize_profitability() %>%
        
        mutate(
          
          start_week =
            start_week,
          
          edge_threshold =
            edge_threshold,
          
          .before =
            1
        )
    }
  )



# ============================================================
# 20. NON-OVERLAPPING EDGE BANDS
# ============================================================
#
# This is different from threshold analysis.
#
# Instead of:
#
# >= 1
# >= 2
# >= 3
#
# we get:
#
# 0-1
# 1-2
# 2-3
# 3-4
# 4-5
# 5-6
# 6+
#
# This tells us whether bigger disagreements actually appear
# to contain more information.
#
# ============================================================

edge_band_results <-
  bet_data %>%
  
  mutate(
    
    edge_band =
      cut(
        
        abs_edge,
        
        breaks =
          c(
            0,
            1,
            2,
            3,
            4,
            5,
            6,
            Inf
          ),
        
        right =
          FALSE,
        
        labels =
          c(
            "0-1",
            "1-2",
            "2-3",
            "3-4",
            "4-5",
            "5-6",
            "6+"
          )
      )
  ) %>%
  
  group_by(
    edge_band
  ) %>%
  
  group_modify(
    ~ summarize_profitability(
      .x
    )
  ) %>%
  
  ungroup()



# ============================================================
# 21. CUMULATIVE PRIMARY-TEST RESULTS
# ============================================================

primary_game_log <-
  primary_games %>%
  
  arrange(
    season,
    week,
    game_id
  ) %>%
  
  mutate(
    
    bet_number =
      row_number(),
    
    cumulative_profit_units =
      cumsum(
        profit_units
      )
  )



# ============================================================
# 22. PRINT MAIN RESULTS
# ============================================================

cat("\n")
cat("========================================\n")
cat("VERSION 12.1 PROFITABILITY BACKTEST\n")
cat("========================================\n\n")


cat(
  "Input file:",
  INPUT_FILE,
  "\n"
)


cat(
  "Raw OOS seasons:",
  min(
    model_data$season
  ),
  "-",
  max(
    model_data$season
  ),
  "\n"
)


cat(
  "Minimum prior calibration seasons:",
  MIN_PRIOR_SEASONS,
  "\n"
)


cat(
  "Evaluated seasons:",
  min(
    calibrated_predictions$season
  ),
  "-",
  max(
    calibrated_predictions$season
  ),
  "\n"
)


cat(
  "Games with closing spreads:",
  nrow(
    bet_data
  ),
  "\n"
)


cat(
  "Standardized price:",
  STANDARD_PRICE,
  "\n"
)


cat(
  "Break-even ATS rate:",
  round(
    100 *
      break_even_rate,
    2
  ),
  "%\n"
)



# ============================================================
# PRIMARY 3-POINT RESULT
# ============================================================

cat("\n")
cat("========================================\n")
cat(
  "PRIMARY TEST: MODEL EDGE >= ",
  PRIMARY_EDGE,
  " POINTS\n",
  sep = ""
)
cat("========================================\n\n")


print(
  primary_result,
  width =
    Inf
)



# ============================================================
# STARTING WEEK RESULT
# ============================================================

cat("\n")
cat("========================================\n")
cat(
  "EDGE >= ",
  PRIMARY_EDGE,
  " BY STARTING WEEK\n",
  sep = ""
)
cat("========================================\n\n")


print(
  start_week_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# EDGE THRESHOLDS
# ============================================================

cat("\n")
cat("========================================\n")
cat("RESULTS BY EDGE THRESHOLD\n")
cat("========================================\n\n")


print(
  threshold_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# RESULTS BY SEASON
# ============================================================

cat("\n")
cat("========================================\n")
cat(
  "EDGE >= ",
  PRIMARY_EDGE,
  " BY SEASON\n",
  sep = ""
)
cat("========================================\n\n")


print(
  season_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# EDGE BANDS
# ============================================================

cat("\n")
cat("========================================\n")
cat("NON-OVERLAPPING EDGE BANDS\n")
cat("========================================\n\n")


print(
  edge_band_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 23. PLOT:
# ROI BY EDGE THRESHOLD
# ============================================================

p1 <-
  ggplot(
    
    threshold_results,
    
    aes(
      x =
        edge_threshold,
      y =
        roi_pct
    )
  ) +
  
  geom_hline(
    yintercept =
      0,
    linetype =
      "dashed"
  ) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  labs(
    
    title =
      "Hypothetical ROI vs Minimum Model Edge",
    
    subtitle =
      "Leakage-free expanding calibration; standardized -110 pricing",
    
    x =
      "Minimum Model vs Closing-Line Difference (Points)",
    
    y =
      "ROI (%)"
  ) +
  
  theme_minimal()



# ============================================================
# 24. PLOT:
# ROI BY STARTING WEEK
# ============================================================

p2 <-
  ggplot(
    
    start_week_results,
    
    aes(
      x =
        start_week,
      y =
        roi_pct
    )
  ) +
  
  geom_hline(
    yintercept =
      0,
    linetype =
      "dashed"
  ) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  scale_x_continuous(
    breaks =
      start_weeks
  ) +
  
  labs(
    
    title =
      paste0(
        "ROI When Evaluation Begins in Week X: Edge >= ",
        PRIMARY_EDGE,
        " Points"
      ),
    
    subtitle =
      "Each point includes that week and all later regular-season weeks",
    
    x =
      "Starting Week",
    
    y =
      "ROI (%)"
  ) +
  
  theme_minimal()



# ============================================================
# 25. PLOT:
# CUMULATIVE PROFIT
# ============================================================

p3 <-
  ggplot(
    
    primary_game_log,
    
    aes(
      x =
        bet_number,
      y =
        cumulative_profit_units
    )
  ) +
  
  geom_hline(
    yintercept =
      0,
    linetype =
      "dashed"
  ) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  labs(
    
    title =
      paste0(
        "Cumulative Hypothetical Profit: Edge >= ",
        PRIMARY_EDGE,
        " Points"
      ),
    
    subtitle =
      "1 unit risked per qualifying game at standardized -110",
    
    x =
      "Qualifying Game Number",
    
    y =
      "Cumulative Profit (Units)"
  ) +
  
  theme_minimal()



print(
  p1
)

print(
  p2
)

print(
  p3
)



# ============================================================
# 26. SAVE ALL RESULTS
# ============================================================

write_csv(
  
  calibrated_predictions,
  
  "profitability_properly_calibrated_predictions.csv"
)



write_csv(
  
  calibration_weights,
  
  "profitability_expanding_calibration_weights.csv"
)



write_csv(
  
  primary_result,
  
  "profitability_primary_3_point_result.csv"
)



write_csv(
  
  primary_game_log,
  
  "profitability_primary_3_point_games.csv"
)



write_csv(
  
  threshold_results,
  
  "profitability_by_edge_threshold.csv"
)



write_csv(
  
  start_week_results,
  
  "profitability_by_start_week.csv"
)



write_csv(
  
  individual_week_results,
  
  "profitability_by_individual_week.csv"
)



write_csv(
  
  season_results,
  
  "profitability_by_season.csv"
)



write_csv(
  
  week_edge_results,
  
  "profitability_week_edge_grid.csv"
)



write_csv(
  
  edge_band_results,
  
  "profitability_by_edge_band.csv"
)



# ============================================================
# 27. SAVE PLOTS
# ============================================================

ggsave(
  
  "profitability_roi_by_edge.png",
  
  p1,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)



ggsave(
  
  "profitability_roi_by_start_week.png",
  
  p2,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)



ggsave(
  
  "profitability_cumulative_units.png",
  
  p3,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)



# ============================================================
# 28. FINISHED
# ============================================================

cat("\n")
cat("========================================\n")
cat("BACKTEST COMPLETE\n")
cat("========================================\n\n")


cat(
  "Main overall result:\n",
  "  profitability_primary_3_point_result.csv\n\n"
)


cat(
  "Main week question:\n",
  "  profitability_by_start_week.csv\n\n"
)


cat(
  "Consistency across seasons:\n",
  "  profitability_by_season.csv\n\n"
)


cat(
  "Different model-market differences:\n",
  "  profitability_by_edge_threshold.csv\n\n"
)


cat(
  "Audit the historical calibration weights:\n",
  "  profitability_expanding_calibration_weights.csv\n\n"
)
# ============================================================
# NFL VERSION 12.1
#
# PART A:
# FORECAST EXTREMITY CALIBRATION
#
# PART B:
# OPENING -> CLOSING LINE MOVEMENT
#
# ============================================================
#
# REQUIRED:
#
#   version12_1_historical_oos_predictions.csv
#
#
# OPTIONAL FOR PART B:
#
#   initial_lines.csv
#
#
# IMPORTANT:
#
# We recreate Version 12.1 forecast shrinkage using
# expanding-season calibration.
#
# A target season NEVER contributes to the calibration
# used to forecast that same season.
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
# 2. SETTINGS
# ============================================================

MODEL_FILE <-
  "version12_1_historical_oos_predictions.csv"


# ------------------------------------------------------------
# Opening line file
# ------------------------------------------------------------
#
# Free nflverse initial_lines.csv:
#
#   2021 Westgate opening lines
#
# Script also supports a custom CSV containing:
#
#   game_id
#   opening_home_margin
#
# where:
#
# +3.5 = home team favored by 3.5
# -3.5 = away team favored by 3.5
#
OPENING_URL <-
  paste0(
    "https://raw.githubusercontent.com/",
    "nflverse/nfldata/master/data/initial_lines.csv"
  )



OPENING_SPORTSBOOK <-
  "WSGT"


RUN_OPENING_ANALYSIS <-
  TRUE


# ------------------------------------------------------------
# Minimum prior seasons for original Version 12.1
# forecast shrinkage
# ------------------------------------------------------------

MIN_PRIOR_SEASONS <-
  1


# ------------------------------------------------------------
# Minimum prior seasons for NEW extremity correction
#
# Since properly calibrated forecasts begin in 2000,
# the first extremity-correction evaluation season will
# generally be 2001.
# ------------------------------------------------------------

MIN_EXTREMITY_PRIOR_SEASONS <-
  1


# ------------------------------------------------------------
# Forecast magnitude bands
# ------------------------------------------------------------

EXTREMITY_BREAKS <-
  c(
    -Inf,
    3,
    7,
    10,
    14,
    Inf
  )


EXTREMITY_LABELS <-
  c(
    "0-3",
    "3-7",
    "7-10",
    "10-14",
    "14+"
  )


# ------------------------------------------------------------
# Opening-line disagreement thresholds
# ------------------------------------------------------------

OPENING_EDGE_THRESHOLDS <-
  c(
    0.5,
    1,
    2,
    3,
    4,
    5,
    6
  )



# ============================================================
# 3. LOAD HISTORICAL MODEL DATA
# ============================================================

raw <- readr::read_csv(
  MODEL_FILE,
  show_col_types = FALSE
)


cat("\n")
cat("========================================\n")
cat("LOADED VERSION 12.1 OOS DATA\n")
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
# 4. VERIFY COLUMNS
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


missing_columns <-
  setdiff(
    required_columns,
    names(raw)
  )


if (
  length(missing_columns) >
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
# 5. CLEAN DATA
# ============================================================

model_data <- raw %>%
  
  transmute(
    
    season =
      as.integer(season),
    
    week =
      as.integer(week),
    
    game_id =
      as.character(game_id),
    
    home_team =
      as.character(home_team),
    
    away_team =
      as.character(away_team),
    
    actual_margin =
      as.numeric(actual_margin),
    
    raw_predicted_margin =
      as.numeric(predicted_margin),
    
    baseline_margin =
      as.numeric(baseline_margin),
    
    games_played_bucket =
      as.integer(games_played_bucket),
    
    spread_line =
      as.numeric(spread_line)
  ) %>%
  
  filter(
    
    is.finite(actual_margin),
    
    is.finite(raw_predicted_margin),
    
    is.finite(baseline_margin),
    
    games_played_bucket %in%
      1:5
  ) %>%
  
  arrange(
    season,
    week,
    game_id
  )



# ============================================================
# 6. CHECK DUPLICATES
# ============================================================

duplicates <- model_data %>%
  
  count(game_id) %>%
  
  filter(
    n > 1
  )


if (
  nrow(duplicates) >
  0
) {
  
  stop(
    "Duplicate game_id values found."
  )
}



# ============================================================
# 7. VERSION 12.1 FALLBACK WEIGHT
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
# 8. VERSION 12.1 SHRINKAGE FUNCTION
# ============================================================

learn_shrinkage_weights <- function(
    calibration_data
) {
  
  purrr::map_dfr(
    
    1:5,
    
    function(bucket) {
      
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
        nrow(current) ==
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
          !is.finite(denominator) ||
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
        
        calibration_games =
          nrow(current),
        
        learned_forecast_weight =
          learned_weight,
        
        fallback_weight =
          fallback_weight
      )
    }
  )
}



# ============================================================
# 9. BUILD PROPER EXPANDING VERSION 12.1 FORECASTS
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
    target_season in seasons
  ) {
    
    prior_seasons <-
      seasons[
        seasons <
          target_season
      ]
    
    
    if (
      length(prior_seasons) <
      min_prior_seasons
    ) {
      
      next
    }
    
    
    calibration_data <-
      data %>%
      
      filter(
        season <
          target_season
      )
    
    
    target_data <-
      data %>%
      
      filter(
        season ==
          target_season
      )
    
    
    weights <-
      learn_shrinkage_weights(
        calibration_data
      )
    
    
    target_output <-
      target_data %>%
      
      left_join(
        
        weights %>%
          
          select(
            games_played_bucket,
            learned_forecast_weight
          ),
        
        by =
          "games_played_bucket"
      ) %>%
      
      mutate(
        
        proper_forecast_margin = (
          
          baseline_margin +
            
            learned_forecast_weight *
            
            (
              raw_predicted_margin -
                baseline_margin
            )
        ),
        
        calibration_start_season =
          min(calibration_data$season),
        
        calibration_end_season =
          max(calibration_data$season)
      )
    
    
    prediction_storage[[prediction_position]] <-target_output
    
    
    weight_storage[[weight_position]] <- weights %>%
      
      mutate(
        
        target_season =
          target_season,
        
        calibration_start_season =
          min(calibration_data$season),
        
        calibration_end_season =
          max(calibration_data$season),
        
        .before =
          1
      )
    
    
    prediction_position <-
      prediction_position +
      1
    
    
    weight_position <-
      weight_position +
      1
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



expanding <-
  build_expanding_forecasts(
    
    model_data,
    
    min_prior_seasons =
      MIN_PRIOR_SEASONS
  )


forecast_data <-
  expanding$predictions



# ############################################################
#
# PART A
#
# FORECAST EXTREMITY CALIBRATION
#
# ############################################################



# ============================================================
# 10. CREATE EXTREMITY VARIABLES
# ============================================================

extremity_data <-
  forecast_data %>%
  
  mutate(
    
    abs_forecast_margin =
      abs(
        proper_forecast_margin
      ),
    
    
    # --------------------------------------------------------
    # Direction the model predicts
    #
    # +1 = home
    # -1 = away
    # --------------------------------------------------------
    
    forecast_direction =
      case_when(
        
        proper_forecast_margin >
          0 ~
          1,
        
        proper_forecast_margin <
          0 ~
          -1,
        
        TRUE ~
          NA_real_
      ),
    
    
    # --------------------------------------------------------
    # Actual result from the MODEL'S predicted direction
    #
    # Example:
    #
    # Model predicts home by 10
    # Actual home by 4
    #
    # actual_in_model_direction = +4
    #
    #
    # Model predicts away by 10
    # Away actually wins by 4
    #
    # actual_margin = -4
    # direction = -1
    #
    # actual_in_model_direction = +4
    #
    #
    # If predicted favorite loses:
    #
    # actual_in_model_direction becomes negative.
    # --------------------------------------------------------
    
    actual_in_model_direction = (
      actual_margin *
        forecast_direction
    ),
    
    
    extremity_band =
      cut(
        
        abs_forecast_margin,
        
        breaks =
          EXTREMITY_BREAKS,
        
        labels =
          EXTREMITY_LABELS,
        
        right =
          FALSE
      ),
    
    
    original_error = (
      proper_forecast_margin -
        actual_margin
    ),
    
    original_abs_error =
      abs(
        original_error
      )
  )



# ============================================================
# 11. EXTREMITY CALIBRATION TABLE
# ============================================================
#
# KEY COLUMN:
#
# overstatement_points
#
# Positive:
#
#   model predicts larger margins than actually occur
#   in the predicted direction.
#
#
# Example:
#
# average predicted magnitude = 12
# average actual in predicted direction = 8
#
# overstatement = 4
#
# ============================================================

extremity_summary <-
  extremity_data %>%
  
  group_by(
    extremity_band
  ) %>%
  
  summarise(
    
    games =
      n(),
    
    mean_predicted_margin_magnitude =
      mean(
        abs_forecast_margin
      ),
    
    median_predicted_margin_magnitude =
      median(
        abs_forecast_margin
      ),
    
    mean_actual_margin_in_model_direction =
      mean(
        actual_in_model_direction,
        na.rm = TRUE
      ),
    
    median_actual_margin_in_model_direction =
      median(
        actual_in_model_direction,
        na.rm = TRUE
      ),
    
    overstatement_points = (
      mean(
        abs_forecast_margin
      ) -
        mean(
          actual_in_model_direction,
          na.rm = TRUE
        )
    ),
    
    calibration_ratio = (
      mean(
        actual_in_model_direction,
        na.rm = TRUE
      ) /
        mean(
          abs_forecast_margin
        )
    ),
    
    mean_absolute_actual_margin =
      mean(
        abs(actual_margin)
      ),
    
    model_MAE =
      mean(
        original_abs_error
      ),
    
    .groups =
      "drop"
  )



# ============================================================
# 12. OVERALL CALIBRATION REGRESSIONS
# ============================================================
#
# These are DIAGNOSTIC fits using the entire sample.
#
# They help us understand the relationship.
#
# They are NOT used to claim OOS improvement.
#
#
# LINEAR:
#
# actual =
#   intercept + slope * forecast
#
#
# Ideal:
#
# intercept = 0
# slope = 1
#
#
# If slope < 1:
#
# margins are generally too extreme.
#
#
# NONLINEAR EXTREMITY MODEL:
#
# actual =
#   a +
#   b * forecast +
#   c * forecast*abs(forecast)
#
#
# Negative c:
#
# increasingly shrinks large margins toward zero.
#
# ============================================================

linear_diagnostic_model <-
  lm(
    actual_margin ~
      proper_forecast_margin,
    data =
      extremity_data
  )


extremity_diagnostic_model <-
  lm(
    actual_margin ~
      proper_forecast_margin +
      I(
        proper_forecast_margin *
          abs(proper_forecast_margin)
      ),
    data =
      extremity_data
  )



linear_diagnostic <-
  tibble(
    
    intercept =
      coef(
        linear_diagnostic_model
      )[1],
    
    forecast_slope =
      coef(
        linear_diagnostic_model
      )[2],
    
    r_squared =
      summary(
        linear_diagnostic_model
      )$r.squared
  )



extremity_diagnostic <-
  tibble(
    
    intercept =
      coef(
        extremity_diagnostic_model
      )[1],
    
    linear_forecast_coefficient =
      coef(
        extremity_diagnostic_model
      )[2],
    
    extremity_coefficient =
      coef(
        extremity_diagnostic_model
      )[3],
    
    r_squared =
      summary(
        extremity_diagnostic_model
      )$r.squared
  )



# ============================================================
# 13. EXPANDING OOS EXTREMITY RECALIBRATION
# ============================================================
#
# Now we test whether adding another forecast calibration
# layer ACTUALLY improves future seasons.
#
#
# For each target season:
#
#   train correction using earlier properly-calibrated
#   forecasts only.
#
#
# Two candidates:
#
#   A. Linear recalibration
#
#   B. Nonlinear extremity recalibration
#
#
# The Version 12.1 ratings themselves are untouched.
#
# ============================================================

build_extremity_oos <- function(
    data,
    min_prior_seasons = 1
) {
  
  seasons <-
    sort(
      unique(
        data$season
      )
    )
  
  
  output_storage <-
    list()
  
  
  coefficient_storage <-
    list()
  
  
  output_position <-
    1
  
  
  coefficient_position <-
    1
  
  
  for (
    target_season in seasons
  ) {
    
    prior_seasons <-
      seasons[
        seasons <
          target_season
      ]
    
    
    if (
      length(prior_seasons) <
      min_prior_seasons
    ) {
      
      next
    }
    
    
    training <-
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
    
    
    if (
      nrow(training) <
      100
    ) {
      
      next
    }
    
    
    # --------------------------------------------------------
    # Candidate 1:
    # Simple linear recalibration
    # --------------------------------------------------------
    
    linear_fit <-
      lm(
        actual_margin ~
          proper_forecast_margin,
        data =
          training
      )
    
    
    # --------------------------------------------------------
    # Candidate 2:
    # Nonlinear extremity-aware calibration
    # --------------------------------------------------------
    
    extremity_fit <-
      lm(
        
        actual_margin ~
          
          proper_forecast_margin +
          
          I(
            proper_forecast_margin *
              abs(proper_forecast_margin)
          ),
        
        data =
          training
      )
    
    
    target$linear_calibrated_margin <-
      predict(
        linear_fit,
        newdata =
          target
      )
    
    
    target$extremity_calibrated_margin <-
      predict(
        extremity_fit,
        newdata =
          target
      )
    
    
    target$extremity_calibration_start <-
      min(
        training$season
      )
    
    
    target$extremity_calibration_end <-
      max(
        training$season
      )
    
    
    target$extremity_training_games <-
      nrow(
        training
      )
    
    
    nonlinear_coef <-
      coef(
        extremity_fit
      )
    
    
    coefficient_storage[[coefficient_position]] <- tibble(
        
        target_season =
          target_season,
        
        training_start_season =
          min(training$season),
        
        training_end_season =
          max(training$season),
        
        training_games =
          nrow(training),
        
        linear_intercept =
          coef(
            linear_fit
          )[1],
        
        linear_slope =
          coef(
            linear_fit
          )[2],
        
        extremity_intercept =
          nonlinear_coef[1],
        
        extremity_linear_coefficient =
          nonlinear_coef[2],
        
        extremity_nonlinear_coefficient =
          nonlinear_coef[3]
      )
    
    
    output_storage[[output_position]] <-
      target
    
    
    output_position <-
      output_position +
      1
    
    
    coefficient_position <-
      coefficient_position +
      1
  }
  
  
  list(
    
    predictions =
      bind_rows(
        output_storage
      ),
    
    coefficients =
      bind_rows(
        coefficient_storage
      )
  )
}



extremity_oos <-
  build_extremity_oos(
    
    extremity_data,
    
    min_prior_seasons =
      MIN_EXTREMITY_PRIOR_SEASONS
  )



# ============================================================
# 14. COMPARE VERSION 12.1 VS NEW CALIBRATION LAYERS
# ============================================================

calibration_comparison_long <-
  extremity_oos$predictions %>%
  
  select(
    
    season,
    week,
    game_id,
    actual_margin,
    
    original_version12_1 =
      proper_forecast_margin,
    
    linear_recalibration =
      linear_calibrated_margin,
    
    nonlinear_extremity =
      extremity_calibrated_margin
  ) %>%
  
  pivot_longer(
    
    cols =
      c(
        original_version12_1,
        linear_recalibration,
        nonlinear_extremity
      ),
    
    names_to =
      "method",
    
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
      error^2
  )



calibration_comparison <-
  calibration_comparison_long %>%
  
  group_by(
    method
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
    
    .groups =
      "drop"
  ) %>%
  
  arrange(
    MAE
  )



# ============================================================
# 15. CALIBRATION COMPARISON BY SEASON
# ============================================================

calibration_by_season <-
  calibration_comparison_long %>%
  
  group_by(
    season,
    method
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
    
    .groups =
      "drop"
  )



# ============================================================
# 16. EXTREMITY PERFORMANCE OF ORIGINAL VS NONLINEAR
# ============================================================

extremity_oos_band_comparison <-
  extremity_oos$predictions %>%
  
  mutate(
    
    extremity_band =
      cut(
        
        abs(
          proper_forecast_margin
        ),
        
        breaks =
          EXTREMITY_BREAKS,
        
        labels =
          EXTREMITY_LABELS,
        
        right =
          FALSE
      ),
    
    original_abs_error =
      abs(
        proper_forecast_margin -
          actual_margin
      ),
    
    corrected_abs_error =
      abs(
        extremity_calibrated_margin -
          actual_margin
      )
  ) %>%
  
  group_by(
    extremity_band
  ) %>%
  
  summarise(
    
    games =
      n(),
    
    original_MAE =
      mean(
        original_abs_error
      ),
    
    corrected_MAE =
      mean(
        corrected_abs_error
      ),
    
    MAE_improvement = (
      original_MAE -
        corrected_MAE
    ),
    
    .groups =
      "drop"
  )



# ============================================================
# 17. PRINT EXTREMITY RESULTS
# ============================================================

cat("\n")
cat("################################################\n")
cat("PART A - FORECAST EXTREMITY CALIBRATION\n")
cat("################################################\n\n")


cat(
  "EXTREMITY CALIBRATION BY PREDICTED MARGIN:\n\n"
)


print(
  extremity_summary,
  n = Inf,
  width = Inf
)



cat("\n")
cat("LINEAR CALIBRATION DIAGNOSTIC:\n\n")


print(
  linear_diagnostic,
  width = Inf
)



cat("\n")
cat("NONLINEAR EXTREMITY DIAGNOSTIC:\n\n")


print(
  extremity_diagnostic,
  width = Inf
)



cat("\n")
cat("========================================\n")
cat("TRUE OOS FORECAST-LAYER COMPARISON\n")
cat("========================================\n\n")


print(
  calibration_comparison,
  n = Inf,
  width = Inf
)



cat("\n")
cat("========================================\n")
cat("OOS IMPROVEMENT BY FORECAST MAGNITUDE\n")
cat("========================================\n\n")


print(
  extremity_oos_band_comparison,
  n = Inf,
  width = Inf
)



# ============================================================
# 18. EXTREMITY PLOT
# ============================================================

p_extremity <-
  ggplot(
    
    extremity_summary,
    
    aes(
      x =
        mean_predicted_margin_magnitude,
      y =
        mean_actual_margin_in_model_direction
    )
  ) +
  
  geom_abline(
    intercept =
      0,
    slope =
      1,
    linetype =
      "dashed"
  ) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  geom_point(
    size =
      3
  ) +
  
  labs(
    
    title =
      "Version 12.1 Forecast Extremity Calibration",
    
    subtitle =
      "Dashed line = perfect calibration",
    
    x =
      "Average Predicted Margin Magnitude",
    
    y =
      "Average Actual Margin in Predicted Direction"
  ) +
  
  theme_minimal()



print(
  p_extremity
)



# ============================================================
# 19. SAVE PART A RESULTS
# ============================================================

write_csv(
  extremity_summary,
  "extremity_calibration_by_margin_band.csv"
)


write_csv(
  linear_diagnostic,
  "extremity_linear_diagnostic.csv"
)


write_csv(
  extremity_diagnostic,
  "extremity_nonlinear_diagnostic.csv"
)


write_csv(
  extremity_oos$predictions,
  "extremity_oos_predictions.csv"
)


write_csv(
  extremity_oos$coefficients,
  "extremity_oos_coefficients.csv"
)


write_csv(
  calibration_comparison,
  "extremity_oos_method_comparison.csv"
)


write_csv(
  calibration_by_season,
  "extremity_oos_comparison_by_season.csv"
)


write_csv(
  extremity_oos_band_comparison,
  "extremity_oos_improvement_by_band.csv"
)


ggsave(
  "extremity_calibration_plot.png",
  p_extremity,
  width = 9,
  height = 5,
  dpi = 300
)

# ############################################################
#
# PART B
#
# OPENING -> CLOSING LINE MOVEMENT
#
# ############################################################


if (
  RUN_OPENING_ANALYSIS
) {
  
  cat("\n")
  cat("========================================\n")
  cat("LOADING OPENING LINES\n")
  cat("========================================\n\n")
  
  
  # ==========================================================
  # READ DIRECTLY FROM NFLVERSE
  # ==========================================================
  
  opening_raw <-
    readr::read_csv(
      OPENING_URL,
      show_col_types = FALSE
    )
  
  
  cat(
    "Opening-line rows loaded:",
    nrow(opening_raw),
    "\n"
  )
  
  
  cat(
    "Opening-line seasons:",
    paste(
      sort(
        unique(
          opening_raw$season
        )
      ),
      collapse = ", "
    ),
    "\n\n"
  )
  
  
  # ==========================================================
  # VERIFY EXPECTED FORMAT
  # ==========================================================
  
  required_opening_columns <-
    c(
      "season",
      "sportsbook",
      "type",
      "about",
      "side",
      "line"
    )
  
  
  missing_opening_columns <-
    setdiff(
      required_opening_columns,
      names(opening_raw)
    )
  
  
  if (
    length(missing_opening_columns) >
    0
  ) {
    
    stop(
      paste0(
        "Opening-line data is missing columns: ",
        paste(
          missing_opening_columns,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ==========================================================
  # PREPARE HOME-TEAM LOOKUP
  # ==========================================================
  
  game_home_lookup <-
    forecast_data %>%
    
    distinct(
      game_id,
      home_team
    )
  
  
  # ==========================================================
  # GET OPENING SPREAD
  # ==========================================================
  #
  # nflverse example:
  #
  # 2021_01_DAL_TB
  # TB   -6.5
  # DAL  +6.5
  #
  # TB is home.
  #
  # We want everything from HOME margin perspective:
  #
  # TB -6.5 sportsbook notation
  #
  # becomes:
  #
  # opening_home_margin = +6.5
  #
  # ==========================================================
  
  opening_lines <-
    opening_raw %>%
    
    filter(
      type == "SPREAD",
      sportsbook == OPENING_SPORTSBOOK
    ) %>%
    
    inner_join(
      game_home_lookup,
      by =
        c(
          "about" =
            "game_id"
        )
    ) %>%
    
    filter(
      side ==
        home_team
    ) %>%
    
    transmute(
      
      game_id =
        as.character(
          about
        ),
      
      opening_home_margin =
        -as.numeric(
          line
        ),
      
      opening_sportsbook =
        sportsbook
    ) %>%
    
    distinct(
      game_id,
      .keep_all =
        TRUE
    )
  
  
  cat(
    "Opening spreads matched to home teams:",
    nrow(opening_lines),
    "\n\n"
  )
  
  
  # ==========================================================
  # JOIN MODEL + OPENING + CLOSING
  # ==========================================================
  
  movement_data <-
    forecast_data %>%
    
    filter(
      is.finite(
        spread_line
      )
    ) %>%
    
    inner_join(
      opening_lines,
      by =
        "game_id"
    ) %>%
    
    mutate(
      
      closing_home_margin =
        spread_line,
      
      
      # -------------------------------------------------------
      # MODEL RELATIVE TO OPENING LINE
      # -------------------------------------------------------
      #
      # Positive:
      # model likes HOME more than opener
      #
      # Negative:
      # model likes AWAY more than opener
      #
      
      model_open_edge = (
        proper_forecast_margin -
          opening_home_margin
      ),
      
      
      abs_model_open_edge =
        abs(
          model_open_edge
        ),
      
      
      # -------------------------------------------------------
      # OPEN -> CLOSE MARKET MOVEMENT
      # -------------------------------------------------------
      #
      # Positive:
      # market moved toward HOME
      #
      # Negative:
      # market moved toward AWAY
      #
      
      market_move = (
        closing_home_margin -
          opening_home_margin
      ),
      
      
      abs_market_move =
        abs(
          market_move
        ),
      
      
      # -------------------------------------------------------
      # DISTANCE BETWEEN MODEL AND MARKET
      # -------------------------------------------------------
      
      opening_distance_to_model =
        abs(
          proper_forecast_margin -
            opening_home_margin
        ),
      
      
      closing_distance_to_model =
        abs(
          proper_forecast_margin -
            closing_home_margin
        ),
      
      
      # Positive:
      # closing line got closer to model
      
      distance_reduction = (
        opening_distance_to_model -
          closing_distance_to_model
      ),
      
      
      movement_relative_to_model =
        case_when(
          
          closing_distance_to_model <
            opening_distance_to_model ~
            "Toward Model",
          
          closing_distance_to_model >
            opening_distance_to_model ~
            "Away From Model",
          
          TRUE ~
            "No Distance Change"
        ),
      
      
      # -------------------------------------------------------
      # SAME DIRECTION?
      # -------------------------------------------------------
      
      directional_eligible = (
        abs(model_open_edge) >
          1e-8 &
          abs(market_move) >
          1e-8
      ),
      
      
      direction_match =
        case_when(
          
          directional_eligible ~
            sign(
              model_open_edge
            ) ==
            sign(
              market_move
            ),
          
          TRUE ~
            NA
        ),
      
      
      # -------------------------------------------------------
      # ACCURACY
      # -------------------------------------------------------
      
      model_abs_error =
        abs(
          proper_forecast_margin -
            actual_margin
        ),
      
      
      opening_abs_error =
        abs(
          opening_home_margin -
            actual_margin
        ),
      
      
      closing_abs_error =
        abs(
          closing_home_margin -
            actual_margin
        )
    )
  
  
  # ==========================================================
  # OVERALL SUMMARY
  # ==========================================================
  
  movement_summary <-
    movement_data %>%
    
    summarise(
      
      games =
        n(),
      
      seasons =
        paste(
          sort(
            unique(
              season
            )
          ),
          collapse = ", "
        ),
      
      mean_abs_model_open_edge =
        mean(
          abs_model_open_edge
        ),
      
      mean_abs_line_move =
        mean(
          abs_market_move
        ),
      
      line_moved_games =
        sum(
          abs_market_move >
            1e-8
        ),
      
      directional_games =
        sum(
          directional_eligible
        ),
      
      directional_matches =
        sum(
          direction_match,
          na.rm = TRUE
        ),
      
      directional_match_pct =
        100 *
        mean(
          direction_match,
          na.rm = TRUE
        ),
      
      moved_toward_model_games =
        sum(
          movement_relative_to_model ==
            "Toward Model"
        ),
      
      moved_away_from_model_games =
        sum(
          movement_relative_to_model ==
            "Away From Model"
        ),
      
      no_distance_change_games =
        sum(
          movement_relative_to_model ==
            "No Distance Change"
        ),
      
      moved_toward_model_pct =
        100 *
        mean(
          movement_relative_to_model ==
            "Toward Model"
        ),
      
      mean_opening_distance_to_model =
        mean(
          opening_distance_to_model
        ),
      
      mean_closing_distance_to_model =
        mean(
          closing_distance_to_model
        ),
      
      mean_distance_reduction =
        mean(
          distance_reduction
        ),
      
      model_MAE =
        mean(
          model_abs_error
        ),
      
      opening_MAE =
        mean(
          opening_abs_error
        ),
      
      closing_MAE =
        mean(
          closing_abs_error
        )
    )
  
  
  # ==========================================================
  # DIRECTIONAL SIGN TEST
  # ==========================================================
  
  eligible_direction <-
    movement_data %>%
    
    filter(
      directional_eligible
    )
  
  
  if (
    nrow(
      eligible_direction
    ) >
    0
  ) {
    
    direction_test <-
      binom.test(
        
        x =
          sum(
            eligible_direction$direction_match
          ),
        
        n =
          nrow(
            eligible_direction
          ),
        
        p =
          0.5,
        
        alternative =
          "greater"
      )
    
    
    direction_test_output <-
      tibble(
        
        directional_games =
          nrow(
            eligible_direction
          ),
        
        matches =
          sum(
            eligible_direction$direction_match
          ),
        
        match_pct =
          100 *
          mean(
            eligible_direction$direction_match
          ),
        
        null_pct =
          50,
        
        p_value_above_50 =
          direction_test$p.value
      )
    
  } else {
    
    direction_test_output <-
      tibble()
  }
  
  
  # ==========================================================
  # LINE MOVEMENT REGRESSION
  # ==========================================================
  
  movement_regression <-
    lm(
      market_move ~
        model_open_edge,
      data =
        movement_data
    )
  
  
  movement_regression_output <-
    tibble(
      
      intercept =
        coef(
          movement_regression
        )[1],
      
      model_edge_slope =
        coef(
          movement_regression
        )[2],
      
      slope_p_value =
        summary(
          movement_regression
        )$coefficients[
          2,
          4
        ],
      
      r_squared =
        summary(
          movement_regression
        )$r.squared,
      
      correlation =
        cor(
          movement_data$model_open_edge,
          movement_data$market_move,
          use =
            "complete.obs"
        )
    )
  
  
  # ==========================================================
  # SUMMARY FUNCTION
  # ==========================================================
  
  summarize_movement <- function(
    data
  ) {
    
    eligible <-
      data[
        data$directional_eligible,
      ]
    
    
    tibble(
      
      games =
        nrow(
          data
        ),
      
      directional_games =
        nrow(
          eligible
        ),
      
      direction_match_pct =
        if (
          nrow(eligible) >
          0
        ) {
          
          100 *
            mean(
              eligible$direction_match
            )
          
        } else {
          
          NA_real_
        },
      
      moved_toward_model_pct =
        100 *
        mean(
          data$movement_relative_to_model ==
            "Toward Model"
        ),
      
      mean_model_open_edge =
        mean(
          data$abs_model_open_edge
        ),
      
      mean_abs_market_move =
        mean(
          data$abs_market_move
        ),
      
      mean_distance_reduction =
        mean(
          data$distance_reduction
        ),
      
      model_MAE =
        mean(
          data$model_abs_error
        ),
      
      opening_MAE =
        mean(
          data$opening_abs_error
        ),
      
      closing_MAE =
        mean(
          data$closing_abs_error
        )
    )
  }
  
  
  # ==========================================================
  # RESULTS BY EDGE THRESHOLD
  # ==========================================================
  
  movement_by_threshold <-
    purrr::map_dfr(
      
      OPENING_EDGE_THRESHOLDS,
      
      function(
    threshold
      ) {
        
        movement_data %>%
          
          filter(
            abs_model_open_edge >=
              threshold
          ) %>%
          
          summarize_movement() %>%
          
          mutate(
            
            edge_threshold =
              threshold,
            
            .before =
              1
          )
      }
    )
  
  
  # ==========================================================
  # NON-OVERLAPPING EDGE BANDS
  # ==========================================================
  
  movement_band_data <-
    movement_data %>%
    
    mutate(
      
      model_open_band =
        cut(
          
          abs_model_open_edge,
          
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
    )
  
  
  movement_by_band <-
    movement_band_data %>%
    
    group_by(
      model_open_band
    ) %>%
    
    group_modify(
      ~ summarize_movement(
        .x
      )
    ) %>%
    
    ungroup()
  
  
  # ==========================================================
  # PRINT RESULTS
  # ==========================================================
  
  cat("\n")
  cat("################################################\n")
  cat("PART B - OPENING -> CLOSING MOVEMENT\n")
  cat("################################################\n\n")
  
  
  cat(
    "Matched games:",
    nrow(
      movement_data
    ),
    "\n\n"
  )
  
  
  cat("OVERALL SUMMARY:\n\n")
  
  print(
    movement_summary,
    width = Inf
  )
  
  
  cat("\n")
  cat("DIRECTIONAL SIGN TEST:\n\n")
  
  print(
    direction_test_output,
    width = Inf
  )
  
  
  cat("\n")
  cat("LINE MOVEMENT REGRESSION:\n\n")
  
  print(
    movement_regression_output,
    width = Inf
  )
  
  
  cat("\n")
  cat("MOVEMENT BY MODEL-OPEN EDGE THRESHOLD:\n\n")
  
  print(
    movement_by_threshold,
    n = Inf,
    width = Inf
  )
  
  
  cat("\n")
  cat("MOVEMENT BY MODEL-OPEN EDGE BAND:\n\n")
  
  print(
    movement_by_band,
    n = Inf,
    width = Inf
  )
  
  
  # ==========================================================
  # SAVE
  # ==========================================================
  
  write_csv(
    movement_data,
    "opening_closing_game_level.csv"
  )
  
  write_csv(
    movement_summary,
    "opening_closing_overall_summary.csv"
  )
  
  write_csv(
    direction_test_output,
    "opening_closing_direction_test.csv"
  )
  
  write_csv(
    movement_regression_output,
    "opening_closing_regression.csv"
  )
  
  write_csv(
    movement_by_threshold,
    "opening_closing_by_model_edge_threshold.csv"
  )
  
  write_csv(
    movement_by_band,
    "opening_closing_by_model_edge_band.csv"
  )
}
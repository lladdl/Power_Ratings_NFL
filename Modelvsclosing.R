# ============================================================
# NFL VERSION 12.1
# FORECAST ACCURACY VS NFL CLOSING SPREAD
# PROPER EXPANDING-SEASON CALIBRATION
# ============================================================
#
# PURPOSE
# -------
# Compare Version 12.1 point-margin forecasts directly against
# the NFL closing spread.
#
# This is NOT a profitability test.
#
# Main question:
#
#   Is the model closer to the actual final margin than Vegas?
#
#
# IMPORTANT:
#
# We do NOT use the existing forecast_margin column because
# its shrinkage weights were learned from the complete
# historical OOS sample.
#
# Instead:
#
#   Target season 2000:
#       calibrate using 1999 only
#
#   Target season 2001:
#       calibrate using 1999-2000
#
#   ...
#
#   Target season 2025:
#       calibrate using 1999-2024
#
#
# Then compare:
#
#   Model absolute error
#
#       abs(actual_margin - proper_forecast_margin)
#
# versus:
#
#   Vegas absolute error
#
#       abs(actual_margin - spread_line)
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


# Minimum number of prior seasons needed before evaluating
MIN_PRIOR_SEASONS <- 1



# ============================================================
# 3. LOAD DATA
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
    
    games_played_bucket %in% 1:5
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
# 7. VERSION 12.1 FIXED FALLBACK WEIGHT
# ============================================================

get_fixed_blend_weight <- function(
    average_games_per_team
) {
  
  pmin(
    1,
    pmax(
      0,
      average_games_per_team / 4
    )
  )
}



# ============================================================
# 8. LEARN VERSION 12.1 SHRINKAGE WEIGHTS
# ============================================================
#
# For each games-played bucket:
#
# delta =
#
#   raw prediction - baseline
#
# target =
#
#   actual margin - baseline
#
#
# weight =
#
#   sum(delta * target) /
#   sum(delta^2)
#
#
# constrained to:
#
#   0 <= weight <= 1
#
# ============================================================

learn_shrinkage_weights <- function(
    calibration_data
) {
  
  purrr::map_dfr(
    
    1:5,
    
    function(bucket) {
      
      current <- calibration_data %>%
        
        filter(
          games_played_bucket ==
            bucket
        )
      
      
      fallback_weight <-
        get_fixed_blend_weight(
          
          if (
            bucket < 5
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
          denominator <= 1e-12
        ) {
          
          learned_weight <-
            fallback_weight
          
        } else {
          
          learned_weight <- (
            sum(
              delta * target
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
# 9. EXPANDING-SEASON CALIBRATION
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
    
    
    # ----------------------------------------------------------
    # Calibration:
    # ONLY seasons before target season
    # ----------------------------------------------------------
    
    calibration_data <-
      data %>%
      
      filter(
        season <
          target_season
      )
    
    
    # ----------------------------------------------------------
    # Evaluation:
    # target season only
    # ----------------------------------------------------------
    
    target_data <-
      data %>%
      
      filter(
        season ==
          target_season
      )
    
    
    if (
      nrow(calibration_data) ==
      0 ||
      nrow(target_data) ==
      0
    ) {
      
      next
    }
    
    
    weights <-
      learn_shrinkage_weights(
        calibration_data
      ) %>%
      
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
    
    
    calibrated_target <-
      target_data %>%
      
      left_join(
        
        weights %>%
          
          select(
            games_played_bucket,
            calibration_games,
            learned_forecast_weight,
            fallback_weight
          ),
        
        by =
          "games_played_bucket"
      ) %>%
      
      mutate(
        
        calibration_start_season =
          min(calibration_data$season),
        
        calibration_end_season =
          max(calibration_data$season),
        
        
        # ------------------------------------------------------
        # FINAL LEAKAGE-FREE FORECAST
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
    
    
    prediction_storage[[prediction_position]] <-calibrated_target
    
    
    weight_storage[[weight_position]] <-weights
    
    
    prediction_position <-
      prediction_position +
      1
    
    
    weight_position <-
      weight_position +
      1
  }
  
  
  if (
    length(prediction_storage) ==
    0
  ) {
    
    stop(
      "No seasons available after calibration requirements."
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
# 10. CREATE LEAKAGE-FREE FORECASTS
# ============================================================

expanding <-
  build_expanding_forecasts(
    
    model_data,
    
    min_prior_seasons =
      MIN_PRIOR_SEASONS
  )


comparison_data <-
  expanding$predictions %>%
  
  filter(
    is.finite(spread_line)
  )



# ============================================================
# 11. MODEL ERROR VS VEGAS ERROR
# ============================================================

comparison_data <-
  comparison_data %>%
  
  mutate(
    
    # --------------------------------------------------------
    # Signed errors
    # --------------------------------------------------------
    
    model_error = (
      proper_forecast_margin -
        actual_margin
    ),
    
    vegas_error = (
      spread_line -
        actual_margin
    ),
    
    
    # --------------------------------------------------------
    # Absolute errors
    # --------------------------------------------------------
    
    model_abs_error =
      abs(
        model_error
      ),
    
    vegas_abs_error =
      abs(
        vegas_error
      ),
    
    
    # --------------------------------------------------------
    # Squared errors
    # --------------------------------------------------------
    
    model_squared_error =
      model_error^2,
    
    vegas_squared_error =
      vegas_error^2,
    
    
    # --------------------------------------------------------
    # Positive = MODEL was better
    #
    # Example:
    #
    # Model error = 6
    # Vegas error = 9
    #
    # Advantage = +3
    # --------------------------------------------------------
    
    model_accuracy_advantage = (
      vegas_abs_error -
        model_abs_error
    ),
    
    
    # --------------------------------------------------------
    # Model vs Vegas disagreement
    # --------------------------------------------------------
    
    model_vs_vegas_edge = (
      proper_forecast_margin -
        spread_line
    ),
    
    abs_model_vs_vegas_edge =
      abs(
        model_vs_vegas_edge
      ),
    
    
    # --------------------------------------------------------
    # Who was closer?
    # --------------------------------------------------------
    
    closer_forecast =
      case_when(
        
        model_abs_error <
          vegas_abs_error ~
          "Model",
        
        vegas_abs_error <
          model_abs_error ~
          "Vegas",
        
        TRUE ~
          "Tie"
      )
  )



# ============================================================
# 12. OVERALL SUMMARY
# ============================================================

overall_summary <-
  comparison_data %>%
  
  summarise(
    
    games =
      n(),
    
    model_MAE =
      mean(
        model_abs_error
      ),
    
    vegas_MAE =
      mean(
        vegas_abs_error
      ),
    
    MAE_difference =
      model_MAE -
      vegas_MAE,
    
    model_RMSE =
      sqrt(
        mean(
          model_squared_error
        )
      ),
    
    vegas_RMSE =
      sqrt(
        mean(
          vegas_squared_error
        )
      ),
    
    RMSE_difference =
      model_RMSE -
      vegas_RMSE,
    
    model_mean_bias =
      mean(
        model_error
      ),
    
    vegas_mean_bias =
      mean(
        vegas_error
      ),
    
    model_closer_games =
      sum(
        closer_forecast ==
          "Model"
      ),
    
    vegas_closer_games =
      sum(
        closer_forecast ==
          "Vegas"
      ),
    
    tied_games =
      sum(
        closer_forecast ==
          "Tie"
      ),
    
    model_closer_pct =
      100 *
      mean(
        closer_forecast ==
          "Model"
      ),
    
    vegas_closer_pct =
      100 *
      mean(
        closer_forecast ==
          "Vegas"
      ),
    
    mean_model_accuracy_advantage =
      mean(
        model_accuracy_advantage
      )
  )



# ============================================================
# 13. PAIRED STATISTICAL TESTS
# ============================================================
#
# Because both forecasts predict the SAME games, this is a
# paired comparison.
#
#
# Difference:
#
# model_abs_error - vegas_abs_error
#
# Negative:
#     model better
#
# Positive:
#     Vegas better
#
# ============================================================

error_difference <- (
  comparison_data$model_abs_error -
    comparison_data$vegas_abs_error
)



# ------------------------------------------------------------
# Paired t-test
# ------------------------------------------------------------

paired_t_test <-
  t.test(
    
    comparison_data$model_abs_error,
    
    comparison_data$vegas_abs_error,
    
    paired =
      TRUE
  )



# ------------------------------------------------------------
# Wilcoxon signed-rank test
# ------------------------------------------------------------

wilcox_test <-
  wilcox.test(
    
    comparison_data$model_abs_error,
    
    comparison_data$vegas_abs_error,
    
    paired =
      TRUE,
    
    exact =
      FALSE
  )



statistical_tests <-
  tibble(
    
    mean_error_difference =
      mean(
        error_difference
      ),
    
    difference_ci_low =
      paired_t_test$conf.int[1],
    
    difference_ci_high =
      paired_t_test$conf.int[2],
    
    paired_t_p_value =
      paired_t_test$p.value,
    
    wilcoxon_p_value =
      wilcox_test$p.value
  )



# ============================================================
# 14. SUMMARY FUNCTION
# ============================================================

summarize_accuracy <- function(
    data
) {
  
  tibble(
    
    games =
      nrow(data),
    
    model_MAE =
      mean(
        data$model_abs_error
      ),
    
    vegas_MAE =
      mean(
        data$vegas_abs_error
      ),
    
    MAE_difference = (
      mean(
        data$model_abs_error
      ) -
        mean(
          data$vegas_abs_error
        )
    ),
    
    model_RMSE =
      sqrt(
        mean(
          data$model_squared_error
        )
      ),
    
    vegas_RMSE =
      sqrt(
        mean(
          data$vegas_squared_error
        )
      ),
    
    model_closer_pct =
      100 *
      mean(
        data$closer_forecast ==
          "Model"
      ),
    
    vegas_closer_pct =
      100 *
      mean(
        data$closer_forecast ==
          "Vegas"
      ),
    
    ties =
      sum(
        data$closer_forecast ==
          "Tie"
      ),
    
    mean_model_advantage =
      mean(
        data$model_accuracy_advantage
      ),
    
    median_model_advantage =
      median(
        data$model_accuracy_advantage
      )
  )
}



# ============================================================
# 15. ACCURACY BY WEEK
# ============================================================

week_results <-
  comparison_data %>%
  
  group_by(
    week
  ) %>%
  
  group_modify(
    ~ summarize_accuracy(
      .x
    )
  ) %>%
  
  ungroup()



# ============================================================
# 16. ACCURACY BY STARTING WEEK
# ============================================================
#
# Example:
#
# start_week = 5
#
# evaluates every game from Week 5 onward.
#
# This helps answer:
#
# "Does the model eventually become more accurate than Vegas?"
#
# ============================================================

start_weeks <-
  sort(
    unique(
      comparison_data$week
    )
  )


start_week_results <-
  purrr::map_dfr(
    
    start_weeks,
    
    function(start_week) {
      
      comparison_data %>%
        
        filter(
          week >=
            start_week
        ) %>%
        
        summarize_accuracy() %>%
        
        mutate(
          
          start_week =
            start_week,
          
          .before =
            1
        )
    }
  )



# ============================================================
# 17. ACCURACY BY SEASON
# ============================================================

season_results <-
  comparison_data %>%
  
  group_by(
    season
  ) %>%
  
  group_modify(
    ~ summarize_accuracy(
      .x
    )
  ) %>%
  
  ungroup()



# ============================================================
# 18. MODEL-vs-VEGAS DISAGREEMENT BANDS
# ============================================================
#
# This directly addresses:
#
# "When my model disagrees more strongly with Vegas,
#  is my model actually more accurate?"
#
# ============================================================

comparison_data <-
  comparison_data %>%
  
  mutate(
    
    disagreement_band =
      cut(
        
        abs_model_vs_vegas_edge,
        
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


disagreement_results <-
  comparison_data %>%
  
  group_by(
    disagreement_band
  ) %>%
  
  group_modify(
    ~ summarize_accuracy(
      .x
    )
  ) %>%
  
  ungroup()



# ============================================================
# 19. MODEL CLOSER / VEGAS CLOSER COUNTS
# ============================================================

closer_counts <-
  comparison_data %>%
  
  count(
    closer_forecast
  ) %>%
  
  mutate(
    
    pct =
      100 *
      n /
      sum(n)
  )



# ============================================================
# 20. PRINT OVERALL RESULTS
# ============================================================

cat("\n")
cat("========================================\n")
cat("MODEL VS CLOSING SPREAD ACCURACY\n")
cat("========================================\n\n")


print(
  overall_summary,
  width =
    Inf
)



cat("\n")
cat("========================================\n")
cat("PAIRED STATISTICAL TESTS\n")
cat("========================================\n\n")


print(
  statistical_tests,
  width =
    Inf
)



cat("\n")
cat("INTERPRETATION OF MAE DIFFERENCE:\n\n")

cat(
  "Negative MAE_difference = MODEL better\n"
)

cat(
  "Positive MAE_difference = VEGAS better\n\n"
)



# ============================================================
# 21. PRINT BY WEEK
# ============================================================

cat("\n")
cat("========================================\n")
cat("ACCURACY BY INDIVIDUAL WEEK\n")
cat("========================================\n\n")


print(
  week_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 22. PRINT BY STARTING WEEK
# ============================================================

cat("\n")
cat("========================================\n")
cat("ACCURACY FROM WEEK X ONWARD\n")
cat("========================================\n\n")


print(
  start_week_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 23. PRINT BY SEASON
# ============================================================

cat("\n")
cat("========================================\n")
cat("ACCURACY BY SEASON\n")
cat("========================================\n\n")


print(
  season_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 24. PRINT BY MODEL/VEGAS DISAGREEMENT
# ============================================================

cat("\n")
cat("========================================\n")
cat("ACCURACY BY MODEL-VS-VEGAS DISAGREEMENT\n")
cat("========================================\n\n")


print(
  disagreement_results,
  n =
    Inf,
  width =
    Inf
)



# ============================================================
# 25. PLOT:
# MODEL MAE VS VEGAS MAE BY WEEK
# ============================================================

week_plot_data <-
  week_results %>%
  
  select(
    week,
    model_MAE,
    vegas_MAE
  ) %>%
  
  pivot_longer(
    
    cols =
      c(
        model_MAE,
        vegas_MAE
      ),
    
    names_to =
      "forecast_source",
    
    values_to =
      "MAE"
  ) %>%
  
  mutate(
    
    forecast_source =
      recode(
        
        forecast_source,
        
        model_MAE =
          "Version 12.1",
        
        vegas_MAE =
          "Vegas Closing Spread"
      )
  )


p1 <-
  ggplot(
    
    week_plot_data,
    
    aes(
      x =
        week,
      y =
        MAE,
      group =
        forecast_source,
      linetype =
        forecast_source
    )
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
      sort(
        unique(
          week_plot_data$week
        )
      )
  ) +
  
  labs(
    
    title =
      "Forecast MAE by NFL Week",
    
    subtitle =
      "Version 12.1 vs Closing Spread",
    
    x =
      "NFL Week",
    
    y =
      "Mean Absolute Error (Points)",
    
    linetype =
      NULL
  ) +
  
  theme_minimal()



# ============================================================
# 26. PLOT:
# MODEL ADVANTAGE BY WEEK
# ============================================================
#
# Positive =
# model more accurate
#
# Negative =
# Vegas more accurate
#
# ============================================================

p2 <-
  ggplot(
    
    week_results,
    
    aes(
      x =
        week,
      y =
        mean_model_advantage
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
      sort(
        unique(
          week_results$week
        )
      )
  ) +
  
  labs(
    
    title =
      "Version 12.1 Accuracy Advantage by Week",
    
    subtitle =
      "Positive values mean the model was closer than the closing spread",
    
    x =
      "NFL Week",
    
    y =
      "Vegas Absolute Error - Model Absolute Error"
  ) +
  
  theme_minimal()



# ============================================================
# 27. PLOT:
# ACCURACY BY DISAGREEMENT BAND
# ============================================================

band_plot_data <-
  disagreement_results %>%
  
  select(
    disagreement_band,
    model_MAE,
    vegas_MAE
  ) %>%
  
  pivot_longer(
    
    cols =
      c(
        model_MAE,
        vegas_MAE
      ),
    
    names_to =
      "forecast_source",
    
    values_to =
      "MAE"
  ) %>%
  
  mutate(
    
    forecast_source =
      recode(
        
        forecast_source,
        
        model_MAE =
          "Version 12.1",
        
        vegas_MAE =
          "Vegas Closing Spread"
      )
  )


p3 <-
  ggplot(
    
    band_plot_data,
    
    aes(
      x =
        disagreement_band,
      y =
        MAE,
      group =
        forecast_source,
      linetype =
        forecast_source
    )
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
      "Forecast Accuracy by Model-Vegas Disagreement",
    
    subtitle =
      "Does a larger disagreement identify games where the model knows more?",
    
    x =
      "Absolute Model vs Closing-Line Difference (Points)",
    
    y =
      "Mean Absolute Error (Points)",
    
    linetype =
      NULL
  ) +
  
  theme_minimal()



# ============================================================
# 28. PLOT:
# MODEL ADVANTAGE BY SEASON
# ============================================================

p4 <-
  ggplot(
    
    season_results,
    
    aes(
      x =
        season,
      y =
        mean_model_advantage
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
      "Version 12.1 Accuracy Advantage by Season",
    
    subtitle =
      "Positive means the model beat the closing spread on average",
    
    x =
      "Season",
    
    y =
      "Vegas Absolute Error - Model Absolute Error"
  ) +
  
  theme_minimal()



# ============================================================
# 29. PRINT PLOTS
# ============================================================

print(
  p1
)

print(
  p2
)

print(
  p3
)

print(
  p4
)



# ============================================================
# 30. SAVE TABLES
# ============================================================

write_csv(
  
  comparison_data,
  
  "accuracy_model_vs_vegas_game_level.csv"
)


write_csv(
  
  overall_summary,
  
  "accuracy_model_vs_vegas_overall.csv"
)


write_csv(
  
  statistical_tests,
  
  "accuracy_model_vs_vegas_statistical_tests.csv"
)


write_csv(
  
  week_results,
  
  "accuracy_model_vs_vegas_by_week.csv"
)


write_csv(
  
  start_week_results,
  
  "accuracy_model_vs_vegas_by_start_week.csv"
)


write_csv(
  
  season_results,
  
  "accuracy_model_vs_vegas_by_season.csv"
)


write_csv(
  
  disagreement_results,
  
  "accuracy_model_vs_vegas_by_disagreement.csv"
)


write_csv(
  
  closer_counts,
  
  "accuracy_model_vs_vegas_closer_counts.csv"
)


write_csv(
  
  expanding$weights,
  
  "accuracy_expanding_calibration_weights.csv"
)



# ============================================================
# 31. SAVE PLOTS
# ============================================================

ggsave(
  
  "accuracy_MAE_by_week.png",
  
  p1,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)


ggsave(
  
  "accuracy_advantage_by_week.png",
  
  p2,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)


ggsave(
  
  "accuracy_by_disagreement.png",
  
  p3,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)


ggsave(
  
  "accuracy_advantage_by_season.png",
  
  p4,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)



# ============================================================
# 32. FINAL CONSOLE MESSAGE
# ============================================================

cat("\n")
cat("========================================\n")
cat("ACCURACY COMPARISON COMPLETE\n")
cat("========================================\n\n")


cat(
  "Most important output:\n",
  "  accuracy_model_vs_vegas_overall.csv\n\n"
)


cat(
  "Does model improve later in season?\n",
  "  accuracy_model_vs_vegas_by_week.csv\n",
  "  accuracy_model_vs_vegas_by_start_week.csv\n\n"
)


cat(
  "Does bigger disagreement mean stronger model signal?\n",
  "  accuracy_model_vs_vegas_by_disagreement.csv\n\n"
)


cat(
  "Season-to-season consistency:\n",
  "  accuracy_model_vs_vegas_by_season.csv\n\n"
)
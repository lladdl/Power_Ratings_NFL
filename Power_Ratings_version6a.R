# ============================================================
# FOOTBALL POWER RATINGS - VERSION 6A
# TEST WHETHER LEAGUE PARITY CAN BE ESTIMATED
# ============================================================
#
# QUESTION:
#
# Can we estimate the hidden true team-strength SD using
# only games that have already been played?
#
#
# We test:
#
# True hidden rating SD:
#   2, 3, 4, 5, 6, 7, 8
#
# Parity-estimation rating models:
#
#   Cap 14
#   Cap 21
#   Raw margin
#
#
# For every week:
#
#   1. Use only games played through that week.
#   2. Estimate team ratings.
#   3. Calculate SD of estimated ratings.
#   4. Compare estimated SD with hidden true SD.
#
#
# We then calibrate:
#
#   True SD ~ Estimated Rating SD
#
# using training simulations only.
#
# ============================================================



# ============================================================
# 1. SETTINGS
# ============================================================

parity_true_sd_values <- c(
  2,
  3,
  4,
  5,
  6,
  7,
  8
)


# Number of simulated seasons at each SD

n_parity_simulations <- 1000



# ------------------------------------------------------------
# Test several ways of fitting the ratings used ONLY
# to estimate league parity.
# ------------------------------------------------------------

parity_cap_values <- c(
  14,
  21,
  Inf
)


parity_cap_labels <- c(
  "Cap 14",
  "Cap 21",
  "Raw"
)



# ------------------------------------------------------------
# Weeks at which parity will be estimated
# ------------------------------------------------------------
#
# Week 1 would be extremely unstable.
#
# We start after Week 2.
#
# Week 13 represents the information available entering
# Week 14.
# ------------------------------------------------------------

parity_weeks <- 2:(max_week - 1)



# Fixed team shrinkage for parity-estimation experiment

parity_team_lambda <- 2



# ============================================================
# 2. PREALLOCATE RESULTS
# ============================================================

total_parity_rows <-
  
  length(parity_true_sd_values) *
  
  n_parity_simulations *
  
  length(parity_weeks) *
  
  length(parity_cap_values)



parity_results <- data.frame(
  
  true_rating_SD =
    rep(
      NA_real_,
      total_parity_rows
    ),
  
  simulation =
    rep(
      NA_integer_,
      total_parity_rows
    ),
  
  week =
    rep(
      NA_integer_,
      total_parity_rows
    ),
  
  estimator =
    rep(
      NA_character_,
      total_parity_rows
    ),
  
  estimated_rating_SD =
    rep(
      NA_real_,
      total_parity_rows
    ),
  
  estimation_error =
    rep(
      NA_real_,
      total_parity_rows
    ),
  
  absolute_error =
    rep(
      NA_real_,
      total_parity_rows
    ),
  
  squared_error =
    rep(
      NA_real_,
      total_parity_rows
    )
  
)



row_index <- 1



# ============================================================
# 3. SET SEED
# ============================================================

set.seed(
  20260916
)



# ============================================================
# 4. MONTE CARLO LOOP
# ============================================================

for (
  current_true_sd in parity_true_sd_values
) {
  
  
  cat(
    "\n========================================\n"
  )
  
  cat(
    "TRUE RATING SD:",
    current_true_sd,
    "\n"
  )
  
  cat(
    "========================================\n"
  )
  
  
  
  for (
    s in 1:n_parity_simulations
  ) {
    
    
    # ========================================================
    # GENERATE HIDDEN TEAM STRENGTHS
    # ========================================================
    
    season_true_ratings <- generate_true_ratings(
      
      league_teams =
        teams,
      
      target_sd =
        current_true_sd
      
    )
    
    
    
    # ========================================================
    # SIMULATE COMPLETE SEASON
    # ========================================================
    
    sim_games <- simulate_season(
      
      schedule =
        schedule,
      
      true_ratings =
        season_true_ratings,
      
      true_hfa =
        true_hfa,
      
      noise_sd =
        noise_sd
      
    )
    
    
    
    # ========================================================
    # ESTIMATE PARITY AFTER EACH WEEK
    # ========================================================
    
    for (
      current_week in parity_weeks
    ) {
      
      
      # ------------------------------------------------------
      # ONLY USE GAMES ALREADY PLAYED
      # ------------------------------------------------------
      
      training_games <- sim_games[
        
        sim_games$week <=
          current_week,
        
      ]
      
      
      
      # ------------------------------------------------------
      # HFA SHRINKAGE USING ONLY AVAILABLE DATA
      # ------------------------------------------------------
      
      current_hfa_lambda <- get_hfa_lambda(
        
        data =
          training_games,
        
        league_teams =
          teams
        
      )
      
      
      
      # ======================================================
      # FIT EACH PARITY ESTIMATOR
      # ======================================================
      
      for (
        estimator_index in seq_along(parity_cap_values)
      ) {
        
        
        current_cap <-
          
          parity_cap_values[
            estimator_index
          ]
        
        
        current_label <-
          
          parity_cap_labels[
            estimator_index
          ]
        
        
        
        # ----------------------------------------------------
        # FIT RATINGS
        # ----------------------------------------------------
        
        parity_model <- fit_power_ratings(
          
          data =
            training_games,
          
          league_teams =
            teams,
          
          margin_cap =
            current_cap,
          
          team_lambda =
            parity_team_lambda,
          
          hfa_lambda =
            current_hfa_lambda,
          
          hfa_prior =
            hfa_prior
          
        )
        
        
        
        # ----------------------------------------------------
        # OBSERVABLE PARITY ESTIMATE
        # ----------------------------------------------------
        #
        # This is what we would actually have in real life.
        #
        # NO hidden ratings are used here.
        # ----------------------------------------------------
        
        estimated_sd <- sd(
          
          parity_model$ratings$rating
          
        )
        
        
        
        # ----------------------------------------------------
        # SAVE RESULTS
        # ----------------------------------------------------
        
        parity_results$true_rating_SD[
          row_index
        ] <-
          
          current_true_sd
        
        
        parity_results$simulation[
          row_index
        ] <-
          
          s
        
        
        parity_results$week[
          row_index
        ] <-
          
          current_week
        
        
        parity_results$estimator[
          row_index
        ] <-
          
          current_label
        
        
        parity_results$estimated_rating_SD[
          row_index
        ] <-
          
          estimated_sd
        
        
        
        error <-
          
          estimated_sd -
          
          current_true_sd
        
        
        
        parity_results$estimation_error[
          row_index
        ] <-
          
          error
        
        
        parity_results$absolute_error[
          row_index
        ] <-
          
          abs(error)
        
        
        parity_results$squared_error[
          row_index
        ] <-
          
          error^2
        
        
        
        row_index <-
          
          row_index + 1
        
      }
      
    }
    
    
    
    # ========================================================
    # PROGRESS
    # ========================================================
    
    if (
      s %% 100 == 0
    ) {
      
      cat(
        "True SD",
        current_true_sd,
        "- completed",
        s,
        "of",
        n_parity_simulations,
        "\n"
      )
      
    }
    
  }
  
}



# ============================================================
# 5. SUMMARY BY TRUE SD, WEEK, ESTIMATOR
# ============================================================

parity_summary <- aggregate(
  
  cbind(
    
    estimated_rating_SD,
    estimation_error,
    absolute_error,
    squared_error
    
  ) ~
    
    true_rating_SD +
    week +
    estimator,
  
  data =
    parity_results,
  
  FUN =
    mean
  
)



# Convert mean squared error to RMSE

parity_summary$RMSE <-
  
  sqrt(
    parity_summary$squared_error
  )



names(parity_summary)[
  names(parity_summary) ==
    "estimation_error"
] <-
  
  "bias"


names(parity_summary)[
  names(parity_summary) ==
    "absolute_error"
] <-
  
  "MAE"



# ============================================================
# 6. CORRELATION WITH TRUE PARITY BY WEEK
# ============================================================
#
# Here we pool all true SD levels together.
#
# A good parity estimator should have a high correlation
# between:
#
#   estimated SD
#
# and:
#
#   hidden true SD
#
# ============================================================

correlation_groups <- split(
  
  parity_results,
  
  list(
    parity_results$week,
    parity_results$estimator
  ),
  
  drop = TRUE
  
)



correlation_list <- lapply(
  
  correlation_groups,
  
  function(x) {
    
    
    data.frame(
      
      week =
        unique(x$week),
      
      estimator =
        unique(x$estimator),
      
      correlation =
        cor(
          x$estimated_rating_SD,
          x$true_rating_SD
        ),
      
      parity_MAE =
        mean(
          abs(
            x$estimated_rating_SD -
              x$true_rating_SD
          )
        ),
      
      parity_RMSE =
        sqrt(
          
          mean(
            
            (
              x$estimated_rating_SD -
                x$true_rating_SD
            )^2
            
          )
          
        )
      
    )
    
  }
  
)



parity_quality <- do.call(
  
  rbind,
  
  correlation_list
  
)


rownames(parity_quality) <- NULL


parity_quality <- parity_quality[
  
  order(
    parity_quality$week,
    parity_quality$estimator
  ),
  
]


rownames(parity_quality) <- NULL



cat(
  "\n\n========================================\n"
)

cat(
  "RAW PARITY ESTIMATOR QUALITY\n"
)

cat(
  "========================================\n\n"
)


print(
  parity_quality
)



# ============================================================
# 7. TEST THE IMPORTANT 4.5 THRESHOLD
# ============================================================
#
# Version 5C suggested:
#
# Below ~4.5:
#   stronger shrinkage worked better
#
# Above ~4.5:
#   lambda = 2 worked better
#
#
# Can our observed rating SD correctly determine
# which side of 4.5 the league is on?
# ============================================================

parity_threshold <- 4.5



parity_results$true_high_spread <-
  
  parity_results$true_rating_SD >
  parity_threshold


parity_results$estimated_high_spread <-
  
  parity_results$estimated_rating_SD >
  parity_threshold


parity_results$correct_threshold_side <-
  
  parity_results$true_high_spread ==
  
  parity_results$estimated_high_spread



threshold_groups <- split(
  
  parity_results,
  
  list(
    parity_results$week,
    parity_results$estimator
  ),
  
  drop = TRUE
  
)



threshold_list <- lapply(
  
  threshold_groups,
  
  function(x) {
    
    
    data.frame(
      
      week =
        unique(x$week),
      
      estimator =
        unique(x$estimator),
      
      threshold_accuracy =
        mean(
          x$correct_threshold_side
        )
      
    )
    
  }
  
)



threshold_results <- do.call(
  
  rbind,
  
  threshold_list
  
)


rownames(threshold_results) <- NULL


threshold_results <- threshold_results[
  
  order(
    threshold_results$week,
    threshold_results$estimator
  ),
  
]



cat(
  "\n========================================\n"
)

cat(
  "RAW 4.5 THRESHOLD CLASSIFICATION\n"
)

cat(
  "========================================\n\n"
)


print(
  threshold_results
)



# ============================================================
# 8. CALIBRATION TEST
# ============================================================
#
# Very important:
#
# The estimated rating SD may be biased.
#
# Example:
#
# True SD = 6
# Observed rating SD = 4.8
#
# That does NOT mean the estimator is useless.
#
# If the relationship is predictable, we can calibrate it.
#
#
# Split simulations:
#
# 1-700:
#   calibration training set
#
# 701-1000:
#   validation set
#
# ============================================================

calibration_train <- parity_results[
  
  parity_results$simulation <= 700,
  
]


calibration_test <- parity_results[
  
  parity_results$simulation > 700,
  
]



# Create output column

calibration_test$calibrated_SD <-
  NA_real_



# ============================================================
# 9. FIT SEPARATE CALIBRATION MODEL FOR EACH WEEK
# ============================================================

for (
  current_week in parity_weeks
) {
  
  
  for (
    current_estimator in parity_cap_labels
  ) {
    
    
    train_subset <- calibration_train[
      
      calibration_train$week ==
        current_week &
        
        calibration_train$estimator ==
        current_estimator,
      
    ]
    
    
    test_subset_index <- which(
      
      calibration_test$week ==
        current_week &
        
        calibration_test$estimator ==
        current_estimator
      
    )
    
    
    
    test_subset <-
      
      calibration_test[
        test_subset_index,
      ]
    
    
    
    # --------------------------------------------------------
    # SIMPLE LINEAR CALIBRATION
    # --------------------------------------------------------
    #
    # True SD =
    #
    #     intercept
    #     + slope * observed rating SD
    #
    # --------------------------------------------------------
    
    calibration_model <- lm(
      
      true_rating_SD ~
        
        estimated_rating_SD,
      
      data =
        train_subset
      
    )
    
    
    
    predicted_true_sd <- predict(
      
      calibration_model,
      
      newdata =
        test_subset
      
    )
    
    
    
    calibration_test$calibrated_SD[
      test_subset_index
    ] <-
      
      predicted_true_sd
    
  }
  
}



# ============================================================
# 10. CALIBRATED ERRORS
# ============================================================

calibration_test$calibrated_error <-
  
  calibration_test$calibrated_SD -
  
  calibration_test$true_rating_SD


calibration_test$calibrated_absolute_error <-
  
  abs(
    calibration_test$calibrated_error
  )


calibration_test$calibrated_squared_error <-
  
  calibration_test$calibrated_error^2



# ============================================================
# 11. CALIBRATED THRESHOLD CLASSIFICATION
# ============================================================

calibration_test$calibrated_high_spread <-
  
  calibration_test$calibrated_SD >
  parity_threshold


calibration_test$calibrated_correct_side <-
  
  calibration_test$calibrated_high_spread ==
  
  calibration_test$true_high_spread



# ============================================================
# 12. SUMMARIZE CALIBRATED PERFORMANCE
# ============================================================

calibrated_groups <- split(
  
  calibration_test,
  
  list(
    calibration_test$week,
    calibration_test$estimator
  ),
  
  drop = TRUE
  
)



calibrated_list <- lapply(
  
  calibrated_groups,
  
  function(x) {
    
    
    data.frame(
      
      week =
        unique(x$week),
      
      estimator =
        unique(x$estimator),
      
      calibrated_MAE =
        mean(
          x$calibrated_absolute_error
        ),
      
      calibrated_RMSE =
        sqrt(
          mean(
            x$calibrated_squared_error
          )
        ),
      
      calibrated_bias =
        mean(
          x$calibrated_error
        ),
      
      calibrated_correlation =
        cor(
          x$calibrated_SD,
          x$true_rating_SD
        ),
      
      threshold_accuracy =
        mean(
          x$calibrated_correct_side
        )
      
    )
    
  }
  
)



calibrated_quality <- do.call(
  
  rbind,
  
  calibrated_list
  
)


rownames(calibrated_quality) <- NULL


calibrated_quality <- calibrated_quality[
  
  order(
    calibrated_quality$week,
    calibrated_quality$estimator
  ),
  
]


rownames(calibrated_quality) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "CALIBRATED PARITY ESTIMATOR QUALITY\n"
)

cat(
  "========================================\n\n"
)


print(
  calibrated_quality
)



# ============================================================
# 13. FIND BEST ESTIMATOR BY WEEK
# ============================================================

best_estimator_list <- lapply(
  
  split(
    calibrated_quality,
    calibrated_quality$week
  ),
  
  function(x) {
    
    x[
      which.min(
        x$calibrated_MAE
      ),
    ]
    
  }
  
)



best_estimator_by_week <- do.call(
  
  rbind,
  
  best_estimator_list
  
)


rownames(best_estimator_by_week) <- NULL



cat(
  "\n========================================\n"
)

cat(
  "BEST PARITY ESTIMATOR BY WEEK\n"
)

cat(
  "========================================\n\n"
)


print(
  best_estimator_by_week
)



# ============================================================
# 14. PLOT TRUE VS OBSERVED SD
# ============================================================
#
# Example using Week 6.
# ============================================================

week_to_plot <- 6



parity_scatter <- ggplot(
  
  parity_results[
    parity_results$week ==
      week_to_plot,
  ],
  
  aes(
    
    x =
      true_rating_SD,
    
    y =
      estimated_rating_SD,
    
    shape =
      estimator
    
  )
  
) +
  
  geom_point(
    alpha = 0.15
  ) +
  
  geom_abline(
    
    intercept = 0,
    
    slope = 1,
    
    linetype = "dashed"
    
  ) +
  
  geom_smooth(
    
    method = "lm",
    
    se = FALSE
    
  ) +
  
  labs(
    
    title =
      paste(
        "Can We Estimate League Parity After Week",
        week_to_plot,
        "?"
      ),
    
    subtitle =
      "Dashed line represents perfect estimation",
    
    x =
      "Hidden True Rating SD",
    
    y =
      "Observed Rating SD",
    
    shape =
      "Estimator"
    
  ) +
  
  theme_minimal()



print(
  parity_scatter
)



# ============================================================
# 15. PLOT CALIBRATED MAE OVER TIME
# ============================================================

parity_mae_plot <- ggplot(
  
  calibrated_quality,
  
  aes(
    
    x =
      week,
    
    y =
      calibrated_MAE,
    
    group =
      estimator,
    
    linetype =
      estimator
    
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  scale_x_continuous(
    breaks =
      parity_weeks
  ) +
  
  labs(
    
    title =
      "Accuracy of League-Parity Estimation",
    
    subtitle =
      "Lower MAE is better",
    
    x =
      "Games Through Week",
    
    y =
      "MAE of Estimated True Rating SD",
    
    linetype =
      "Parity Estimator"
    
  ) +
  
  theme_minimal()



print(
  parity_mae_plot
)



# ============================================================
# 16. PLOT THRESHOLD ACCURACY OVER TIME
# ============================================================

threshold_plot <- ggplot(
  
  calibrated_quality,
  
  aes(
    
    x =
      week,
    
    y =
      threshold_accuracy * 100,
    
    group =
      estimator,
    
    linetype =
      estimator
    
  )
  
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  scale_x_continuous(
    breaks =
      parity_weeks
  ) +
  
  scale_y_continuous(
    limits = c(
      0,
      100
    )
  ) +
  
  labs(
    
    title =
      "Can the Model Identify High vs Low Parity?",
    
    subtitle =
      "Correct side of the 4.5 rating-SD threshold",
    
    x =
      "Games Through Week",
    
    y =
      "Correct Classification (%)",
    
    linetype =
      "Parity Estimator"
    
  ) +
  
  theme_minimal()



print(
  threshold_plot
)



# ============================================================
# 17. SAVE RESULTS
# ============================================================

write.csv(
  
  parity_results,
  
  "version6A_raw_parity_estimates.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  parity_summary,
  
  "version6A_parity_summary.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  parity_quality,
  
  "version6A_raw_parity_quality.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  calibration_test,
  
  "version6A_calibration_validation.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  calibrated_quality,
  
  "version6A_calibrated_parity_quality.csv",
  
  row.names = FALSE
  
)


write.csv(
  
  best_estimator_by_week,
  
  "version6A_best_estimator_by_week.csv",
  
  row.names = FALSE
  
)



# ============================================================
# 18. FINAL SUMMARY
# ============================================================

cat(
  "\n\n========================================\n"
)

cat(
  "VERSION 6A COMPLETE\n"
)

cat(
  "========================================\n\n"
)


cat(
  "True SD values tested:",
  paste(
    parity_true_sd_values,
    collapse = ", "
  ),
  "\n"
)


cat(
  "Simulations per SD:",
  n_parity_simulations,
  "\n"
)


cat(
  "Parity estimators:",
  paste(
    parity_cap_labels,
    collapse = ", "
  ),
  "\n"
)


cat(
  "Threshold tested:",
  parity_threshold,
  "\n\n"
)


cat(
  "BEST CALIBRATED ESTIMATOR BY WEEK:\n\n"
)


print(
  best_estimator_by_week
)


# ============================================================
# END VERSION 6A
# ============================================================
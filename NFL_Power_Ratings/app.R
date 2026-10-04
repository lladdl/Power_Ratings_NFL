# ============================================================
# NFL POWER RATINGS SHINY APP
# Front end for Football Power Ratings Version 12.1
# ============================================================

required_packages <- c("shiny", "ggplot2", "DT", "scales")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the following packages before running the app: ",
      paste(missing_packages, collapse = ", "),
      "\n\nRun:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    )
  )
}

library(shiny)
library(ggplot2)
library(DT)
library(scales)

ENGINE_FILE <- "PR_version12.1.R"

if (!file.exists(ENGINE_FILE)) {
  stop(
    paste0(
      "Could not find ", ENGINE_FILE,
      ". Keep app.R and nfl_power_ratings_v12_1.R in the same folder."
    )
  )
}

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "\u2014", formatC(x, digits = digits, format = "f"))
}

fmt_signed <- function(x, digits = 2) {
  ifelse(
    is.na(x),
    "\u2014",
    ifelse(
      x > 0,
      paste0("+", formatC(x, digits = digits, format = "f")),
      formatC(x, digits = digits, format = "f")
    )
  )
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "\u2014", paste0(formatC(100 * x, digits = digits, format = "f"), "%"))
}

safe_columns <- function(data, columns) {
  columns[columns %in% names(data)]
}

round_numeric_columns <- function(data, digits = 2) {
  out <- data
  is_num <- vapply(out, is.numeric, logical(1))
  out[is_num] <- lapply(out[is_num], function(x) round(x, digits))
  out
}

metric_card <- function(title, value, subtitle = NULL) {
  div(
    class = "metric-card",
    div(class = "metric-title", title),
    div(class = "metric-value", value),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}

empty_plot <- function(message = "No data available for this selection.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message, size = 5) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void()
}

# Source the production script into a private environment.
# If sourcing fails, the app keeps the previous successful model state.
load_model_environment <- function() {
  env <- new.env(parent = globalenv())
  sys.source(ENGINE_FILE, envir = env)
  env
}

# Run once when the app starts.
initial_model_env <- load_model_environment()


# ============================================================
# UI
# ============================================================

ui <- navbarPage(
  title = "NFL Power Ratings",
  id = "main_nav",
  collapsible = TRUE,
  
  header = tagList(
    tags$style(HTML("
      body {
        background: #f6f7f9;
      }

      .navbar {
        margin-bottom: 0;
      }

      .app-header {
        padding: 18px 20px 10px 20px;
        background: white;
        border-bottom: 1px solid #e5e7eb;
        margin-bottom: 16px;
      }

      .app-header-row {
        display: flex;
        gap: 14px;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
      }

      .status-text {
        color: #5f6b7a;
        font-size: 13px;
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(155px, 1fr));
        gap: 12px;
        margin-bottom: 16px;
      }

      .metric-card {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        padding: 14px 16px;
        min-height: 92px;
      }

      .metric-title {
        color: #667085;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        margin-bottom: 5px;
      }

      .metric-value {
        color: #111827;
        font-size: 25px;
        font-weight: 700;
        line-height: 1.15;
      }

      .metric-subtitle {
        color: #667085;
        font-size: 12px;
        margin-top: 5px;
      }

      .panel-card {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        padding: 16px;
        margin-bottom: 16px;
      }

      .section-title {
        font-size: 18px;
        font-weight: 700;
        margin-bottom: 12px;
      }

      .control-row {
        background: white;
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        padding: 12px 14px 2px 14px;
        margin-bottom: 16px;
      }

      .shiny-output-error-validation {
        color: #8a3b12;
      }

      @media (max-width: 767px) {
        .app-header {
          padding-left: 10px;
          padding-right: 10px;
        }
      }
    ")),
    div(
      class = "app-header",
      div(
        class = "app-header-row",
        div(
          h3(style = "margin:0;", "Version 12.1 NFL Dashboard"),
          div(
            class = "status-text",
            textOutput("model_status", inline = TRUE)
          )
        ),
        actionButton(
          "refresh_model",
          "Update ratings from nflreadr",
          class = "btn-primary"
        )
      )
    )
  ),
  
  tabPanel(
    "Rankings",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(
                4,
                selectInput(
                  "ranking_metric",
                  "Ranking metric",
                  choices = c(
                    "Overall rating" = "rating",
                    "Offense rating" = "offense_rating",
                    "Defense rating" = "defense_rating"
                  ),
                  selected = "rating"
                )
              ),
              column(
                4,
                selectInput(
                  "ranking_count",
                  "Teams to display",
                  choices = c("Top 10" = 10, "Top 16" = 16, "All 32" = 32),
                  selected = 32
                )
              )
            )
          )
        )
      ),
      uiOutput("ranking_cards"),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Power ranking chart"),
            plotOutput("ranking_plot", height = "720px")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Current ranking table"),
            DTOutput("ranking_table")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Offense vs defense"),
            plotOutput("off_def_plot", height = "520px")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Team Explorer",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(
                4,
                selectInput("team_explorer", "Team", choices = NULL)
              ),
              column(
                4,
                selectInput(
                  "team_metric",
                  "History metric",
                  choices = c(
                    "Overall rating" = "rating",
                    "Offense rating" = "offense_rating",
                    "Defense rating" = "defense_rating",
                    "National rank" = "rank"
                  ),
                  selected = "rating"
                )
              )
            )
          )
        )
      ),
      uiOutput("team_cards"),
      fluidRow(
        column(
          7,
          div(
            class = "panel-card",
            div(class = "section-title", "Weekly trajectory"),
            plotOutput("team_history_plot", height = "470px")
          )
        ),
        column(
          5,
          div(
            class = "panel-card",
            div(class = "section-title", "Schedule and results"),
            DTOutput("team_games_table")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Compare Teams",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(
                8,
                selectizeInput(
                  "compare_teams",
                  "Teams",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(maxItems = 8)
                )
              ),
              column(
                4,
                selectInput(
                  "compare_metric",
                  "Metric",
                  choices = c(
                    "Overall rating" = "rating",
                    "Offense rating" = "offense_rating",
                    "Defense rating" = "defense_rating",
                    "National rank" = "rank"
                  ),
                  selected = "rating"
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Team trajectories"),
            plotOutput("compare_plot", height = "600px")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Matchup Predictor",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(4, selectInput("team_a", "Team A", choices = NULL)),
              column(4, selectInput("team_b", "Team B", choices = NULL)),
              column(
                4,
                selectInput(
                  "matchup_site",
                  "Game site",
                  choices = c(
                    "Team A home" = "a_home",
                    "Neutral field" = "neutral",
                    "Team B home" = "b_home"
                  ),
                  selected = "a_home"
                )
              )
            )
          )
        )
      ),
      uiOutput("matchup_cards"),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Forecast"),
            uiOutput("matchup_summary")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "What the model is using"),
            uiOutput("matchup_details")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Forecasts",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(
                4,
                selectInput("forecast_week", "Week", choices = NULL)
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Win probabilities"),
            plotOutput("forecast_probability_plot", height = "560px")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Game forecasts"),
            DTOutput("forecast_table")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Weekly Movers",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(4, selectInput("movers_week", "Week", choices = NULL)),
              column(
                4,
                radioButtons(
                  "movers_metric",
                  "Movement",
                  choices = c(
                    "Rating change" = "rating_change",
                    "Rank change" = "rank_change"
                  ),
                  selected = "rating_change",
                  inline = TRUE
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          7,
          div(
            class = "panel-card",
            div(class = "section-title", "Biggest weekly changes"),
            plotOutput("movers_plot", height = "620px")
          )
        ),
        column(
          5,
          div(
            class = "panel-card",
            div(class = "section-title", "Movers table"),
            DTOutput("movers_table")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Strength of Schedule",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(
                6,
                selectInput(
                  "sos_metric",
                  "SOS measure",
                  choices = c(
                    "Played SOS" = "played_SOS",
                    "Remaining SOS" = "remaining_SOS",
                    "Full SOS" = "full_SOS",
                    "Played location difficulty" = "played_location_difficulty",
                    "Remaining location difficulty" = "remaining_location_difficulty",
                    "Full location difficulty" = "full_location_difficulty"
                  ),
                  selected = "remaining_SOS"
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          7,
          div(
            class = "panel-card",
            div(class = "section-title", "League SOS comparison"),
            plotOutput("sos_plot", height = "720px")
          )
        ),
        column(
          5,
          div(
            class = "panel-card",
            div(class = "section-title", "SOS table"),
            DTOutput("sos_table")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Model Diagnostics",
    fluidPage(
      uiOutput("diagnostic_cards"),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Forecast margin vs actual margin"),
            plotOutput("oos_scatter", height = "480px")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Weekly out-of-sample MAE"),
            plotOutput("oos_weekly_mae", height = "480px")
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Current-season OOS summary"),
            DTOutput("oos_summary_table")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Model snapshot"),
            DTOutput("model_snapshot_table")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Historical Seasons",
    fluidPage(
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(4, selectInput("history_season", "Season", choices = NULL)),
              column(
                4,
                selectInput(
                  "historical_metric",
                  "Metric",
                  choices = c(
                    "Overall rating" = "rating",
                    "Offense rating" = "offense_rating",
                    "Defense rating" = "defense_rating",
                    "Point differential/game" = "point_differential_per_game"
                  ),
                  selected = "rating"
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Season snapshot"),
            plotOutput("historical_plot", height = "720px")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Season table"),
            DTOutput("historical_table")
          )
        )
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  model_env <- reactiveVal(initial_model_env)
  refresh_time <- reactiveVal(Sys.time())
  refresh_message <- reactiveVal("Model loaded successfully.")
  
  # ----------------------------------------------------------
  # Refresh / update
  # ----------------------------------------------------------
  
  observeEvent(input$refresh_model, {
    refresh_message("Updating ratings from nflreadr...")
    
    tryCatch(
      {
        new_env <- withProgress(
          message = "Updating NFL ratings",
          detail = "Reloading schedules and rerunning Version 12.1...",
          value = 0.2,
          {
            env <- load_model_environment()
            incProgress(0.8, detail = "Refreshing dashboard objects...")
            env
          }
        )
        
        model_env(new_env)
        refresh_time(Sys.time())
        refresh_message("Update completed successfully.")
      },
      error = function(err) {
        refresh_message(
          paste0(
            "Update failed; the previous successful model is still displayed. ",
            conditionMessage(err)
          )
        )
        showNotification(
          paste("Model update failed:", conditionMessage(err)),
          type = "error",
          duration = 10
        )
      }
    )
  })
  
  output$model_status <- renderText({
    e <- model_env()
    
    paste0(
      "Season ", e$production_season,
      " \u2022 through Week ", e$publication_week,
      " \u2022 last app refresh ",
      format(refresh_time(), "%Y-%m-%d %H:%M:%S"),
      " \u2022 ", refresh_message()
    )
  })
  
  # ----------------------------------------------------------
  # Refresh input choices whenever a new model is loaded
  # ----------------------------------------------------------
  
  observeEvent(model_env(), {
    e <- model_env()
    teams <- e$production_teams
    
    current_top <- e$current_power_rankings$team[
      order(e$current_power_rankings$rank)
    ]
    
    updateSelectInput(
      session,
      "team_explorer",
      choices = teams,
      selected = current_top[1]
    )
    
    updateSelectizeInput(
      session,
      "compare_teams",
      choices = teams,
      selected = head(current_top, 4),
      server = TRUE
    )
    
    updateSelectInput(
      session,
      "team_a",
      choices = teams,
      selected = current_top[1]
    )
    
    updateSelectInput(
      session,
      "team_b",
      choices = teams,
      selected = current_top[min(2, length(current_top))]
    )
    
    forecast_weeks <- sort(unique(e$all_remaining_predictions$week))
    forecast_weeks <- forecast_weeks[!is.na(forecast_weeks)]
    
    updateSelectInput(
      session,
      "forecast_week",
      choices = forecast_weeks,
      selected = if (length(forecast_weeks)) forecast_weeks[1] else NULL
    )
    
    mover_weeks <- sort(unique(e$current_weekly_power_history$week))
    
    updateSelectInput(
      session,
      "movers_week",
      choices = mover_weeks,
      selected = max(mover_weeks)
    )
    
    historical_seasons <- sort(
      unique(e$all_season_team_summaries$season),
      decreasing = TRUE
    )
    
    updateSelectInput(
      session,
      "history_season",
      choices = historical_seasons,
      selected = e$production_season
    )
  }, ignoreInit = FALSE)
  
  # ----------------------------------------------------------
  # Rankings
  # ----------------------------------------------------------
  
  ranking_data <- reactive({
    e <- model_env()
    data <- e$current_power_rankings
    metric <- input$ranking_metric
    
    req(metric %in% names(data))
    
    rank_column <- switch(
      metric,
      rating = "rank",
      offense_rating = "offense_rank",
      defense_rating = "defense_rank"
    )
    
    data$selected_rank <- data[[rank_column]]
    data$selected_metric <- data[[metric]]
    data <- data[order(data$selected_rank, data$team), ]
    
    count <- as.integer(input$ranking_count)
    if (is.na(count)) count <- nrow(data)
    
    head(data, min(count, nrow(data)))
  })
  
  output$ranking_cards <- renderUI({
    e <- model_env()
    top <- e$current_power_rankings[
      order(e$current_power_rankings$rank),
    ][1, ]
    
    div(
      class = "metric-grid",
      metric_card("No. 1 team", top$team, paste("Rating", fmt_signed(top$rating))),
      metric_card("Estimated HFA", fmt_signed(e$current_model$hfa, 2), "Points"),
      metric_card(
        "League scoring level",
        fmt_num(e$current_model$league_mean_points, 2),
        "Expected points/team"
      ),
      metric_card(
        "Predictive margin SD",
        fmt_num(e$current_predictive_margin_sd, 2),
        "Forecast uncertainty"
      )
    )
  })
  
  output$ranking_plot <- renderPlot({
    e <- model_env()
    data <- ranking_data()
    
    metric <- input$ranking_metric
    metric_label <- switch(
      metric,
      rating = "Neutral-field power rating",
      offense_rating = "Offense rating",
      defense_rating = "Defense rating"
    )
    
    data$team_factor <- factor(
      data$team,
      levels = rev(data$team[order(data$selected_rank)])
    )
    
    p <- ggplot(
      data,
      aes(x = team_factor, y = selected_metric, fill = team)
    ) +
      geom_col(width = 0.76) +
      coord_flip() +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
      labs(
        x = NULL,
        y = metric_label,
        title = paste0(
          e$production_season,
          " NFL ",
          metric_label,
          " \u2014 through Week ",
          e$publication_week
        )
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank()
      )
    
    if (exists("team_colors", envir = e, inherits = FALSE)) {
      p <- p + scale_fill_manual(values = e$team_colors)
    }
    
    p
  })
  
  output$ranking_table <- renderDT({
    e <- model_env()
    data <- e$current_power_rankings
    
    columns <- safe_columns(
      data,
      c(
        "rank", "team", "rating",
        "offense_rank", "offense_rating",
        "defense_rank", "defense_rating",
        "record",
        "rank_change", "rating_change",
        "played_SOS_rank", "remaining_SOS_rank", "full_SOS_rank"
      )
    )
    
    data <- round_numeric_columns(data[, columns, drop = FALSE], 2)
    
    datatable(
      data,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 32,
        scrollX = TRUE,
        dom = "tip"
      )
    )
  })
  
  output$off_def_plot <- renderPlot({
    e <- model_env()
    data <- e$current_power_rankings
    
    ggplot(
      data,
      aes(
        x = offense_rating,
        y = defense_rating,
        label = team
      )
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_point(size = 3) +
      geom_text(nudge_y = 0.18, size = 3.4, check_overlap = TRUE) +
      labs(
        x = "Offense rating",
        y = "Defense rating",
        title = "Team profile: offense vs defense",
        subtitle = "Higher is better on both axes in the Version 12.1 component ratings"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # ----------------------------------------------------------
  # Team Explorer
  # ----------------------------------------------------------
  
  selected_team_current <- reactive({
    e <- model_env()
    req(input$team_explorer)
    
    data <- e$current_power_rankings
    row <- data[data$team == input$team_explorer, , drop = FALSE]
    
    validate(need(nrow(row) == 1, "Team not found in current rankings."))
    row
  })
  
  output$team_cards <- renderUI({
    row <- selected_team_current()
    
    div(
      class = "metric-grid",
      metric_card("Power rank", paste0("#", row$rank), paste("Rating", fmt_signed(row$rating))),
      metric_card("Record", row$record),
      metric_card(
        "Offense",
        paste0("#", row$offense_rank),
        paste("Rating", fmt_signed(row$offense_rating))
      ),
      metric_card(
        "Defense",
        paste0("#", row$defense_rank),
        paste("Rating", fmt_signed(row$defense_rating))
      ),
      metric_card(
        "Weekly rating change",
        fmt_signed(row$rating_change),
        ifelse(
          is.na(row$rank_change),
          NULL,
          paste("Rank change", fmt_signed(row$rank_change, 0))
        )
      ),
      metric_card(
        "Remaining SOS",
        ifelse(is.na(row$remaining_SOS_rank), "\u2014", paste0("#", row$remaining_SOS_rank)),
        paste("SOS", fmt_signed(row$remaining_SOS))
      )
    )
  })
  
  output$team_history_plot <- renderPlot({
    e <- model_env()
    req(input$team_explorer, input$team_metric)
    
    data <- e$current_weekly_power_history
    data <- data[data$team == input$team_explorer, , drop = FALSE]
    
    metric <- input$team_metric
    req(metric %in% names(data))
    
    data$value <- data[[metric]]
    
    y_label <- switch(
      metric,
      rating = "Power rating",
      offense_rating = "Offense rating",
      defense_rating = "Defense rating",
      rank = "National rank"
    )
    
    p <- ggplot(data, aes(x = week, y = value)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.8) +
      scale_x_continuous(breaks = sort(unique(data$week))) +
      labs(
        title = paste(input$team_explorer, y_label, "by week"),
        x = "Week",
        y = y_label
      ) +
      theme_minimal(base_size = 12)
    
    if (metric == "rank") {
      p <- p + scale_y_reverse(breaks = pretty(data$value))
    } else {
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
    }
    
    p
  })
  
  output$team_games_table <- renderDT({
    e <- model_env()
    req(input$team_explorer)
    
    data <- e$production_schedule[
      e$production_schedule$home_team == input$team_explorer |
        e$production_schedule$away_team == input$team_explorer,
      ,
      drop = FALSE
    ]
    
    data$opponent <- ifelse(
      data$home_team == input$team_explorer,
      data$away_team,
      data$home_team
    )
    
    data$site <- ifelse(
      data$neutral_site,
      "Neutral",
      ifelse(data$home_team == input$team_explorer, "Home", "Away")
    )
    
    data$result <- ifelse(
      !data$completed,
      "Upcoming",
      ifelse(
        data$home_score == data$away_score,
        "T",
        ifelse(
          (data$home_team == input$team_explorer & data$home_score > data$away_score) |
            (data$away_team == input$team_explorer & data$away_score > data$home_score),
          "W",
          "L"
        )
      )
    )
    
    data$team_score <- ifelse(
      data$completed,
      ifelse(
        data$home_team == input$team_explorer,
        data$home_score,
        data$away_score
      ),
      NA
    )
    
    data$opponent_score <- ifelse(
      data$completed,
      ifelse(
        data$home_team == input$team_explorer,
        data$away_score,
        data$home_score
      ),
      NA
    )
    
    columns <- safe_columns(
      data,
      c("week", "opponent", "site", "result", "team_score", "opponent_score")
    )
    
    datatable(
      data[, columns, drop = FALSE],
      rownames = FALSE,
      options = list(
        pageLength = 18,
        scrollX = TRUE,
        dom = "tip"
      )
    )
  })
  
  # ----------------------------------------------------------
  # Compare teams
  # ----------------------------------------------------------
  
  output$compare_plot <- renderPlot({
    e <- model_env()
    req(length(input$compare_teams) >= 1, input$compare_metric)
    
    data <- e$current_weekly_power_history[
      e$current_weekly_power_history$team %in% input$compare_teams,
      ,
      drop = FALSE
    ]
    
    metric <- input$compare_metric
    req(metric %in% names(data))
    
    data$value <- data[[metric]]
    
    y_label <- switch(
      metric,
      rating = "Power rating",
      offense_rating = "Offense rating",
      defense_rating = "Defense rating",
      rank = "National rank"
    )
    
    p <- ggplot(
      data,
      aes(
        x = week,
        y = value,
        group = team,
        color = team
      )
    ) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.3) +
      scale_x_continuous(breaks = sort(unique(data$week))) +
      labs(
        title = paste(y_label, "comparison"),
        x = "Week",
        y = y_label,
        color = "Team"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    
    if (metric == "rank") {
      p <- p + scale_y_reverse()
    } else {
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
    }
    
    if (exists("team_colors", envir = e, inherits = FALSE)) {
      available <- e$team_colors[names(e$team_colors) %in% input$compare_teams]
      if (length(available) > 0) {
        p <- p + scale_color_manual(values = available)
      }
    }
    
    p
  })
  
  # ----------------------------------------------------------
  # Matchup predictor
  # ----------------------------------------------------------
  
  matchup_prediction <- reactive({
    e <- model_env()
    req(input$team_a, input$team_b, input$matchup_site)
    
    validate(
      need(input$team_a != input$team_b, "Choose two different teams.")
    )
    
    if (input$matchup_site == "a_home") {
      home_team <- input$team_a
      away_team <- input$team_b
      neutral <- FALSE
    } else if (input$matchup_site == "b_home") {
      home_team <- input$team_b
      away_team <- input$team_a
      neutral <- FALSE
    } else {
      # Team A is stored in the home column only for bookkeeping;
      # neutral_site = TRUE removes home-field advantage.
      home_team <- input$team_a
      away_team <- input$team_b
      neutral <- TRUE
    }
    
    custom_game <- data.frame(
      game_id = "CUSTOM_MATCHUP",
      season = e$production_season,
      game_type = "REG",
      week = e$publication_week + 1L,
      home_team = home_team,
      away_team = away_team,
      home_score = NA_real_,
      away_score = NA_real_,
      neutral_site = neutral,
      stringsAsFactors = FALSE
    )
    
    raw <- e$predict_games_from_model(
      model = e$current_model,
      target_schedule = custom_game
    )
    
    forecast <- e$apply_version12_forecast_layer(
      raw_predictions = raw,
      training_games = e$completed_production_games,
      league_teams = e$production_teams
    )
    
    row <- forecast[1, ]
    
    if (input$team_a == home_team) {
      team_a_score <- row$forecast_home_score
      team_b_score <- row$forecast_away_score
      team_a_prob <- row$home_win_probability
      team_b_prob <- row$away_win_probability
      raw_a_margin <- row$raw_rating_margin
      forecast_a_margin <- row$forecast_margin
    } else {
      team_a_score <- row$forecast_away_score
      team_b_score <- row$forecast_home_score
      team_a_prob <- row$away_win_probability
      team_b_prob <- row$home_win_probability
      raw_a_margin <- -row$raw_rating_margin
      forecast_a_margin <- -row$forecast_margin
    }
    
    if (forecast_a_margin > 0) {
      favorite <- input$team_a
      underdog <- input$team_b
      spread <- abs(forecast_a_margin)
    } else if (forecast_a_margin < 0) {
      favorite <- input$team_b
      underdog <- input$team_a
      spread <- abs(forecast_a_margin)
    } else {
      favorite <- "Pick'em"
      underdog <- ""
      spread <- 0
    }
    
    list(
      row = row,
      team_a_score = team_a_score,
      team_b_score = team_b_score,
      team_a_prob = team_a_prob,
      team_b_prob = team_b_prob,
      raw_a_margin = raw_a_margin,
      forecast_a_margin = forecast_a_margin,
      favorite = favorite,
      underdog = underdog,
      spread = spread
    )
  })
  
  output$matchup_cards <- renderUI({
    pred <- matchup_prediction()
    
    spread_text <- if (pred$favorite == "Pick'em") {
      "Pick'em"
    } else {
      paste0(pred$favorite, " -", fmt_num(pred$spread, 1))
    }
    
    div(
      class = "metric-grid",
      metric_card("Projected spread", spread_text),
      metric_card(
        paste(input$team_a, "win probability"),
        fmt_pct(pred$team_a_prob)
      ),
      metric_card(
        paste(input$team_b, "win probability"),
        fmt_pct(pred$team_b_prob)
      ),
      metric_card(
        "Projected score",
        paste0(
          input$team_a, " ", fmt_num(pred$team_a_score, 1),
          " \u2013 ",
          input$team_b, " ", fmt_num(pred$team_b_score, 1)
        )
      )
    )
  })
  
  output$matchup_summary <- renderUI({
    pred <- matchup_prediction()
    
    favorite_text <- if (pred$favorite == "Pick'em") {
      "The model sees this matchup as essentially even."
    } else {
      paste0(
        strong(pred$favorite),
        " is favored by ",
        strong(fmt_num(pred$spread, 1)),
        " points in the Version 12.1 point forecast."
      )
    }
    
    tagList(
      p(favorite_text),
      p(
        "Projected score: ",
        strong(paste0(input$team_a, " ", fmt_num(pred$team_a_score, 1))),
        " to ",
        strong(paste0(input$team_b, " ", fmt_num(pred$team_b_score, 1))),
        "."
      ),
      p(
        "Win probabilities: ",
        strong(paste0(input$team_a, " ", fmt_pct(pred$team_a_prob))),
        " / ",
        strong(paste0(input$team_b, " ", fmt_pct(pred$team_b_prob))),
        "."
      )
    )
  })
  
  output$matchup_details <- renderUI({
    pred <- matchup_prediction()
    row <- pred$row
    
    tagList(
      p(
        "Raw rating margin from Team A's perspective: ",
        strong(fmt_signed(pred$raw_a_margin, 2))
      ),
      p(
        "Forecast margin after early-season shrinkage: ",
        strong(fmt_signed(pred$forecast_a_margin, 2))
      ),
      p(
        "Forecast shrinkage weight: ",
        strong(fmt_num(row$learned_forecast_weight, 3))
      ),
      p(
        "Probability blend weight: ",
        strong(fmt_num(row$fixed_probability_weight, 3))
      ),
      p(
        "Predictive margin SD: ",
        strong(fmt_num(row$predictive_margin_sd, 2))
      ),
      p(
        "Estimated HFA in current rating fit: ",
        strong(fmt_signed(model_env()$current_model$hfa, 2))
      )
    )
  })
  
  # ----------------------------------------------------------
  # Forecasts
  # ----------------------------------------------------------
  
  selected_forecasts <- reactive({
    e <- model_env()
    
    if (nrow(e$all_remaining_predictions) == 0) {
      return(data.frame())
    }
    
    req(input$forecast_week)
    
    e$all_remaining_predictions[
      e$all_remaining_predictions$week == as.integer(input$forecast_week),
      ,
      drop = FALSE
    ]
  })
  
  output$forecast_probability_plot <- renderPlot({
    data <- selected_forecasts()
    
    if (nrow(data) == 0) {
      return(empty_plot("No remaining forecasts for this week."))
    }
    
    data$matchup <- paste0(data$away_team, " @ ", data$home_team)
    data$matchup <- factor(
      data$matchup,
      levels = data$matchup[order(data$home_win_probability)]
    )
    
    ggplot(
      data,
      aes(x = matchup, y = home_win_probability)
    ) +
      geom_col() +
      coord_flip() +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      scale_y_continuous(
        limits = c(0, 1),
        labels = percent_format(accuracy = 1)
      ) +
      labs(
        x = NULL,
        y = "Home win probability",
        title = paste("Week", input$forecast_week, "win probabilities")
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$forecast_table <- renderDT({
    data <- selected_forecasts()
    
    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No remaining forecasts."), rownames = FALSE))
    }
    
    columns <- safe_columns(
      data,
      c(
        "week", "away_team", "home_team",
        "forecast_away_score", "forecast_home_score",
        "forecast_margin",
        "home_win_probability", "away_win_probability",
        "predicted_winner", "predicted_winner_probability",
        "raw_rating_margin", "learned_forecast_weight"
      )
    )
    
    out <- data[, columns, drop = FALSE]
    out <- round_numeric_columns(out, 3)
    
    datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE, dom = "tip")
    )
  })
  
  # ----------------------------------------------------------
  # Weekly movers
  # ----------------------------------------------------------
  
  movers_data <- reactive({
    e <- model_env()
    req(input$movers_week, input$movers_metric)
    
    data <- e$current_weekly_power_history[
      e$current_weekly_power_history$week == as.integer(input$movers_week),
      ,
      drop = FALSE
    ]
    
    metric <- input$movers_metric
    data$movement <- data[[metric]]
    data <- data[!is.na(data$movement), , drop = FALSE]
    
    data[order(data$movement), , drop = FALSE]
  })
  
  output$movers_plot <- renderPlot({
    data <- movers_data()
    
    if (nrow(data) == 0) {
      return(empty_plot("No previous-week comparison is available for this week."))
    }
    
    # Show the 8 biggest risers and 8 biggest fallers.
    n_each <- min(8, max(1, floor(nrow(data) / 2)))
    
    selected <- unique(
      rbind(
        head(data, n_each),
        tail(data, n_each)
      )
    )
    
    selected <- selected[order(selected$movement), ]
    selected$team_factor <- factor(selected$team, levels = selected$team)
    
    y_label <- if (input$movers_metric == "rating_change") {
      "Rating change"
    } else {
      "Rank positions gained"
    }
    
    ggplot(
      selected,
      aes(x = team_factor, y = movement)
    ) +
      geom_col() +
      coord_flip() +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      labs(
        x = NULL,
        y = y_label,
        title = paste("Week", input$movers_week, "biggest movers")
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$movers_table <- renderDT({
    data <- movers_data()
    
    if (nrow(data) == 0) {
      return(
        datatable(
          data.frame(Message = "No movement data for this week."),
          rownames = FALSE
        )
      )
    }
    
    data <- data[order(-abs(data$movement)), ]
    
    columns <- safe_columns(
      data,
      c(
        "team", "rank", "previous_rank", "rank_change",
        "rating", "previous_rating", "rating_change", "record"
      )
    )
    
    out <- round_numeric_columns(data[, columns, drop = FALSE], 2)
    
    datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 32, scrollX = TRUE, dom = "tip")
    )
  })
  
  # ----------------------------------------------------------
  # SOS
  # ----------------------------------------------------------
  
  sos_data <- reactive({
    e <- model_env()
    req(input$sos_metric)
    
    data <- e$current_power_rankings
    metric <- input$sos_metric
    req(metric %in% names(data))
    
    rank_column <- paste0(metric, "_rank")
    data$sos_value <- data[[metric]]
    data$sos_rank <- if (rank_column %in% names(data)) data[[rank_column]] else NA_integer_
    
    data <- data[!is.na(data$sos_value), , drop = FALSE]
    data[order(data$sos_value), , drop = FALSE]
  })
  
  output$sos_plot <- renderPlot({
    data <- sos_data()
    
    if (nrow(data) == 0) {
      return(empty_plot())
    }
    
    data$team_factor <- factor(data$team, levels = data$team)
    
    ggplot(data, aes(x = team_factor, y = sos_value)) +
      geom_col() +
      coord_flip() +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
      labs(
        x = NULL,
        y = "Opponent rating / location-adjusted difficulty",
        title = paste("Current", input$sos_metric),
        subtitle = "Higher values indicate a more difficult schedule"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$sos_table <- renderDT({
    data <- sos_data()
    
    out <- data.frame(
      rank = data$sos_rank,
      team = data$team,
      value = round(data$sos_value, 2),
      power_rank = data$rank,
      rating = round(data$rating, 2),
      stringsAsFactors = FALSE
    )
    
    out <- out[order(out$rank), ]
    
    datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 32, dom = "tip")
    )
  })
  
  # ----------------------------------------------------------
  # Diagnostics
  # ----------------------------------------------------------
  
  output$diagnostic_cards <- renderUI({
    e <- model_env()
    
    if (nrow(e$current_season_oos_summary) == 0) {
      return(
        div(
          class = "metric-grid",
          metric_card("OOS diagnostics", "Not available yet")
        )
      )
    }
    
    s <- e$current_season_oos_summary[1, ]
    
    div(
      class = "metric-grid",
      metric_card("OOS games", s$games),
      metric_card("Margin MAE", fmt_num(s$margin_MAE, 2)),
      metric_card("Margin RMSE", fmt_num(s$margin_RMSE, 2)),
      metric_card("Winner accuracy", fmt_pct(s$winner_accuracy)),
      metric_card("Brier score", fmt_num(s$brier_score, 3)),
      metric_card("Log loss", fmt_num(s$log_loss, 3))
    )
  })
  
  output$oos_scatter <- renderPlot({
    e <- model_env()
    data <- e$current_season_oos_predictions
    
    if (nrow(data) == 0) {
      return(empty_plot("No current-season OOS predictions yet."))
    }
    
    ggplot(
      data,
      aes(x = forecast_margin, y = actual_margin)
    ) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point(alpha = 0.75, size = 2.2) +
      labs(
        x = "Forecast margin",
        y = "Actual margin",
        title = "Out-of-sample game forecasts"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$oos_weekly_mae <- renderPlot({
    e <- model_env()
    data <- e$current_season_oos_predictions
    
    if (nrow(data) == 0) {
      return(empty_plot("No current-season OOS predictions yet."))
    }
    
    weekly <- aggregate(
      absolute_margin_error ~ week,
      data = data,
      FUN = mean
    )
    
    ggplot(weekly, aes(x = week, y = absolute_margin_error)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      scale_x_continuous(breaks = weekly$week) +
      labs(
        x = "Week",
        y = "Mean absolute error",
        title = "Walk-forward margin MAE by week"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$oos_summary_table <- renderDT({
    e <- model_env()
    
    out <- round_numeric_columns(e$current_season_oos_summary, 4)
    
    datatable(
      out,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })
  
  output$model_snapshot_table <- renderDT({
    e <- model_env()
    
    snapshot <- e$model_snapshot[1, ]
    
    out <- data.frame(
      parameter = names(snapshot),
      value = vapply(
        snapshot,
        function(x) {
          if (is.numeric(x)) {
            formatC(x, digits = 4, format = "fg")
          } else {
            as.character(x)
          }
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 20, dom = "tip")
    )
  })
  
  # ----------------------------------------------------------
  # Historical seasons
  # ----------------------------------------------------------
  
  historical_data <- reactive({
    e <- model_env()
    req(input$history_season)
    
    e$all_season_team_summaries[
      e$all_season_team_summaries$season == as.integer(input$history_season),
      ,
      drop = FALSE
    ]
  })
  
  output$historical_plot <- renderPlot({
    data <- historical_data()
    req(input$historical_metric)
    
    metric <- input$historical_metric
    req(metric %in% names(data))
    
    data$value <- data[[metric]]
    data <- data[order(data$value), ]
    data$team_factor <- factor(data$team, levels = data$team)
    
    y_label <- switch(
      metric,
      rating = "Power rating",
      offense_rating = "Offense rating",
      defense_rating = "Defense rating",
      point_differential_per_game = "Point differential/game"
    )
    
    ggplot(data, aes(x = team_factor, y = value)) +
      geom_col() +
      coord_flip() +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
      labs(
        x = NULL,
        y = y_label,
        title = paste(input$history_season, "final/current season snapshot")
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$historical_table <- renderDT({
    data <- historical_data()
    
    columns <- safe_columns(
      data,
      c(
        "rank", "team", "rating",
        "offense_rank", "offense_rating",
        "defense_rank", "defense_rating",
        "record",
        "point_differential_per_game",
        "played_SOS_rank", "remaining_SOS_rank", "full_SOS_rank"
      )
    )
    
    out <- round_numeric_columns(data[, columns, drop = FALSE], 2)
    
    datatable(
      out,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 32, scrollX = TRUE, dom = "tip")
    )
  })
}

shinyApp(ui = ui, server = server)

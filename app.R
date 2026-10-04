# ============================================================
# NFL POWER RATINGS SHINY APP
# Front end for NFL Power Ratings + 1970-present history
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


dashboard_plot_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.title = element_text(
        color = "#0B1F33",
        face = "bold",
        size = base_size + 4,
        margin = margin(b = 7)
      ),
      plot.subtitle = element_text(
        color = "#5D6B7C",
        size = base_size,
        margin = margin(b = 10)
      ),
      axis.title = element_text(
        color = "#334155",
        face = "bold"
      ),
      axis.text = element_text(
        color = "#334155"
      ),
      panel.grid.major = element_line(
        color = "#E8EDF3",
        linewidth = 0.45
      ),
      panel.grid.minor = element_blank(),
      legend.title = element_text(
        face = "bold",
        color = "#0B1F33"
      ),
      legend.text = element_text(
        color = "#334155"
      )
    )
}

# Source the production script into a private environment.
#
# A successful live load is cached. If nflreadr/network access fails
# later, the app can fall back to the last known good model snapshot
# instead of failing the entire public application.
MODEL_CACHE_DIR <- file.path(
  getwd(),
  "data",
  "cache"
)

MODEL_ENV_CACHE_FILE <- file.path(
  MODEL_CACHE_DIR,
  "last_good_model_environment.rds"
)

dir.create(
  MODEL_CACHE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

restore_fast_refresh_setting <- function(
    old_value,
    old_was_set
) {
  
  if (
    old_was_set
  ) {
    
    Sys.setenv(
      POWER_RATINGS_FAST_REFRESH =
        old_value
    )
    
  } else {
    
    Sys.unsetenv(
      "POWER_RATINGS_FAST_REFRESH"
    )
  }
}

load_model_environment <- function(
    fast_refresh = FALSE,
    allow_cache = TRUE
) {
  
  old_fast_refresh <- Sys.getenv(
    "POWER_RATINGS_FAST_REFRESH",
    unset = ""
  )
  
  old_fast_refresh_was_set <- nzchar(
    old_fast_refresh
  )
  
  if (
    isTRUE(
      fast_refresh
    )
  ) {
    
    Sys.setenv(
      POWER_RATINGS_FAST_REFRESH = "1"
    )
    
  } else {
    
    Sys.unsetenv(
      "POWER_RATINGS_FAST_REFRESH"
    )
  }
  
  on.exit(
    restore_fast_refresh_setting(
      old_fast_refresh,
      old_fast_refresh_was_set
    ),
    add = TRUE
  )
  
  tryCatch(
    {
      
      env <- new.env(
        parent = globalenv()
      )
      
      sys.source(
        ENGINE_FILE,
        envir = env
      )
      
      attr(
        env,
        "model_load_source"
      ) <- "live"
      
      attr(
        env,
        "model_loaded_at"
      ) <- Sys.time()
      
      try(
        saveRDS(
          env,
          MODEL_ENV_CACHE_FILE
        ),
        silent = TRUE
      )
      
      env
    },
    error = function(err) {
      
      if (
        isTRUE(
          allow_cache
        ) &&
        file.exists(
          MODEL_ENV_CACHE_FILE
        )
      ) {
        
        cached_env <- tryCatch(
          readRDS(
            MODEL_ENV_CACHE_FILE
          ),
          error = function(cache_err) {
            NULL
          }
        )
        
        if (
          !is.null(
            cached_env
          )
        ) {
          
          attr(
            cached_env,
            "model_load_source"
          ) <- "cache"
          
          attr(
            cached_env,
            "model_cache_reason"
          ) <- conditionMessage(
            err
          )
          
          return(
            cached_env
          )
        }
      }
      
      stop(
        err
      )
    }
  )
}

# Run once when the app starts.
initial_model_env <- load_model_environment(
  fast_refresh = FALSE,
  allow_cache = TRUE
)


# ------------------------------------------------------------
# Historical archive
# ------------------------------------------------------------

HISTORICAL_ARCHIVE_FILENAME <-
  "version12_1_1970_2025_all_season_team_summaries_enriched.csv"

historical_archive_candidates <- unique(
  c(
    file.path(
      getwd(),
      HISTORICAL_ARCHIVE_FILENAME
    ),
    file.path(
      getwd(),
      "data",
      HISTORICAL_ARCHIVE_FILENAME
    ),
    file.path(
      path.expand("~/Downloads"),
      "version12_1_1970_2025_historical_analysis",
      HISTORICAL_ARCHIVE_FILENAME
    )
  )
)

load_historical_archive <- function() {
  
  existing <- historical_archive_candidates[
    file.exists(
      historical_archive_candidates
    )
  ]
  
  if (
    length(
      existing
    ) ==
    0
  ) {
    
    return(
      list(
        data = NULL,
        path = NA_character_,
        message = paste0(
          "Historical 1970-2025 enriched archive not found. ",
          "Expected it at ~/Downloads/version12_1_1970_2025_historical_analysis/",
          HISTORICAL_ARCHIVE_FILENAME,
          " or in the app/data folder."
        )
      )
    )
  }
  
  archive_path <- existing[1]
  
  archive <- read.csv(
    archive_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  required <- c(
    "season",
    "team",
    "rank",
    "rating",
    "offense_rating",
    "defense_rating",
    "record",
    "season_rating_z",
    "league_mean_points",
    "estimated_HFA",
    "predictive_margin_sd"
  )
  
  missing <- setdiff(
    required,
    names(
      archive
    )
  )
  
  if (
    length(
      missing
    ) >
    0
  ) {
    
    stop(
      paste0(
        "Historical archive is missing required columns: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  archive$season <- as.integer(
    archive$season
  )
  
  list(
    data = archive,
    path = archive_path,
    message = paste0(
      "Historical archive loaded: ",
      normalizePath(
        archive_path,
        mustWork = FALSE
      )
    )
  )
}

initial_historical_archive <- tryCatch(
  load_historical_archive(),
  error = function(err) {
    list(
      data = NULL,
      path = NA_character_,
      message = paste0(
        "Historical archive could not be loaded: ",
        conditionMessage(err)
      )
    )
  }
)


# ------------------------------------------------------------
# Playoff analysis archive
# ------------------------------------------------------------

PLAYOFF_FILENAMES <- c(
  master = "11_playoff_team_season_master_1970_2025.csv",
  games = "04_playoff_games_with_predictions.csv",
  upsets = "05_playoff_upsets_ranked.csv",
  ratings = "06_pre_vs_post_playoff_ratings.csv",
  trajectories = "07_playoff_rating_trajectories.csv",
  odds = "08_super_bowl_probabilities_by_season.csv",
  champions = "08_super_bowl_champion_preplayoff_odds.csv",
  performance = "10_playoff_overperformance.csv",
  validation = "09_playoff_probability_model_validation.csv"
)

playoff_data_directories <- unique(
  c(
    file.path(
      getwd(),
      "data",
      "playoffs"
    ),
    file.path(
      path.expand("~/Downloads"),
      "nfl_playoff_analysis_1970_2025"
    )
  )
)

read_optional_playoff_csv <- function(
    directory,
    filename
) {
  
  path <- file.path(
    directory,
    filename
  )
  
  if (
    !file.exists(
      path
    )
  ) {
    
    return(
      NULL
    )
  }
  
  data <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  if (
    "season" %in%
    names(
      data
    )
  ) {
    
    data$season <- suppressWarnings(
      as.integer(
        data$season
      )
    )
  }
  
  data
}

load_playoff_archive <- function() {
  
  existing_directories <- playoff_data_directories[
    dir.exists(
      playoff_data_directories
    )
  ]
  
  if (
    length(
      existing_directories
    ) ==
    0
  ) {
    
    return(
      list(
        data = setNames(
          vector(
            "list",
            length(
              PLAYOFF_FILENAMES
            )
          ),
          names(
            PLAYOFF_FILENAMES
          )
        ),
        directory = NA_character_,
        missing = unname(
          PLAYOFF_FILENAMES
        ),
        message = paste0(
          "Playoff analysis files were not found. Run the playoff pipeline first. ",
          "The app looks in app/data/playoffs and ",
          "~/Downloads/nfl_playoff_analysis_1970_2025."
        )
      )
    )
  }
  
  # Prefer a directory containing the master team-season file.
  has_master <- vapply(
    existing_directories,
    function(directory) {
      file.exists(
        file.path(
          directory,
          PLAYOFF_FILENAMES[["master"]]
        )
      )
    },
    logical(1)
  )
  
  if (
    any(
      has_master
    )
  ) {
    
    directory <- existing_directories[
      which(
        has_master
      )[1]
    ]
    
  } else {
    
    directory <- existing_directories[1]
  }
  
  data <- lapply(
    PLAYOFF_FILENAMES,
    function(filename) {
      read_optional_playoff_csv(
        directory,
        filename
      )
    }
  )
  
  missing <- unname(
    PLAYOFF_FILENAMES[
      vapply(
        data,
        is.null,
        logical(1)
      )
    ]
  )
  
  available_count <- sum(
    !vapply(
      data,
      is.null,
      logical(1)
    )
  )
  
  list(
    data = data,
    directory = directory,
    missing = missing,
    message = paste0(
      "Playoff archive loaded from ",
      normalizePath(
        directory,
        mustWork = FALSE
      ),
      " (",
      available_count,
      "/",
      length(
        PLAYOFF_FILENAMES
      ),
      " analysis files available)."
    )
  )
}

initial_playoff_archive <- tryCatch(
  load_playoff_archive(),
  error = function(err) {
    list(
      data = setNames(
        vector(
          "list",
          length(
            PLAYOFF_FILENAMES
          )
        ),
        names(
          PLAYOFF_FILENAMES
        )
      ),
      directory = NA_character_,
      missing = unname(
        PLAYOFF_FILENAMES
      ),
      message = paste0(
        "Playoff archive could not be loaded: ",
        conditionMessage(
          err
        )
      )
    )
  }
)

make_era_label <- function(season) {
  
  ifelse(
    season >= 1970 & season <= 1979,
    "1970s",
    ifelse(
      season >= 1980 & season <= 1989,
      "1980s",
      ifelse(
        season >= 1990 & season <= 1999,
        "1990s",
        ifelse(
          season >= 2000 & season <= 2009,
          "2000s",
          ifelse(
            season >= 2010 & season <= 2019,
            "2010s",
            ifelse(
              season >= 2020 & season <= 2029,
              "2020s",
              NA_character_
            )
          )
        )
      )
    )
  )
}

add_missing_columns <- function(data, target_names) {
  
  missing <- setdiff(
    target_names,
    names(
      data
    )
  )
  
  for (
    column_name in
    missing
  ) {
    
    data[[column_name]] <- NA
  }
  
  data
}


# ------------------------------------------------------------
# Historical game-level data for weekly trajectories
# ------------------------------------------------------------

LEGACY_GAMES_FILENAME <-
  "historical_nfl_1970_1998_games_COMPLETE.csv"

legacy_games_candidates <- unique(
  c(
    file.path(
      getwd(),
      LEGACY_GAMES_FILENAME
    ),
    file.path(
      getwd(),
      "data",
      LEGACY_GAMES_FILENAME
    ),
    file.path(
      path.expand("~/Downloads"),
      "historical_nfl_1970_1998",
      LEGACY_GAMES_FILENAME
    )
  )
)

load_legacy_games <- function() {
  
  existing <- legacy_games_candidates[
    file.exists(
      legacy_games_candidates
    )
  ]
  
  if (
    length(
      existing
    ) ==
    0
  ) {
    
    return(
      list(
        data = NULL,
        path = NA_character_,
        message = paste0(
          "1970-1998 weekly trajectories need ",
          LEGACY_GAMES_FILENAME,
          ". The app looks in the app folder, app/data, and ",
          "~/Downloads/historical_nfl_1970_1998/."
        )
      )
    )
  }
  
  game_path <- existing[1]
  
  games <- read.csv(
    game_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  required <- c(
    "season",
    "week",
    "home_team",
    "away_team",
    "home_score",
    "away_score"
  )
  
  missing <- setdiff(
    required,
    names(
      games
    )
  )
  
  if (
    length(
      missing
    ) >
    0
  ) {
    
    stop(
      paste0(
        "Historical game archive is missing required columns: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  games$season <- as.integer(
    games$season
  )
  
  games$week <- suppressWarnings(
    as.integer(
      games$week
    )
  )
  
  games$home_team <- as.character(
    games$home_team
  )
  
  games$away_team <- as.character(
    games$away_team
  )
  
  games$home_score <- as.numeric(
    games$home_score
  )
  
  games$away_score <- as.numeric(
    games$away_score
  )
  
  if (
    !"game_type" %in%
    names(
      games
    )
  ) {
    
    games$game_type <- "REG"
  }
  
  if (
    !"neutral_site" %in%
    names(
      games
    )
  ) {
    
    games$neutral_site <- FALSE
    
  } else if (
    !is.logical(
      games$neutral_site
    )
  ) {
    
    games$neutral_site <- toupper(
      as.character(
        games$neutral_site
      )
    ) %in%
      c(
        "TRUE",
        "T",
        "1",
        "YES"
      )
  }
  
  if (
    !"completed" %in%
    names(
      games
    )
  ) {
    
    games$completed <- (
      !is.na(
        games$home_score
      ) &
        !is.na(
          games$away_score
        )
    )
  }
  
  games$completed <- as.logical(
    games$completed
  )
  
  if (
    !"actual_margin" %in%
    names(
      games
    )
  ) {
    
    games$actual_margin <- ifelse(
      games$completed,
      games$home_score -
        games$away_score,
      NA_real_
    )
  }
  
  if (
    !"actual_total" %in%
    names(
      games
    )
  ) {
    
    games$actual_total <- ifelse(
      games$completed,
      games$home_score +
        games$away_score,
      NA_real_
    )
  }
  
  if (
    !"game_id" %in%
    names(
      games
    )
  ) {
    
    games$game_id <- sprintf(
      "%d_%04d",
      games$season,
      ave(
        seq_len(
          nrow(
            games
          )
        ),
        games$season,
        FUN = seq_along
      )
    )
  }
  
  games <- games[
    games$game_type ==
      "REG" &
      !is.na(
        games$week
      ),
    ,
    drop = FALSE
  ]
  
  list(
    data = games,
    path = game_path,
    message = paste0(
      "Historical weekly game data loaded: ",
      normalizePath(
        game_path,
        mustWork = FALSE
      )
    )
  )
}

initial_legacy_games <- tryCatch(
  load_legacy_games(),
  error = function(err) {
    list(
      data = NULL,
      path = NA_character_,
      message = paste0(
        "Historical weekly game data could not be loaded: ",
        conditionMessage(
          err
        )
      )
    )
  }
)

COMPARE_COLORS <- c(
  "#D7263D",
  "#2F80ED",
  "#F2A71B",
  "#0F8B6D"
)

COMPARE_SHAPES <- c(
  16,
  17,
  15,
  18
)

make_team_season_id <- function(
    season,
    team
) {
  
  paste(
    as.integer(
      season
    ),
    team,
    sep = "|"
  )
}

parse_team_season_ids <- function(
    values
) {
  
  pieces <- strsplit(
    values,
    "\\|"
  )
  
  data.frame(
    selection_order =
      seq_along(
        values
      ),
    season =
      as.integer(
        vapply(
          pieces,
          function(x) x[1],
          character(1)
        )
      ),
    team =
      vapply(
        pieces,
        function(x) x[2],
        character(1)
      ),
    stringsAsFactors =
      FALSE
  )
}


# ============================================================
# UI
# ============================================================

ui <- navbarPage(
  title = "NFL Power Ratings",
  windowTitle = "NFL Power Ratings | Luke Ladd",
  id = "main_nav",
  collapsible = TRUE,
  
  header = tagList(
    tags$style(HTML("
      body {
        background: #EEF2F7;
        color: #172033;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }

      .navbar {
        margin-bottom: 0;
        border-radius: 0;
        border: none;
        box-shadow: 0 2px 12px rgba(11, 31, 51, 0.18);
      }

      .navbar-default {
        background: #0B1F33;
      }

      .navbar-default .navbar-brand {
        color: #FFFFFF;
        font-weight: 800;
        letter-spacing: 0.02em;
      }

      .navbar-default .navbar-brand:hover,
      .navbar-default .navbar-brand:focus {
        color: #FFFFFF;
      }

      .navbar-default .navbar-nav > li > a {
        color: #D8E3EF;
        font-weight: 600;
      }

      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        color: #FFFFFF;
        background: #153754;
      }

      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        color: #FFFFFF;
        background: #D7263D;
      }

      .app-header {
        padding: 20px 24px 16px 24px;
        background: linear-gradient(135deg, #0B1F33 0%, #163B62 70%, #205E8C 100%);
        border-bottom: 4px solid #F2C94C;
        margin-bottom: 18px;
        color: white;
      }

      .app-header h3 {
        color: #FFFFFF;
        font-weight: 800;
        letter-spacing: 0.01em;
      }

      .app-header-row {
        display: flex;
        gap: 14px;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
      }

      .status-text {
        color: #C8D7E8;
        font-size: 13px;
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(155px, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }

      .metric-card {
        background: #FFFFFF;
        border: 1px solid #DDE4EC;
        border-top: 4px solid #2F80ED;
        border-radius: 12px;
        padding: 15px 17px;
        min-height: 96px;
        box-shadow: 0 4px 14px rgba(11, 31, 51, 0.07);
      }

      .metric-grid .metric-card:nth-child(4n+2) {
        border-top-color: #D7263D;
      }

      .metric-grid .metric-card:nth-child(4n+3) {
        border-top-color: #F2C94C;
      }

      .metric-grid .metric-card:nth-child(4n+4) {
        border-top-color: #27AE60;
      }

      .metric-title {
        color: #667085;
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.055em;
        margin-bottom: 6px;
      }

      .metric-value {
        color: #0B1F33;
        font-size: 26px;
        font-weight: 800;
        line-height: 1.15;
      }

      .metric-subtitle {
        color: #667085;
        font-size: 12px;
        margin-top: 6px;
      }

      .panel-card {
        background: #FFFFFF;
        border: 1px solid #DDE4EC;
        border-radius: 12px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 4px 16px rgba(11, 31, 51, 0.06);
      }

      .section-title {
        color: #0B1F33;
        font-size: 19px;
        font-weight: 800;
        margin-bottom: 14px;
        padding-bottom: 8px;
        border-bottom: 2px solid #E7EDF4;
      }

      .control-row {
        background: #FFFFFF;
        border: 1px solid #DDE4EC;
        border-left: 5px solid #2F80ED;
        border-radius: 12px;
        padding: 14px 16px 4px 16px;
        margin-bottom: 18px;
        box-shadow: 0 3px 12px rgba(11, 31, 51, 0.05);
      }

      .form-control,
      .selectize-input {
        border-radius: 8px;
        border-color: #CAD5E1;
      }

      .form-control:focus,
      .selectize-input.focus {
        border-color: #2F80ED;
        box-shadow: 0 0 0 2px rgba(47, 128, 237, 0.12);
      }

      .btn-primary {
        background: #D7263D;
        border-color: #D7263D;
        border-radius: 8px;
        font-weight: 700;
        box-shadow: 0 3px 10px rgba(215, 38, 61, 0.24);
      }

      .btn-primary:hover,
      .btn-primary:focus {
        background: #B91F34;
        border-color: #B91F34;
      }

      .dataTables_wrapper {
        color: #253348;
      }

      table.dataTable thead th {
        color: #0B1F33;
        font-weight: 800;
      }

      table.dataTable tbody tr:hover {
        background-color: #F1F6FC !important;
      }

      .shiny-output-error-validation {
        color: #9A3412;
        font-weight: 600;
      }


      .all-time-nav .nav-pills {
        margin-bottom: 18px;
      }

      .all-time-nav .nav-pills > li > a {
        color: #24415E;
        background: #E7EDF4;
        border-radius: 999px;
        margin-right: 7px;
        font-weight: 700;
        padding: 9px 15px;
      }

      .all-time-nav .nav-pills > li.active > a,
      .all-time-nav .nav-pills > li.active > a:hover,
      .all-time-nav .nav-pills > li.active > a:focus {
        color: #FFFFFF;
        background: #2F80ED;
      }

      .historical-action-row {
        display: flex;
        align-items: center;
        gap: 9px;
        flex-wrap: wrap;
        margin-bottom: 18px;
      }

      .historical-selection {
        flex: 1 1 260px;
        background: #FFFFFF;
        border: 1px solid #DDE4EC;
        border-left: 5px solid #F2C94C;
        border-radius: 10px;
        padding: 11px 14px;
        color: #334155;
        font-weight: 600;
      }

      .btn-history {
        background: #FFFFFF;
        color: #163B62;
        border: 1px solid #BFCADA;
        border-radius: 8px;
        font-weight: 700;
      }

      .btn-history:hover,
      .btn-history:focus {
        color: #FFFFFF;
        background: #163B62;
        border-color: #163B62;
      }

      .header-actions {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
      }

      .freshness-pill {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        background: rgba(255, 255, 255, 0.11);
        border: 1px solid rgba(255, 255, 255, 0.24);
        border-radius: 999px;
        color: #FFFFFF;
        padding: 8px 12px;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0.015em;
      }

      .freshness-pill.cached {
        background: rgba(242, 201, 76, 0.16);
        border-color: rgba(242, 201, 76, 0.65);
      }

      .status-dot {
        width: 8px;
        height: 8px;
        border-radius: 50%;
        background: #27AE60;
        box-shadow: 0 0 0 3px rgba(39, 174, 96, 0.18);
      }

      .freshness-pill.cached .status-dot {
        background: #F2C94C;
        box-shadow: 0 0 0 3px rgba(242, 201, 76, 0.18);
      }

      .about-hero {
        background: linear-gradient(135deg, #0B1F33 0%, #163B62 62%, #205E8C 100%);
        color: #FFFFFF;
        border-radius: 16px;
        padding: 28px 30px;
        margin-bottom: 18px;
        box-shadow: 0 7px 22px rgba(11, 31, 51, 0.16);
      }

      .about-hero h2 {
        color: #FFFFFF;
        margin-top: 0;
        font-weight: 850;
        letter-spacing: 0.01em;
      }

      .about-hero p {
        color: #E3EDF7;
        font-size: 16px;
        line-height: 1.55;
        max-width: 930px;
        margin-bottom: 0;
      }

      .method-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
        gap: 14px;
        margin-bottom: 18px;
      }

      .method-card {
        background: #FFFFFF;
        border: 1px solid #DDE4EC;
        border-radius: 12px;
        padding: 18px;
        box-shadow: 0 4px 14px rgba(11, 31, 51, 0.06);
      }

      .method-card h4 {
        color: #0B1F33;
        font-weight: 800;
        margin-top: 0;
      }

      .method-card p,
      .method-card li {
        color: #4C5D70;
        line-height: 1.55;
      }

      .formula-box {
        background: #F5F8FC;
        border-left: 5px solid #F2C94C;
        border-radius: 10px;
        padding: 15px 18px;
        margin: 12px 0;
        color: #0B1F33;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-size: 15px;
        font-weight: 700;
      }

      .readiness-box {
        background: #FFFFFF;
        border: 1px solid #DDE4EC;
        border-radius: 12px;
        padding: 18px;
        margin-top: 16px;
        box-shadow: 0 4px 14px rgba(11, 31, 51, 0.06);
      }

      .readiness-ok {
        color: #16723A;
        font-weight: 800;
      }

      .readiness-warn {
        color: #9A3412;
        font-weight: 800;
      }

      .header-subtitle {
        color: #E3EDF7;
        font-size: 13px;
        font-weight: 600;
        margin-top: 3px;
        margin-bottom: 3px;
      }

      .version-label {
        display: inline-block;
        margin-top: 7px;
        color: #C8D7E8;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }

      .cta-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(180px, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }

      .cta-button {
        width: 100%;
        background: #FFFFFF;
        color: #163B62;
        border: 1px solid #BFCADA;
        border-left: 5px solid #2F80ED;
        border-radius: 11px;
        padding: 14px 16px;
        font-size: 14px;
        font-weight: 800;
        text-align: left;
        box-shadow: 0 3px 12px rgba(11, 31, 51, 0.05);
      }

      .cta-button:hover,
      .cta-button:focus {
        color: #FFFFFF;
        background: #163B62;
        border-color: #163B62;
      }

      .cta-grid .btn:nth-child(2) {
        border-left-color: #F2C94C;
      }

      .cta-grid .btn:nth-child(3) {
        border-left-color: #D7263D;
      }

      .interpretation-note {
        background: #F7FAFD;
        border: 1px solid #DDE4EC;
        border-left: 5px solid #2F80ED;
        border-radius: 9px;
        padding: 11px 14px;
        margin: -4px 0 18px 0;
        color: #42546A;
        font-size: 13px;
        line-height: 1.5;
      }

      .interpretation-note strong {
        color: #0B1F33;
      }

      .uncertainty-note {
        background: #FFF9E8;
        border: 1px solid #F3D878;
        border-left: 5px solid #F2C94C;
        border-radius: 9px;
        padding: 11px 14px;
        margin: 0 0 18px 0;
        color: #6B5712;
        font-size: 13px;
        line-height: 1.5;
      }

      .maintenance-details {
        background: #F7FAFD;
        border: 1px solid #DDE4EC;
        border-radius: 10px;
        padding: 12px 14px;
        margin-top: 12px;
      }

      .maintenance-details summary {
        color: #536579;
        cursor: pointer;
        font-size: 13px;
        font-weight: 800;
      }

      .maintenance-details p {
        color: #5D6B7C;
        font-size: 12px;
        line-height: 1.5;
        margin-top: 12px;
      }

      .maintenance-details .btn {
        margin-top: 4px;
      }

      .app-footer {
        margin-top: 22px;
        padding: 18px 20px 22px 20px;
        text-align: center;
        color: #607086;
        font-size: 12px;
        line-height: 1.7;
        border-top: 1px solid #DDE4EC;
        background: #F8FAFC;
      }

      .app-footer a {
        color: #205E8C;
        font-weight: 700;
        text-decoration: none;
      }

      .app-footer a:hover,
      .app-footer a:focus {
        text-decoration: underline;
      }

      .footer-separator {
        color: #A0AEC0;
        padding: 0 6px;
      }

      @media (max-width: 767px) {
        .app-header {
          padding-left: 12px;
          padding-right: 12px;
        }

        .metric-value {
          font-size: 22px;
        }

        .app-header-row {
          align-items: flex-start;
        }

        .header-actions {
          width: 100%;
        }

        .cta-grid {
          grid-template-columns: 1fr;
        }

        .method-grid {
          grid-template-columns: 1fr;
        }

        .all-time-nav .nav-pills {
          display: flex;
          flex-wrap: wrap;
          gap: 4px;
        }

        .all-time-nav .nav-pills > li > a {
          margin-right: 0;
          padding: 8px 11px;
          font-size: 12px;
        }
      }
    ")),
    div(
      class = "app-header",
      div(
        class = "app-header-row",
        div(
          h3(style = "margin:0;", "NFL Power Ratings"),
          div(
            class = "header-subtitle",
            "Results-based team strength, forecasts & historical analysis • 1970–present"
          ),
          div(
            class = "status-text",
            textOutput("model_status", inline = TRUE)
          ),
          div(
            class = "version-label",
            "Model v12.1 • Dashboard v1.0"
          )
        ),
        div(
          class = "header-actions",
          uiOutput(
            "freshness_badge"
          )
        )
      )
    )
  ),
  
  footer = tags$footer(
    class = "app-footer",
    div(
      strong("Built by Luke Ladd"),
      span(class = "footer-separator", "•"),
      tags$a(
        href = "https://github.com/lladdl",
        target = "_blank",
        rel = "noopener noreferrer",
        "GitHub"
      ),
      span(class = "footer-separator", "•"),
      tags$a(
        href = "https://www.linkedin.com/in/luke-ladd-777b1025a",
        target = "_blank",
        rel = "noopener noreferrer",
        "LinkedIn"
      ),
      span(class = "footer-separator", "•"),
      span("R + Shiny")
    ),
    div(
      "Data: nflreadr + Pro Football Reference historical archive",
      span(class = "footer-separator", "•"),
      "Model v12.1",
      span(class = "footer-separator", "•"),
      "Dashboard v1.0"
    ),
    div(
      "Independent analytics project; not affiliated with or endorsed by the NFL."
    )
  ),
  
  tabPanel(
    "About",
    value = "About",
    fluidPage(
      div(
        class = "about-hero",
        h2("NFL Power Ratings"),
        p(
          "A results-based NFL team-strength model built without preseason ranking priors. ",
          "Every season begins from an even starting point, and ratings evolve from completed games, ",
          "opponent-adjusted scoring performance, recency, home-field context, and historically calibrated forecasts."
        )
      ),
      
      uiOutput("about_summary_cards"),
      
      div(
        class = "cta-grid",
        actionButton(
          "about_go_rankings",
          "View Current Rankings →",
          class = "cta-button"
        ),
        actionButton(
          "about_go_history",
          "Explore NFL History →",
          class = "cta-button"
        ),
        actionButton(
          "about_go_playoffs",
          "Explore Playoffs →",
          class = "cta-button"
        )
      ),
      
      div(
        class = "panel-card",
        div(
          class = "section-title",
          "Why this model?"
        ),
        p(
          "Most power-rating systems begin a season with information carried forward from previous seasons, ",
          "preseason expectations, market prices, or subjective priors. This project deliberately asks a different question: ",
          strong(
            "what can the season's completed results tell us if every team starts on equal footing?"
          )
        ),
        p(
          "Opponent strength is estimated simultaneously rather than handled as a simple schedule adjustment after the fact. ",
          "Published team ratings are also kept separate from the forecasting layer: the rating describes estimated team strength, ",
          "while game predictions can be shrunk and probability-calibrated based on historical out-of-sample performance."
        )
      ),
      
      div(
        class = "method-grid",
        
        div(
          class = "method-card",
          h4("How to read a rating"),
          p(
            "Ratings are neutral-field points above or below league average. ",
            "A team rated +7.0 is estimated to be about seven points better than an average team ",
            "in a common neutral-field environment."
          ),
          div(
            class = "formula-box",
            "Overall Rating = Offense Rating + Defense Rating"
          ),
          p(
            "Positive offense means the offense adds scoring strength. Positive defense means the defense ",
            "prevents points relative to league average."
          )
        ),
        
        div(
          class = "method-card",
          h4("What drives the model"),
          tags$ul(
            tags$li("Completed game results only — no preseason poll or subjective team-strength prior."),
            tags$li("Opponent-adjusted offense and defense estimated jointly with a robust Huber loss."),
            tags$li("Recent games receive more weight through a 12-week recency half-life."),
            tags$li("Home-field advantage is estimated dynamically and shrunk toward a 2.5-point prior.")
          )
        ),
        
        div(
          class = "method-card",
          h4("Ratings vs forecasts"),
          p("Published power ratings are not forecast-shrunk. Game predictions are a separate layer."),
          div(
            class = "formula-box",
            "Forecast = baseline HFA + historical shrinkage × (raw rating margin − baseline HFA)"
          ),
          p("Win probabilities are then calibrated using prior NFL out-of-sample results.")
        ),
        
        div(
          class = "method-card",
          h4("Historical comparisons"),
          p(
            strong("Raw Rating"),
            " keeps the point-scale interpretation within the scoring environment of that season."
          ),
          p(
            strong("Season-Adjusted Dominance"),
            " standardizes a team against the rest of its own season, which is more appropriate for asking ",
            "how dominant a team was relative to its contemporaries."
          )
        ),
        
        div(
          class = "method-card",
          h4("Playoff analysis"),
          p(
            "The final regular-season rating is preserved before postseason analysis begins. ",
            "Playoff results therefore do not rewrite how strong the model believed the team was at the end ",
            "of the regular season."
          ),
          p(
            "Playoff win probabilities are calibrated from expected point margin with no intercept, ",
            "so equally rated teams on a neutral field are exactly 50/50."
          )
        ),
        
        div(
          class = "method-card",
          h4("Important limitations"),
          tags$ul(
            tags$li(
              "The core rating is team-results based; it does not directly model injuries, quarterback availability, or individual player value."
            ),
            tags$li(
              "Early-season ratings are intentionally uncertain because every team begins without a preseason strength prior."
            ),
            tags$li(
              "Cross-era matchups are analytical comparisons, not claims that the historical probability model is perfectly calibrated across eras."
            )
          )
        )
      ),
      
      div(
        class = "panel-card",
        div(class = "section-title", "Data and update behavior"),
        p(
          "Current-season schedules and results are loaded with nflreadr. The published rating advances only after ",
          "a regular-season week is fully completed. The header shows whether the currently displayed model was loaded ",
          "from live data or from the last-known-good cached snapshot."
        ),
        p(
          "Historical team-season analysis covers 1970–2025, while the active production season is appended dynamically. ",
          "The production season is selected automatically and can be overridden with the NFL_PRODUCTION_SEASON environment variable."
        ),
        uiOutput("about_data_readiness"),
        tags$details(
          class = "maintenance-details",
          tags$summary(
            "Maintenance & data refresh"
          ),
          p(
            "This control is intentionally kept out of the main navigation. Use it when you want to reload the latest ",
            "nflreadr results and rebuild the current-season ratings. A failed live refresh retains the previous successful model."
          ),
          actionButton(
            "refresh_model",
            "Refresh latest NFL data",
            class = "btn-history"
          )
        )
      ),
      
      div(
        class = "panel-card",
        div(class = "section-title", "Model philosophy"),
        p(
          "The goal is not to recreate standings or reward a team simply for winning. The rating asks a different question: ",
          strong(
            "how strong has this team played, after accounting for opponent, location, scoring performance, and recency?"
          )
        ),
        p(
          "Strength of schedule is reported separately rather than fed back into the rating a second time, because opponent strength ",
          "is already handled inside the game-level rating system."
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
      uiOutput(
        "early_season_note"
      ),
      uiOutput("ranking_cards"),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Power rankings"),
            plotOutput("ranking_plot", height = "760px")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Ranking table"),
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
                3,
                selectInput(
                  "team_explorer_season",
                  "Season",
                  choices = NULL
                )
              ),
              column(
                4,
                selectInput(
                  "team_explorer",
                  "Team",
                  choices = NULL
                )
              ),
              column(
                5,
                selectInput(
                  "team_metric",
                  "History metric",
                  choices = c(
                    "Overall rating" = "rating",
                    "Offense rating" = "offense_rating",
                    "Defense rating" = "defense_rating",
                    "League rank" = "rank"
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
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Weekly trajectory"),
            plotOutput("team_history_plot", height = "500px")
          )
        )
      ),
      fluidRow(
        column(
          12,
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
                  "Team-seasons",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 4,
                    placeholder = "Search by season and team, e.g. 1998 MIN"
                  )
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
            div(class = "section-title", "Team-season trajectories"),
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
            div(class = "section-title", "Model context"),
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
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Win probabilities"),
            plotOutput("forecast_probability_plot", height = "620px")
          )
        )
      ),
      fluidRow(
        column(
          12,
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
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Biggest weekly changes"),
            plotOutput("movers_plot", height = "660px")
          )
        )
      ),
      fluidRow(
        column(
          12,
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
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "League schedule difficulty"),
            plotOutput("sos_plot", height = "760px")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Schedule difficulty table"),
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
    "All-Time Explorer",
    fluidPage(
      div(
        class = "all-time-nav",
        tabsetPanel(
          id = "all_time_view",
          type = "pills",
          
          tabPanel(
            "All-Time Teams",
            value = "teams",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      3,
                      selectInput(
                        "all_time_team_category",
                        "Category",
                        choices = c(
                          "Overall" = "overall",
                          "Offense" = "offense",
                          "Defense" = "defense"
                        ),
                        selected = "overall"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "all_time_team_method",
                        "Ranking method",
                        choices = c(
                          "Raw rating" = "raw",
                          "Season-adjusted dominance" = "season_z"
                        ),
                        selected = "raw"
                      )
                    ),
                    column(
                      2,
                      selectInput(
                        "all_time_team_era",
                        "Era",
                        choices = NULL
                      )
                    ),
                    column(
                      2,
                      selectInput(
                        "all_time_team_franchise",
                        "Franchise",
                        choices = NULL
                      )
                    ),
                    column(
                      2,
                      selectInput(
                        "all_time_team_count",
                        "Show",
                        choices = c(
                          "Top 10" = 10,
                          "Top 25" = 25,
                          "Top 50" = 50,
                          "Top 100" = 100,
                          "All" = "all"
                        ),
                        selected = 25
                      )
                    )
                  )
                )
              )
            ),
            div(
              class = "interpretation-note",
              strong("How to interpret: "),
              "Raw rating is the point-scale estimate of team strength in that season's scoring environment. ",
              "Season-adjusted dominance standardizes the team relative to the rest of its own season and is better for ",
              "questions about how dominant a team was compared with its contemporaries."
            ),
            uiOutput("all_time_team_cards"),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Historical team leaderboard"
                  ),
                  plotOutput(
                    "all_time_team_plot",
                    height = "780px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Leaderboard table"
                  ),
                  DTOutput(
                    "all_time_team_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "Schedules",
            value = "schedules",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      4,
                      selectInput(
                        "all_time_schedule_metric",
                        "Schedule view",
                        choices = c(
                          "Hardest schedules" = "hardest",
                          "Easiest schedules" = "easiest",
                          "Largest positive rating adjustment" = "positive_adjustment",
                          "Largest negative rating adjustment" = "negative_adjustment"
                        ),
                        selected = "hardest"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "all_time_schedule_era",
                        "Era",
                        choices = NULL
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "all_time_schedule_franchise",
                        "Franchise",
                        choices = NULL
                      )
                    ),
                    column(
                      2,
                      selectInput(
                        "all_time_schedule_count",
                        "Show",
                        choices = c(
                          "Top 10" = 10,
                          "Top 25" = 25,
                          "Top 50" = 50,
                          "Top 100" = 100
                        ),
                        selected = 25
                      )
                    )
                  )
                )
              )
            ),
            uiOutput("all_time_schedule_cards"),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Historical schedule comparison"
                  ),
                  plotOutput(
                    "all_time_schedule_plot",
                    height = "780px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Schedule table"
                  ),
                  DTOutput(
                    "all_time_schedule_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "Overachievers / Underachievers",
            value = "over_under",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      4,
                      selectInput(
                        "all_time_over_metric",
                        "Comparison",
                        choices = c(
                          "Record rank vs model rank" = "record_gap",
                          "Rating vs point differential" = "point_diff_adjustment"
                        ),
                        selected = "record_gap"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "all_time_over_direction",
                        "Direction",
                        choices = c(
                          "Record / results look better" = "results_better",
                          "Model rating looks better" = "model_better"
                        ),
                        selected = "results_better"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "all_time_over_era",
                        "Era",
                        choices = NULL
                      )
                    ),
                    column(
                      2,
                      selectInput(
                        "all_time_over_count",
                        "Show",
                        choices = c(
                          "Top 10" = 10,
                          "Top 25" = 25,
                          "Top 50" = 50,
                          "Top 100" = 100
                        ),
                        selected = 25
                      )
                    )
                  )
                )
              )
            ),
            uiOutput("all_time_over_cards"),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Largest gaps"
                  ),
                  plotOutput(
                    "all_time_over_plot",
                    height = "780px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Team-season table"
                  ),
                  DTOutput(
                    "all_time_over_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "NFL Through Time",
            value = "time",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      5,
                      selectInput(
                        "all_time_time_metric",
                        "League trend",
                        choices = c(
                          "Scoring environment" = "scoring",
                          "Home-field advantage" = "hfa",
                          "Rating spread" = "rating_sd",
                          "Parity" = "parity",
                          "Model vs point differential relationship" = "rating_pd_correlation"
                        ),
                        selected = "scoring"
                      )
                    )
                  )
                )
              )
            ),
            uiOutput("all_time_time_cards"),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "NFL history"
                  ),
                  plotOutput(
                    "all_time_time_plot",
                    height = "560px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Season-by-season values"
                  ),
                  DTOutput(
                    "all_time_time_table"
                  )
                )
              )
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.all_time_view != 'time'",
        div(
          class = "historical-action-row",
          div(
            class = "historical-selection",
            uiOutput(
              "all_time_selected_label"
            )
          ),
          actionButton(
            "all_time_open_team",
            "Open in Team Explorer",
            class = "btn-history"
          ),
          actionButton(
            "all_time_add_compare",
            "Add to Compare",
            class = "btn-history"
          ),
          actionButton(
            "all_time_use_a",
            "Use as Matchup A",
            class = "btn-history"
          ),
          actionButton(
            "all_time_use_b",
            "Use as Matchup B",
            class = "btn-history"
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Playoffs",
    fluidPage(
      uiOutput(
        "playoff_data_status"
      ),
      
      div(
        class = "all-time-nav",
        tabsetPanel(
          id = "playoff_view",
          type = "pills",
          
          tabPanel(
            "Playoff Teams",
            value = "teams",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      4,
                      selectInput(
                        "playoff_team_season",
                        "Season",
                        choices = NULL
                      )
                    ),
                    column(
                      4,
                      selectInput(
                        "playoff_team_metric",
                        "Rank playoff teams by",
                        choices = c(
                          "Pre-playoff rating" = "pre_playoff_rating",
                          "Super Bowl probability" = "super_bowl_win_probability_retrospective",
                          "Playoff wins" = "playoff_wins",
                          "Performance vs expectation" = "total_margin_vs_expectation",
                          "Postseason rating change" = "playoff_rating_change"
                        ),
                        selected = "pre_playoff_rating"
                      )
                    )
                  )
                )
              )
            ),
            uiOutput(
              "playoff_team_cards"
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Playoff field"
                  ),
                  plotOutput(
                    "playoff_team_plot",
                    height = "650px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Playoff team table"
                  ),
                  DTOutput(
                    "playoff_team_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "Super Bowl Odds",
            value = "odds",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      4,
                      selectInput(
                        "playoff_odds_season",
                        "Season",
                        choices = NULL
                      )
                    ),
                    column(
                      4,
                      selectInput(
                        "playoff_odds_calibration",
                        "Probability calibration",
                        choices = c(
                          "Retrospective — all 1970–2025 playoff results" = "retrospective",
                          "Walk-forward — prior playoff results only" = "walkforward"
                        ),
                        selected = "retrospective"
                      )
                    ),
                    column(
                      4,
                      selectInput(
                        "playoff_odds_metric",
                        "Probability",
                        choices = c(
                          "Win Super Bowl" = "win",
                          "Reach Super Bowl" = "appearance",
                          "Reach Conference Championship" = "conference"
                        ),
                        selected = "win"
                      )
                    )
                  )
                )
              )
            ),
            div(
              class = "interpretation-note",
              strong("Calibration note: "),
              "Retrospective probabilities use the complete 1970–2025 playoff sample to estimate the playoff margin-to-win relationship. ",
              "Walk-forward probabilities use only playoff seasons that occurred before the selected season, so they are the more appropriate ",
              "view when judging what the model could have known historically."
            ),
            uiOutput(
              "playoff_odds_cards"
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Pre-playoff championship outlook"
                  ),
                  plotOutput(
                    "playoff_odds_plot",
                    height = "650px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Simulation probabilities"
                  ),
                  DTOutput(
                    "playoff_odds_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "Upsets",
            value = "upsets",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      3,
                      selectInput(
                        "playoff_upset_era",
                        "Era",
                        choices = c(
                          "All eras" = "ALL",
                          "1970s" = "1970s",
                          "1980s" = "1980s",
                          "1990s" = "1990s",
                          "2000s" = "2000s",
                          "2010s" = "2010s",
                          "2020s" = "2020s"
                        ),
                        selected = "ALL"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "playoff_upset_round",
                        "Round",
                        choices = c(
                          "All rounds" = "ALL",
                          "Wild Card / opening" = "WC",
                          "Divisional" = "DIV",
                          "Conference Championship" = "CONF",
                          "Super Bowl" = "SB"
                        ),
                        selected = "ALL"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "playoff_upset_count",
                        "Show",
                        choices = c(
                          "Top 10" = 10,
                          "Top 20" = 20,
                          "Top 50" = 50
                        ),
                        selected = 20
                      )
                    )
                  )
                )
              )
            ),
            uiOutput(
              "playoff_upset_cards"
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Biggest playoff upsets"
                  ),
                  plotOutput(
                    "playoff_upset_plot",
                    height = "760px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Upset table"
                  ),
                  DTOutput(
                    "playoff_upset_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "Champions",
            value = "champions",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      5,
                      selectInput(
                        "playoff_champion_metric",
                        "Compare champions by",
                        choices = c(
                          "Most surprising champion" = "sb_odds",
                          "Strongest entering playoffs" = "pre_rating",
                          "Most dominant regular season" = "season_z",
                          "Largest postseason rating gain" = "rating_change",
                          "Best performance vs expectation" = "performance"
                        ),
                        selected = "sb_odds"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "playoff_champion_count",
                        "Show",
                        choices = c(
                          "Top 10" = 10,
                          "Top 20" = 20,
                          "Top 30" = 30,
                          "All champions" = "all"
                        ),
                        selected = 20
                      )
                    )
                  )
                )
              )
            ),
            uiOutput(
              "playoff_champion_cards"
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Super Bowl champions"
                  ),
                  plotOutput(
                    "playoff_champion_plot",
                    height = "760px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Champion table"
                  ),
                  DTOutput(
                    "playoff_champion_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "Playoff Performance",
            value = "performance",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      3,
                      selectInput(
                        "playoff_performance_era",
                        "Era",
                        choices = c(
                          "All eras" = "ALL",
                          "1970s" = "1970s",
                          "1980s" = "1980s",
                          "1990s" = "1990s",
                          "2000s" = "2000s",
                          "2010s" = "2010s",
                          "2020s" = "2020s"
                        ),
                        selected = "ALL"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "playoff_performance_metric",
                        "Metric",
                        choices = c(
                          "Total margin vs expectation" = "total_margin_vs_expectation",
                          "Average margin vs expectation" = "average_margin_vs_expectation"
                        ),
                        selected = "total_margin_vs_expectation"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "playoff_performance_direction",
                        "View",
                        choices = c(
                          "Biggest overperformers" = "over",
                          "Biggest underperformers" = "under"
                        ),
                        selected = "over"
                      )
                    ),
                    column(
                      3,
                      selectInput(
                        "playoff_performance_count",
                        "Show",
                        choices = c(
                          "Top 10" = 10,
                          "Top 20" = 20,
                          "Top 50" = 50
                        ),
                        selected = 20
                      )
                    )
                  )
                )
              )
            ),
            div(
              class = "interpretation-note",
              strong("Margin vs expectation: "),
              "actual playoff scoring margin minus the model's expected scoring margin. Positive values mean the team performed better ",
              "than its pregame rating-based expectation; negative values mean it performed worse."
            ),
            uiOutput(
              "playoff_performance_cards"
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Performance relative to expectation"
                  ),
                  plotOutput(
                    "playoff_performance_plot",
                    height = "760px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Performance table"
                  ),
                  DTOutput(
                    "playoff_performance_table"
                  )
                )
              )
            )
          ),
          
          tabPanel(
            "Rating Changes",
            value = "ratings",
            fluidRow(
              column(
                12,
                div(
                  class = "control-row",
                  fluidRow(
                    column(
                      4,
                      selectInput(
                        "playoff_rating_season",
                        "Season",
                        choices = NULL
                      )
                    ),
                    column(
                      4,
                      selectInput(
                        "playoff_rating_team",
                        "Team",
                        choices = NULL
                      )
                    ),
                    column(
                      4,
                      selectInput(
                        "playoff_rating_component",
                        "Rating component",
                        choices = c(
                          "Overall" = "rating",
                          "Offense" = "offense_rating",
                          "Defense" = "defense_rating"
                        ),
                        selected = "rating"
                      )
                    )
                  )
                )
              )
            ),
            uiOutput(
              "playoff_rating_cards"
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Rating through the postseason"
                  ),
                  plotOutput(
                    "playoff_rating_trajectory_plot",
                    height = "520px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Season playoff rating changes"
                  ),
                  plotOutput(
                    "playoff_rating_change_plot",
                    height = "650px"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "panel-card",
                  div(
                    class = "section-title",
                    "Pre- vs post-playoff ratings"
                  ),
                  DTOutput(
                    "playoff_rating_table"
                  )
                )
              )
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.playoff_view == 'teams'",
        div(
          class = "historical-action-row",
          div(
            class = "historical-selection",
            uiOutput(
              "playoff_selected_label"
            )
          ),
          actionButton(
            "playoff_open_team",
            "Open in Team Explorer",
            class = "btn-history"
          ),
          actionButton(
            "playoff_add_compare",
            "Add to Compare",
            class = "btn-history"
          ),
          actionButton(
            "playoff_use_a",
            "Use as Matchup A",
            class = "btn-history"
          ),
          actionButton(
            "playoff_use_b",
            "Use as Matchup B",
            class = "btn-history"
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Historical Matchup",
    fluidPage(
      uiOutput("historical_archive_status"),
      fluidRow(
        column(
          12,
          div(
            class = "control-row",
            fluidRow(
              column(
                3,
                selectInput(
                  "hist_a_season",
                  "Team A season",
                  choices = NULL
                )
              ),
              column(
                3,
                selectInput(
                  "hist_a_team",
                  "Team A",
                  choices = NULL
                )
              ),
              column(
                3,
                selectInput(
                  "hist_b_season",
                  "Team B season",
                  choices = NULL
                )
              ),
              column(
                3,
                selectInput(
                  "hist_b_team",
                  "Team B",
                  choices = NULL
                )
              )
            ),
            fluidRow(
              column(
                6,
                selectInput(
                  "historical_site",
                  "Game site",
                  choices = c(
                    "Neutral field" = "neutral",
                    "Team A home" = "a_home",
                    "Team B home" = "b_home"
                  ),
                  selected = "neutral"
                )
              ),
              column(
                6,
                selectInput(
                  "historical_scoring_environment",
                  "Scoring environment",
                  choices = c(
                    "Neutralized — average of both seasons" = "average",
                    "Team A season" = "a",
                    "Team B season" = "b"
                  ),
                  selected = "average"
                )
              )
            )
          )
        )
      ),
      uiOutput("historical_matchup_cards"),
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(
              class = "section-title",
              "Hypothetical matchup"
            ),
            uiOutput("historical_matchup_summary")
          )
        ),
        column(
          6,
          div(
            class = "panel-card",
            div(
              class = "section-title",
              "Era comparison"
            ),
            plotOutput(
              "historical_matchup_plot",
              height = "430px"
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(
              class = "section-title",
              "Team profiles"
            ),
            DTOutput(
              "historical_matchup_table"
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(
              class = "section-title",
              "How to interpret this"
            ),
            p(
              "Ratings are centered within each season, so the season z-score is the best quick measure of how dominant a team was relative to its own league."
            ),
            p(
              "The neutralized scoring option places both teams into a shared scoring environment by averaging the two seasons' league scoring levels."
            ),
            p(
              "Historical win probability is an approximation based on the two seasons' predictive margin uncertainty."
            )
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
              column(
                3,
                selectInput(
                  "history_season",
                  "Season",
                  choices = NULL
                )
              ),
              column(
                4,
                selectInput(
                  "historical_metric",
                  "Snapshot metric",
                  choices = c(
                    "Overall rating" = "rating",
                    "Offense rating" = "offense_rating",
                    "Defense rating" = "defense_rating",
                    "Point differential/game" = "point_differential_per_game"
                  ),
                  selected = "rating"
                )
              ),
              column(
                5,
                selectInput(
                  "historical_trajectory_team",
                  "Weekly trajectory team",
                  choices = NULL
                )
              )
            )
          )
        )
      ),
      uiOutput("historical_season_cards"),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Season snapshot"),
            plotOutput("historical_plot", height = "800px")
          )
        )
      ),
      fluidRow(
        column(
          12,
          div(
            class = "panel-card",
            div(class = "section-title", "Weekly season trajectory"),
            plotOutput(
              "historical_season_trajectory_plot",
              height = "500px"
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
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
  
  initial_load_source <- attr(
    initial_model_env,
    "model_load_source"
  )
  
  refresh_message <- reactiveVal(
    if (
      identical(
        initial_load_source,
        "cache"
      )
    ) {
      "Live data were unavailable at startup; displaying the last known good snapshot."
    } else {
      "Model loaded successfully from live nflreadr data."
    }
  )
  
  
  historical_archive_info <- reactiveVal(
    initial_historical_archive
  )
  
  
  playoff_archive_info <- reactiveVal(
    initial_playoff_archive
  )
  
  playoff_data <- reactive({
    playoff_archive_info()$data
  })
  
  output$freshness_badge <- renderUI({
    
    e <- model_env()
    
    is_cached <- identical(
      attr(
        e,
        "model_load_source"
      ),
      "cache"
    )
    
    div(
      class = if (
        is_cached
      ) {
        "freshness-pill cached"
      } else {
        "freshness-pill"
      },
      span(
        class = "status-dot"
      ),
      if (
        is_cached
      ) {
        paste0(
          "Cached • ",
          e$production_season,
          " W",
          e$publication_week
        )
      } else {
        paste0(
          "Live • ",
          e$production_season,
          " W",
          e$publication_week
        )
      }
    )
  })
  
  output$about_summary_cards <- renderUI({
    
    e <- model_env()
    
    div(
      class = "metric-grid",
      metric_card(
        "Current season",
        e$production_season,
        "Selected automatically unless overridden"
      ),
      metric_card(
        "Current through",
        paste(
          "Week",
          e$publication_week
        ),
        "Latest fully completed regular-season week"
      ),
      metric_card(
        "Historical archive",
        "1970–2025",
        "Regular-season team snapshots"
      ),
      metric_card(
        "Starting prior",
        "Even",
        "No preseason power-ranking prior"
      )
    )
  })
  
  output$about_data_readiness <- renderUI({
    
    historical_ok <- !is.null(
      historical_archive_info()$data
    )
    
    legacy_info <- legacy_games_info()
    
    legacy_ok <- !is.null(
      legacy_info$data
    )
    
    playoff_info <- playoff_archive_info()
    
    playoff_master_ok <- !is.null(
      playoff_info$data$master
    )
    
    missing_playoff_files <- playoff_info$missing
    
    all_ready <- (
      historical_ok &&
        legacy_ok &&
        playoff_master_ok &&
        length(
          missing_playoff_files
        ) ==
        0
    )
    
    if (
      all_ready
    ) {
      
      return(
        div(
          class = "readiness-box",
          div(
            class = "readiness-ok",
            "Publication data check: complete"
          ),
          p(
            "Historical archive, 1970–1998 weekly game history, and all playoff analysis files are available inside or alongside the app."
          )
        )
      )
    }
    
    missing_items <- character(0)
    
    if (
      !historical_ok
    ) {
      missing_items <- c(
        missing_items,
        "historical enriched team-season archive"
      )
    }
    
    if (
      !legacy_ok
    ) {
      missing_items <- c(
        missing_items,
        "1970–1998 historical game-level archive"
      )
    }
    
    if (
      !playoff_master_ok
    ) {
      missing_items <- c(
        missing_items,
        "playoff team-season master"
      )
    }
    
    if (
      length(
        missing_playoff_files
      ) >
      0
    ) {
      missing_items <- c(
        missing_items,
        missing_playoff_files
      )
    }
    
    div(
      class = "readiness-box",
      div(
        class = "readiness-warn",
        "Publication data check: action needed"
      ),
      p(
        "The local app can still use files from your Downloads folder, but a hosted deployment cannot. ",
        "Run prepare_for_publish.R before deployment."
      ),
      tags$ul(
        lapply(
          unique(
            missing_items
          ),
          tags$li
        )
      )
    )
  })
  
  observeEvent(
    input$about_go_rankings,
    {
      updateTabsetPanel(
        session,
        "main_nav",
        selected = "Rankings"
      )
    }
  )
  
  observeEvent(
    input$about_go_history,
    {
      updateTabsetPanel(
        session,
        "main_nav",
        selected = "All-Time Explorer"
      )
    }
  )
  
  observeEvent(
    input$about_go_playoffs,
    {
      updateTabsetPanel(
        session,
        "main_nav",
        selected = "Playoffs"
      )
    }
  )
  
  output$early_season_note <- renderUI({
    
    e <- model_env()
    
    if (
      is.null(
        e$publication_week
      ) ||
      !is.finite(
        e$publication_week
      ) ||
      e$publication_week >
      4
    ) {
      
      return(
        NULL
      )
    }
    
    div(
      class = "uncertainty-note",
      strong(
        "Early-season uncertainty: "
      ),
      "these ratings are intentionally more volatile because the model does not import previous-season strength or preseason rankings. ",
      "As more completed games enter the system, opponent-adjusted estimates become more stable."
    )
  })
  
  historical_archive <- reactive({
    
    e <- model_env()
    
    static <- historical_archive_info()$data
    
    current <- e$current_season_team_summary
    
    if (
      is.null(
        current
      ) ||
      nrow(
        current
      ) ==
      0
    ) {
      
      return(
        static
      )
    }
    
    current$season <- as.integer(
      current$season
    )
    
    current$era <- make_era_label(
      current$season
    )
    
    current$season_rating_mean <- mean(
      current$rating,
      na.rm = TRUE
    )
    
    current$season_rating_sd <- sd(
      current$rating,
      na.rm = TRUE
    )
    
    current$season_rating_z <- (
      current$rating -
        current$season_rating_mean
    ) /
      current$season_rating_sd
    
    current$season_offense_z <- (
      current$offense_rating -
        mean(
          current$offense_rating,
          na.rm = TRUE
        )
    ) /
      sd(
        current$offense_rating,
        na.rm = TRUE
      )
    
    current$season_defense_z <- (
      current$defense_rating -
        mean(
          current$defense_rating,
          na.rm = TRUE
        )
    ) /
      sd(
        current$defense_rating,
        na.rm = TRUE
      )
    
    current$season_rating_percentile <-
      100 *
      rank(
        current$rating,
        ties.method = "min"
      ) /
      max(
        1,
        nrow(
          current
        )
      )
    
    current$record_rank <- as.integer(
      rank(
        -current$win_pct,
        ties.method = "min"
      )
    )
    
    current$record_rating_gap <- (
      current$record_rank -
        current$rank
    )
    
    current$schedule_adjustment <- (
      current$rating -
        current$point_differential_per_game
    )
    
    if (
      is.null(
        static
      ) ||
      nrow(
        static
      ) ==
      0
    ) {
      
      combined <- current
      
    } else {
      
      all_names <- union(
        names(
          static
        ),
        names(
          current
        )
      )
      
      static <- add_missing_columns(
        static,
        all_names
      )
      
      current <- add_missing_columns(
        current,
        all_names
      )
      
      static <- static[
        static$season !=
          e$production_season,
        all_names,
        drop = FALSE
      ]
      
      current <- current[
        ,
        all_names,
        drop = FALSE
      ]
      
      combined <- rbind(
        static,
        current
      )
    }
    
    combined$historical_raw_rank <- as.integer(
      rank(
        -combined$rating,
        ties.method = "min"
      )
    )
    
    combined$historical_standardized_rank <- as.integer(
      rank(
        -combined$season_rating_z,
        ties.method = "min"
      )
    )
    
    combined$historical_offense_rank <- as.integer(
      rank(
        -combined$offense_rating,
        ties.method = "min"
      )
    )
    
    combined$historical_defense_rank <- as.integer(
      rank(
        -combined$defense_rating,
        ties.method = "min"
      )
    )
    
    combined[
      order(
        combined$season,
        combined$rank,
        combined$team
      ),
      ,
      drop = FALSE
    ]
  })
  
  legacy_games_info <- reactiveVal(
    initial_legacy_games
  )
  
  weekly_history_cache <- new.env(
    parent = emptyenv()
  )
  
  season_schedule_data <- function(
    season_value
  ) {
    
    e <- model_env()
    
    season_value <- as.integer(
      season_value
    )
    
    if (
      season_value >=
      min(
        e$full_schedule$season,
        na.rm = TRUE
      ) &&
      season_value <=
      max(
        e$full_schedule$season,
        na.rm = TRUE
      )
    ) {
      
      data <- e$full_schedule[
        e$full_schedule$season ==
          season_value &
          e$full_schedule$game_type ==
          "REG",
        ,
        drop = FALSE
      ]
      
      if (
        nrow(
          data
        ) >
        0
      ) {
        
        return(
          data
        )
      }
    }
    
    legacy <- legacy_games_info()$data
    
    if (
      is.null(
        legacy
      )
    ) {
      
      return(
        NULL
      )
    }
    
    data <- legacy[
      legacy$season ==
        season_value &
        legacy$game_type ==
        "REG",
      ,
      drop = FALSE
    ]
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      
      return(
        NULL
      )
    }
    
    data
  }
  
  season_weekly_history <- function(
    season_value
  ) {
    
    e <- model_env()
    
    season_value <- as.integer(
      season_value
    )
    
    if (
      season_value ==
      e$production_season
    ) {
      
      return(
        e$current_weekly_power_history
      )
    }
    
    cache_key <- paste0(
      "season_",
      season_value
    )
    
    if (
      exists(
        cache_key,
        envir = weekly_history_cache,
        inherits = FALSE
      )
    ) {
      
      return(
        get(
          cache_key,
          envir = weekly_history_cache,
          inherits = FALSE
        )
      )
    }
    
    schedule <- season_schedule_data(
      season_value
    )
    
    if (
      is.null(
        schedule
      ) ||
      nrow(
        schedule
      ) ==
      0
    ) {
      
      return(
        NULL
      )
    }
    
    completed <- schedule[
      schedule$completed &
        !is.na(
          schedule$week
        ),
      ,
      drop = FALSE
    ]
    
    if (
      nrow(
        completed
      ) ==
      0
    ) {
      
      return(
        NULL
      )
    }
    
    league_teams <- sort(
      unique(
        c(
          schedule$home_team,
          schedule$away_team
        )
      )
    )
    
    through_week <- max(
      completed$week,
      na.rm = TRUE
    )
    
    history <- e$build_weekly_power_history(
      completed_games =
        completed,
      season_schedule =
        schedule,
      through_week =
        through_week,
      league_teams =
        league_teams
    )
    
    history$season <- season_value
    
    assign(
      cache_key,
      history,
      envir = weekly_history_cache
    )
    
    history
  }
  
  # ----------------------------------------------------------
  # Refresh / update
  # ----------------------------------------------------------
  
  observeEvent(input$refresh_model, {
    refresh_message("Updating ratings from nflreadr...")
    
    tryCatch(
      {
        new_env <- withProgress(
          message = "Updating NFL ratings",
          detail = "Reloading schedules and updating the power ratings...",
          value = 0.2,
          {
            env <- load_model_environment(
              fast_refresh = TRUE,
              allow_cache = TRUE
            )
            incProgress(0.8, detail = "Refreshing dashboard objects...")
            env
          }
        )
        
        model_env(new_env)
        
        playoff_archive_info(
          load_playoff_archive()
        )
        
        cached_items <- ls(
          envir = weekly_history_cache
        )
        
        if (
          length(
            cached_items
          ) >
          0
        ) {
          
          rm(
            list = cached_items,
            envir = weekly_history_cache
          )
        }
        
        refresh_time(Sys.time())
        
        refreshed_source <- attr(
          new_env,
          "model_load_source"
        )
        
        if (
          identical(
            refreshed_source,
            "cache"
          )
        ) {
          
          refresh_message(
            paste0(
              "Live update was unavailable; retained the last known good snapshot. ",
              attr(
                new_env,
                "model_cache_reason"
              )
            )
          )
          
        } else {
          
          refresh_message(
            "Update completed successfully. Historical calibration cache reused when available."
          )
        }
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
    
    source_label <- if (
      identical(
        attr(
          e,
          "model_load_source"
        ),
        "cache"
      )
    ) {
      "cached fallback"
    } else {
      "live nflreadr"
    }
    
    paste0(
      "Season ", e$production_season,
      " • through Week ", e$publication_week,
      " • source: ", source_label,
      " • last app refresh ",
      format(refresh_time(), "%Y-%m-%d %H:%M:%S"),
      " • ", refresh_message()
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
    
    # Historical-season choices are updated in a separate observer
    # because they combine the static 1970-2025 archive with the
    # current production-season snapshot.
  }, ignoreInit = FALSE)
  
  observeEvent(
    historical_archive(),
    {
      
      archive <- historical_archive()
      
      if (
        is.null(
          archive
        ) ||
        nrow(
          archive
        ) ==
        0
      ) {
        
        return()
      }
      
      seasons <- sort(
        unique(
          archive$season
        ),
        decreasing = TRUE
      )
      
      selected_a <- if (
        1998 %in%
        seasons
      ) {
        1998
      } else {
        seasons[1]
      }
      
      selected_b <- if (
        2004 %in%
        seasons
      ) {
        2004
      } else {
        seasons[min(2, length(seasons))]
      }
      
      updateSelectInput(
        session,
        "hist_a_season",
        choices = seasons,
        selected = selected_a
      )
      
      updateSelectInput(
        session,
        "hist_b_season",
        choices = seasons,
        selected = selected_b
      )
      
      current_season <- model_env()$production_season
      
      updateSelectInput(
        session,
        "history_season",
        choices = seasons,
        selected = current_season
      )
      
      updateSelectInput(
        session,
        "team_explorer_season",
        choices = seasons,
        selected = current_season
      )
      
      archive_ordered <- archive[
        order(
          -archive$season,
          archive$rank,
          archive$team
        ),
        ,
        drop = FALSE
      ]
      
      compare_values <- make_team_season_id(
        archive_ordered$season,
        archive_ordered$team
      )
      
      compare_labels <- paste(
        archive_ordered$season,
        archive_ordered$team
      )
      
      compare_choices <- setNames(
        compare_values,
        compare_labels
      )
      
      current_rows <- archive[
        archive$season ==
          current_season,
        ,
        drop = FALSE
      ]
      
      current_rows <- current_rows[
        order(
          current_rows$rank,
          current_rows$team
        ),
        ,
        drop = FALSE
      ]
      
      n_default <- min(
        4,
        nrow(
          current_rows
        )
      )
      
      default_compare <- if (
        n_default >
        0
      ) {
        make_team_season_id(
          current_rows$season[
            seq_len(
              n_default
            )
          ],
          current_rows$team[
            seq_len(
              n_default
            )
          ]
        )
      } else {
        character(0)
      }
      
      updateSelectizeInput(
        session,
        "compare_teams",
        choices = compare_choices,
        selected = default_compare,
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )
  
  observeEvent(
    list(
      historical_archive(),
      input$team_explorer_season
    ),
    {
      
      archive <- historical_archive()
      
      req(
        !is.null(
          archive
        ),
        input$team_explorer_season
      )
      
      season_value <- as.integer(
        input$team_explorer_season
      )
      
      season_rows <- archive[
        archive$season ==
          season_value,
        ,
        drop = FALSE
      ]
      
      season_rows <- season_rows[
        order(
          season_rows$rank,
          season_rows$team
        ),
        ,
        drop = FALSE
      ]
      
      teams <- season_rows$team
      
      selected <- if (
        length(
          teams
        ) >
        0
      ) {
        teams[1]
      } else {
        NULL
      }
      
      updateSelectInput(
        session,
        "team_explorer",
        choices = teams,
        selected = selected
      )
    },
    ignoreInit = FALSE
  )
  
  observeEvent(
    list(
      historical_archive(),
      input$history_season
    ),
    {
      
      archive <- historical_archive()
      
      req(
        !is.null(
          archive
        ),
        input$history_season
      )
      
      season_value <- as.integer(
        input$history_season
      )
      
      season_rows <- archive[
        archive$season ==
          season_value,
        ,
        drop = FALSE
      ]
      
      season_rows <- season_rows[
        order(
          season_rows$rank,
          season_rows$team
        ),
        ,
        drop = FALSE
      ]
      
      updateSelectInput(
        session,
        "historical_trajectory_team",
        choices = season_rows$team,
        selected = if (
          nrow(
            season_rows
          ) >
          0
        ) {
          season_rows$team[1]
        } else {
          NULL
        }
      )
    },
    ignoreInit = FALSE
  )
  
  observeEvent(
    list(
      historical_archive(),
      input$hist_a_season
    ),
    {
      
      archive <- historical_archive()
      
      req(
        !is.null(
          archive
        ),
        input$hist_a_season
      )
      
      teams <- sort(
        archive$team[
          archive$season ==
            as.integer(
              input$hist_a_season
            )
        ]
      )
      
      selected <- if (
        as.integer(
          input$hist_a_season
        ) ==
        1998 &&
        "MIN" %in%
        teams
      ) {
        "MIN"
      } else {
        teams[1]
      }
      
      updateSelectInput(
        session,
        "hist_a_team",
        choices = teams,
        selected = selected
      )
    },
    ignoreInit = FALSE
  )
  
  observeEvent(
    list(
      historical_archive(),
      input$hist_b_season
    ),
    {
      
      archive <- historical_archive()
      
      req(
        !is.null(
          archive
        ),
        input$hist_b_season
      )
      
      teams <- sort(
        archive$team[
          archive$season ==
            as.integer(
              input$hist_b_season
            )
        ]
      )
      
      selected <- if (
        as.integer(
          input$hist_b_season
        ) ==
        2004 &&
        "NE" %in%
        teams
      ) {
        "NE"
      } else {
        teams[1]
      }
      
      updateSelectInput(
        session,
        "hist_b_team",
        choices = teams,
        selected = selected
      )
    },
    ignoreInit = FALSE
  )
  
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
    
    data$metric_label <- ifelse(
      data$selected_metric > 0,
      paste0("+", formatC(data$selected_metric, digits = 1, format = "f")),
      formatC(data$selected_metric, digits = 1, format = "f")
    )
    
    p <- ggplot(
      data,
      aes(x = team_factor, y = selected_metric, fill = team)
    ) +
      geom_col(width = 0.76) +
      geom_text(
        aes(label = metric_label),
        hjust = ifelse(
          data$selected_metric >= 0,
          -0.15,
          1.15
        ),
        fontface = "bold",
        size = 3.1,
        color = "#0B1F33"
      ) +
      coord_flip(clip = "off") +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
      scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) +
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
      dashboard_plot_theme() +
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
        subtitle = "Higher is better on both axes"
      ) +
      dashboard_plot_theme()
  })
  
  # ----------------------------------------------------------
  # Team Explorer
  # ----------------------------------------------------------
  
  selected_team_profile <- reactive({
    
    archive <- historical_archive()
    
    req(
      input$team_explorer_season,
      input$team_explorer
    )
    
    validate(
      need(
        !is.null(
          archive
        ) &&
          nrow(
            archive
          ) >
          0,
        "Historical team archive is not available."
      )
    )
    
    row <- archive[
      archive$season ==
        as.integer(
          input$team_explorer_season
        ) &
        archive$team ==
        input$team_explorer,
      ,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(
          row
        ) ==
          1,
        "Team-season not found."
      )
    )
    
    row[1, , drop = FALSE]
  })
  
  selected_team_weekly_history <- reactive({
    
    req(
      input$team_explorer_season,
      input$team_explorer
    )
    
    season_value <- as.integer(
      input$team_explorer_season
    )
    
    history <- season_weekly_history(
      season_value
    )
    
    validate(
      need(
        !is.null(
          history
        ) &&
          nrow(
            history
          ) >
          0,
        if (
          season_value <=
          1998
        ) {
          legacy_games_info()$message
        } else {
          "Weekly game data are not available for this season."
        }
      )
    )
    
    history[
      history$team ==
        input$team_explorer,
      ,
      drop = FALSE
    ]
  })
  
  output$team_cards <- renderUI({
    
    row <- selected_team_profile()
    
    sos_value <- if (
      "full_SOS" %in%
      names(
        row
      ) &&
      is.finite(
        row$full_SOS
      )
    ) {
      row$full_SOS
    } else if (
      "played_SOS" %in%
      names(
        row
      )
    ) {
      row$played_SOS
    } else {
      NA_real_
    }
    
    sos_rank <- if (
      "full_SOS_rank" %in%
      names(
        row
      ) &&
      is.finite(
        row$full_SOS_rank
      )
    ) {
      row$full_SOS_rank
    } else if (
      "played_SOS_rank" %in%
      names(
        row
      )
    ) {
      row$played_SOS_rank
    } else {
      NA_real_
    }
    
    z_value <- if (
      "season_rating_z" %in%
      names(
        row
      )
    ) {
      row$season_rating_z
    } else {
      NA_real_
    }
    
    div(
      class = "metric-grid",
      metric_card(
        "Power rank",
        paste0(
          "#",
          row$rank
        ),
        paste(
          "Rating",
          fmt_signed(
            row$rating
          )
        )
      ),
      metric_card(
        "Record",
        row$record,
        paste(
          row$season,
          "season"
        )
      ),
      metric_card(
        "Offense",
        paste0(
          "#",
          row$offense_rank
        ),
        paste(
          "Rating",
          fmt_signed(
            row$offense_rating
          )
        )
      ),
      metric_card(
        "Defense",
        paste0(
          "#",
          row$defense_rank
        ),
        paste(
          "Rating",
          fmt_signed(
            row$defense_rating
          )
        )
      ),
      metric_card(
        "Season dominance",
        paste0(
          fmt_signed(
            z_value,
            2
          ),
          " SD"
        ),
        "Relative to that season"
      ),
      metric_card(
        "Schedule strength",
        ifelse(
          is.na(
            sos_rank
          ),
          "—",
          paste0(
            "#",
            sos_rank
          )
        ),
        paste(
          "SOS",
          fmt_signed(
            sos_value
          )
        )
      )
    )
  })
  
  output$team_history_plot <- renderPlot({
    
    req(
      input$team_explorer_season,
      input$team_explorer,
      input$team_metric
    )
    
    data <- selected_team_weekly_history()
    
    metric <- input$team_metric
    
    req(
      metric %in%
        names(
          data
        )
    )
    
    data$value <- data[[metric]]
    
    y_label <- switch(
      metric,
      rating = "Power rating",
      offense_rating = "Offense rating",
      defense_rating = "Defense rating",
      rank = "League rank"
    )
    
    data$value_label <- if (
      metric ==
      "rank"
    ) {
      paste0(
        "#",
        as.integer(
          round(
            data$value
          )
        )
      )
    } else {
      fmt_signed(
        data$value,
        1
      )
    }
    
    p <- ggplot(
      data,
      aes(
        x = week,
        y = value
      )
    ) +
      geom_line(
        linewidth = 1.25,
        color = "#D7263D"
      ) +
      geom_point(
        size = 3.2,
        color = "#D7263D"
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        vjust = -0.85,
        fontface = "bold",
        color = "#0B1F33",
        size = 3.2,
        check_overlap = FALSE
      ) +
      scale_x_continuous(
        breaks = sort(
          unique(
            data$week
          )
        ),
        expand = expansion(
          mult = c(
            0.02,
            0.03
          )
        )
      ) +
      labs(
        title = paste(
          input$team_explorer_season,
          input$team_explorer,
          y_label,
          "by week"
        ),
        subtitle = "Each label is the team's value after that completed week.",
        x = "Week",
        y = y_label
      ) +
      dashboard_plot_theme() +
      theme(
        plot.margin = margin(
          18,
          18,
          10,
          10
        )
      )
    
    if (
      metric ==
      "rank"
    ) {
      
      p <- p +
        scale_y_reverse(
          breaks = pretty(
            data$value
          ),
          expand = expansion(
            mult = c(
              0.10,
              0.14
            )
          )
        )
      
    } else {
      
      p <- p +
        geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "#7A8797",
          alpha = 0.7
        ) +
        scale_y_continuous(
          expand = expansion(
            mult = c(
              0.10,
              0.16
            )
          )
        )
    }
    
    p
  })
  
  output$team_games_table <- renderDT({
    
    req(
      input$team_explorer_season,
      input$team_explorer
    )
    
    schedule <- season_schedule_data(
      as.integer(
        input$team_explorer_season
      )
    )
    
    validate(
      need(
        !is.null(
          schedule
        ) &&
          nrow(
            schedule
          ) >
          0,
        "Game-level schedule data are not available for this season."
      )
    )
    
    data <- schedule[
      schedule$home_team ==
        input$team_explorer |
        schedule$away_team ==
        input$team_explorer,
      ,
      drop = FALSE
    ]
    
    data$opponent <- ifelse(
      data$home_team ==
        input$team_explorer,
      data$away_team,
      data$home_team
    )
    
    data$site <- ifelse(
      data$neutral_site,
      "Neutral",
      ifelse(
        data$home_team ==
          input$team_explorer,
        "Home",
        "Away"
      )
    )
    
    data$result <- ifelse(
      !data$completed,
      "Upcoming",
      ifelse(
        data$home_score ==
          data$away_score,
        "T",
        ifelse(
          (
            data$home_team ==
              input$team_explorer &
              data$home_score >
              data$away_score
          ) |
            (
              data$away_team ==
                input$team_explorer &
                data$away_score >
                data$home_score
            ),
          "W",
          "L"
        )
      )
    )
    
    data$team_score <- ifelse(
      data$completed,
      ifelse(
        data$home_team ==
          input$team_explorer,
        data$home_score,
        data$away_score
      ),
      NA
    )
    
    data$opponent_score <- ifelse(
      data$completed,
      ifelse(
        data$home_team ==
          input$team_explorer,
        data$away_score,
        data$home_score
      ),
      NA
    )
    
    columns <- safe_columns(
      data,
      c(
        "week",
        "opponent",
        "site",
        "result",
        "team_score",
        "opponent_score"
      )
    )
    
    datatable(
      data[
        ,
        columns,
        drop = FALSE
      ],
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
    
    req(
      length(
        input$compare_teams
      ) >=
        1,
      input$compare_metric
    )
    
    validate(
      need(
        length(
          input$compare_teams
        ) <=
          4,
        "Choose up to four team-seasons."
      )
    )
    
    selections <- parse_team_season_ids(
      input$compare_teams
    )
    
    data_storage <- list()
    
    for (
      i in
      seq_len(
        nrow(
          selections
        )
      )
    ) {
      
      season_value <- selections$season[i]
      team_value <- selections$team[i]
      
      history <- season_weekly_history(
        season_value
      )
      
      validate(
        need(
          !is.null(
            history
          ) &&
            nrow(
              history
            ) >
            0,
          if (
            season_value <=
            1998
          ) {
            legacy_games_info()$message
          } else {
            paste(
              "Weekly history is unavailable for",
              season_value
            )
          }
        )
      )
      
      team_data <- history[
        history$team ==
          team_value,
        ,
        drop = FALSE
      ]
      
      validate(
        need(
          nrow(
            team_data
          ) >
            0,
          paste(
            season_value,
            team_value,
            "was not found in the weekly history."
          )
        )
      )
      
      team_data$team_season <- paste(
        season_value,
        team_value
      )
      
      team_data$selection_order <- i
      
      data_storage[[i]] <- team_data
    }
    
    data <- do.call(
      rbind,
      data_storage
    )
    
    metric <- input$compare_metric
    
    req(
      metric %in%
        names(
          data
        )
    )
    
    data$value <- data[[metric]]
    
    y_label <- switch(
      metric,
      rating = "Power rating",
      offense_rating = "Offense rating",
      defense_rating = "Defense rating",
      rank = "League rank"
    )
    
    selected_labels <- paste(
      selections$season,
      selections$team
    )
    
    color_values <- setNames(
      COMPARE_COLORS[
        seq_len(
          nrow(
            selections
          )
        )
      ],
      selected_labels
    )
    
    shape_values <- setNames(
      COMPARE_SHAPES[
        seq_len(
          nrow(
            selections
          )
        )
      ],
      selected_labels
    )
    
    end_rows <- do.call(
      rbind,
      lapply(
        split(
          data,
          data$team_season
        ),
        function(team_data) {
          
          team_data[
            which.max(
              team_data$week
            ),
            ,
            drop = FALSE
          ]
        }
      )
    )
    
    end_rows$end_label <- if (
      metric ==
      "rank"
    ) {
      paste0(
        "#",
        as.integer(
          round(
            end_rows$value
          )
        )
      )
    } else {
      fmt_signed(
        end_rows$value,
        1
      )
    }
    
    p <- ggplot(
      data,
      aes(
        x = week,
        y = value,
        group = team_season,
        color = team_season,
        shape = team_season
      )
    ) +
      geom_line(
        linewidth = 1.2
      ) +
      geom_point(
        size = 2.8
      ) +
      geom_text(
        data = end_rows,
        aes(
          label = end_label
        ),
        hjust = -0.28,
        fontface = "bold",
        size = 3.4,
        show.legend = FALSE
      ) +
      scale_color_manual(
        values = color_values
      ) +
      scale_shape_manual(
        values = shape_values
      ) +
      scale_x_continuous(
        breaks = sort(
          unique(
            data$week
          )
        ),
        expand = expansion(
          mult = c(
            0.02,
            0.10
          )
        )
      ) +
      labs(
        title = paste(
          y_label,
          "comparison"
        ),
        subtitle = "Compare up to four teams from the same or different seasons.",
        x = "Week",
        y = y_label,
        color = "Team-season",
        shape = "Team-season"
      ) +
      dashboard_plot_theme() +
      coord_cartesian(
        clip = "off"
      ) +
      theme(
        legend.position = "bottom",
        plot.margin = margin(
          10,
          48,
          10,
          10
        )
      )
    
    if (
      metric ==
      "rank"
    ) {
      
      p <- p +
        scale_y_reverse()
      
    } else {
      
      p <- p +
        geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "#7A8797",
          alpha = 0.7
        )
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
        pred$favorite,
        " is favored by ",
        fmt_num(pred$spread, 1),
        " points in the point forecast."
      )
    }
    
    tagList(
      p(favorite_text),
      p(
        paste0(
          "Projected score: ",
          input$team_a, " ", fmt_num(pred$team_a_score, 1),
          " to ",
          input$team_b, " ", fmt_num(pred$team_b_score, 1),
          "."
        )
      ),
      p(
        paste0(
          "Win probabilities: ",
          input$team_a, " ", fmt_pct(pred$team_a_prob),
          " / ",
          input$team_b, " ", fmt_pct(pred$team_b_prob),
          "."
        )
      )
    )
  })
  
  output$matchup_details <- renderUI({
    pred <- matchup_prediction()
    row <- pred$row
    
    tagList(
      p(
        paste0(
          "Raw rating margin from Team A's perspective: ",
          fmt_signed(pred$raw_a_margin, 2)
        )
      ),
      p(
        paste0(
          "Forecast margin after early-season shrinkage: ",
          fmt_signed(pred$forecast_a_margin, 2)
        )
      ),
      p(
        paste0(
          "Forecast shrinkage weight: ",
          fmt_num(row$learned_forecast_weight, 3)
        )
      ),
      p(
        paste0(
          "Probability blend weight: ",
          fmt_num(row$fixed_probability_weight, 3)
        )
      ),
      p(
        paste0(
          "Predictive margin SD: ",
          fmt_num(row$predictive_margin_sd, 2)
        )
      ),
      p(
        paste0(
          "Estimated HFA in current rating fit: ",
          fmt_signed(model_env()$current_model$hfa, 2)
        )
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
    
    data$probability_label <- percent(
      data$home_win_probability,
      accuracy = 1
    )
    
    data$home_side <- ifelse(
      data$home_win_probability >= 0.5,
      "Home favored",
      "Away favored"
    )
    
    ggplot(
      data,
      aes(
        x = matchup,
        y = home_win_probability,
        fill = home_side
      )
    ) +
      geom_col(
        width = 0.72
      ) +
      geom_text(
        aes(
          label = probability_label
        ),
        hjust = -0.15,
        fontface = "bold",
        color = "#0B1F33",
        size = 3.4
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0.5,
        linetype = "dashed",
        color = "#536579"
      ) +
      scale_fill_manual(
        values = c(
          "Home favored" = "#2F80ED",
          "Away favored" = "#D7263D"
        ),
        guide = "none"
      ) +
      scale_y_continuous(
        limits = c(0, 1.08),
        breaks = seq(0, 1, by = 0.25),
        labels = percent_format(accuracy = 1)
      ) +
      labs(
        x = NULL,
        y = "Home win probability",
        title = paste("Week", input$forecast_week, "win probabilities"),
        subtitle = "The number at the end of each bar is the home team's win probability."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 34, 10, 18)
      )
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
    
    selected$movement_label <- if (
      input$movers_metric ==
      "rank_change"
    ) {
      ifelse(
        selected$movement > 0,
        paste0("+", as.integer(round(selected$movement))),
        as.character(as.integer(round(selected$movement)))
      )
    } else {
      ifelse(
        selected$movement > 0,
        paste0(
          "+",
          formatC(
            selected$movement,
            digits = 2,
            format = "f"
          )
        ),
        formatC(
          selected$movement,
          digits = 2,
          format = "f"
        )
      )
    }
    
    positive <- selected[
      selected$movement >= 0,
      ,
      drop = FALSE
    ]
    
    negative <- selected[
      selected$movement < 0,
      ,
      drop = FALSE
    ]
    
    p <- ggplot(
      selected,
      aes(
        x = team_factor,
        y = movement,
        fill = movement
      )
    ) +
      geom_col(
        width = 0.74
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0,
        linewidth = 0.5,
        color = "#536579"
      ) +
      scale_fill_gradient2(
        low = "#D7263D",
        mid = "#E7EDF4",
        high = "#27AE60",
        midpoint = 0,
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(0.15, 0.15)
        )
      ) +
      labs(
        x = NULL,
        y = y_label,
        title = paste("Week", input$movers_week, "biggest movers"),
        subtitle = "Green indicates improvement; red indicates decline."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 34, 10, 18)
      )
    
    if (
      nrow(
        positive
      ) >
      0
    ) {
      
      p <- p +
        geom_text(
          data = positive,
          aes(
            label = movement_label
          ),
          hjust = -0.18,
          fontface = "bold",
          color = "#0B1F33",
          size = 3.4
        )
    }
    
    if (
      nrow(
        negative
      ) >
      0
    ) {
      
      p <- p +
        geom_text(
          data = negative,
          aes(
            label = movement_label
          ),
          hjust = 1.18,
          fontface = "bold",
          color = "#0B1F33",
          size = 3.4
        )
    }
    
    p
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
    
    data$sos_label <- ifelse(
      data$sos_value > 0,
      paste0(
        "+",
        formatC(
          data$sos_value,
          digits = 2,
          format = "f"
        )
      ),
      formatC(
        data$sos_value,
        digits = 2,
        format = "f"
      )
    )
    
    positive <- data[
      data$sos_value >= 0,
      ,
      drop = FALSE
    ]
    
    negative <- data[
      data$sos_value < 0,
      ,
      drop = FALSE
    ]
    
    p <- ggplot(
      data,
      aes(
        x = team_factor,
        y = sos_value,
        fill = sos_value
      )
    ) +
      geom_col(
        width = 0.74
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#536579",
        alpha = 0.8
      ) +
      scale_fill_gradient2(
        low = "#27AE60",
        mid = "#E7EDF4",
        high = "#D7263D",
        midpoint = 0,
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(0.15, 0.15)
        )
      ) +
      labs(
        x = NULL,
        y = "Opponent rating / location-adjusted difficulty",
        title = "League schedule difficulty",
        subtitle = "Higher values mean a tougher schedule; red is harder and green is easier."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 34, 10, 18)
      )
    
    if (
      nrow(
        positive
      ) >
      0
    ) {
      
      p <- p +
        geom_text(
          data = positive,
          aes(
            label = sos_label
          ),
          hjust = -0.18,
          fontface = "bold",
          color = "#0B1F33",
          size = 3.2
        )
    }
    
    if (
      nrow(
        negative
      ) >
      0
    ) {
      
      p <- p +
        geom_text(
          data = negative,
          aes(
            label = sos_label
          ),
          hjust = 1.18,
          fontface = "bold",
          color = "#0B1F33",
          size = 3.2
        )
    }
    
    p
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
      dashboard_plot_theme()
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
      dashboard_plot_theme()
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
  # All-Time Explorer
  # ----------------------------------------------------------
  
  all_time_era_choices <- reactive({
    
    archive <- historical_archive()
    
    eras <- sort(
      unique(
        archive$era[
          !is.na(
            archive$era
          )
        ]
      )
    )
    
    c(
      "All eras" = "ALL",
      setNames(
        eras,
        eras
      )
    )
  })
  
  all_time_franchise_choices <- reactive({
    
    archive <- historical_archive()
    
    teams <- sort(
      unique(
        archive$team
      )
    )
    
    c(
      "All franchises" = "ALL",
      setNames(
        teams,
        teams
      )
    )
  })
  
  observeEvent(
    historical_archive(),
    {
      
      era_choices <- all_time_era_choices()
      team_choices <- all_time_franchise_choices()
      
      updateSelectInput(
        session,
        "all_time_team_era",
        choices = era_choices,
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "all_time_schedule_era",
        choices = era_choices,
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "all_time_over_era",
        choices = era_choices,
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "all_time_team_franchise",
        choices = team_choices,
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "all_time_schedule_franchise",
        choices = team_choices,
        selected = "ALL"
      )
    },
    ignoreInit = FALSE
  )
  
  filter_all_time_archive <- function(
    data,
    era_value = "ALL",
    franchise_value = "ALL"
  ) {
    
    out <- data
    
    if (
      !is.null(
        era_value
      ) &&
      era_value !=
      "ALL"
    ) {
      
      out <- out[
        out$era ==
          era_value,
        ,
        drop = FALSE
      ]
    }
    
    if (
      !is.null(
        franchise_value
      ) &&
      franchise_value !=
      "ALL"
    ) {
      
      out <- out[
        out$team ==
          franchise_value,
        ,
        drop = FALSE
      ]
    }
    
    out
  }
  
  apply_top_n <- function(
    data,
    count_value
  ) {
    
    if (
      is.null(
        count_value
      ) ||
      count_value ==
      "all"
    ) {
      
      return(
        data
      )
    }
    
    n_keep <- suppressWarnings(
      as.integer(
        count_value
      )
    )
    
    if (
      is.na(
        n_keep
      )
    ) {
      
      return(
        data
      )
    }
    
    data[
      seq_len(
        min(
          n_keep,
          nrow(
            data
          )
        )
      ),
      ,
      drop = FALSE
    ]
  }
  
  all_time_team_data <- reactive({
    
    archive <- historical_archive()
    
    data <- filter_all_time_archive(
      archive,
      input$all_time_team_era,
      input$all_time_team_franchise
    )
    
    req(
      input$all_time_team_category,
      input$all_time_team_method
    )
    
    category <- input$all_time_team_category
    method <- input$all_time_team_method
    
    value_column <- if (
      category ==
      "overall"
    ) {
      
      if (
        method ==
        "raw"
      ) {
        "rating"
      } else {
        "season_rating_z"
      }
      
    } else if (
      category ==
      "offense"
    ) {
      
      if (
        method ==
        "raw"
      ) {
        "offense_rating"
      } else {
        "season_offense_z"
      }
      
    } else {
      
      if (
        method ==
        "raw"
      ) {
        "defense_rating"
      } else {
        "season_defense_z"
      }
    }
    
    validate(
      need(
        value_column %in%
          names(
            data
          ),
        "The selected historical metric is not available."
      )
    )
    
    data$leaderboard_value <- data[[value_column]]
    
    data <- data[
      is.finite(
        data$leaderboard_value
      ),
      ,
      drop = FALSE
    ]
    
    data <- data[
      order(
        -data$leaderboard_value,
        data$season,
        data$team
      ),
      ,
      drop = FALSE
    ]
    
    data$all_time_display_rank <- seq_len(
      nrow(
        data
      )
    )
    
    apply_top_n(
      data,
      input$all_time_team_count
    )
  })
  
  output$all_time_team_cards <- renderUI({
    
    data <- all_time_team_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No historical teams match these filters."
      )
    )
    
    leader <- data[1, , drop = FALSE]
    
    value_label <- if (
      input$all_time_team_method ==
      "raw"
    ) {
      fmt_signed(
        leader$leaderboard_value,
        2
      )
    } else {
      paste0(
        fmt_signed(
          leader$leaderboard_value,
          2
        ),
        " SD"
      )
    }
    
    div(
      class = "metric-grid",
      metric_card(
        "Leader",
        paste(
          leader$season,
          leader$team
        ),
        paste(
          leader$record,
          "·",
          value_label
        )
      ),
      metric_card(
        "Team-seasons shown",
        format(
          nrow(
            data
          ),
          big.mark = ","
        )
      ),
      metric_card(
        "Best raw rating",
        fmt_signed(
          max(
            data$rating,
            na.rm = TRUE
          ),
          2
        )
      ),
      metric_card(
        "Best season dominance",
        paste0(
          fmt_signed(
            max(
              data$season_rating_z,
              na.rm = TRUE
            ),
            2
          ),
          " SD"
        )
      )
    )
  })
  
  output$all_time_team_plot <- renderPlot({
    
    data <- all_time_team_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No historical teams match these filters."
      )
    )
    
    plot_data <- data
    
    if (
      nrow(
        plot_data
      ) >
      50
    ) {
      
      plot_data <- plot_data[
        seq_len(
          50
        ),
        ,
        drop = FALSE
      ]
    }
    
    plot_data$team_season <- paste(
      plot_data$season,
      plot_data$team
    )
    
    plot_data$team_factor <- factor(
      plot_data$team_season,
      levels = rev(
        plot_data$team_season
      )
    )
    
    plot_data$value_label <- if (
      input$all_time_team_method ==
      "raw"
    ) {
      fmt_signed(
        plot_data$leaderboard_value,
        2
      )
    } else {
      paste0(
        fmt_signed(
          plot_data$leaderboard_value,
          2
        ),
        " SD"
      )
    }
    
    x_label <- if (
      input$all_time_team_method ==
      "raw"
    ) {
      "Rating"
    } else {
      "Within-season standard deviations"
    }
    
    ggplot(
      plot_data,
      aes(
        x = team_factor,
        y = leaderboard_value,
        fill = leaderboard_value
      )
    ) +
      geom_col(
        width = 0.74
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        hjust = -0.15,
        fontface = "bold",
        color = "#0B1F33",
        size = 3.1
      ) +
      coord_flip(
        clip = "off"
      ) +
      scale_fill_gradient(
        low = "#B7D5F7",
        high = "#2F80ED",
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.02,
            0.18
          )
        )
      ) +
      labs(
        x = NULL,
        y = x_label,
        title = paste(
          "All-time",
          tolower(
            input$all_time_team_category
          ),
          "leaders"
        ),
        subtitle = if (
          nrow(
            data
          ) >
          50
        ) {
          "The chart shows the first 50 rows; the table below contains the full selected leaderboard."
        } else {
          "Select a row in the table to send that team-season elsewhere in the app."
        }
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          58,
          10,
          18
        )
      )
  })
  
  output$all_time_team_table <- renderDT({
    
    data <- all_time_team_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "all_time_display_rank",
          "season",
          "era",
          "team",
          "record",
          "rating",
          "season_rating_z",
          "offense_rating",
          "season_offense_z",
          "defense_rating",
          "season_defense_z",
          "point_differential_per_game",
          "played_SOS",
          "rank",
          "historical_raw_rank",
          "historical_standardized_rank"
        )
      ),
      drop = FALSE
    ]
    
    names(
      out
    )[
      names(
        out
      ) ==
        "all_time_display_rank"
    ] <- "display_rank"
    
    out <- round_numeric_columns(
      out,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(
          list(
            0,
            "asc"
          )
        )
      )
    )
  })
  
  all_time_schedule_data <- reactive({
    
    archive <- historical_archive()
    
    data <- filter_all_time_archive(
      archive,
      input$all_time_schedule_era,
      input$all_time_schedule_franchise
    )
    
    req(
      input$all_time_schedule_metric
    )
    
    metric <- input$all_time_schedule_metric
    
    if (
      metric ==
      "hardest"
    ) {
      
      data$schedule_value <- data$played_SOS
      data <- data[
        order(
          -data$schedule_value
        ),
        ,
        drop = FALSE
      ]
      
    } else if (
      metric ==
      "easiest"
    ) {
      
      data$schedule_value <- data$played_SOS
      data <- data[
        order(
          data$schedule_value
        ),
        ,
        drop = FALSE
      ]
      
    } else if (
      metric ==
      "positive_adjustment"
    ) {
      
      data$schedule_value <- data$schedule_adjustment
      data <- data[
        order(
          -data$schedule_value
        ),
        ,
        drop = FALSE
      ]
      
    } else {
      
      data$schedule_value <- data$schedule_adjustment
      data <- data[
        order(
          data$schedule_value
        ),
        ,
        drop = FALSE
      ]
    }
    
    data <- data[
      is.finite(
        data$schedule_value
      ),
      ,
      drop = FALSE
    ]
    
    data$all_time_display_rank <- seq_len(
      nrow(
        data
      )
    )
    
    apply_top_n(
      data,
      input$all_time_schedule_count
    )
  })
  
  output$all_time_schedule_cards <- renderUI({
    
    data <- all_time_schedule_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No schedules match these filters."
      )
    )
    
    leader <- data[1, , drop = FALSE]
    
    div(
      class = "metric-grid",
      metric_card(
        "First in view",
        paste(
          leader$season,
          leader$team
        ),
        paste(
          leader$record,
          "·",
          fmt_signed(
            leader$schedule_value,
            2
          )
        )
      ),
      metric_card(
        "Played SOS",
        fmt_signed(
          leader$played_SOS,
          2
        )
      ),
      metric_card(
        "Rating adjustment",
        fmt_signed(
          leader$schedule_adjustment,
          2
        )
      ),
      metric_card(
        "Team rating",
        fmt_signed(
          leader$rating,
          2
        )
      )
    )
  })
  
  output$all_time_schedule_plot <- renderPlot({
    
    data <- all_time_schedule_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No schedules match these filters."
      )
    )
    
    plot_data <- data
    
    if (
      nrow(
        plot_data
      ) >
      50
    ) {
      
      plot_data <- plot_data[
        seq_len(
          50
        ),
        ,
        drop = FALSE
      ]
    }
    
    plot_data$team_season <- paste(
      plot_data$season,
      plot_data$team
    )
    
    plot_data$team_factor <- factor(
      plot_data$team_season,
      levels = rev(
        plot_data$team_season
      )
    )
    
    plot_data$value_label <- fmt_signed(
      plot_data$schedule_value,
      2
    )
    
    colors <- if (
      input$all_time_schedule_metric ==
      "easiest"
    ) {
      c(
        low = "#DDF3E6",
        high = "#27AE60"
      )
    } else if (
      input$all_time_schedule_metric %in%
      c(
        "hardest"
      )
    ) {
      c(
        low = "#FAD4DA",
        high = "#D7263D"
      )
    } else {
      c(
        low = "#D9EAFE",
        high = "#2F80ED"
      )
    }
    
    ggplot(
      plot_data,
      aes(
        x = team_factor,
        y = schedule_value,
        fill = abs(
          schedule_value
        )
      )
    ) +
      geom_col(
        width = 0.74
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        hjust = ifelse(
          plot_data$schedule_value >=
            0,
          -0.15,
          1.15
        ),
        color = "#0B1F33",
        fontface = "bold",
        size = 3.1
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0,
        color = "#536579",
        linewidth = 0.45
      ) +
      scale_fill_gradient(
        low = colors["low"],
        high = colors["high"],
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.18,
            0.18
          )
        )
      ) +
      labs(
        x = NULL,
        y = if (
          input$all_time_schedule_metric %in%
          c(
            "hardest",
            "easiest"
          )
        ) {
          "Played SOS"
        } else {
          "Rating minus point differential/game"
        },
        title = switch(
          input$all_time_schedule_metric,
          hardest = "Hardest historical schedules",
          easiest = "Easiest historical schedules",
          positive_adjustment = "Largest positive rating adjustments",
          negative_adjustment = "Largest negative rating adjustments"
        )
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          58,
          10,
          18
        )
      )
  })
  
  output$all_time_schedule_table <- renderDT({
    
    data <- all_time_schedule_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "all_time_display_rank",
          "season",
          "era",
          "team",
          "record",
          "played_SOS",
          "played_SOS_rank",
          "schedule_adjustment",
          "rating",
          "point_differential_per_game",
          "rank",
          "historical_raw_rank"
        )
      ),
      drop = FALSE
    ]
    
    names(
      out
    )[
      names(
        out
      ) ==
        "all_time_display_rank"
    ] <- "display_rank"
    
    out <- round_numeric_columns(
      out,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  all_time_over_data <- reactive({
    
    archive <- historical_archive()
    
    data <- filter_all_time_archive(
      archive,
      input$all_time_over_era,
      "ALL"
    )
    
    req(
      input$all_time_over_metric,
      input$all_time_over_direction
    )
    
    if (
      input$all_time_over_metric ==
      "record_gap"
    ) {
      
      # Positive record_rating_gap means the model ranks the team
      # better than its record rank. Negative means the record rank
      # looks better than the model rank.
      data$gap_value <- data$record_rating_gap
      
      if (
        input$all_time_over_direction ==
        "results_better"
      ) {
        
        data <- data[
          order(
            data$gap_value
          ),
          ,
          drop = FALSE
        ]
        
      } else {
        
        data <- data[
          order(
            -data$gap_value
          ),
          ,
          drop = FALSE
        ]
      }
      
    } else {
      
      data$gap_value <- data$schedule_adjustment
      
      if (
        input$all_time_over_direction ==
        "results_better"
      ) {
        
        data <- data[
          order(
            data$gap_value
          ),
          ,
          drop = FALSE
        ]
        
      } else {
        
        data <- data[
          order(
            -data$gap_value
          ),
          ,
          drop = FALSE
        ]
      }
    }
    
    data <- data[
      is.finite(
        data$gap_value
      ),
      ,
      drop = FALSE
    ]
    
    data$all_time_display_rank <- seq_len(
      nrow(
        data
      )
    )
    
    apply_top_n(
      data,
      input$all_time_over_count
    )
  })
  
  output$all_time_over_cards <- renderUI({
    
    data <- all_time_over_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No team-seasons match these filters."
      )
    )
    
    leader <- data[1, , drop = FALSE]
    
    div(
      class = "metric-grid",
      metric_card(
        "Largest gap",
        paste(
          leader$season,
          leader$team
        ),
        paste(
          leader$record,
          "· gap",
          fmt_signed(
            leader$gap_value,
            2
          )
        )
      ),
      metric_card(
        "Model rank",
        paste0(
          "#",
          leader$rank
        )
      ),
      metric_card(
        "Record rank",
        paste0(
          "#",
          leader$record_rank
        )
      ),
      metric_card(
        "Schedule adjustment",
        fmt_signed(
          leader$schedule_adjustment,
          2
        )
      )
    )
  })
  
  output$all_time_over_plot <- renderPlot({
    
    data <- all_time_over_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No team-seasons match these filters."
      )
    )
    
    plot_data <- data
    
    if (
      nrow(
        plot_data
      ) >
      50
    ) {
      
      plot_data <- plot_data[
        seq_len(
          50
        ),
        ,
        drop = FALSE
      ]
    }
    
    plot_data$team_season <- paste(
      plot_data$season,
      plot_data$team
    )
    
    plot_data$team_factor <- factor(
      plot_data$team_season,
      levels = rev(
        plot_data$team_season
      )
    )
    
    plot_data$value_label <- if (
      input$all_time_over_metric ==
      "record_gap"
    ) {
      ifelse(
        plot_data$gap_value >
          0,
        paste0(
          "+",
          as.integer(
            plot_data$gap_value
          )
        ),
        as.character(
          as.integer(
            plot_data$gap_value
          )
        )
      )
    } else {
      fmt_signed(
        plot_data$gap_value,
        2
      )
    }
    
    ggplot(
      plot_data,
      aes(
        x = team_factor,
        y = gap_value,
        fill = gap_value
      )
    ) +
      geom_col(
        width = 0.74
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        hjust = ifelse(
          plot_data$gap_value >=
            0,
          -0.15,
          1.15
        ),
        color = "#0B1F33",
        fontface = "bold",
        size = 3.1
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0,
        color = "#536579",
        linewidth = 0.45
      ) +
      scale_fill_gradient2(
        low = "#D7263D",
        mid = "#E7EDF4",
        high = "#2F80ED",
        midpoint = 0,
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.18,
            0.18
          )
        )
      ) +
      labs(
        x = NULL,
        y = if (
          input$all_time_over_metric ==
          "record_gap"
        ) {
          "Record rank minus model rank"
        } else {
          "Rating minus point differential/game"
        },
        title = if (
          input$all_time_over_metric ==
          "record_gap"
        ) {
          "Record rank vs model rank"
        } else {
          "Rating vs point differential"
        },
        subtitle = if (
          input$all_time_over_direction ==
          "results_better"
        ) {
          "These teams' records/results looked stronger than the model rating."
        } else {
          "These teams' model ratings looked stronger than the raw results."
        }
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          58,
          10,
          18
        )
      )
  })
  
  output$all_time_over_table <- renderDT({
    
    data <- all_time_over_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "all_time_display_rank",
          "season",
          "era",
          "team",
          "record",
          "win_pct",
          "record_rank",
          "rank",
          "record_rating_gap",
          "rating",
          "point_differential_per_game",
          "schedule_adjustment",
          "played_SOS",
          "historical_raw_rank",
          "historical_standardized_rank"
        )
      ),
      drop = FALSE
    ]
    
    names(
      out
    )[
      names(
        out
      ) ==
        "all_time_display_rank"
    ] <- "display_rank"
    
    out <- round_numeric_columns(
      out,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  all_time_time_data <- reactive({
    
    archive <- historical_archive()
    
    season_values <- sort(
      unique(
        archive$season
      )
    )
    
    storage <- vector(
      "list",
      length(
        season_values
      )
    )
    
    for (
      i in
      seq_along(
        season_values
      )
    ) {
      
      season_value <- season_values[i]
      
      season_data <- archive[
        archive$season ==
          season_value,
        ,
        drop = FALSE
      ]
      
      valid_pd <- (
        is.finite(
          season_data$rating
        ) &
          is.finite(
            season_data$point_differential_per_game
          )
      )
      
      rating_pd_cor <- if (
        sum(
          valid_pd
        ) >=
        3
      ) {
        cor(
          season_data$rating[
            valid_pd
          ],
          season_data$point_differential_per_game[
            valid_pd
          ]
        )
      } else {
        NA_real_
      }
      
      rating_sd <- sd(
        season_data$rating,
        na.rm = TRUE
      )
      
      storage[[i]] <- data.frame(
        season =
          season_value,
        era =
          as.character(
            season_data$era[1]
          ),
        teams =
          nrow(
            season_data
          ),
        league_mean_points =
          season_data$league_mean_points[1],
        estimated_HFA =
          season_data$estimated_HFA[1],
        rating_sd =
          rating_sd,
        parity_index =
          ifelse(
            is.finite(
              rating_sd
            ) &&
              rating_sd >
              0,
            1 /
              rating_sd,
            NA_real_
          ),
        rating_point_diff_correlation =
          rating_pd_cor,
        predictive_margin_sd =
          season_data$predictive_margin_sd[1],
        top_team =
          season_data$team[
            which.max(
              season_data$rating
            )
          ],
        top_rating =
          max(
            season_data$rating,
            na.rm = TRUE
          ),
        stringsAsFactors =
          FALSE
      )
    }
    
    do.call(
      rbind,
      storage
    )
  })
  
  output$all_time_time_cards <- renderUI({
    
    data <- all_time_time_data()
    
    latest <- data[
      nrow(
        data
      ),
      ,
      drop = FALSE
    ]
    
    highest_scoring <- data[
      which.max(
        data$league_mean_points
      ),
      ,
      drop = FALSE
    ]
    
    highest_hfa <- data[
      which.max(
        data$estimated_HFA
      ),
      ,
      drop = FALSE
    ]
    
    most_parity <- data[
      which.max(
        data$parity_index
      ),
      ,
      drop = FALSE
    ]
    
    div(
      class = "metric-grid",
      metric_card(
        "Latest season",
        latest$season,
        paste(
          latest$top_team,
          "top rating",
          fmt_signed(
            latest$top_rating,
            2
          )
        )
      ),
      metric_card(
        "Highest scoring season",
        highest_scoring$season,
        paste(
          fmt_num(
            highest_scoring$league_mean_points,
            2
          ),
          "points/team"
        )
      ),
      metric_card(
        "Largest HFA",
        highest_hfa$season,
        paste(
          fmt_signed(
            highest_hfa$estimated_HFA,
            2
          ),
          "points"
        )
      ),
      metric_card(
        "Most parity",
        most_parity$season,
        paste(
          "Rating SD",
          fmt_num(
            most_parity$rating_sd,
            2
          )
        )
      )
    )
  })
  
  output$all_time_time_plot <- renderPlot({
    
    data <- all_time_time_data()
    
    req(
      input$all_time_time_metric
    )
    
    metric <- input$all_time_time_metric
    
    if (
      metric ==
      "scoring"
    ) {
      
      data$value <- data$league_mean_points
      y_label <- "League points per team"
      title <- "NFL scoring environment"
      subtitle <- "Average points scored per team, by season."
      
    } else if (
      metric ==
      "hfa"
    ) {
      
      data$value <- data$estimated_HFA
      y_label <- "Estimated home-field advantage"
      title <- "Home-field advantage through time"
      subtitle <- "Estimated home-field advantage in points."
      
    } else if (
      metric ==
      "rating_sd"
    ) {
      
      data$value <- data$rating_sd
      y_label <- "Within-season rating SD"
      title <- "Rating spread through time"
      subtitle <- "Higher values indicate a wider spread between strong and weak teams."
      
    } else if (
      metric ==
      "parity"
    ) {
      
      data$value <- data$parity_index
      y_label <- "Parity index (1 / rating SD)"
      title <- "NFL parity through time"
      subtitle <- "Higher values indicate a tighter distribution of team strength."
      
    } else {
      
      data$value <- data$rating_point_diff_correlation
      y_label <- "Correlation"
      title <- "Model rating vs point differential"
      subtitle <- "Season-level correlation between team rating and point differential per game."
    }
    
    label_rows <- data[
      data$season %%
        5 ==
        0 |
        data$season ==
        min(
          data$season
        ) |
        data$season ==
        max(
          data$season
        ),
      ,
      drop = FALSE
    ]
    
    label_rows$value_label <- fmt_num(
      label_rows$value,
      2
    )
    
    ggplot(
      data,
      aes(
        x = season,
        y = value
      )
    ) +
      geom_line(
        linewidth = 1.1,
        color = "#2F80ED"
      ) +
      geom_point(
        size = 2.4,
        color = "#2F80ED"
      ) +
      geom_text(
        data = label_rows,
        aes(
          label = value_label
        ),
        vjust = -0.8,
        fontface = "bold",
        color = "#0B1F33",
        size = 3
      ) +
      scale_x_continuous(
        breaks = seq(
          min(
            data$season
          ),
          max(
            data$season
          ),
          by = 5
        )
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.08,
            0.14
          )
        )
      ) +
      labs(
        x = "Season",
        y = y_label,
        title = title,
        subtitle = paste0(
          subtitle,
          if (
            max(
              data$season
            ) ==
            model_env()$production_season
          ) {
            paste0(
              " ",
              max(
                data$season
              ),
              " reflects the current through-week snapshot."
            )
          } else {
            ""
          }
        )
      ) +
      dashboard_plot_theme()
  })
  
  output$all_time_time_table <- renderDT({
    
    data <- all_time_time_data()
    
    out <- round_numeric_columns(
      data,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        order = list(
          list(
            0,
            "desc"
          )
        )
      )
    )
  })
  
  selected_all_time_team <- reactive({
    
    req(
      input$all_time_view
    )
    
    if (
      input$all_time_view ==
      "teams"
    ) {
      
      data <- all_time_team_data()
      selected <- input$all_time_team_table_rows_selected
      
    } else if (
      input$all_time_view ==
      "schedules"
    ) {
      
      data <- all_time_schedule_data()
      selected <- input$all_time_schedule_table_rows_selected
      
    } else if (
      input$all_time_view ==
      "over_under"
    ) {
      
      data <- all_time_over_data()
      selected <- input$all_time_over_table_rows_selected
      
    } else {
      
      return(
        NULL
      )
    }
    
    if (
      is.null(
        selected
      ) ||
      length(
        selected
      ) ==
      0
    ) {
      
      return(
        NULL
      )
    }
    
    row_index <- selected[1]
    
    if (
      row_index <
      1 ||
      row_index >
      nrow(
        data
      )
    ) {
      
      return(
        NULL
      )
    }
    
    data[
      row_index,
      ,
      drop = FALSE
    ]
  })
  
  output$all_time_selected_label <- renderUI({
    
    selected <- selected_all_time_team()
    
    if (
      is.null(
        selected
      )
    ) {
      
      return(
        span(
          "Select one team-season from the table above to use it elsewhere in the app."
        )
      )
    }
    
    span(
      strong(
        paste(
          selected$season,
          selected$team
        )
      ),
      " · ",
      selected$record,
      " · rating ",
      fmt_signed(
        selected$rating,
        2
      )
    )
  })
  
  observeEvent(
    input$all_time_open_team,
    {
      
      selected <- selected_all_time_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a team-season first."
        )
      )
      
      archive <- historical_archive()
      
      season_value <- as.integer(
        selected$season
      )
      
      season_teams <- sort(
        archive$team[
          archive$season ==
            season_value
        ]
      )
      
      updateSelectInput(
        session,
        "team_explorer_season",
        choices = sort(
          unique(
            archive$season
          ),
          decreasing = TRUE
        ),
        selected = season_value
      )
      
      updateSelectInput(
        session,
        "team_explorer",
        choices = season_teams,
        selected = selected$team
      )
      
      updateTabsetPanel(
        session,
        "main_nav",
        selected = "Team Explorer"
      )
    }
  )
  
  observeEvent(
    input$all_time_add_compare,
    {
      
      selected <- selected_all_time_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a team-season first."
        )
      )
      
      new_id <- make_team_season_id(
        selected$season,
        selected$team
      )
      
      current <- input$compare_teams
      
      if (
        is.null(
          current
        )
      ) {
        
        current <- character(0)
      }
      
      if (
        new_id %in%
        current
      ) {
        
        showNotification(
          "That team-season is already in Compare Teams.",
          type = "message"
        )
        
      } else if (
        length(
          current
        ) >=
        4
      ) {
        
        showNotification(
          "Compare Teams already has four selections. Remove one before adding another.",
          type = "warning"
        )
        
      } else {
        
        updateSelectizeInput(
          session,
          "compare_teams",
          selected = c(
            current,
            new_id
          ),
          server = TRUE
        )
      }
      
      updateTabsetPanel(
        session,
        "main_nav",
        selected = "Compare Teams"
      )
    }
  )
  
  set_historical_matchup_side <- function(
    side,
    selected
  ) {
    
    archive <- historical_archive()
    
    season_value <- as.integer(
      selected$season
    )
    
    teams <- sort(
      archive$team[
        archive$season ==
          season_value
      ]
    )
    
    if (
      side ==
      "a"
    ) {
      
      updateSelectInput(
        session,
        "hist_a_season",
        choices = sort(
          unique(
            archive$season
          ),
          decreasing = TRUE
        ),
        selected = season_value
      )
      
      updateSelectInput(
        session,
        "hist_a_team",
        choices = teams,
        selected = selected$team
      )
      
    } else {
      
      updateSelectInput(
        session,
        "hist_b_season",
        choices = sort(
          unique(
            archive$season
          ),
          decreasing = TRUE
        ),
        selected = season_value
      )
      
      updateSelectInput(
        session,
        "hist_b_team",
        choices = teams,
        selected = selected$team
      )
    }
    
    updateTabsetPanel(
      session,
      "main_nav",
      selected = "Historical Matchup"
    )
  }
  
  observeEvent(
    input$all_time_use_a,
    {
      
      selected <- selected_all_time_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a team-season first."
        )
      )
      
      set_historical_matchup_side(
        "a",
        selected
      )
    }
  )
  
  observeEvent(
    input$all_time_use_b,
    {
      
      selected <- selected_all_time_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a team-season first."
        )
      )
      
      set_historical_matchup_side(
        "b",
        selected
      )
    }
  )
  
  # ----------------------------------------------------------
  # Playoffs
  # ----------------------------------------------------------
  
  playoff_master <- reactive({
    
    data <- playoff_data()$master
    
    validate(
      need(
        !is.null(
          data
        ) &&
          nrow(
            data
          ) >
          0,
        "Playoff team-season master data are not available. Run the playoff pipeline first."
      )
    )
    
    data
  })
  
  output$playoff_data_status <- renderUI({
    
    info <- playoff_archive_info()
    data <- info$data
    
    master_rows <- if (
      !is.null(
        data$master
      )
    ) {
      nrow(
        data$master
      )
    } else {
      0
    }
    
    game_rows <- if (
      !is.null(
        data$games
      )
    ) {
      nrow(
        data$games
      )
    } else {
      0
    }
    
    if (
      master_rows ==
      0
    ) {
      
      return(
        div(
          class = "panel-card",
          div(
            class = "section-title",
            "Playoff data"
          ),
          p(
            info$message
          ),
          p(
            "Expected output directory: ~/Downloads/nfl_playoff_analysis_1970_2025"
          )
        )
      )
    }
    
    div(
      class = "metric-grid",
      metric_card(
        "Playoff seasons",
        paste0(
          min(
            data$master$season,
            na.rm = TRUE
          ),
          "–",
          max(
            data$master$season,
            na.rm = TRUE
          )
        )
      ),
      metric_card(
        "Playoff team-seasons",
        format(
          master_rows,
          big.mark = ","
        )
      ),
      metric_card(
        "Playoff games",
        if (
          game_rows >
          0
        ) {
          format(
            game_rows,
            big.mark = ","
          )
        } else {
          "—"
        }
      ),
      metric_card(
        "Probability model",
        "Expected margin",
        "Equal teams on a neutral field = 50/50"
      )
    )
  })
  
  observeEvent(
    playoff_archive_info(),
    {
      
      data <- playoff_data()
      master <- data$master
      
      if (
        is.null(
          master
        ) ||
        nrow(
          master
        ) ==
        0
      ) {
        return()
      }
      
      seasons <- sort(
        unique(
          master$season
        ),
        decreasing = TRUE
      )
      
      latest <- max(
        seasons,
        na.rm = TRUE
      )
      
      updateSelectInput(
        session,
        "playoff_team_season",
        choices = seasons,
        selected = latest
      )
      
      updateSelectInput(
        session,
        "playoff_odds_season",
        choices = seasons,
        selected = latest
      )
      
      updateSelectInput(
        session,
        "playoff_rating_season",
        choices = seasons,
        selected = latest
      )
    },
    ignoreInit = FALSE
  )
  
  observeEvent(
    input$playoff_rating_season,
    {
      
      data <- playoff_data()$trajectories
      
      if (
        is.null(
          data
        ) ||
        nrow(
          data
        ) ==
        0
      ) {
        return()
      }
      
      season_value <- suppressWarnings(
        as.integer(
          input$playoff_rating_season
        )
      )
      
      teams <- sort(
        unique(
          data$team[
            data$season ==
              season_value
          ]
        )
      )
      
      selected_team <- if (
        length(
          teams
        ) >
        0
      ) {
        teams[1]
      } else {
        character(0)
      }
      
      updateSelectInput(
        session,
        "playoff_rating_team",
        choices = teams,
        selected = selected_team
      )
    },
    ignoreInit = FALSE
  )
  
  playoff_team_data <- reactive({
    
    data <- playoff_master()
    
    req(
      input$playoff_team_season,
      input$playoff_team_metric
    )
    
    season_value <- as.integer(
      input$playoff_team_season
    )
    
    data <- data[
      data$season ==
        season_value,
      ,
      drop = FALSE
    ]
    
    metric <- input$playoff_team_metric
    
    validate(
      need(
        metric %in%
          names(
            data
          ),
        paste0(
          "The playoff master is missing ",
          metric,
          ". Rerun scripts 02–06 from the corrected playoff pipeline."
        )
      )
    )
    
    data$display_value <- suppressWarnings(
      as.numeric(
        data[[metric]]
      )
    )
    
    data <- data[
      is.finite(
        data$display_value
      ),
      ,
      drop = FALSE
    ]
    
    data[
      order(
        -data$display_value,
        data$team
      ),
      ,
      drop = FALSE
    ]
  })
  
  output$playoff_team_cards <- renderUI({
    
    data <- playoff_team_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No playoff teams are available for this season and metric."
      )
    )
    
    champion <- data[
      data$champion ==
        1,
      ,
      drop = FALSE
    ]
    
    top_rating <- data[
      which.max(
        data$pre_playoff_rating
      ),
      ,
      drop = FALSE
    ]
    
    favorite <- data[
      which.max(
        data$super_bowl_win_probability_retrospective
      ),
      ,
      drop = FALSE
    ]
    
    best_performance <- data[
      which.max(
        data$total_margin_vs_expectation
      ),
      ,
      drop = FALSE
    ]
    
    div(
      class = "metric-grid",
      metric_card(
        "Champion",
        if (
          nrow(
            champion
          ) ==
          1
        ) {
          champion$team
        } else {
          "—"
        },
        if (
          nrow(
            champion
          ) ==
          1
        ) {
          champion$record
        } else {
          NULL
        }
      ),
      metric_card(
        "Top pre-playoff rating",
        top_rating$team,
        fmt_signed(
          top_rating$pre_playoff_rating,
          2
        )
      ),
      metric_card(
        "Pre-playoff favorite",
        favorite$team,
        fmt_pct(
          favorite$super_bowl_win_probability_retrospective,
          1
        )
      ),
      metric_card(
        "Best vs expectation",
        best_performance$team,
        paste(
          fmt_signed(
            best_performance$total_margin_vs_expectation,
            1
          ),
          "points"
        )
      )
    )
  })
  
  output$playoff_team_plot <- renderPlot({
    
    data <- playoff_team_data()
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      return(
        empty_plot(
          "No playoff team data are available."
        )
      )
    }
    
    metric <- input$playoff_team_metric
    
    metric_labels <- c(
      pre_playoff_rating =
        "Pre-playoff rating",
      super_bowl_win_probability_retrospective =
        "Super Bowl probability",
      playoff_wins =
        "Playoff wins",
      total_margin_vs_expectation =
        "Total margin vs expectation",
      playoff_rating_change =
        "Postseason rating change"
    )
    
    data$team_factor <- factor(
      data$team,
      levels = rev(
        data$team
      )
    )
    
    data$champion_label <- ifelse(
      data$champion ==
        1,
      "Champion",
      "Other playoff team"
    )
    
    is_probability <- (
      metric ==
        "super_bowl_win_probability_retrospective"
    )
    
    data$value_label <- if (
      is_probability
    ) {
      fmt_pct(
        data$display_value,
        1
      )
    } else if (
      metric ==
      "playoff_wins"
    ) {
      as.character(
        as.integer(
          data$display_value
        )
      )
    } else {
      fmt_signed(
        data$display_value,
        2
      )
    }
    
    p <- ggplot(
      data,
      aes(
        x = team_factor,
        y = display_value,
        fill = champion_label
      )
    ) +
      geom_col(
        width = 0.72
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        hjust = ifelse(
          data$display_value >=
            0,
          -0.15,
          1.15
        ),
        color = "#0B1F33",
        fontface = "bold",
        size = 3.2
      ) +
      coord_flip(
        clip = "off"
      ) +
      scale_fill_manual(
        values = c(
          "Champion" = "#F2A71B",
          "Other playoff team" = "#2F80ED"
        )
      ) +
      labs(
        x = NULL,
        y = unname(
          metric_labels[
            metric
          ]
        ),
        title = paste(
          input$playoff_team_season,
          "playoff field"
        ),
        subtitle = "The Super Bowl champion is highlighted in gold.",
        fill = NULL
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(
          10,
          55,
          10,
          18
        )
      )
    
    if (
      is_probability
    ) {
      
      p <- p +
        scale_y_continuous(
          labels = percent_format(
            accuracy = 1
          ),
          expand = expansion(
            mult = c(
              0.02,
              0.18
            )
          )
        )
      
    } else {
      
      p <- p +
        scale_y_continuous(
          expand = expansion(
            mult = c(
              0.18,
              0.18
            )
          )
        )
    }
    
    p
  })
  
  output$playoff_team_table <- renderDT({
    
    data <- playoff_team_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "season",
          "team",
          "conference",
          "seed",
          "record",
          "rank",
          "pre_playoff_rating",
          "season_rating_z",
          "super_bowl_win_probability_retrospective",
          "playoff_wins",
          "playoff_losses",
          "playoff_finish",
          "total_margin_vs_expectation",
          "average_margin_vs_expectation",
          "playoff_rating_change",
          "post_playoff_rating",
          "champion"
        )
      ),
      drop = FALSE
    ]
    
    out <- round_numeric_columns(
      out,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
  playoff_selected_team <- reactive({
    
    data <- playoff_team_data()
    selected <- input$playoff_team_table_rows_selected
    
    if (
      is.null(
        selected
      ) ||
      length(
        selected
      ) ==
      0
    ) {
      
      return(
        NULL
      )
    }
    
    row_index <- selected[1]
    
    if (
      row_index <
      1 ||
      row_index >
      nrow(
        data
      )
    ) {
      
      return(
        NULL
      )
    }
    
    data[
      row_index,
      ,
      drop = FALSE
    ]
  })
  
  output$playoff_selected_label <- renderUI({
    
    selected <- playoff_selected_team()
    
    if (
      is.null(
        selected
      )
    ) {
      
      return(
        span(
          "Select a team-season from the playoff table above to use it elsewhere in the app."
        )
      )
    }
    
    span(
      strong(
        paste(
          selected$season,
          selected$team
        )
      ),
      " · ",
      selected$record,
      " · pre-playoff rating ",
      fmt_signed(
        selected$pre_playoff_rating,
        2
      )
    )
  })
  
  observeEvent(
    input$playoff_open_team,
    {
      
      selected <- playoff_selected_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a playoff team-season first."
        )
      )
      
      archive <- historical_archive()
      season_value <- as.integer(
        selected$season
      )
      
      season_teams <- sort(
        archive$team[
          archive$season ==
            season_value
        ]
      )
      
      updateSelectInput(
        session,
        "team_explorer_season",
        choices = sort(
          unique(
            archive$season
          ),
          decreasing = TRUE
        ),
        selected = season_value
      )
      
      updateSelectInput(
        session,
        "team_explorer",
        choices = season_teams,
        selected = selected$team
      )
      
      updateTabsetPanel(
        session,
        "main_nav",
        selected = "Team Explorer"
      )
    }
  )
  
  observeEvent(
    input$playoff_add_compare,
    {
      
      selected <- playoff_selected_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a playoff team-season first."
        )
      )
      
      new_id <- make_team_season_id(
        selected$season,
        selected$team
      )
      
      current <- input$compare_teams
      
      if (
        is.null(
          current
        )
      ) {
        current <- character(0)
      }
      
      if (
        new_id %in%
        current
      ) {
        
        showNotification(
          "That team-season is already in Compare Teams.",
          type = "message"
        )
        
      } else if (
        length(
          current
        ) >=
        4
      ) {
        
        showNotification(
          "Compare Teams already has four selections. Remove one before adding another.",
          type = "warning"
        )
        
      } else {
        
        updateSelectizeInput(
          session,
          "compare_teams",
          selected = c(
            current,
            new_id
          ),
          server = TRUE
        )
      }
      
      updateTabsetPanel(
        session,
        "main_nav",
        selected = "Compare Teams"
      )
    }
  )
  
  observeEvent(
    input$playoff_use_a,
    {
      
      selected <- playoff_selected_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a playoff team-season first."
        )
      )
      
      set_historical_matchup_side(
        "a",
        selected
      )
    }
  )
  
  observeEvent(
    input$playoff_use_b,
    {
      
      selected <- playoff_selected_team()
      
      validate(
        need(
          !is.null(
            selected
          ),
          "Select a playoff team-season first."
        )
      )
      
      set_historical_matchup_side(
        "b",
        selected
      )
    }
  )
  
  playoff_odds_data <- reactive({
    
    data <- playoff_data()$odds
    
    validate(
      need(
        !is.null(
          data
        ) &&
          nrow(
            data
          ) >
          0,
        "Super Bowl simulation data are not available."
      )
    )
    
    req(
      input$playoff_odds_season,
      input$playoff_odds_calibration,
      input$playoff_odds_metric
    )
    
    season_value <- as.integer(
      input$playoff_odds_season
    )
    
    data <- data[
      data$season ==
        season_value,
      ,
      drop = FALSE
    ]
    
    suffix <- if (
      input$playoff_odds_calibration ==
      "retrospective"
    ) {
      "retrospective"
    } else {
      "walkforward"
    }
    
    prefix <- switch(
      input$playoff_odds_metric,
      win = "super_bowl_win_probability",
      appearance = "super_bowl_appearance_probability",
      conference = "conference_championship_probability"
    )
    
    value_column <- paste0(
      prefix,
      "_",
      suffix
    )
    
    validate(
      need(
        value_column %in%
          names(
            data
          ),
        paste(
          "Missing probability column:",
          value_column
        )
      )
    )
    
    data$display_probability <- suppressWarnings(
      as.numeric(
        data[[value_column]]
      )
    )
    
    data <- data[
      is.finite(
        data$display_probability
      ),
      ,
      drop = FALSE
    ]
    
    data[
      order(
        -data$display_probability,
        data$team
      ),
      ,
      drop = FALSE
    ]
  })
  
  output$playoff_odds_cards <- renderUI({
    
    data <- playoff_odds_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "This walk-forward probability is not available for the selected early season."
      )
    )
    
    favorite <- data[1, , drop = FALSE]
    
    champion <- data[
      data$champion ==
        1,
      ,
      drop = FALSE
    ]
    
    champion_odds <- if (
      nrow(
        champion
      ) ==
      1
    ) {
      champion$display_probability
    } else {
      NA_real_
    }
    
    format_label <- if (
      "playoff_format" %in%
      names(
        data
      )
    ) {
      data$playoff_format[1]
    } else {
      "—"
    }
    
    div(
      class = "metric-grid",
      metric_card(
        "Favorite",
        favorite$team,
        fmt_pct(
          favorite$display_probability,
          1
        )
      ),
      metric_card(
        "Actual champion",
        if (
          nrow(
            champion
          ) ==
          1
        ) {
          champion$team
        } else {
          "—"
        },
        fmt_pct(
          champion_odds,
          1
        )
      ),
      metric_card(
        "Playoff format",
        format_label
      ),
      metric_card(
        "Simulation",
        if (
          input$playoff_odds_calibration ==
          "retrospective"
        ) {
          "Retrospective"
        } else {
          "Walk-forward"
        },
        "50,000 simulations per season/calibration"
      )
    )
  })
  
  output$playoff_odds_plot <- renderPlot({
    
    data <- playoff_odds_data()
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      return(
        empty_plot(
          "Probability is not available for this season/calibration."
        )
      )
    }
    
    data$team_factor <- factor(
      data$team,
      levels = rev(
        data$team
      )
    )
    
    data$champion_label <- ifelse(
      data$champion ==
        1,
      "Actual champion",
      "Other playoff team"
    )
    
    data$probability_label <- fmt_pct(
      data$display_probability,
      1
    )
    
    metric_title <- switch(
      input$playoff_odds_metric,
      win = "Super Bowl win probability",
      appearance = "Super Bowl appearance probability",
      conference = "Conference Championship appearance probability"
    )
    
    ggplot(
      data,
      aes(
        x = team_factor,
        y = display_probability,
        fill = champion_label
      )
    ) +
      geom_col(
        width = 0.72
      ) +
      geom_text(
        aes(
          label = probability_label
        ),
        hjust = -0.15,
        color = "#0B1F33",
        fontface = "bold",
        size = 3.2
      ) +
      coord_flip(
        clip = "off"
      ) +
      scale_fill_manual(
        values = c(
          "Actual champion" = "#F2A71B",
          "Other playoff team" = "#2F80ED"
        )
      ) +
      scale_y_continuous(
        labels = percent_format(
          accuracy = 1
        ),
        limits = c(
          0,
          NA
        ),
        expand = expansion(
          mult = c(
            0,
            0.18
          )
        )
      ) +
      labs(
        x = NULL,
        y = metric_title,
        title = paste(
          input$playoff_odds_season,
          metric_title
        ),
        subtitle = if (
          input$playoff_odds_calibration ==
          "retrospective"
        ) {
          "Calibrated using all 1970–2025 playoff results."
        } else {
          "Historical walk-forward calibration using only playoff results from prior seasons."
        },
        fill = NULL
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(
          10,
          60,
          10,
          18
        )
      )
  })
  
  output$playoff_odds_table <- renderDT({
    
    data <- playoff_odds_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "season",
          "team",
          "conference",
          "seed",
          "record",
          "rank",
          "rating",
          "season_rating_z",
          "conference_championship_probability_retrospective",
          "super_bowl_appearance_probability_retrospective",
          "super_bowl_win_probability_retrospective",
          "conference_championship_probability_walkforward",
          "super_bowl_appearance_probability_walkforward",
          "super_bowl_win_probability_walkforward",
          "playoff_finish",
          "champion"
        )
      ),
      drop = FALSE
    ]
    
    out <- round_numeric_columns(
      out,
      4
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
  playoff_upset_data <- reactive({
    
    data <- playoff_data()$upsets
    
    validate(
      need(
        !is.null(
          data
        ) &&
          nrow(
            data
          ) >
          0,
        "Upset rankings are not available."
      )
    )
    
    data$era <- make_era_label(
      data$season
    )
    
    if (
      !is.null(
        input$playoff_upset_era
      ) &&
      input$playoff_upset_era !=
      "ALL"
    ) {
      
      data <- data[
        data$era ==
          input$playoff_upset_era,
        ,
        drop = FALSE
      ]
    }
    
    if (
      !is.null(
        input$playoff_upset_round
      ) &&
      input$playoff_upset_round !=
      "ALL"
    ) {
      
      data <- data[
        data$round ==
          input$playoff_upset_round,
        ,
        drop = FALSE
      ]
    }
    
    data <- data[
      order(
        data$winner_probability_retrospective,
        data$season
      ),
      ,
      drop = FALSE
    ]
    
    n_keep <- as.integer(
      input$playoff_upset_count
    )
    
    if (
      is.finite(
        n_keep
      )
    ) {
      
      data <- data[
        seq_len(
          min(
            n_keep,
            nrow(
              data
            )
          )
        ),
        ,
        drop = FALSE
      ]
    }
    
    data
  })
  
  output$playoff_upset_cards <- renderUI({
    
    data <- playoff_upset_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No upsets match these filters."
      )
    )
    
    biggest <- data[1, , drop = FALSE]
    
    largest_rating_dog <- data[
      which.min(
        data$winner_rating_edge
      ),
      ,
      drop = FALSE
    ]
    
    div(
      class = "metric-grid",
      metric_card(
        "Biggest upset",
        paste(
          biggest$season,
          biggest$winner,
          "over",
          biggest$loser
        ),
        fmt_pct(
          biggest$winner_probability_retrospective,
          1
        )
      ),
      metric_card(
        "Winner rating edge",
        fmt_signed(
          biggest$winner_rating_edge,
          2
        ),
        "Negative means the winner entered as the lower-rated team"
      ),
      metric_card(
        "Largest rating underdog",
        paste(
          largest_rating_dog$season,
          largest_rating_dog$winner
        ),
        fmt_signed(
          largest_rating_dog$winner_rating_edge,
          2
        )
      ),
      metric_card(
        "Upsets shown",
        nrow(
          data
        )
      )
    )
  })
  
  output$playoff_upset_plot <- renderPlot({
    
    data <- playoff_upset_data()
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      return(
        empty_plot(
          "No upsets match these filters."
        )
      )
    }
    
    data$matchup <- paste0(
      data$season,
      " ",
      data$winner,
      " over ",
      data$loser,
      " (",
      data$round,
      ")"
    )
    
    data$matchup_factor <- factor(
      data$matchup,
      levels = rev(
        data$matchup
      )
    )
    
    data$upset_surprise <- (
      1 -
        data$winner_probability_retrospective
    )
    
    data$probability_label <- paste0(
      "P(win) ",
      fmt_pct(
        data$winner_probability_retrospective,
        1
      )
    )
    
    ggplot(
      data,
      aes(
        x = matchup_factor,
        y = upset_surprise
      )
    ) +
      geom_col(
        width = 0.72,
        fill = "#D7263D"
      ) +
      geom_text(
        aes(
          label = probability_label
        ),
        hjust = -0.12,
        color = "#0B1F33",
        fontface = "bold",
        size = 3
      ) +
      coord_flip(
        clip = "off"
      ) +
      scale_y_continuous(
        labels = percent_format(
          accuracy = 1
        ),
        limits = c(
          0,
          1
        ),
        expand = expansion(
          mult = c(
            0,
            0.13
          )
        )
      ) +
      labs(
        x = NULL,
        y = "Upset surprise (1 − winner pregame probability)",
        title = "Biggest playoff upsets",
        subtitle = "The label shows the eventual winner's pregame model probability."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          85,
          10,
          18
        )
      )
  })
  
  output$playoff_upset_table <- renderDT({
    
    data <- playoff_upset_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "historical_upset_rank",
          "season",
          "round",
          "winner",
          "loser",
          "home_score",
          "away_score",
          "winner_probability_retrospective",
          "winner_probability_walkforward",
          "winner_rating_edge",
          "winner_margin_vs_expectation",
          "surprise_score_retrospective"
        )
      ),
      drop = FALSE
    ]
    
    out <- round_numeric_columns(
      out,
      4
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
  playoff_champion_data <- reactive({
    
    master <- playoff_master()
    
    data <- master[
      master$champion ==
        1,
      ,
      drop = FALSE
    ]
    
    req(
      input$playoff_champion_metric
    )
    
    metric <- input$playoff_champion_metric
    
    if (
      metric ==
      "sb_odds"
    ) {
      
      data$display_value <-
        data$super_bowl_win_probability_retrospective
      data <- data[
        order(
          data$display_value,
          data$season
        ),
        ,
        drop = FALSE
      ]
      
    } else if (
      metric ==
      "pre_rating"
    ) {
      
      data$display_value <-
        data$pre_playoff_rating
      data <- data[
        order(
          -data$display_value,
          data$season
        ),
        ,
        drop = FALSE
      ]
      
    } else if (
      metric ==
      "season_z"
    ) {
      
      data$display_value <-
        data$season_rating_z
      data <- data[
        order(
          -data$display_value,
          data$season
        ),
        ,
        drop = FALSE
      ]
      
    } else if (
      metric ==
      "rating_change"
    ) {
      
      data$display_value <-
        data$playoff_rating_change
      data <- data[
        order(
          -data$display_value,
          data$season
        ),
        ,
        drop = FALSE
      ]
      
    } else {
      
      data$display_value <-
        data$total_margin_vs_expectation
      data <- data[
        order(
          -data$display_value,
          data$season
        ),
        ,
        drop = FALSE
      ]
    }
    
    data <- data[
      is.finite(
        data$display_value
      ),
      ,
      drop = FALSE
    ]
    
    count_value <- input$playoff_champion_count
    
    if (
      !is.null(
        count_value
      ) &&
      count_value !=
      "all"
    ) {
      
      n_keep <- as.integer(
        count_value
      )
      
      data <- data[
        seq_len(
          min(
            n_keep,
            nrow(
              data
            )
          )
        ),
        ,
        drop = FALSE
      ]
    }
    
    data
  })
  
  output$playoff_champion_cards <- renderUI({
    
    master <- playoff_master()
    champions <- master[
      master$champion ==
        1,
      ,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(
          champions
        ) >
          0,
        "Champion data are not available."
      )
    )
    
    unlikely <- champions[
      which.min(
        champions$super_bowl_win_probability_retrospective
      ),
      ,
      drop = FALSE
    ]
    
    strongest <- champions[
      which.max(
        champions$pre_playoff_rating
      ),
      ,
      drop = FALSE
    ]
    
    gain <- champions[
      which.max(
        champions$playoff_rating_change
      ),
      ,
      drop = FALSE
    ]
    
    performance <- champions[
      which.max(
        champions$total_margin_vs_expectation
      ),
      ,
      drop = FALSE
    ]
    
    div(
      class = "metric-grid",
      metric_card(
        "Most unlikely champion",
        paste(
          unlikely$season,
          unlikely$team
        ),
        fmt_pct(
          unlikely$super_bowl_win_probability_retrospective,
          2
        )
      ),
      metric_card(
        "Strongest entering playoffs",
        paste(
          strongest$season,
          strongest$team
        ),
        fmt_signed(
          strongest$pre_playoff_rating,
          2
        )
      ),
      metric_card(
        "Largest rating gain",
        paste(
          gain$season,
          gain$team
        ),
        fmt_signed(
          gain$playoff_rating_change,
          2
        )
      ),
      metric_card(
        "Best vs expectation",
        paste(
          performance$season,
          performance$team
        ),
        fmt_signed(
          performance$total_margin_vs_expectation,
          1
        )
      )
    )
  })
  
  output$playoff_champion_plot <- renderPlot({
    
    data <- playoff_champion_data()
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      return(
        empty_plot(
          "No champion data are available."
        )
      )
    }
    
    metric <- input$playoff_champion_metric
    
    data$team_season <- paste(
      data$season,
      data$team
    )
    
    data$team_factor <- factor(
      data$team_season,
      levels = rev(
        data$team_season
      )
    )
    
    is_probability <- (
      metric ==
        "sb_odds"
    )
    
    data$value_label <- if (
      is_probability
    ) {
      fmt_pct(
        data$display_value,
        2
      )
    } else {
      fmt_signed(
        data$display_value,
        2
      )
    }
    
    y_label <- switch(
      metric,
      sb_odds = "Pre-playoff Super Bowl probability",
      pre_rating = "Pre-playoff rating",
      season_z = "Season rating z-score",
      rating_change = "Postseason rating change",
      performance = "Total margin vs expectation"
    )
    
    title <- switch(
      metric,
      sb_odds = "Most surprising Super Bowl champions",
      pre_rating = "Strongest champions entering the playoffs",
      season_z = "Most dominant regular-season champions",
      rating_change = "Champions with the largest postseason rating gains",
      performance = "Champions with the best performance vs expectation"
    )
    
    p <- ggplot(
      data,
      aes(
        x = team_factor,
        y = display_value
      )
    ) +
      geom_col(
        width = 0.72,
        fill = "#F2A71B"
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        hjust = ifelse(
          data$display_value >=
            0,
          -0.15,
          1.15
        ),
        color = "#0B1F33",
        fontface = "bold",
        size = 3
      ) +
      coord_flip(
        clip = "off"
      ) +
      labs(
        x = NULL,
        y = y_label,
        title = title
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          65,
          10,
          18
        )
      )
    
    if (
      is_probability
    ) {
      
      p <- p +
        scale_y_continuous(
          labels = percent_format(
            accuracy = 0.1
          ),
          expand = expansion(
            mult = c(
              0.02,
              0.18
            )
          )
        )
      
    } else {
      
      p <- p +
        scale_y_continuous(
          expand = expansion(
            mult = c(
              0.18,
              0.18
            )
          )
        )
    }
    
    p
  })
  
  output$playoff_champion_table <- renderDT({
    
    master <- playoff_master()
    
    data <- master[
      master$champion ==
        1,
      ,
      drop = FALSE
    ]
    
    data <- data[
      order(
        data$season,
        decreasing = TRUE
      ),
      ,
      drop = FALSE
    ]
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "season",
          "team",
          "seed",
          "record",
          "rank",
          "pre_playoff_rating",
          "season_rating_z",
          "super_bowl_win_probability_retrospective",
          "super_bowl_win_probability_walkforward",
          "playoff_wins",
          "total_margin_vs_expectation",
          "playoff_rating_change",
          "post_playoff_rating"
        )
      ),
      drop = FALSE
    ]
    
    out <- round_numeric_columns(
      out,
      4
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
  playoff_performance_data <- reactive({
    
    data <- playoff_data()$performance
    
    validate(
      need(
        !is.null(
          data
        ) &&
          nrow(
            data
          ) >
          0,
        "Playoff performance data are not available."
      )
    )
    
    data$era <- make_era_label(
      data$season
    )
    
    if (
      input$playoff_performance_era !=
      "ALL"
    ) {
      
      data <- data[
        data$era ==
          input$playoff_performance_era,
        ,
        drop = FALSE
      ]
    }
    
    metric <- input$playoff_performance_metric
    
    validate(
      need(
        metric %in%
          names(
            data
          ),
        "Selected performance metric is missing."
      )
    )
    
    data$display_value <- suppressWarnings(
      as.numeric(
        data[[metric]]
      )
    )
    
    data <- data[
      is.finite(
        data$display_value
      ),
      ,
      drop = FALSE
    ]
    
    if (
      input$playoff_performance_direction ==
      "over"
    ) {
      
      data <- data[
        order(
          -data$display_value
        ),
        ,
        drop = FALSE
      ]
      
    } else {
      
      data <- data[
        order(
          data$display_value
        ),
        ,
        drop = FALSE
      ]
    }
    
    n_keep <- as.integer(
      input$playoff_performance_count
    )
    
    data[
      seq_len(
        min(
          n_keep,
          nrow(
            data
          )
        )
      ),
      ,
      drop = FALSE
    ]
  })
  
  output$playoff_performance_cards <- renderUI({
    
    data <- playoff_performance_data()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No performance rows match these filters."
      )
    )
    
    first <- data[1, , drop = FALSE]
    
    div(
      class = "metric-grid",
      metric_card(
        if (
          input$playoff_performance_direction ==
          "over"
        ) {
          "Top overperformer"
        } else {
          "Top underperformer"
        },
        paste(
          first$season,
          first$team
        ),
        fmt_signed(
          first$display_value,
          1
        )
      ),
      metric_card(
        "Playoff games",
        first$playoff_games
      ),
      metric_card(
        "Playoff wins",
        first$playoff_wins
      ),
      metric_card(
        "Average pregame win probability",
        fmt_pct(
          first$average_pregame_win_probability,
          1
        )
      )
    )
  })
  
  output$playoff_performance_plot <- renderPlot({
    
    data <- playoff_performance_data()
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      return(
        empty_plot(
          "No performance rows match these filters."
        )
      )
    }
    
    data$team_season <- paste(
      data$season,
      data$team
    )
    
    data$team_factor <- factor(
      data$team_season,
      levels = rev(
        data$team_season
      )
    )
    
    data$value_label <- fmt_signed(
      data$display_value,
      1
    )
    
    ggplot(
      data,
      aes(
        x = team_factor,
        y = display_value,
        fill = display_value
      )
    ) +
      geom_col(
        width = 0.72
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        hjust = ifelse(
          data$display_value >=
            0,
          -0.15,
          1.15
        ),
        color = "#0B1F33",
        fontface = "bold",
        size = 3
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0,
        color = "#536579",
        linewidth = 0.45
      ) +
      scale_fill_gradient2(
        low = "#D7263D",
        mid = "#E7EDF4",
        high = "#27AE60",
        midpoint = 0,
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.18,
            0.18
          )
        )
      ) +
      labs(
        x = NULL,
        y = if (
          input$playoff_performance_metric ==
          "total_margin_vs_expectation"
        ) {
          "Total margin vs expectation"
        } else {
          "Average margin vs expectation"
        },
        title = if (
          input$playoff_performance_direction ==
          "over"
        ) {
          "Biggest playoff overperformers"
        } else {
          "Biggest playoff underperformers"
        },
        subtitle = "Positive values mean the team outscored the model's expected margins."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          58,
          10,
          18
        )
      )
  })
  
  output$playoff_performance_table <- renderDT({
    
    data <- playoff_performance_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "season",
          "team",
          "playoff_games",
          "playoff_wins",
          "total_margin_vs_expectation",
          "average_margin_vs_expectation",
          "average_pregame_win_probability",
          "overperformance_rank"
        )
      ),
      drop = FALSE
    ]
    
    out <- round_numeric_columns(
      out,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
  playoff_rating_team_data <- reactive({
    
    data <- playoff_data()$trajectories
    
    validate(
      need(
        !is.null(
          data
        ) &&
          nrow(
            data
          ) >
          0,
        "Playoff rating trajectory data are not available."
      )
    )
    
    req(
      input$playoff_rating_season,
      input$playoff_rating_team,
      input$playoff_rating_component
    )
    
    season_value <- as.integer(
      input$playoff_rating_season
    )
    
    data <- data[
      data$season ==
        season_value &
        data$team ==
        input$playoff_rating_team,
      ,
      drop = FALSE
    ]
    
    component <- input$playoff_rating_component
    
    validate(
      need(
        component %in%
          names(
            data
          ),
        "Selected rating component is missing."
      )
    )
    
    data$display_rating <- suppressWarnings(
      as.numeric(
        data[[component]]
      )
    )
    
    data <- data[
      order(
        data$stage_order
      ),
      ,
      drop = FALSE
    ]
    
    data$stage_label <- c(
      REG_FINAL = "Regular season",
      AFTER_WC = "After WC",
      AFTER_DIV = "After DIV",
      AFTER_CONF = "After CONF",
      AFTER_SB = "After SB"
    )[
      data$stage
    ]
    
    data$stage_label[
      is.na(
        data$stage_label
      )
    ] <- data$stage[
      is.na(
        data$stage_label
      )
    ]
    
    data
  })
  
  output$playoff_rating_cards <- renderUI({
    
    data <- playoff_rating_team_data()
    master <- playoff_master()
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "No trajectory is available for this team-season."
      )
    )
    
    season_value <- as.integer(
      input$playoff_rating_season
    )
    
    team_value <- input$playoff_rating_team
    
    row <- master[
      master$season ==
        season_value &
        master$team ==
        team_value,
      ,
      drop = FALSE
    ]
    
    first <- data[
      which.min(
        data$stage_order
      ),
      ,
      drop = FALSE
    ]
    
    last <- data[
      which.max(
        data$stage_order
      ),
      ,
      drop = FALSE
    ]
    
    change <- (
      last$display_rating -
        first$display_rating
    )
    
    div(
      class = "metric-grid",
      metric_card(
        "Regular-season rating",
        fmt_signed(
          first$display_rating,
          2
        )
      ),
      metric_card(
        "Final postseason rating",
        fmt_signed(
          last$display_rating,
          2
        )
      ),
      metric_card(
        "Postseason change",
        fmt_signed(
          change,
          2
        )
      ),
      metric_card(
        "Playoff finish",
        if (
          nrow(
            row
          ) ==
          1
        ) {
          row$playoff_finish
        } else {
          "—"
        }
      )
    )
  })
  
  output$playoff_rating_trajectory_plot <- renderPlot({
    
    data <- playoff_rating_team_data()
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      return(
        empty_plot(
          "No trajectory is available."
        )
      )
    }
    
    data$value_label <- fmt_signed(
      data$display_rating,
      2
    )
    
    data$stage_factor <- factor(
      data$stage_label,
      levels = data$stage_label
    )
    
    ggplot(
      data,
      aes(
        x = stage_factor,
        y = display_rating,
        group = 1
      )
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#7A8797"
      ) +
      geom_line(
        linewidth = 1.2,
        color = "#D7263D"
      ) +
      geom_point(
        size = 3.4,
        color = "#D7263D"
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        vjust = -1,
        color = "#0B1F33",
        fontface = "bold",
        size = 3.4
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.12,
            0.18
          )
        )
      ) +
      labs(
        x = NULL,
        y = switch(
          input$playoff_rating_component,
          rating = "Overall rating",
          offense_rating = "Offense rating",
          defense_rating = "Defense rating"
        ),
        title = paste(
          input$playoff_rating_season,
          input$playoff_rating_team,
          "rating through the playoffs"
        ),
        subtitle = "The regular-season rating remains preserved separately; these checkpoints refit the season after each postseason round."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.x = element_blank()
      )
  })
  
  playoff_rating_season_data <- reactive({
    
    data <- playoff_master()
    
    req(
      input$playoff_rating_season
    )
    
    season_value <- as.integer(
      input$playoff_rating_season
    )
    
    data <- data[
      data$season ==
        season_value &
        is.finite(
          data$playoff_rating_change
        ),
      ,
      drop = FALSE
    ]
    
    data[
      order(
        -data$playoff_rating_change
      ),
      ,
      drop = FALSE
    ]
  })
  
  output$playoff_rating_change_plot <- renderPlot({
    
    data <- playoff_rating_season_data()
    
    if (
      nrow(
        data
      ) ==
      0
    ) {
      return(
        empty_plot(
          "No pre/post rating comparison is available for this season."
        )
      )
    }
    
    data$team_factor <- factor(
      data$team,
      levels = rev(
        data$team
      )
    )
    
    data$value_label <- fmt_signed(
      data$playoff_rating_change,
      2
    )
    
    ggplot(
      data,
      aes(
        x = team_factor,
        y = playoff_rating_change,
        fill = playoff_rating_change
      )
    ) +
      geom_col(
        width = 0.72
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        hjust = ifelse(
          data$playoff_rating_change >=
            0,
          -0.15,
          1.15
        ),
        color = "#0B1F33",
        fontface = "bold",
        size = 3.2
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0,
        color = "#536579",
        linewidth = 0.45
      ) +
      scale_fill_gradient2(
        low = "#D7263D",
        mid = "#E7EDF4",
        high = "#27AE60",
        midpoint = 0,
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.18,
            0.18
          )
        )
      ) +
      labs(
        x = NULL,
        y = "Post-playoff rating minus regular-season rating",
        title = paste(
          input$playoff_rating_season,
          "postseason rating changes"
        ),
        subtitle = "Green indicates a rating improvement through the postseason; red indicates decline."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          58,
          10,
          18
        )
      )
  })
  
  output$playoff_rating_table <- renderDT({
    
    data <- playoff_rating_season_data()
    
    out <- data[
      ,
      safe_columns(
        data,
        c(
          "team",
          "record",
          "playoff_finish",
          "pre_playoff_rating",
          "post_playoff_rating",
          "playoff_rating_change",
          "refit_regular_offense",
          "post_playoff_offense",
          "playoff_offense_change",
          "refit_regular_defense",
          "post_playoff_defense",
          "playoff_defense_change"
        )
      ),
      drop = FALSE
    ]
    
    out <- round_numeric_columns(
      out,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
  # ----------------------------------------------------------
  # Historical Matchup
  # ----------------------------------------------------------
  
  output$historical_archive_status <- renderUI({
    
    info <- historical_archive_info()
    archive <- historical_archive()
    
    if (
      is.null(
        info$data
      )
    ) {
      
      div(
        class = "panel-card",
        strong(
          "Historical archive not found."
        ),
        p(
          info$message
        ),
        p(
          "Run the included historical scripts once to create the enriched 1970-2025 CSV. ",
          "The current production season can still be added automatically after that."
        )
      )
      
    } else {
      
      div(
        class = "panel-card",
        p(
          strong(
            "Historical data ready: "
          ),
          min(
            archive$season,
            na.rm = TRUE
          ),
          " through ",
          max(
            archive$season,
            na.rm = TRUE
          ),
          "."
        ),
        div(
          class = "status-text",
          info$message
        )
      )
    }
  })
  
  historical_selected_teams <- reactive({
    
    archive <- historical_archive()
    
    validate(
      need(
        !is.null(
          archive
        ) &&
          nrow(
            archive
          ) >
          0,
        "Historical archive is not available."
      )
    )
    
    req(
      input$hist_a_season,
      input$hist_a_team,
      input$hist_b_season,
      input$hist_b_team
    )
    
    a <- archive[
      archive$season ==
        as.integer(
          input$hist_a_season
        ) &
        archive$team ==
        input$hist_a_team,
      ,
      drop = FALSE
    ]
    
    b <- archive[
      archive$season ==
        as.integer(
          input$hist_b_season
        ) &
        archive$team ==
        input$hist_b_team,
      ,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(
          a
        ) ==
          1,
        "Team A season/team row was not found."
      ),
      need(
        nrow(
          b
        ) ==
          1,
        "Team B season/team row was not found."
      )
    )
    
    list(
      a = a[1, , drop = FALSE],
      b = b[1, , drop = FALSE]
    )
  })
  
  historical_matchup_projection <- reactive({
    
    selected <- historical_selected_teams()
    
    a <- selected$a
    b <- selected$b
    
    req(
      input$historical_site,
      input$historical_scoring_environment
    )
    
    if (
      input$historical_scoring_environment ==
      "a"
    ) {
      
      common_mean <- a$league_mean_points
      common_hfa <- a$estimated_HFA
      environment_label <- paste0(
        a$season,
        " scoring environment"
      )
      
    } else if (
      input$historical_scoring_environment ==
      "b"
    ) {
      
      common_mean <- b$league_mean_points
      common_hfa <- b$estimated_HFA
      environment_label <- paste0(
        b$season,
        " scoring environment"
      )
      
    } else {
      
      common_mean <- mean(
        c(
          a$league_mean_points,
          b$league_mean_points
        ),
        na.rm = TRUE
      )
      
      common_hfa <- mean(
        c(
          a$estimated_HFA,
          b$estimated_HFA
        ),
        na.rm = TRUE
      )
      
      environment_label <- "Neutralized scoring environment"
    }
    
    hfa_for_a <- switch(
      input$historical_site,
      a_home = common_hfa,
      b_home = -common_hfa,
      neutral = 0
    )
    
    expected_a_score <- (
      common_mean +
        a$offense_rating -
        b$defense_rating +
        hfa_for_a /
        2
    )
    
    expected_b_score <- (
      common_mean +
        b$offense_rating -
        a$defense_rating -
        hfa_for_a /
        2
    )
    
    expected_a_margin <- (
      expected_a_score -
        expected_b_score
    )
    
    rating_identity_margin <- (
      a$rating -
        b$rating +
        hfa_for_a
    )
    
    if (
      abs(
        expected_a_margin -
        rating_identity_margin
      ) >
      1e-6
    ) {
      
      warning(
        "Historical matchup expected-score and rating-margin identities differ."
      )
    }
    
    common_sd <- mean(
      c(
        a$predictive_margin_sd,
        b$predictive_margin_sd
      ),
      na.rm = TRUE
    )
    
    approximate_a_win_probability <- if (
      is.finite(
        common_sd
      ) &&
      common_sd >
      0
    ) {
      
      pnorm(
        expected_a_margin /
          common_sd
      )
      
    } else {
      
      NA_real_
    }
    
    list(
      a = a,
      b = b,
      common_mean = common_mean,
      common_hfa = common_hfa,
      hfa_for_a = hfa_for_a,
      environment_label = environment_label,
      expected_a_score = expected_a_score,
      expected_b_score = expected_b_score,
      expected_a_margin = expected_a_margin,
      common_sd = common_sd,
      approximate_a_win_probability =
        approximate_a_win_probability
    )
  })
  
  output$historical_matchup_cards <- renderUI({
    
    pred <- historical_matchup_projection()
    
    a <- pred$a
    b <- pred$b
    
    a_label <- paste(
      a$season,
      a$team
    )
    
    b_label <- paste(
      b$season,
      b$team
    )
    
    favorite <- if (
      pred$expected_a_margin >
      0
    ) {
      
      paste0(
        a_label,
        " by ",
        fmt_num(
          abs(
            pred$expected_a_margin
          ),
          1
        )
      )
      
    } else if (
      pred$expected_a_margin <
      0
    ) {
      
      paste0(
        b_label,
        " by ",
        fmt_num(
          abs(
            pred$expected_a_margin
          ),
          1
        )
      )
      
    } else {
      
      "Pick'em"
    }
    
    div(
      class = "metric-grid",
      metric_card(
        "Projected margin",
        favorite
      ),
      metric_card(
        "Projected score",
        paste0(
          a_label,
          " ",
          fmt_num(
            pred$expected_a_score,
            1
          ),
          " \u2013 ",
          b_label,
          " ",
          fmt_num(
            pred$expected_b_score,
            1
          )
        )
      ),
      metric_card(
        paste0(
          a_label,
          " era dominance"
        ),
        paste0(
          fmt_signed(
            a$season_rating_z,
            2
          ),
          " SD"
        ),
        paste(
          "Raw rating",
          fmt_signed(
            a$rating,
            2
          )
        )
      ),
      metric_card(
        paste0(
          b_label,
          " era dominance"
        ),
        paste0(
          fmt_signed(
            b$season_rating_z,
            2
          ),
          " SD"
        ),
        paste(
          "Raw rating",
          fmt_signed(
            b$rating,
            2
          )
        )
      )
    )
  })
  
  output$historical_matchup_summary <- renderUI({
    
    pred <- historical_matchup_projection()
    
    a <- pred$a
    b <- pred$b
    
    a_label <- paste(
      a$season,
      a$team
    )
    
    b_label <- paste(
      b$season,
      b$team
    )
    
    favorite_sentence <- if (
      pred$expected_a_margin >
      0
    ) {
      
      paste0(
        a_label,
        " is favored by ",
        fmt_num(
          abs(
            pred$expected_a_margin
          ),
          1
        ),
        " points."
      )
      
    } else if (
      pred$expected_a_margin <
      0
    ) {
      
      paste0(
        b_label,
        " is favored by ",
        fmt_num(
          abs(
            pred$expected_a_margin
          ),
          1
        ),
        " points."
      )
      
    } else {
      
      "The matchup is a pick'em."
    }
    
    tagList(
      p(
        strong(
          favorite_sentence
        )
      ),
      p(
        "Projected score: ",
        strong(
          paste0(
            a_label,
            " ",
            fmt_num(
              pred$expected_a_score,
              1
            ),
            " \u2013 ",
            b_label,
            " ",
            fmt_num(
              pred$expected_b_score,
              1
            )
          )
        ),
        "."
      ),
      p(
        "Approximate ",
        a_label,
        " win probability: ",
        strong(
          fmt_pct(
            pred$approximate_a_win_probability
          )
        ),
        "."
      ),
      p(
        "Scoring basis: ",
        strong(
          pred$environment_label
        ),
        ". Common scoring mean = ",
        strong(
          fmt_num(
            pred$common_mean,
            2
          )
        ),
        " points/team."
      ),
      p(
        "Historical HFA used for this setup: ",
        strong(
          fmt_signed(
            abs(
              pred$hfa_for_a
            ),
            2
          )
        ),
        if (
          input$historical_site ==
          "neutral"
        ) {
          " (neutral site, so the applied HFA is 0)."
        } else {
          " points."
        }
      )
    )
  })
  
  output$historical_matchup_plot <- renderPlot({
    
    selected <- historical_selected_teams()
    
    a <- selected$a
    b <- selected$b
    
    a_label <- paste(
      a$season,
      a$team
    )
    
    b_label <- paste(
      b$season,
      b$team
    )
    
    plot_data <- rbind(
      data.frame(
        team_season = a_label,
        metric = c(
          "Overall rating",
          "Offense rating",
          "Defense rating"
        ),
        value = c(
          a$rating,
          a$offense_rating,
          a$defense_rating
        ),
        stringsAsFactors = FALSE
      ),
      data.frame(
        team_season = b_label,
        metric = c(
          "Overall rating",
          "Offense rating",
          "Defense rating"
        ),
        value = c(
          b$rating,
          b$offense_rating,
          b$defense_rating
        ),
        stringsAsFactors = FALSE
      )
    )
    
    plot_data$value_label <- ifelse(
      plot_data$value > 0,
      paste0(
        "+",
        formatC(
          plot_data$value,
          digits = 1,
          format = "f"
        )
      ),
      formatC(
        plot_data$value,
        digits = 1,
        format = "f"
      )
    )
    
    ggplot(
      plot_data,
      aes(
        x = metric,
        y = value,
        fill = team_season
      )
    ) +
      geom_col(
        position = position_dodge(
          width = 0.82
        ),
        width = 0.72
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        position = position_dodge(
          width = 0.82
        ),
        vjust = ifelse(
          plot_data$value >= 0,
          -0.45,
          1.35
        ),
        fontface = "bold",
        size = 3.4,
        color = "#0B1F33"
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#536579",
        alpha = 0.7
      ) +
      scale_fill_manual(
        values = setNames(
          c(
            "#2F80ED",
            "#D7263D"
          ),
          unique(
            plot_data$team_season
          )
        )
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(0.16, 0.16)
        )
      ) +
      labs(
        x = NULL,
        y = "Points above/below league average",
        fill = "Team-season",
        subtitle = "Higher bars indicate stronger performance relative to that season's league average."
      ) +
      dashboard_plot_theme() +
      theme(
        legend.position = "bottom"
      )
  })
  
  output$historical_matchup_table <- renderDT({
    
    selected <- historical_selected_teams()
    
    a <- selected$a
    b <- selected$b
    
    columns <- safe_columns(
      a,
      c(
        "season",
        "team",
        "record",
        "rank",
        "rating",
        "season_rating_z",
        "offense_rank",
        "offense_rating",
        "season_offense_z",
        "defense_rank",
        "defense_rating",
        "season_defense_z",
        "point_differential_per_game",
        "played_SOS",
        "historical_raw_rank",
        "historical_standardized_rank"
      )
    )
    
    out <- rbind(
      a[
        ,
        columns,
        drop = FALSE
      ],
      b[
        ,
        columns,
        drop = FALSE
      ]
    )
    
    out <- round_numeric_columns(
      out,
      3
    )
    
    datatable(
      out,
      rownames = FALSE,
      options = list(
        dom = "t",
        scrollX = TRUE
      )
    )
  })
  
  # ----------------------------------------------------------
  # Historical seasons
  # ----------------------------------------------------------
  
  historical_data <- reactive({
    
    archive <- historical_archive()
    
    req(
      input$history_season
    )
    
    validate(
      need(
        !is.null(
          archive
        ) &&
          nrow(
            archive
          ) >
          0,
        "Historical archive is not available."
      )
    )
    
    archive[
      archive$season ==
        as.integer(
          input$history_season
        ),
      ,
      drop = FALSE
    ]
  })
  
  output$historical_season_cards <- renderUI({
    
    data <- historical_data()
    
    top_overall <- data[
      which.max(
        data$rating
      ),
      ,
      drop = FALSE
    ]
    
    top_offense <- data[
      which.max(
        data$offense_rating
      ),
      ,
      drop = FALSE
    ]
    
    top_defense <- data[
      which.max(
        data$defense_rating
      ),
      ,
      drop = FALSE
    ]
    
    div(
      class = "metric-grid",
      metric_card(
        "No. 1 team",
        top_overall$team,
        paste(
          top_overall$record,
          "· rating",
          fmt_signed(
            top_overall$rating,
            2
          )
        )
      ),
      metric_card(
        "Best offense",
        top_offense$team,
        paste(
          "Offense rating",
          fmt_signed(
            top_offense$offense_rating,
            2
          )
        )
      ),
      metric_card(
        "Best defense",
        top_defense$team,
        paste(
          "Defense rating",
          fmt_signed(
            top_defense$defense_rating,
            2
          )
        )
      ),
      metric_card(
        "League scoring",
        fmt_num(
          data$league_mean_points[1],
          2
        ),
        "Points per team"
      )
    )
  })
  
  output$historical_plot <- renderPlot({
    
    data <- historical_data()
    
    req(
      input$historical_metric
    )
    
    metric <- input$historical_metric
    
    req(
      metric %in%
        names(
          data
        )
    )
    
    data$value <- data[[metric]]
    
    data <- data[
      order(
        data$value
      ),
      ,
      drop = FALSE
    ]
    
    data$team_factor <- factor(
      data$team,
      levels = data$team
    )
    
    data$value_label <- ifelse(
      data$value > 0,
      paste0(
        "+",
        formatC(
          data$value,
          digits = 1,
          format = "f"
        )
      ),
      formatC(
        data$value,
        digits = 1,
        format = "f"
      )
    )
    
    y_label <- switch(
      metric,
      rating = "Power rating",
      offense_rating = "Offense rating",
      defense_rating = "Defense rating",
      point_differential_per_game = "Point differential/game"
    )
    
    positive <- data[
      data$value >=
        0,
      ,
      drop = FALSE
    ]
    
    negative <- data[
      data$value <
        0,
      ,
      drop = FALSE
    ]
    
    p <- ggplot(
      data,
      aes(
        x = team_factor,
        y = value,
        fill = value
      )
    ) +
      geom_col(
        width = 0.78
      ) +
      coord_flip(
        clip = "off"
      ) +
      geom_hline(
        yintercept = 0,
        color = "#536579",
        linewidth = 0.55
      ) +
      scale_fill_gradient2(
        low = "#D7263D",
        mid = "#E7EDF4",
        high = "#2F80ED",
        midpoint = 0,
        guide = "none"
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.14,
            0.14
          )
        )
      ) +
      labs(
        x = NULL,
        y = y_label,
        title = paste(
          input$history_season,
          y_label
        ),
        subtitle = "Blue is above league average; red is below league average."
      ) +
      dashboard_plot_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(
          10,
          34,
          10,
          18
        )
      )
    
    if (
      nrow(
        positive
      ) >
      0
    ) {
      
      p <- p +
        geom_text(
          data = positive,
          aes(
            label = value_label
          ),
          hjust = -0.18,
          color = "#0B1F33",
          fontface = "bold",
          size = 3.5
        )
    }
    
    if (
      nrow(
        negative
      ) >
      0
    ) {
      
      p <- p +
        geom_text(
          data = negative,
          aes(
            label = value_label
          ),
          hjust = 1.18,
          color = "#0B1F33",
          fontface = "bold",
          size = 3.5
        )
    }
    
    p
  })
  
  output$historical_season_trajectory_plot <- renderPlot({
    
    req(
      input$history_season,
      input$historical_trajectory_team
    )
    
    season_value <- as.integer(
      input$history_season
    )
    
    history <- season_weekly_history(
      season_value
    )
    
    validate(
      need(
        !is.null(
          history
        ) &&
          nrow(
            history
          ) >
          0,
        if (
          season_value <=
          1998
        ) {
          legacy_games_info()$message
        } else {
          "Weekly history is unavailable for this season."
        }
      )
    )
    
    data <- history[
      history$team ==
        input$historical_trajectory_team,
      ,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(
          data
        ) >
          0,
        "Selected team does not have weekly history for this season."
      )
    )
    
    data$value_label <- fmt_signed(
      data$rating,
      1
    )
    
    ggplot(
      data,
      aes(
        x = week,
        y = rating
      )
    ) +
      geom_line(
        linewidth = 1.25,
        color = "#D7263D"
      ) +
      geom_point(
        size = 3.2,
        color = "#D7263D"
      ) +
      geom_text(
        aes(
          label = value_label
        ),
        vjust = -0.85,
        fontface = "bold",
        color = "#0B1F33",
        size = 3.2
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#7A8797",
        alpha = 0.7
      ) +
      scale_x_continuous(
        breaks = sort(
          unique(
            data$week
          )
        )
      ) +
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0.10,
            0.16
          )
        )
      ) +
      labs(
        title = paste(
          season_value,
          input$historical_trajectory_team,
          "power rating by week"
        ),
        subtitle = "The season is recalculated through each completed week using the same rating model.",
        x = "Week",
        y = "Power rating"
      ) +
      dashboard_plot_theme()
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

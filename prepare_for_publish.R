# ============================================================
# PREPARE NFL POWER RATINGS SHINY APP FOR PUBLIC DEPLOYMENT
# ROBUST FILE-DISCOVERY VERSION
# ============================================================
#
# You may run this from ANY working directory:
#
# source("~/Downloads/Power_Ratings_NFL/prepare_for_publish.R")
#
# The script identifies the directory containing this file rather
# than assuming getwd() is the app directory.
# ============================================================


# ------------------------------------------------------------
# 1. FIND THIS SCRIPT / APP DIRECTORY
# ------------------------------------------------------------

get_script_path <- function() {
  
  frames <- sys.frames()
  
  discovered <- character(0)
  
  for (
    i in
    seq_along(
      frames
    )
  ) {
    
    current_ofile <- frames[[i]]$ofile
    
    if (
      !is.null(
        current_ofile
      ) &&
      length(
        current_ofile
      ) ==
      1 &&
      !is.na(
        current_ofile
      ) &&
      nzchar(
        current_ofile
      )
    ) {
      
      discovered <- c(
        discovered,
        current_ofile
      )
    }
  }
  
  if (
    length(
      discovered
    ) >
    0
  ) {
    
    return(
      normalizePath(
        tail(
          discovered,
          1
        ),
        mustWork = FALSE
      )
    )
  }
  
  NA_character_
}


SCRIPT_PATH <- get_script_path()

if (
  !is.na(
    SCRIPT_PATH
  )
) {
  
  APP_DIR <- dirname(
    SCRIPT_PATH
  )
  
} else {
  
  APP_DIR <- normalizePath(
    getwd(),
    mustWork = TRUE
  )
}

cat(
  "\nApp directory detected as:\n  ",
  APP_DIR,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------
# 2. CORE PATHS
# ------------------------------------------------------------

DATA_DIR <- file.path(
  APP_DIR,
  "data"
)

PLAYOFF_DESTINATION <- file.path(
  DATA_DIR,
  "playoffs"
)

CACHE_DIR <- file.path(
  DATA_DIR,
  "cache"
)

dir.create(
  DATA_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  PLAYOFF_DESTINATION,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  CACHE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. FIND APP + ENGINE
# ------------------------------------------------------------

app_file <- file.path(
  APP_DIR,
  "app.R"
)

engine_candidates <- c(
  file.path(
    APP_DIR,
    "nfl_power_ratings_v12_1.R"
  ),
  file.path(
    APP_DIR,
    "PR_version12.1.R"
  )
)

engine_candidates <- engine_candidates[
  file.exists(
    engine_candidates
  )
]

ENGINE_FILE <- if (
  length(
    engine_candidates
  ) >
  0
) {
  engine_candidates[1]
} else {
  NA_character_
}


# ------------------------------------------------------------
# 4. FILE SEARCH HELPERS
# ------------------------------------------------------------

safe_normalize <- function(path) {
  
  if (
    length(
      path
    ) ==
    0 ||
    is.na(
      path
    ) ||
    !nzchar(
      path
    )
  ) {
    
    return(
      NA_character_
    )
  }
  
  normalizePath(
    path,
    mustWork = FALSE
  )
}


existing_directories <- function(paths) {
  
  paths <- unique(
    paths[
      !is.na(
        paths
      ) &
        nzchar(
          paths
        )
    ]
  )
  
  paths[
    dir.exists(
      paths
    )
  ]
}


find_exact_file <- function(
    filename,
    preferred_files = character(0),
    search_directories = character(0)
) {
  
  preferred_files <- unique(
    preferred_files
  )
  
  direct_matches <- preferred_files[
    file.exists(
      preferred_files
    )
  ]
  
  if (
    length(
      direct_matches
    ) >
    0
  ) {
    
    return(
      safe_normalize(
        direct_matches[1]
      )
    )
  }
  
  directories <- existing_directories(
    search_directories
  )
  
  for (
    directory in
    directories
  ) {
    
    matches <- list.files(
      directory,
      pattern = paste0(
        "^",
        gsub(
          "([.()+*?^$|{}\\[\\]\\\\])",
          "\\\\\\1",
          filename
        ),
        "$"
      ),
      recursive = TRUE,
      full.names = TRUE
    )
    
    if (
      length(
        matches
      ) >
      0
    ) {
      
      return(
        safe_normalize(
          matches[1]
        )
      )
    }
  }
  
  NA_character_
}


copy_discovered_file <- function(
    source,
    destination,
    description
) {
  
  if (
    is.na(
      source
    ) ||
    !file.exists(
      source
    )
  ) {
    
    cat(
      "[MISSING] ",
      description,
      "\n",
      sep = ""
    )
    
    return(
      FALSE
    )
  }
  
  dir.create(
    dirname(
      destination
    ),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  source_norm <- safe_normalize(
    source
  )
  
  destination_norm <- safe_normalize(
    destination
  )
  
  if (
    !is.na(
      destination_norm
    ) &&
    identical(
      source_norm,
      destination_norm
    )
  ) {
    
    cat(
      "[READY] ",
      description,
      "\n  ",
      destination,
      "\n",
      sep = ""
    )
    
    return(
      TRUE
    )
  }
  
  ok <- file.copy(
    source,
    destination,
    overwrite = TRUE
  )
  
  if (
    ok
  ) {
    
    cat(
      "[COPIED] ",
      description,
      "\n  From: ",
      source,
      "\n  To:   ",
      destination,
      "\n",
      sep = ""
    )
    
  } else {
    
    cat(
      "[FAILED] ",
      description,
      "\n  From: ",
      source,
      "\n  To:   ",
      destination,
      "\n",
      sep = ""
    )
  }
  
  ok
}


# ------------------------------------------------------------
# 5. LIKELY SEARCH LOCATIONS
# ------------------------------------------------------------

downloads_dir <- path.expand(
  "~/Downloads"
)

power_ratings_root <- file.path(
  downloads_dir,
  "Power_Ratings_NFL"
)

playoff_default_dir <- file.path(
  downloads_dir,
  "nfl_playoff_analysis_1970_2025"
)

historical_analysis_dir <- file.path(
  downloads_dir,
  "version12_1_1970_2025_historical_analysis"
)

historical_games_dir <- file.path(
  downloads_dir,
  "historical_nfl_1970_1998"
)

# Search the targeted project directories first.
project_search_directories <- existing_directories(
  c(
    APP_DIR,
    power_ratings_root,
    playoff_default_dir,
    historical_analysis_dir,
    historical_games_dir
  )
)


# ------------------------------------------------------------
# 6. ENRICHED HISTORICAL TEAM-SEASON MASTER
# ------------------------------------------------------------

historical_master_filename <-
  "version12_1_1970_2025_all_season_team_summaries_enriched.csv"

historical_master_destination <- file.path(
  DATA_DIR,
  historical_master_filename
)

historical_master_source <- find_exact_file(
  filename =
    historical_master_filename,
  preferred_files =
    c(
      historical_master_destination,
      file.path(
        APP_DIR,
        historical_master_filename
      ),
      file.path(
        historical_analysis_dir,
        historical_master_filename
      ),
      file.path(
        downloads_dir,
        historical_master_filename
      )
    ),
  search_directories =
    project_search_directories
)

historical_master_ok <- copy_discovered_file(
  historical_master_source,
  historical_master_destination,
  "1970-2025 enriched historical team-season archive"
)


# ------------------------------------------------------------
# 7. HISTORICAL 1970-1998 GAME ARCHIVE
# ------------------------------------------------------------

legacy_filename <-
  "historical_nfl_1970_1998_games_COMPLETE.csv"

legacy_destination <- file.path(
  DATA_DIR,
  legacy_filename
)

legacy_source <- find_exact_file(
  filename =
    legacy_filename,
  preferred_files =
    c(
      legacy_destination,
      file.path(
        historical_games_dir,
        legacy_filename
      ),
      file.path(
        APP_DIR,
        legacy_filename
      )
    ),
  search_directories =
    project_search_directories
)

legacy_ok <- copy_discovered_file(
  legacy_source,
  legacy_destination,
  "1970-1998 historical game archive"
)


# ------------------------------------------------------------
# 8. PLAYOFF OUTPUTS
# ------------------------------------------------------------

playoff_files <- c(
  "11_playoff_team_season_master_1970_2025.csv",
  "04_playoff_games_with_predictions.csv",
  "05_playoff_upsets_ranked.csv",
  "06_pre_vs_post_playoff_ratings.csv",
  "07_playoff_rating_trajectories.csv",
  "08_super_bowl_probabilities_by_season.csv",
  "08_super_bowl_champion_preplayoff_odds.csv",
  "10_playoff_overperformance.csv",
  "09_playoff_probability_model_validation.csv"
)

playoff_copy_results <- setNames(
  logical(
    length(
      playoff_files
    )
  ),
  playoff_files
)

for (
  filename in
  playoff_files
) {
  
  source_file <- find_exact_file(
    filename =
      filename,
    preferred_files =
      c(
        file.path(
          PLAYOFF_DESTINATION,
          filename
        ),
        file.path(
          playoff_default_dir,
          filename
        ),
        file.path(
          APP_DIR,
          filename
        )
      ),
    search_directories =
      project_search_directories
  )
  
  playoff_copy_results[[filename]] <-
    copy_discovered_file(
      source_file,
      file.path(
        PLAYOFF_DESTINATION,
        filename
      ),
      paste(
        "playoff file",
        filename
      )
    )
}


# ------------------------------------------------------------
# 9. BUILD / REFRESH LAST-KNOWN-GOOD MODEL CACHE
# ------------------------------------------------------------

cache_file <- file.path(
  CACHE_DIR,
  "last_good_model_environment.rds"
)

cache_ok <- file.exists(
  cache_file
)

if (
  is.na(
    ENGINE_FILE
  )
) {
  
  cat(
    "\n[WARNING] Could not locate the production rating engine in:\n",
    "  ",
    APP_DIR,
    "\n",
    sep = ""
  )
  
} else {
  
  cat(
    "\nProduction engine found:\n  ",
    ENGINE_FILE,
    "\n",
    sep = ""
  )
  
  cat(
    "\nBuilding / refreshing the last-known-good live model snapshot...\n"
  )
  
  old_wd <- getwd()
  
  temp_output_directory <- tempfile(
    pattern = "nfl_power_publish_"
  )
  
  dir.create(
    temp_output_directory,
    recursive = TRUE
  )
  
  cache_result <- tryCatch(
    {
      
      setwd(
        temp_output_directory
      )
      
      Sys.setenv(
        POWER_RATINGS_FAST_REFRESH = "0",
        POWER_RATINGS_CACHE_DIR = CACHE_DIR
      )
      
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
      
      setwd(
        old_wd
      )
      
      saveRDS(
        env,
        cache_file
      )
      
      TRUE
    },
    error = function(err) {
      
      setwd(
        old_wd
      )
      
      cat(
        "[WARNING] Model cache could not be refreshed: ",
        conditionMessage(
          err
        ),
        "\n",
        sep = ""
      )
      
      FALSE
    }
  )
  
  unlink(
    temp_output_directory,
    recursive = TRUE,
    force = TRUE
  )
  
  Sys.unsetenv(
    c(
      "POWER_RATINGS_FAST_REFRESH",
      "POWER_RATINGS_CACHE_DIR"
    )
  )
  
  if (
    isTRUE(
      cache_result
    )
  ) {
    
    cache_ok <- TRUE
    
    cat(
      "[CREATED] ",
      cache_file,
      "\n",
      sep = ""
    )
    
  } else if (
    file.exists(
      cache_file
    )
  ) {
    
    cache_ok <- TRUE
    
    cat(
      "[READY] Existing last-known-good cache retained.\n"
    )
  }
}


# ------------------------------------------------------------
# 10. FINAL PUBLICATION CHECK
# ------------------------------------------------------------

required_bundle_files <- c(
  app_file,
  ENGINE_FILE,
  historical_master_destination,
  legacy_destination,
  file.path(
    PLAYOFF_DESTINATION,
    playoff_files
  )
)

required_bundle_files <- required_bundle_files[
  !is.na(
    required_bundle_files
  )
]

missing_bundle_files <- required_bundle_files[
  !file.exists(
    required_bundle_files
  )
]

cat(
  "\n========================================\n"
)

if (
  length(
    missing_bundle_files
  ) ==
  0 &&
  file.exists(
    app_file
  ) &&
  !is.na(
    ENGINE_FILE
  )
) {
  
  cat(
    "PUBLICATION BUNDLE CHECK: PASS\n"
  )
  
} else {
  
  cat(
    "PUBLICATION BUNDLE CHECK: INCOMPLETE\n\n"
  )
  
  if (
    !file.exists(
      app_file
    )
  ) {
    
    cat(
      "Missing app file: ",
      app_file,
      "\n",
      sep = ""
    )
  }
  
  if (
    is.na(
      ENGINE_FILE
    )
  ) {
    
    cat(
      "Missing production engine in app directory.\n"
    )
  }
  
  if (
    length(
      missing_bundle_files
    ) >
    0
  ) {
    
    cat(
      paste0(
        "Missing: ",
        missing_bundle_files
      ),
      sep = "\n"
    )
    
    cat(
      "\n"
    )
  }
}

cat(
  "Model fallback cache: ",
  if (
    cache_ok
  ) {
    "READY"
  } else {
    "NOT CREATED"
  },
  "\n",
  sep = ""
)

cat(
  "App directory: ",
  APP_DIR,
  "\n",
  sep = ""
)

cat(
  "========================================\n"
)

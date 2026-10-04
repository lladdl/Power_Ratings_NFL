# ============================================================
# PLAYOFF PIPELINE HELPERS
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

read_csv_base <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

write_csv_base <- function(data, path) {
  write.csv(data, path, row.names = FALSE, na = "")
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

normalize_team_code <- function(x) {
  x <- as.character(x)
  lookup <- c(
    "OAK" = "LV",
    "LAR" = "LA",
    "STL" = "LA",
    "SD" = "LAC",
    "SDG" = "LAC",
    "JAC" = "JAX",
    "WSH" = "WAS"
  )
  hit <- x %in% names(lookup)
  x[hit] <- unname(lookup[x[hit]])
  x
}

normalize_playoff_round <- function(x) {
  x0 <- toupper(trimws(as.character(x)))
  out <- rep(NA_character_, length(x0))
  
  out[grepl("SUPER|\\bSB\\b", x0)] <- "SB"
  out[is.na(out) & grepl("CONF|CHAMP", x0)] <- "CONF"
  out[is.na(out) & grepl("DIV|SEMI", x0)] <- "DIV"
  out[is.na(out) & grepl("WILD|\\bWC\\b|FIRST|OPEN", x0)] <- "WC"
  
  # nflverse schedule codes
  out[x0 == "SB"] <- "SB"
  out[x0 %in% c("CON", "CONF")] <- "CONF"
  out[x0 %in% c("DIV")] <- "DIV"
  out[x0 %in% c("WC", "WILDCARD")] <- "WC"
  
  out
}

round_order_from_name <- function(round_name) {
  lookup <- c("WC" = 1L, "DIV" = 2L, "CONF" = 3L, "SB" = 4L)
  as.integer(unname(lookup[as.character(round_name)]))
}

season_playoff_format <- function(season) {
  season <- as.integer(season)
  
  if (season >= 1970 && season <= 1974) return("FOUR_FIXED_ROTATION")
  if (season >= 1975 && season <= 1977) return("FOUR_RECORD_HOME")
  if (season >= 1978 && season <= 1981) return("FIVE_SAME_DIV")
  if (season == 1982) return("EIGHT_RESEED")
  if (season >= 1983 && season <= 1989) return("FIVE_SAME_DIV")
  if (season >= 1990 && season <= 2019) return("SIX_RESEED")
  if (season >= 2020) return("SEVEN_RESEED")
  
  stop("Unsupported season: ", season)
}


legacy_playoff_round_vector <- function(season) {
  
  season <- as.integer(season)
  
  if (season >= 1970 && season <= 1977) {
    return(
      c(
        rep("DIV", 4),
        rep("CONF", 2),
        "SB"
      )
    )
  }
  
  if (season >= 1978 && season <= 1981) {
    return(
      c(
        rep("WC", 2),
        rep("DIV", 4),
        rep("CONF", 2),
        "SB"
      )
    )
  }
  
  if (season == 1982) {
    return(
      c(
        rep("WC", 8),
        rep("DIV", 4),
        rep("CONF", 2),
        "SB"
      )
    )
  }
  
  if (season >= 1983 && season <= 1989) {
    return(
      c(
        rep("WC", 2),
        rep("DIV", 4),
        rep("CONF", 2),
        "SB"
      )
    )
  }
  
  if (season >= 1990 && season <= 1998) {
    return(
      c(
        rep("WC", 4),
        rep("DIV", 4),
        rep("CONF", 2),
        "SB"
      )
    )
  }
  
  stop(
    "Legacy round assignment only supports 1970-1998. Season: ",
    season
  )
}

assign_legacy_playoff_rounds <- function(data) {
  
  out <- as.data.frame(
    data,
    stringsAsFactors = FALSE
  )
  
  out$derived_playoff_round <- NA_character_
  
  seasons <- sort(
    unique(
      as.integer(
        out$season
      )
    )
  )
  
  for (season in seasons) {
    
    idx <- which(
      as.integer(
        out$season
      ) ==
        season
    )
    
    expected_rounds <- legacy_playoff_round_vector(
      season
    )
    
    if (
      length(
        idx
      ) !=
      length(
        expected_rounds
      )
    ) {
      
      stop(
        paste0(
          "Historical postseason game count mismatch for ",
          season,
          ": found ",
          length(
            idx
          ),
          ", expected ",
          length(
            expected_rounds
          ),
          "."
        )
      )
    }
    
    season_rows <- out[
      idx,
      ,
      drop = FALSE
    ]
    
    if (
      "season_game_number" %in%
      names(
        season_rows
      ) &&
      any(
        is.finite(
          suppressWarnings(
            as.numeric(
              season_rows$season_game_number
            )
          )
        )
      )
    ) {
      
      ord <- order(
        suppressWarnings(
          as.numeric(
            season_rows$season_game_number
          )
        )
      )
      
    } else {
      
      date_value <- if (
        "game_date" %in%
        names(
          season_rows
        )
      ) {
        as.character(
          season_rows$game_date
        )
      } else if (
        "date" %in%
        names(
          season_rows
        )
      ) {
        as.character(
          season_rows$date
        )
      } else {
        rep(
          NA_character_,
          nrow(
            season_rows
          )
        )
      }
      
      raw_week_value <- if (
        "raw_week" %in%
        names(
          season_rows
        )
      ) {
        as.character(
          season_rows$raw_week
        )
      } else if (
        "postseason_round" %in%
        names(
          season_rows
        )
      ) {
        as.character(
          season_rows$postseason_round
        )
      } else {
        rep(
          "",
          nrow(
            season_rows
          )
        )
      }
      
      ord <- order(
        date_value,
        raw_week_value,
        season_rows$home_team,
        season_rows$away_team,
        na.last = TRUE
      )
    }
    
    out$derived_playoff_round[
      idx[
        ord
      ]
    ] <- expected_rounds
  }
  
  out
}

team_division <- function(season, team) {
  season <- as.integer(season)
  team <- normalize_team_code(team)
  
  modern <- list(
    "AFC_EAST" = c("BUF", "MIA", "NE", "NYJ"),
    "AFC_NORTH" = c("BAL", "CIN", "CLE", "PIT"),
    "AFC_SOUTH" = c("HOU", "IND", "JAX", "TEN"),
    "AFC_WEST" = c("DEN", "KC", "LV", "LAC"),
    "NFC_EAST" = c("DAL", "NYG", "PHI", "WAS"),
    "NFC_NORTH" = c("CHI", "DET", "GB", "MIN"),
    "NFC_SOUTH" = c("ATL", "CAR", "NO", "TB"),
    "NFC_WEST" = c("ARI", "LA", "SF", "SEA")
  )
  
  old <- list(
    "AFC_EAST" = c("BUF", "MIA", "NE", "NYJ", "IND"),
    "AFC_CENTRAL" = c("CIN", "CLE", "PIT", "TEN", "JAX", "BAL"),
    "AFC_WEST" = c("DEN", "KC", "LV", "LAC", "SEA"),
    "NFC_EAST" = c("DAL", "NYG", "PHI", "WAS", "ARI"),
    "NFC_CENTRAL" = c("CHI", "DET", "GB", "MIN", "TB"),
    "NFC_WEST" = c("ATL", "CAR", "LA", "NO", "SF")
  )
  
  if (season == 1976) {
    if (team == "SEA") return("NFC_WEST")
    if (team == "TB") return("AFC_WEST")
  }
  
  groups <- if (season >= 2002) modern else old
  
  for (nm in names(groups)) {
    if (team %in% groups[[nm]]) return(nm)
  }
  
  NA_character_
}

team_conference <- function(season, team) {
  div <- team_division(season, team)
  if (is.na(div)) return(NA_character_)
  if (startsWith(div, "AFC")) "AFC" else "NFC"
}

add_conference_division <- function(data, season_col = "season", team_col = "team") {
  data$conference <- mapply(
    team_conference,
    data[[season_col]],
    data[[team_col]],
    USE.NAMES = FALSE
  )
  data$division <- mapply(
    team_division,
    data[[season_col]],
    data[[team_col]],
    USE.NAMES = FALSE
  )
  data
}

canonical_game_table <- function(data, round_source = NULL) {
  out <- as.data.frame(data, stringsAsFactors = FALSE)
  
  out$season <- as.integer(out$season)
  out$home_team <- normalize_team_code(out$home_team)
  out$away_team <- normalize_team_code(out$away_team)
  out$home_score <- as.numeric(out$home_score)
  out$away_score <- as.numeric(out$away_score)
  
  if (!"game_id" %in% names(out)) {
    out$game_id <- sprintf(
      "%d_%04d",
      out$season,
      ave(seq_len(nrow(out)), out$season, FUN = seq_along)
    )
  }
  
  if (!"game_date" %in% names(out)) {
    if ("gameday" %in% names(out)) {
      out$game_date <- as.character(out$gameday)
    } else {
      out$game_date <- NA_character_
    }
  }
  
  if (!"neutral_site" %in% names(out)) {
    if ("location" %in% names(out)) {
      out$neutral_site <- tolower(as.character(out$location)) == "neutral"
      out$neutral_site[is.na(out$neutral_site)] <- FALSE
    } else {
      out$neutral_site <- FALSE
    }
  } else {
    if (!is.logical(out$neutral_site)) {
      out$neutral_site <- toupper(as.character(out$neutral_site)) %in%
        c("TRUE", "T", "1", "YES", "NEUTRAL")
    }
  }
  
  if (is.null(round_source)) {
    if ("playoff_round" %in% names(out)) {
      round_source <- out$playoff_round
    } else if ("postseason_round" %in% names(out)) {
      round_source <- out$postseason_round
    } else if ("game_type" %in% names(out)) {
      round_source <- out$game_type
    } else {
      stop("No playoff round field is available.")
    }
  }
  
  out$round <- normalize_playoff_round(round_source)
  out$round_order <- round_order_from_name(out$round)
  
  out$completed <- !is.na(out$home_score) & !is.na(out$away_score)
  out$home_margin <- ifelse(out$completed, out$home_score - out$away_score, NA_real_)
  out$winner <- ifelse(
    !out$completed,
    NA_character_,
    ifelse(out$home_score > out$away_score, out$home_team, out$away_team)
  )
  out$loser <- ifelse(
    !out$completed,
    NA_character_,
    ifelse(out$home_score > out$away_score, out$away_team, out$home_team)
  )
  out$home_win <- ifelse(out$completed, as.integer(out$winner == out$home_team), NA_integer_)
  
  out
}

unordered_pair_key <- function(a, b) {
  paste(sort(c(as.character(a), as.character(b))), collapse = "|")
}

pair_set <- function(games) {
  if (nrow(games) == 0) return(character(0))
  sort(mapply(unordered_pair_key, games$home_team, games$away_team, USE.NAMES = FALSE))
}

seed_lookup <- function(seed_map, team) {
  as.integer(seed_map[[as.character(team)]])
}

actual_winners <- function(games) {
  setNames(as.character(games$winner), as.character(games$game_id))
}

reseed_pairs <- function(remaining_teams, seed_map) {
  seeds <- vapply(remaining_teams, function(t) seed_lookup(seed_map, t), integer(1))
  ord <- order(seeds)
  teams <- remaining_teams[ord]
  n <- length(teams)
  stopifnot(n == 4L)
  
  list(
    c(teams[1], teams[4]),
    c(teams[2], teams[3])
  )
}

five_team_div_pairs <- function(wc_winner, seed_map, division_map) {
  byes <- names(seed_map)[vapply(seed_map, as.integer, integer(1)) %in% 1:3]
  byes <- byes[order(vapply(byes, function(t) seed_lookup(seed_map, t), integer(1)))]
  
  s1 <- byes[1]
  s2 <- byes[2]
  s3 <- byes[3]
  
  if (division_map[[s1]] == division_map[[wc_winner]]) {
    list(c(s2, wc_winner), c(s1, s3))
  } else {
    list(c(s1, wc_winner), c(s2, s3))
  }
}

candidate_pair_score <- function(expected_pairs, actual_games, seed_map) {
  expected_keys <- sort(vapply(expected_pairs, function(x) unordered_pair_key(x[1], x[2]), character(1)))
  actual_keys <- pair_set(actual_games)
  
  pair_score <- sum(expected_keys %in% actual_keys)
  
  home_score <- 0
  for (pair in expected_pairs) {
    key <- unordered_pair_key(pair[1], pair[2])
    idx <- which(mapply(
      unordered_pair_key,
      actual_games$home_team,
      actual_games$away_team,
      USE.NAMES = FALSE
    ) == key)
    if (length(idx) == 1) {
      expected_home <- pair[which.min(c(
        seed_lookup(seed_map, pair[1]),
        seed_lookup(seed_map, pair[2])
      ))]
      home_score <- home_score + as.integer(actual_games$home_team[idx] == expected_home)
    }
  }
  
  pair_score * 10 + home_score
}

seed_record_penalty <- function(seed_map, master_rows, season) {
  teams <- names(seed_map)
  seeds <- vapply(seed_map, as.integer, integer(1))
  win_pct <- master_rows$win_pct[match(teams, master_rows$team)]
  
  format <- season_playoff_format(season)
  
  groups <- if (format == "FIVE_SAME_DIV") {
    list(1:3, 4:5)
  } else if (format == "EIGHT_RESEED") {
    list(1:8)
  } else if (format == "SIX_RESEED") {
    if (season <= 2001) list(1:3, 4:6) else list(1:4, 5:6)
  } else if (format == "SEVEN_RESEED") {
    list(1:4, 5:7)
  } else {
    list(sort(unique(seeds)))
  }
  
  penalty <- 0
  for (g in groups) {
    idx <- which(seeds %in% g)
    if (length(idx) <= 1) next
    for (i in idx) {
      for (j in idx) {
        if (seeds[i] < seeds[j] &&
            is.finite(win_pct[i]) && is.finite(win_pct[j]) &&
            win_pct[i] < win_pct[j]) {
          penalty <- penalty + (win_pct[j] - win_pct[i])
        }
      }
    }
  }
  penalty
}

permutations_small <- function(x) {
  if (length(x) == 1) return(matrix(x, nrow = 1))
  do.call(rbind, lapply(seq_along(x), function(i) {
    rest <- x[-i]
    cbind(x[i], permutations_small(rest))
  }))
}

infer_conference_seeds <- function(season, conference, games, master_rows) {
  format <- season_playoff_format(season)
  cg <- games[games$season == season & games$conference == conference & games$round != "SB", , drop = FALSE]
  participants <- sort(unique(c(cg$home_team, cg$away_team)))
  
  if (format %in% c("FOUR_FIXED_ROTATION", "FOUR_RECORD_HOME")) {
    return(data.frame(
      season = season,
      conference = conference,
      team = participants,
      seed = NA_integer_,
      seed_inference_status = "not_required_fixed_opening_bracket",
      stringsAsFactors = FALSE
    ))
  }
  
  wc <- cg[cg$round == "WC", , drop = FALSE]
  div <- cg[cg$round == "DIV", , drop = FALSE]
  conf <- cg[cg$round == "CONF", , drop = FALSE]
  
  candidates <- list()
  
  if (format == "FIVE_SAME_DIV") {
    if (nrow(wc) != 1) stop("Expected one wild-card game for ", season, " ", conference)
    seed_base <- list()
    seed_base[[wc$home_team[1]]] <- 4L
    seed_base[[wc$away_team[1]]] <- 5L
    byes <- setdiff(participants, c(wc$home_team, wc$away_team))
    perms <- permutations_small(1:3)
    for (r in seq_len(nrow(perms))) {
      sm <- seed_base
      for (i in seq_along(byes)) sm[[byes[i]]] <- perms[r, i]
      candidates[[length(candidates) + 1]] <- sm
    }
  }
  
  if (format == "EIGHT_RESEED") {
    if (nrow(wc) != 4) stop("Expected four opening games for 1982 ", conference)
    homes <- wc$home_team
    perms <- permutations_small(1:4)
    for (r in seq_len(nrow(perms))) {
      sm <- list()
      for (i in seq_along(homes)) {
        hs <- perms[r, i]
        game_i <- wc[wc$home_team == homes[i], , drop = FALSE]
        sm[[homes[i]]] <- hs
        sm[[game_i$away_team[1]]] <- 9L - hs
      }
      candidates[[length(candidates) + 1]] <- sm
    }
  }
  
  if (format == "SIX_RESEED") {
    if (nrow(wc) != 2) stop("Expected two wild-card games for ", season, " ", conference)
    bye <- setdiff(participants, c(wc$home_team, wc$away_team))
    bye_perms <- permutations_small(1:2)
    game_pairs <- list(c(3L, 6L), c(4L, 5L))
    assignments <- list(c(1L, 2L), c(2L, 1L))
    
    for (bp in seq_len(nrow(bye_perms))) {
      for (a in assignments) {
        sm <- list()
        sm[[bye[1]]] <- bye_perms[bp, 1]
        sm[[bye[2]]] <- bye_perms[bp, 2]
        for (g in 1:2) {
          pair <- game_pairs[[a[g]]]
          sm[[wc$home_team[g]]] <- pair[1]
          sm[[wc$away_team[g]]] <- pair[2]
        }
        candidates[[length(candidates) + 1]] <- sm
      }
    }
  }
  
  if (format == "SEVEN_RESEED") {
    if (nrow(wc) != 3) stop("Expected three wild-card games for ", season, " ", conference)
    bye <- setdiff(participants, c(wc$home_team, wc$away_team))
    if (length(bye) != 1) stop("Expected one bye team for ", season, " ", conference)
    perms <- permutations_small(2:4)
    for (r in seq_len(nrow(perms))) {
      sm <- list()
      sm[[bye[1]]] <- 1L
      for (g in 1:3) {
        hs <- perms[r, g]
        sm[[wc$home_team[g]]] <- hs
        sm[[wc$away_team[g]]] <- 9L - hs
      }
      candidates[[length(candidates) + 1]] <- sm
    }
  }
  
  if (length(candidates) == 0) stop("No seed candidates generated.")
  
  division_map <- setNames(
    vapply(participants, function(t) team_division(season, t), character(1)),
    participants
  )
  
  scores <- numeric(length(candidates))
  penalties <- numeric(length(candidates))
  
  for (i in seq_along(candidates)) {
    sm <- candidates[[i]]
    
    if (format == "FIVE_SAME_DIV") {
      wc_winner <- wc$winner[1]
      expected_div <- five_team_div_pairs(wc_winner, sm, division_map)
    } else {
      wc_winners <- as.character(wc$winner)
      bye_teams <- setdiff(participants, c(wc$home_team, wc$away_team))
      remaining <- c(bye_teams, wc_winners)
      expected_div <- reseed_pairs(remaining, sm)
    }
    
    score <- candidate_pair_score(expected_div, div, sm)
    
    if (nrow(conf) == 1 && nrow(div) == 2) {
      finalists <- as.character(div$winner)
      if (length(finalists) == 2) {
        expected_home <- finalists[which.min(vapply(finalists, function(t) seed_lookup(sm, t), integer(1)))]
        score <- score + 5 * as.integer(conf$home_team[1] == expected_home)
      }
    }
    
    scores[i] <- score
    penalties[i] <- seed_record_penalty(sm, master_rows, season)
  }
  
  best_score <- max(scores)
  best <- which(scores == best_score)
  chosen <- best[which.min(penalties[best])]
  
  status <- if (length(best) == 1) {
    "unique_from_bracket"
  } else {
    "ambiguous_chosen_by_record_order"
  }
  
  sm <- candidates[[chosen]]
  
  data.frame(
    season = season,
    conference = conference,
    team = names(sm),
    seed = vapply(sm, as.integer, integer(1)),
    seed_inference_status = status,
    bracket_match_score = scores[chosen],
    record_order_penalty = penalties[chosen],
    candidate_ties_at_best_score = length(best),
    stringsAsFactors = FALSE
  )
}

apply_seed_overrides <- function(seed_table, override_file) {
  if (!file.exists(override_file)) return(seed_table)
  
  ov <- read_csv_base(override_file)
  required <- c("season", "conference", "team", "seed")
  if (!all(required %in% names(ov))) {
    stop("Seed override file must contain: ", paste(required, collapse = ", "))
  }
  
  ov$team <- normalize_team_code(ov$team)
  ov$season <- as.integer(ov$season)
  ov$seed <- as.integer(ov$seed)
  
  key <- paste(seed_table$season, seed_table$conference, seed_table$team)
  ov_key <- paste(ov$season, ov$conference, ov$team)
  idx <- match(key, ov_key)
  hit <- !is.na(idx)
  
  seed_table$seed[hit] <- ov$seed[idx[hit]]
  seed_table$seed_inference_status[hit] <- "manual_override"
  seed_table
}

playoff_probability <- function(home_team, away_team, season, neutral_site,
                                rating_lookup, hfa_value, model_coef) {
  hr <- rating_lookup[[as.character(home_team)]]
  ar <- rating_lookup[[as.character(away_team)]]
  
  if (
    is.null(hr) ||
    is.null(ar) ||
    is.na(hr) ||
    is.na(ar)
  ) {
    return(
      NA_real_
    )
  }
  
  rating_diff <- as.numeric(
    hr -
      ar
  )
  
  hfa_points <- ifelse(
    isTRUE(
      neutral_site
    ),
    0,
    as.numeric(
      hfa_value
    )
  )
  
  if (
    !is.finite(
      hfa_points
    )
  ) {
    hfa_points <- 0
  }
  
  expected_home_margin <- (
    rating_diff +
      hfa_points
  )
  
  # Preferred/current calibration:
  #
  #   logit(P(home win)) =
  #       beta * expected_home_margin
  #
  # There is no intercept. Thus an expected margin of zero is
  # exactly a 50/50 game, including equal-strength teams in a
  # neutral-site Super Bowl.
  if (
    "expected_home_margin" %in%
    names(
      model_coef
    )
  ) {
    
    eta <- (
      model_coef[["expected_home_margin"]] *
        expected_home_margin
    )
    
    return(
      as.numeric(
        plogis(
          eta
        )
      )
    )
  }
  
  # Backward-compatible support for old saved models.
  old_terms <- c(
    "(Intercept)",
    "rating_diff",
    "home_field"
  )
  
  if (
    all(
      old_terms %in%
      names(
        model_coef
      )
    )
  ) {
    
    home_field_indicator <- ifelse(
      isTRUE(
        neutral_site
      ),
      0,
      1
    )
    
    eta <- (
      model_coef[["(Intercept)"]] +
        model_coef[["rating_diff"]] *
        rating_diff +
        model_coef[["home_field"]] *
        home_field_indicator
    )
    
    return(
      as.numeric(
        plogis(
          eta
        )
      )
    )
  }
  
  stop(
    paste0(
      "Unrecognized playoff probability coefficient structure: ",
      paste(
        names(
          model_coef
        ),
        collapse = ", "
      )
    )
  )
}

simulate_game <- function(home_team, away_team, season, neutral_site,
                          rating_lookup, hfa_value, model_coef) {
  p_home <- playoff_probability(
    home_team, away_team, season, neutral_site,
    rating_lookup, hfa_value, model_coef
  )
  if (!is.finite(p_home)) stop("Missing playoff probability for ", home_team, " vs ", away_team)
  if (runif(1) < p_home) home_team else away_team
}

higher_seed_home <- function(team_a, team_b, seed_map) {
  if (seed_lookup(seed_map, team_a) < seed_lookup(seed_map, team_b)) team_a else team_b
}

other_team <- function(home, a, b) if (home == a) b else a

simulate_conf_reseed <- function(season, conf, seed_df, rating_lookup, hfa_value, model_coef) {
  sm <- as.list(setNames(seed_df$seed, seed_df$team))
  format <- season_playoff_format(season)
  
  if (format == "FIVE_SAME_DIV") {
    t4 <- seed_df$team[seed_df$seed == 4]
    t5 <- seed_df$team[seed_df$seed == 5]
    wc_winner <- simulate_game(t4, t5, season, FALSE, rating_lookup, hfa_value, model_coef)
    
    divmap <- setNames(vapply(seed_df$team, function(t) team_division(season, t), character(1)), seed_df$team)
    pairs <- five_team_div_pairs(wc_winner, sm, divmap)
  } else if (format == "EIGHT_RESEED") {
    winners <- character(0)
    for (s in 1:4) {
      h <- seed_df$team[seed_df$seed == s]
      a <- seed_df$team[seed_df$seed == (9 - s)]
      winners <- c(winners, simulate_game(h, a, season, FALSE, rating_lookup, hfa_value, model_coef))
    }
    pairs <- reseed_pairs(winners, sm)
  } else if (format == "SIX_RESEED") {
    w1 <- simulate_game(
      seed_df$team[seed_df$seed == 3],
      seed_df$team[seed_df$seed == 6],
      season, FALSE, rating_lookup, hfa_value, model_coef
    )
    w2 <- simulate_game(
      seed_df$team[seed_df$seed == 4],
      seed_df$team[seed_df$seed == 5],
      season, FALSE, rating_lookup, hfa_value, model_coef
    )
    remaining <- c(
      seed_df$team[seed_df$seed == 1],
      seed_df$team[seed_df$seed == 2],
      w1, w2
    )
    pairs <- reseed_pairs(remaining, sm)
  } else if (format == "SEVEN_RESEED") {
    winners <- c(
      simulate_game(seed_df$team[seed_df$seed == 2], seed_df$team[seed_df$seed == 7],
                    season, FALSE, rating_lookup, hfa_value, model_coef),
      simulate_game(seed_df$team[seed_df$seed == 3], seed_df$team[seed_df$seed == 6],
                    season, FALSE, rating_lookup, hfa_value, model_coef),
      simulate_game(seed_df$team[seed_df$seed == 4], seed_df$team[seed_df$seed == 5],
                    season, FALSE, rating_lookup, hfa_value, model_coef)
    )
    remaining <- c(seed_df$team[seed_df$seed == 1], winners)
    pairs <- reseed_pairs(remaining, sm)
  } else {
    stop("simulate_conf_reseed called for unsupported format.")
  }
  
  div_winners <- character(0)
  for (pair in pairs) {
    home <- higher_seed_home(pair[1], pair[2], sm)
    away <- other_team(home, pair[1], pair[2])
    div_winners <- c(
      div_winners,
      simulate_game(home, away, season, FALSE, rating_lookup, hfa_value, model_coef)
    )
  }
  
  conf_finalists <- div_winners
  conf_home <- higher_seed_home(div_winners[1], div_winners[2], sm)
  conf_away <- other_team(conf_home, div_winners[1], div_winners[2])
  
  conf_winner <- simulate_game(
    conf_home, conf_away, season, FALSE,
    rating_lookup, hfa_value, model_coef
  )
  
  list(
    conference_finalists = conf_finalists,
    conference_winner = conf_winner
  )
}

simulate_conf_four_team <- function(season, conf, games, master_rows,
                                    rating_lookup, hfa_value, model_coef) {
  div <- games[
    games$season == season &
      games$conference == conf &
      games$round == "DIV",
    , drop = FALSE
  ]
  if (nrow(div) != 2) stop("Expected two opening divisional games for ", season, " ", conf)
  
  winners <- c(
    simulate_game(div$home_team[1], div$away_team[1], season, FALSE,
                  rating_lookup, hfa_value, model_coef),
    simulate_game(div$home_team[2], div$away_team[2], season, FALSE,
                  rating_lookup, hfa_value, model_coef)
  )
  
  if (season <= 1974) {
    conf_game <- games[
      games$season == season &
        games$conference == conf &
        games$round == "CONF",
      , drop = FALSE
    ]
    actual_host <- conf_game$home_team[1]
    
    actual_div_winners <- div$winner
    host_slot <- which(actual_div_winners == actual_host)
    
    if (length(host_slot) != 1) host_slot <- 1L
    home <- winners[host_slot]
    away <- winners[setdiff(1:2, host_slot)]
  } else {
    wp <- setNames(master_rows$win_pct, master_rows$team)
    if (wp[[winners[1]]] >= wp[[winners[2]]]) {
      home <- winners[1]
      away <- winners[2]
    } else {
      home <- winners[2]
      away <- winners[1]
    }
  }
  
  conf_winner <- simulate_game(
    home, away, season, FALSE,
    rating_lookup, hfa_value, model_coef
  )
  
  list(
    conference_finalists = winners,
    conference_winner = conf_winner
  )
}

simulate_season_playoffs_once <- function(season, playoff_games, seed_table,
                                          master_rows, model_coef) {
  season_rows <- master_rows[master_rows$season == season, , drop = FALSE]
  rating_lookup <- as.list(setNames(season_rows$rating, season_rows$team))
  hfa_value <- unique(season_rows$estimated_HFA)
  hfa_value <- hfa_value[is.finite(hfa_value)][1] %||% 0
  
  format <- season_playoff_format(season)
  
  results <- list()
  for (conf in c("AFC", "NFC")) {
    if (format %in% c("FOUR_FIXED_ROTATION", "FOUR_RECORD_HOME")) {
      results[[conf]] <- simulate_conf_four_team(
        season, conf, playoff_games, season_rows,
        rating_lookup, hfa_value, model_coef
      )
    } else {
      sdf <- seed_table[
        seed_table$season == season &
          seed_table$conference == conf,
        , drop = FALSE
      ]
      results[[conf]] <- simulate_conf_reseed(
        season, conf, sdf,
        rating_lookup, hfa_value, model_coef
      )
    }
  }
  
  afc <- results$AFC$conference_winner
  nfc <- results$NFC$conference_winner
  
  # Super Bowl is treated as neutral in the rating framework.
  sb_home <- afc
  sb_away <- nfc
  champion <- simulate_game(
    sb_home, sb_away, season, TRUE,
    rating_lookup, hfa_value, model_coef
  )
  
  list(
    conference_finalists = c(
      results$AFC$conference_finalists,
      results$NFC$conference_finalists
    ),
    super_bowl_teams = c(afc, nfc),
    champion = champion
  )
}

build_team_outcomes <- function(playoff_games, master) {
  seasons <- sort(unique(playoff_games$season))
  out <- list()
  k <- 1L
  
  for (season in seasons) {
    sg <- playoff_games[playoff_games$season == season, , drop = FALSE]
    teams <- sort(unique(c(sg$home_team, sg$away_team)))
    
    sb <- sg[sg$round == "SB", , drop = FALSE]
    champion <- if (nrow(sb) == 1) sb$winner[1] else NA_character_
    runnerup <- if (nrow(sb) == 1) sb$loser[1] else NA_character_
    
    conf_games <- sg[sg$round == "CONF", , drop = FALSE]
    conf_winners <- unique(conf_games$winner)
    
    for (team in teams) {
      tg <- sg[sg$home_team == team | sg$away_team == team, , drop = FALSE]
      max_round <- max(tg$round_order, na.rm = TRUE)
      rounds <- c("WC", "DIV", "CONF", "SB")
      reached <- rounds[max_round]
      
      finish <- if (team == champion) {
        "Super Bowl Champion"
      } else if (team == runnerup) {
        "Lost Super Bowl"
      } else if (team %in% conf_winners) {
        "Lost Super Bowl"
      } else if (any(tg$round == "CONF")) {
        "Lost Conference Championship"
      } else if (any(tg$round == "DIV")) {
        "Lost Divisional Round"
      } else {
        "Lost Wild Card / Opening Round"
      }
      
      wins <- sum(tg$winner == team, na.rm = TRUE)
      losses <- sum(tg$loser == team, na.rm = TRUE)
      
      out[[k]] <- data.frame(
        season = season,
        team = team,
        conference = team_conference(season, team),
        division = team_division(season, team),
        playoff_games = nrow(tg),
        playoff_wins = wins,
        playoff_losses = losses,
        furthest_round = reached,
        playoff_finish = finish,
        champion = as.integer(team == champion),
        super_bowl_appearance = as.integer(team %in% c(champion, runnerup)),
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  
  ans <- do.call(rbind, out)
  pre_cols <- c(
    "season", "team", "rank", "rating", "offense_rating", "defense_rating",
    "record", "wins", "losses", "ties", "win_pct", "played_SOS",
    "season_rating_z", "estimated_HFA", "predictive_margin_sd"
  )
  pre_cols <- intersect(pre_cols, names(master))
  merge(ans, master[, pre_cols, drop = FALSE], by = c("season", "team"), all.x = TRUE)
}

# NFL Playoff Analysis Pipeline, 1970-2025

This project builds a postseason layer on top of the existing regular-season power-rating system.

The regular-season ratings are **never overwritten**. The pipeline preserves the final regular-season rating as the team's pre-playoff strength, then creates separate postseason analyses for:

- playoff outcomes
- historical playoff seeds/brackets
- pregame playoff win probabilities
- upset rankings
- postseason performance versus expectation
- postseason-inclusive ratings
- playoff rating trajectories
- end-of-regular-season Super Bowl probabilities
- walk-forward historical Super Bowl probabilities
- one team-season playoff master table for Shiny

## Required inputs

### 1. Historical 1970-1998 game archive

Expected by default at:

`~/Downloads/historical_nfl_1970_1998/historical_nfl_1970_1998_games_COMPLETE.csv`

This is the file created by the historical rebuild script already used for the regular-season archive. It must include both regular-season and postseason games.

### 2. Enriched 1970-2025 regular-season rating archive

A copy is included in this project under:

`data/version12_1_1970_2025_all_season_team_summaries_enriched.csv`

The scripts use the final regular-season rating, offense, defense, record, SOS, HFA, and related fields from this file.

### 3. nflreadr

Install if necessary:

```r
install.packages("nflreadr")
```

The pipeline uses `nflreadr::load_schedules()` for 1999-2025 games.

## Run order

You can run everything with:

```r
source("run_all_playoff_pipeline.R")
```

Or run one stage at a time:

1. `01_build_playoff_archive.R`
2. `02_fit_playoff_model_and_upsets.R`
3. `03_post_playoff_ratings.R`
4. `04_simulate_super_bowl_odds.R`
5. `06_build_playoff_team_master.R`
6. `05_validate_playoff_pipeline.R`

Outputs are written by default to:

`~/Downloads/nfl_playoff_analysis_1970_2025/`

## Stage 1: outcomes, playoff formats, and seeds

`01_build_playoff_archive.R` combines:

- 1970-1998 postseason rows from the existing historical game file
- 1999-2025 postseason rows from nflreadr

Rounds are normalized to:

- `WC`
- `DIV`
- `CONF`
- `SB`

The simulator uses the historical playoff structure appropriate to each season:

- 1970-1974: four teams per conference, actual predetermined opening bracket
- 1975-1977: four teams per conference, actual opening bracket with record-based conference championship hosting
- 1978-1981 and 1983-1989: five teams per conference, one wild-card game, same-division restriction in the divisional round
- 1982: special eight-team-per-conference tournament
- 1990-2019: six teams per conference with reseeding
- 2020-2025: seven teams per conference with only the No. 1 seed receiving a bye

The script infers seeds from the actual postseason bracket wherever numeric seeds are needed.

### Manual seed corrections

The first run creates:

`playoff_seed_overrides.csv`

If the validation identifies an ambiguous historical seed, enter the corrected row there:

```text
season,conference,team,seed,note
1987,AFC,DEN,1,manual verification
```

Then rerun script 01.

## Stage 2: playoff probability model and upsets

The retrospective playoff win model is:

```text
logit(P(home win)) =
    intercept
  + beta_rating * (home final regular-season rating - away final regular-season rating)
  + beta_home * home-field indicator
```

Super Bowls use `home_field = 0`.

This model learns from all 1970-2025 playoff results. It therefore answers:

> Knowing the historical relationship between regular-season rating differences and postseason outcomes, how likely was each playoff game?

The script also creates a walk-forward probability for each game when enough earlier postseason data exist.

### Upset definition

A game is a retrospective upset when the eventual winner had a model win probability below 50%.

The ranked upset file includes:

- winner probability
- pre-playoff rating difference
- surprise score `-log(winner probability)`
- actual margin
- expected margin
- margin versus expectation

## Stage 3: ratings before and after the playoffs

The pre-playoff rating is the existing final regular-season rating.

For the postseason-inclusive rating, playoff games are appended to the season and the same offense/defense rating engine is refit after each playoff round.

The original regular-season rating remains separate.

Outputs include:

- `REG_FINAL`
- `AFTER_WC`
- `AFTER_DIV`
- `AFTER_CONF`
- `AFTER_SB`

for every playoff team where that stage exists.

Because the model is refit league-wide after each round, an eliminated team's postseason-inclusive rating can still move indirectly as opponents continue playing. That is intentional and is separate from the frozen pre-playoff rating used for playoff prediction.

## Stage 4: Super Bowl probabilities

For each season, the simulator starts from the actual playoff field and the frozen end-of-regular-season ratings.

The simulator:

1. uses the correct historical bracket format
2. assigns home field from the historical seed/bracket rules
3. predicts each game with the playoff logistic calibration
4. advances simulated winners
5. repeats the postseason `n_simulations` times

Default:

```r
n_simulations <- 50000
```

Change this in `00_config.R`.

Two championship probabilities are produced:

### Retrospective

Uses the playoff win model fit on all 1970-2025 playoff games.

### Walk-forward

For season Y, fits the playoff win model only on postseason games from seasons before Y.

This is the historical out-of-sample version.

## Main output files

- `01_playoff_games_1970_2025.csv`
- `02_playoff_team_outcomes_1970_2025.csv`
- `03_playoff_seeds_and_brackets_1970_2025.csv`
- `04_playoff_games_with_predictions.csv`
- `05_playoff_upsets_ranked.csv`
- `06_pre_vs_post_playoff_ratings.csv`
- `07_playoff_rating_trajectories.csv`
- `08_super_bowl_probabilities_by_season.csv`
- `08_super_bowl_probabilities_long.csv`
- `08_super_bowl_champion_preplayoff_odds.csv`
- `09_playoff_probability_model_validation.csv`
- `09_complete_playoff_pipeline_validation.csv`
- `10_playoff_team_game_performance.csv`
- `10_playoff_overperformance.csv`
- `11_playoff_team_season_master_1970_2025.csv`

## Recommended validation workflow

Run script 01 first and inspect:

`01_playoff_archive_validation.csv`

Then inspect the seed inference statuses in:

`03_playoff_seeds_and_brackets_1970_2025.csv`

Anything marked:

`ambiguous_chosen_by_record_order`

is a good candidate for manual verification before treating the historical Super Bowl simulations as final.

After the full pipeline finishes, inspect:

`09_complete_playoff_pipeline_validation.csv`

before connecting the files to Shiny.


## Historical archive fix

The 1970-1998 postseason round is now derived from chronological playoff
position and the actual playoff format for that season instead of trusting the
historical `raw_week` / `postseason_round` text.

Expected playoff game counts are now enforced:

- 1970-1998: 269
- 1999-2025: 309
- 1970-2025 total: 578

The pipeline stops immediately if any historical season is missing, so it cannot
silently generate modern-only playoff probabilities.

# starter; notwenig, um die korrekten standings zu berechnen (wk 1-13 etc.)
starter <- purrr::map_df(2016:var.lastSeason, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv")
  )
}) %>%
  dplyr::left_join(franchisesLatest %>% select(franchise_id, franchise_name), by = "franchise_id")

## reg season ----
weeklyTotals <- starter %>%
  # by position
  group_by(franchise_id, season, week, starter_status, should_start, pos) %>%
  summarise(
    pos_score = sum(player_score, na.rm = T),
    .groups = "drop"
  ) %>%
  spread(starter_status, pos_score) %>%

  # bugfix bei 100% eff, NA values werden zu 0
  mutate(
    nonstarter = ifelse(is.na(nonstarter), 0, nonstarter),
    starter = ifelse(is.na(starter), 0, starter)
  ) %>%

  # weekly totals
  group_by(franchise_id, season, week) %>%
  mutate(
    points_potential = ifelse(
      should_start == 1, (starter + nonstarter), 0
    )
  ) %>%
  group_by(franchise_id, season, week, pos) %>%
  summarise(
    starter = sum(starter),
    nonstarter = sum(nonstarter),
    potential = sum(points_potential),
    .groups = "drop"
  ) %>%

  mutate(
    unit = ifelse(
      pos %in% c("QB", "RB", "WR", "TE", "PK"), "offense", "defense"
    )
  ) %>%

  gather(
    key, score, c(starter, nonstarter, potential)
  ) %>%
  mutate(
    key = paste(unit, pos, key, sep = "_")
  ) %>%
  select(-pos, -unit) %>%
  spread(key, score) %>%
  replace(is.na(.), 0) %>%

  group_by(franchise_id, season, week) %>%
  mutate(
    points_for = rowSums(across(ends_with("_starter"), sum)),
    points_bank = rowSums(across(ends_with("nonstarter"), sum)),
    points_potential = rowSums(across(ends_with("potential"), sum)),
    efficiency = points_for / points_potential,
    offense_starter = rowSums(across(starts_with("offense") & ends_with("_starter"))),
    defense_starter = rowSums(across(starts_with("defense") & ends_with("_starter"))),
    offense_nonstarter = rowSums(across(starts_with("offense") & ends_with("nonstarter"))),
    defense_nonstarter = rowSums(across(starts_with("defense") & ends_with("nonstarter"))),
    offense_potential = rowSums(across(starts_with("offense") & ends_with("potential"))),
    defense_potential = rowSums(across(starts_with("defense") & ends_with("potential")))
  ) %>%

  select(
    season, week, franchise_id,
    starts_with("points"),
    efficiency,
    ends_with("_starter"),
    ends_with("nontarter"),
    ends_with("potential")
  ) %>%
  mutate(
    season_type = case_when(
      season == 2016 & week <= 13 ~ "REG",
      season == 2016 & week >= 18 ~ "NONE",
      season >= 2017 & season <= 2020 & week <= 12 ~ "REG",
      season >= 2017 & season <= 2020 & week >= 17 ~ "NONE",
      season >= 2021 & week <= 13 ~ "REG",
      season >= 2021 & week >= 18 ~ "NONE",
      TRUE ~ "POST"
    )
  ) %>%
  filter(season_type != "NONE")

seasonTotals <- weeklyTotals %>%
  filter(
    season_type == "REG"
  ) %>%
  group_by(franchise_id, season) %>%
  summarise(
    across(where(is.numeric), sum),
    efficiency = points_for / points_potential,
    .groups = "drop"
  )

# roster
roster <- ffscrapr::ff_rosters(conn)

# elo
elo <- purrr::map_df(2016:var.lastSeason, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/elo/rfl-elo-{x}.csv")
  )
}) %>%
  dplyr::arrange(season, week, franchise_id)

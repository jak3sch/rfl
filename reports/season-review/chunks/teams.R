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

# personnel groupings ----
rfl_personnel_raw <- starter %>%
  dplyr::filter(starter_status == "starter" & !pos %in% c("QB", "PK")) %>%
  dplyr::group_by(franchise_id, season, week, pos) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
  tidyr::spread(pos, count) %>%
  dplyr::mutate(
    season = as.numeric(season),
    dplyr::across(
      c("RB", "WR", "TE", "DT", "DE", "LB", "CB", "S"),
      ~ ifelse(is.na(.), 0, .)),
    offense_parent = WR, # number of WR
    offense = paste0(RB,WR,TE),
    defense_parent = paste0(DT+DE,LB,CB+S), # DL, LB, DB
    defense = paste0(DT, DE, LB, CB, S)
  ) %>%
  tidyr::gather(unit, grouping, dplyr::ends_with("parent")) %>%
  dplyr::mutate(unit = gsub("_parent", "", unit)) %>%
  tidyr::gather(key, personnel, c(offense, defense)) %>%
  dplyr::filter(unit == key) %>%
  dplyr::select(-key)

rfl_personnel <- rfl_personnel_raw %>%
  dplyr::group_by(franchise_id, season, unit, grouping, personnel) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

rfl_personnel_group_order <- rfl_personnel %>%
  dplyr::group_by(franchise_id, unit, grouping) %>%
  dplyr::summarise(count = sum(count), .groups = "drop") %>%
  dplyr::group_by(franchise_id, unit) %>%
  dplyr::arrange(dplyr::desc(count)) %>%
  dplyr::mutate(grouping_order = dplyr::row_number()) %>% # für fill value in plots
  dplyr::ungroup()
  
rfl_personnel <- rfl_personnel %>%
  dplyr::left_join(rfl_personnel_group_order %>% dplyr::select(-count), by = c("franchise_id", "unit", "grouping")) %>% 
  
  # above league data hinzufügen
  dplyr::group_by(season, unit, grouping, personnel) %>% 
  dplyr::mutate(season_league_avg = round(mean(count), 1)) %>% 
  dplyr::mutate(count_above_league_avg = paste(count - season_league_avg)) %>% 
  dplyr::select(-season_league_avg) %>% 
  dplyr::ungroup()
  
# Erstellen Sie eine Liste mit allen eindeutigen Kombinationen von franchise_id, season und unit in Ihrem DataFrame
combinations <- rfl_personnel %>%
  dplyr::distinct(franchise_id, season, unit)

rfl_personnel_annotations_loop <- dplyr::tibble()

# Schleife über jede Kombination und wenden Sie die Funktion an und fügen Sie das Ergebnis dem result_df hinzu
for (i in 1:nrow(combinations)) {
  tmp.franchise_id <- combinations[i, "franchise_id"]$franchise_id
  tmp.season <- combinations[i, "season"]$season
  tmp.unit <- combinations[i, "unit"]$unit

  df <- rfl_personnel %>%
    filter(franchise_id == tmp.franchise_id, season == tmp.season, unit == tmp.unit)

  treemapify_result <- treemapify(
    df,
    area = "count",
    subgroup = "grouping", subgroup2 = "personnel"
  ) %>%
  dplyr::mutate(
    xcenter = xmax - ((xmax - xmin) / 2),
    ycenter = ymax - ((ymax - ymin) / 2)
  )

  # Fügen Sie das Ergebnis dem result_df hinzu
  rfl_personnel_annotations_loop <- bind_rows(rfl_personnel_annotations_loop, treemapify_result)
}

# treemampify daten zu rfl personnel hinzufühen, um positionierung für avove legue avg zu haben
rfl_personnel <- rfl_personnel %>% 
  dplyr::left_join(rfl_personnel_annotations_loop %>% dplyr::select(1:5, 8:13), by = c("franchise_id", "season", "unit", "grouping", "personnel"))

## annotations ----
var.spacing <- 0.08

rfl_personnel_annotations <- rfl_personnel %>% 
  dplyr::select(-count_above_league_avg) %>% 
  dplyr::left_join(rfl_personnel_raw %>% dplyr::select(franchise_id, unit, personnel, CB:WR), by = c("franchise_id", "unit", "personnel"), multiple = "all", relationship = "many-to-many") %>% 

#  dplyr::mutate(
#    cols = 4,
#    col = round(((season - min(season)) / cols) + 1, 1),
#    col = dplyr::case_when(
#      stringr::str_ends(as.character(col), ".2") ~ 2,
#      stringr::str_ends(as.character(col), ".5") ~ 3,
#      stringr::str_ends(as.character(col), ".8") ~ 4,
#      TRUE ~ 1
#    ),
#    row = floor(((season - min(season)) / cols) + 1),
#    first_empty_season_col = col[which.max(season)] + 1, # finde zeile mit höchster season und returne col value
#    first_empty_season_row = row[which.max(season)],
#  ) %>%
  dplyr::group_by(franchise_id, unit, personnel) %>%
  dplyr::arrange(dplyr::desc(count)) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%   # filtere die größte kachel für jedes grouping eines teams

  # füge anzahl an icons hinzu
  tidyr::gather(position, count, CB:WR) %>%
  dplyr::filter(
    position %in% c("RB", "WR", "TE") & unit == "offense" |
    position %in% c("DT", "DE", "LB", "CB", "S") & unit == "defense"
  ) %>% 
  #dplyr::filter(franchise_id == "0003") %>%
  dplyr::slice(rep(row_number(), count)) %>%
  dplyr::group_by(franchise_id, season, unit, grouping, personnel, position) %>%
  dplyr::mutate(
    #shape = factor(position, levels = var.posOrder), # factor der positions um shape im plot manuell zu vergeben
    order = dplyr::row_number(),
    middle = count / 2,
    x = (xcenter + (var.spacing * order)),
    xoffset = x - ((var.spacing * count) / 2) - 0.025, # wert durch rumprobieren
    y = case_when(
      position %in% c("RB", "DE") ~ ycenter - var.spacing,
      position %in% c("WR", "CB") ~ ycenter + var.spacing,
      position == "DT" ~ ycenter - (var.spacing * 2),
      position == "S" ~ ycenter + (var.spacing * 2),
      TRUE ~ ycenter
    ),
    yoffset = case_when(
      stringr::str_detect(personnel, "^0.*[1-9]$") ~ y - (var.spacing / 2), # kein DT, aber S (beginnt mit 0, endet aber mit zahl)
      stringr::str_detect(personnel, "^[1-9].*0$") ~ y + (var.spacing / 2), # DT, aber kein S (beginnt mit zahl, aber endet mit 0)
      TRUE ~ y
    ),
    is_to_big = ifelse(
      xoffset > xmax - (var.spacing *2) | xoffset < xmin - (var.spacing * 2) | y > ymax - (var.spacing * 2) | y < ymin - (var.spacing * 2), 1, 0
    )
  ) %>%
  dplyr::group_by(franchise_id, season, unit, grouping, personnel) %>%
  dplyr::mutate(is_to_big = sum(is_to_big, na.rm = TRUE)) %>%

  # größte kachel für annotation der punkte
  dplyr::group_by(franchise_id, unit) %>%
  dplyr::mutate(
    A = (xmax - xmin) * (ymax - ymin),
    is_biggest = ifelse(A == max(A), 1, 0)
  ) %>%
  dplyr::ungroup()

rm(combinations, tmp.franchise_id, tmp.season, tmp.unit, treemapify_result, i, rfl_personnel_annotations_loop)

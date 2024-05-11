# needs rfl-drafts.R

fantasy_finishes_raw <- purrr::map_df(2016:var.season, function(x) {
  readr::read_rds(
    glue::glue("https://github.com/jak3sch/rfl/raw/main/data/playerscores/rfl-playerscores-{x}.rds"),
  )
})

rfl_fantasy_finishes <- fantasy_finishes_raw %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::group_by(season, player_id) %>%
  dplyr::summarise(
    pos = dplyr::last(pos),
    points = sum(as.numeric(points), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    rfl_drafts_data %>%
      dplyr::select(season, mfl_id) %>%
      dplyr::distinct() %>%
      dplyr::rename(draft_year = season),
    by = c("player_id" = "mfl_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::group_by(season, pos) %>%
  dplyr::arrange(dplyr::desc(points)) %>%
  dplyr::mutate(
    pos_rank = row_number(),
    top3 = ifelse(pos_rank <= 3, 1, 0),
    top5 = ifelse(pos_rank <= 5, 1, 0),
    top8 = ifelse(pos_rank <= 8, 1, 0),
    top12 = ifelse(pos_rank <= 12, 1, 0),
    top24 = ifelse(pos_rank <= 24, 1, 0),
    top36 = ifelse(pos_rank <= 36, 1, 0),
    top48 = ifelse(pos_rank <= 48, 1, 0),
    top60 = ifelse(pos_rank <= 60, 1, 0),
  )

draft_with_elo <- rfl_drafts_data %>%
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::filter(row_number() == max(row_number())) %>%
      dplyr::select(mfl_id, player_elo_post) %>%
      dplyr::mutate(elo_shift = player_elo_post - 1500),
    by = "mfl_id"
  )

# https://www.rotoballer.com/dynasty-primer-1-how-to-value-dynasty-draft-picks/1343067
draft_with_elo_and_hit_rates <- draft_with_elo %>%
  dplyr::left_join(
    rfl_fantasy_finishes %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise_at(c("top3", "top5", "top8", "top12", "top24", "top36", "top48", "top60"), sum, na.rm = TRUE),
    by = c("mfl_id" = "player_id")
  ) %>%
  dplyr::mutate(
    hr = dplyr::case_when(
      is_rookie == 1 & top5 > 2 ~ 1, # top 5 finish
      is_rookie == 1 & season > (max(season) - 3) & season < max(season) & top5 > 1 ~ 1, # Top5 für junge spieler
      is_rookie == 1 & position %in% c("QB", "TE", "PK") & top3 > 3 ~ 1, # top 3 finish
      is_rookie == 1 & position %in% c("RB", "WR", "DL", "LB", "DB") & top12 > 3 ~ 1, # top 12 finish
      TRUE ~ 0
    ),
    hit = dplyr::case_when(
      is_rookie == 1 & position %in% c("QB", "TE", "PK") & top8 > 3 ~ 1, # top 8 finish
      is_rookie == 1 & season > (max(season) - 3) & position %in% c("QB", "TE", "PK") & top12 > 1 ~ 1, # top 12 für junge spieler
      is_rookie == 1 & position %in% c("RB", "WR", "DL", "LB", "DB") & top36 > 3 ~ 1, # top 36 finish
      is_rookie == 1 & season > (max(season) - 3) & position %in% c("RB", "WR", "DL", "LB", "DB") & top24 > 1 ~ 1, # top 24 für junge spieler
      TRUE ~ 0
    ),
    miss = dplyr::case_when(
      is_rookie == 1 & season < (max(season) - 2) & position %in% c("QB", "TE", "PK") & top24 == 0 ~ 1, # kein top 24 finish
      is_rookie == 1 & season < (max(season) - 2) & position %in% c("RB", "DL", "DB") & top48 == 0 ~ 1, # kein top 48 finish
      is_rookie == 1 & season < (max(season) - 2) & position %in% c("WR", "LB") & top60 == 0 ~ 1, # kein top 48 finish
      TRUE ~ 0
    )
  )

draft_hit_rates <- draft_with_elo_and_hit_rates %>%
  #dplyr::filter(season > 2020)
  dplyr::group_by(round) %>%
  dplyr::summarise(
    count = n(),
    dplyr::across(c("hr", "hit", "miss"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate_at(
    c("hr", "hit", "miss"), ~ round(.x / count, 2)
  )

team_hit_rates <- draft_with_elo_and_hit_rates %>%
  #dplyr::filter(season > 2020) %>%
  dplyr::filter(franchise == "0007") %>%
  dplyr::group_by(franchise, round) %>%
  dplyr::summarise(
    count = n(),
    dplyr::across(c("hr", "hit", "miss"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate_at(
    c("hr", "hit", "miss"), ~ round(.x / count, 2)
  ) %>%
  tidyr::gather(category, pct, c("hr", "hit", "miss"))


draft_hit_rates %>%
  tidyr::gather(category, pct, c("hr", "hit", "miss")) %>%
  ggplot(aes(x = round, y = pct, fill = category, color = category)) +
  geom_col(position = "dodge") +
  geom_point(data = team_hit_rates, aes(size = count), group = 1, position = position_jitter(w = 0.1, h = 0))


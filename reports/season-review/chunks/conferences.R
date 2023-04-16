# einteilung div in conference ----
divInConf <- standings %>%
  select(season, division, division_name, conference_id) %>%
  distinct() %>%
  group_by(season, conference_id) %>%
  arrange(division_name) %>%
  mutate(div_rank_in_conf = row_number()) %>%
  ungroup()

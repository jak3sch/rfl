true_standing <- read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/rfl-true-standing.csv", colClasses = c("franchise_id" = "character")) %>% 
  dplyr::left_join(
    franchises %>% 
      dplyr::select(franchise_id, franchise_name, division_name),
    by = "franchise_id"
  )

current_standing <- read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/app/data/rfl-elo.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "franchise_elo_postgame" = "numeric", "franchise_score" = "numeric")) %>% 
  dplyr::filter(season == max(season)) %>% 
  dplyr::mutate(
    winloss = ifelse(score_differential > 0, 1, 0)
  ) %>% 
  dplyr::group_by(franchise_id) %>% 
  dplyr::arrange(week) %>% 
  dplyr::summarise(
    season = dplyr::first(season),
    week = dplyr::last(week),
    winloss = list(winloss),
    pf_sparkline = list(unique(franchise_score)),
    franchise_elo_postgame = dplyr::last(franchise_elo_postgame),
    elo_shift = dplyr::last(elo_shift),
    .groups = "drop"
  ) %>% 
  dplyr::left_join(
    read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/rfl-true-standing.csv", colClasses=c("franchise_id" = "character")) %>% 
      dplyr::group_by(franchise_id) %>% 
      dplyr::arrange(week) %>% 
      dplyr::summarise(
        pp_dist = list(unique(pp / 2) / week),
        dplyr::across(c(win, pf, pp, dplyr::ends_with("rank")), ~ dplyr::last(.x))
      ),
    by = "franchise_id"
  ) %>% 
  dplyr::arrange(desc(win), desc(pf)) %>% 
  dplyr::mutate(
    place = row_number(),
    loss = (2 * week) - win,
    pf = pf / 2,
    pp = pp / 2,
    across(pf_rank:coach_rank, ~ 37 - .x),
    elo_shift_norm = elo_shift - min(elo_shift)
  ) %>% 
  dplyr::left_join(
    franchises %>% 
      dplyr::select(franchise_id, franchise_name, division_name, conference_name),
    by = "franchise_id"
  ) %>%
  dplyr::mutate(
    subline = paste(division_name, conference_name, sep = ", ")
  ) %>% 
  dplyr::select(place, season, week, franchise_name, win, loss, winloss, pf_sparkline, pp_dist, 12:18, 5:6, elo_shift, subline)

true_standing <- read.csv(paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/true-standing/rfl-true-standing-", var.season, ".csv"), colClasses = c("franchise_id" = "character")) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name, division_name),
    by = "franchise_id"
  )

current_standing <- readr::read_csv(
    paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/elo/rfl-elo-", var.season, ".csv"),
    col_types = "ciiccnnnnnnn"
  ) %>%
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
    true_standing %>%
      dplyr::group_by(franchise_id) %>%
      dplyr::summarise(
        pp_dist = list(unique(pp / 2) / week),
        win = sum(win, na.rm = TRUE),
        dplyr::across(c(pf, pp, dplyr::ends_with("rank")), ~ dplyr::last(.x))
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
  dplyr::group_by(division_name) %>%
  dplyr::arrange(desc(win), desc(pf)) %>%
  dplyr::mutate(rank_div = row_number()) %>%
  dplyr::group_by(conference_name, rank_div) %>%
  dplyr::arrange(desc(win), desc(pf)) %>%
  dplyr::mutate(
    seed = case_when(
      rank_div == 1 ~ row_number()
    )
  ) %>%
  dplyr::group_by(conference_name) %>%
  dplyr::arrange(seed, desc(win), desc(pf)) %>%
  dplyr::mutate(
    seed = ifelse(is.na(seed), row_number(), seed),
    bowl = case_when(
      seed <= 2 ~ emoji::emoji("zzz"),
      seed >= 3 & seed <= 6 ~ emoji::emoji("trophy"),
      seed >= 7 & seed <= 12 ~ emoji::emoji("sports medal"),
      seed >= 13 ~ emoji::emoji("pile of poo")
    ),
    subline = paste(conference_name, division_name, sep = ", ")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(conference_name) %>%
  dplyr::select(season, week, franchise_name, win, loss, winloss, pf_sparkline, pp_dist, 12:17, 5:6, elo_shift, subline, seed, bowl, conference_name)

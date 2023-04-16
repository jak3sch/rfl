# franchise data ?ber kompletten liga zeitraum ----
franchiseList <- list()
divisionsList <- list() # get divisions inside conferences

for(year in var.firstSeason:var.lastSeason) {
  tmpLeagueData <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", year, "/export?TYPE=league&L=", var.mflLeagueID, "&JSON=1"))$league

  tmpFranchises <- tmpLeagueData$franchises$franchise %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::mutate(season = year)

  tmpDivisions <- tmpLeagueData$divisions$division %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::mutate(season = year)

  franchiseList[[year]] <- tmpFranchises
  divisionsList[[year]] <- tmpDivisions
}

franchises = do.call(rbind, franchiseList)
conferences = do.call(rbind, divisionsList) %>%
  dplyr::rename(conference_id = conference, division_id = id)
rm(franchiseList, tmpFranchises, divisionsList, year, tmpDivisions, tmpLeagueData)

# standings ----
## data ?ber kompletten liga zeitraum scrapen ----
standingsList <- list()

for(year in var.firstSeason:var.lastSeason) {
  tmpStandings <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", year, "/export?TYPE=leagueStandings&L=", var.mflLeagueID, "&APIKEY=&COLUMN_NAMES=&ALL=&WEB=&JSON=1"))$leagueStandings$franchise %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::mutate(season = year)

  # for old data w/o pp columns
  if (!("pp" %in% names(tmpStandings))) {
    tmpStandings$pp <- NA
  }

  standingsList[[year]] <- tmpStandings %>%
    dplyr::select(season, id, h2hw, h2hl, h2hpct, pf, avgpf, pa, avgpa, eff, pp)
}

standingsScraped = do.call(rbind, standingsList)
rm(standingsList, tmpStandings, year)

### korrekte PF, PP und Eff aus starter data ----
summarise_cols <- function(x) {
  tibble (
    sum = sum(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    last = last(x, na_rm = TRUE)
  )
}

standingsCalc <- matchups %>%
  ungroup() %>%
  dplyr::reframe(
    dplyr::across(c("points_for", "points_potential", "points_against", "sf_expw", "win", "games", "sf_luck", "sf_skill", "allplay_wins", "allplay_winpct"), summarise_cols, .unpack = TRUE),
    .by = c("season", "franchise_id")
  ) %>%
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("points_"), ~ .x / 2),
    allplay_losses = games_mean * 35 - allplay_wins_last
  ) %>%
  dplyr::rename(
    points_for = points_for_sum,
    points_potential = points_potential_sum,
    points_against = points_against_sum,
    sf_expw = sf_expw_last,
    h2h_wins = win_sum,
    games = games_mean,
    sf_luck = sf_luck_mean,
    sf_skill = sf_skill_mean,
    allplay_wins = allplay_wins_last,
    allplay_winpct = allplay_winpct_last
  ) %>%
  dplyr::select(season, franchise_id, points_for, points_potential, points_against, sf_expw, h2h_wins, games, sf_luck, sf_skill, allplay_wins, allplay_losses, allplay_winpct)

## korrekte standings ----
standings <- standingsCalc %>%
  # add division info for each franchise in each season
  dplyr::left_join(
    franchises %>%
      dplyr::select(id, season, division),
    by = c("franchise_id" = "id", "season")
  ) %>%

  # create div ranks
  dplyr::group_by(season, division) %>%
  dplyr::arrange(desc(h2h_wins), desc(points_for)) %>%
  dplyr::mutate(rank_division = row_number()) %>%
  dplyr::mutate(div_champ = ifelse(rank_division == 1, 1, 0)) %>%  # was team div champ?

  # create conference ranks
  dplyr::left_join(
    conferences %>%
      dplyr::select(-name),
    by = c("division" = "division_id", "season")
  ) %>%
    dplyr::group_by(season, conference_id) %>% #
    dplyr::arrange(desc(div_champ), desc(h2h_wins), desc(points_for)) %>% # sort by wins, div champ and points for
    dplyr::mutate(
      rank_conference = row_number(),
      super_bowl = ifelse(rank_conference <= 6, 1, 0), # top 6 of each conference plays in super bowl
      pro_bowl = ifelse(rank_conference > 6 & rank_conference <= 12, 1, 0), # 7-24 of each conf plays in pro bowl
      toilet_bowl = ifelse(rank_conference > 12, 1, 0), # 25-36 of each conf plays in toilet bowl
    ) %>%

  # create league rank
  dplyr::group_by(season) %>%
  dplyr::arrange(desc(div_champ), desc(h2h_wins), desc(points_for)) %>%
  dplyr::mutate(
    h2h_losses = (games * 2) - h2h_wins,
    rank_league = dplyr::row_number()
  ) %>%

  # https://simulatedfootball.com/leagues/formulas.html
  dplyr::mutate(
    efficiency = points_for / points_potential,
    points_per_game = points_for / games
  ) %>%
  #dplyr::reframe(dplyr::across(c("points_for"), calc_pctl, .unpack = TRUE), .by = "season")

  group_by(season) %>% #season avgs
  mutate(
    sf_aps = mean(points_for) / games,
    win_pct = h2h_wins / (games * 2),
    points_for_pctl_season = percent_rank(points_for),
    points_per_game_pctl_season = percent_rank(points_per_game),
    points_against_pctl_season = percent_rank(points_against),
    points_potential_pctl_season = percent_rank(points_potential),
    win_pctl_season = percent_rank(h2h_wins),
    sf_luck_pctl_season = percent_rank(sf_luck),
    sf_skill_pctl_season = percent_rank(sf_skill),
    sf_expw_pctl_season = percent_rank(sf_expw),
    efficiency_pctl_season = percent_rank(efficiency),
    allplay_win_pctl_season = percent_rank(allplay_winpct)
  ) %>%
  ungroup() %>%
  mutate(
    points_for_pctl_total = percent_rank(points_for),
    points_per_game_pctl_total = percent_rank(points_per_game),
    points_against_pctl_total = percent_rank(points_against),
    points_potential_pctl_total = percent_rank(points_potential),
    win_pctl_total = percent_rank(h2h_wins),
    sf_luck_pctl_total = percent_rank(sf_luck),
    sf_skill_pctl_total = percent_rank(sf_skill),
    sf_expw_pctl_total = percent_rank(sf_expw),
    efficiency_pctl_total = percent_rank(efficiency),
    allplay_win_pctl_total = percent_rank(allplay_winpct)
  ) %>%
  dplyr::left_join(franchisesLatest %>% select(franchise_id, franchise_name, division_name) %>% rename(latest_franchise_name = franchise_name), by = "franchise_id") %>%
  dplyr::left_join(franchisesLatest %>% select(conference, conference_name), by = c("conference_id" = "conference"), multiple = "first")

rm(standingsCalc, standingsScraped)


# div standings ----
divisionStandings <- standings %>%
  select(-starts_with("franchise"), -ends_with("against")) %>%
  group_by(division, season) %>%
  summarise(
    across(c(ends_with("pct"), efficiency), mean),
    across(where(is.numeric), sum),
    .groups = "drop"
  ) %>%
  select(-ends_with("_season"), -ends_with("_total")) %>%
  left_join(franchisesLatest %>% select(division, division_name), by = c("division"), multiple = "all") %>%
  left_join(standings %>% select(season, division, conference_name), by = c("division", "season"), multiple = "all") %>%
  mutate(season = as.character(season)) %>%
  distinct()

# strength of schedule ----
sosOpponent <- standings %>%
  dplyr::filter(season == var.lastSeason - 1) %>%
  dplyr::select(franchise_id, latest_franchise_name, allplay_winpct) %>%
  dplyr::rename(
    opp_allplay_winpct = allplay_winpct
  ) %>%
  dplyr::distinct()

# jede div hat einen gemeinsamen conf sos da die unterschiedlichen matchups aus der division hier nicht reinzählen und somit alle teams einer division gegen die selben gegner spielen
sos <- matchups %>%
  dplyr::filter(season == var.lastSeason & week <= 13) %>%
  dplyr::select(week, franchise_id, opponent_id) %>%
  dplyr::left_join(sosOpponent %>% select(-latest_franchise_name), by = c("opponent_id" = "franchise_id")) %>%
  dplyr::mutate(
    sos_allplay = opp_allplay_winpct / max(opp_allplay_winpct)
  ) %>%
  left_join(franchisesLatest %>% select(franchise_id, franchise_name, division, conference), by = "franchise_id") %>%
  left_join(franchisesLatest %>% select(franchise_id, division, conference) %>% rename(opp_div = division, opp_conf = conference), by = c("opponent_id" = "franchise_id")) %>%
  mutate(
    matchup = case_when(
      division == opp_div ~ "Division",
      conference == opp_conf ~ "Conference",
      TRUE ~ "Random"
    )
  ) %>%
  group_by(franchise_id, matchup) %>%
  mutate(
    sos_matchup = mean(sos_allplay)
  ) %>%
  group_by(franchise_id) %>%
  spread(matchup, sos_matchup) %>%
  summarise(
    Total = mean(sos_allplay, na.rm = TRUE),
    Division = mean(Division, na.rm = TRUE),
    Conference = mean(Conference, na.rm = TRUE),
    Random = mean(Random, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(franchisesLatest %>% select(franchise_id, franchise_name, division, conference), by = "franchise_id")

rm(sosOpponent)

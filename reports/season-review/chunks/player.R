# fantasy
#fantasyPoints <- readRDS(url(paste0("https://github.com/jak3sch/rfl/blob/main/data/playerscores/rfl-playerscores-", var.lastSeason, ".rds?raw=true"))) %>%
#  dplyr::mutate(
#    points = as.numeric(points),
#    week = as.integer(week)
#  )

# WAR
franchiseWARPlayerRaw <- readr::read_delim(paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-", var.lastSeason, ".csv"), delim = "; ", col_types = "cccnn") %>%
  dplyr::left_join(
    readr::read_delim(paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-", var.lastSeason - 1, ".csv"), delim = "; ", col_types = "cccnn") %>%
      dplyr::select(player_id, war) %>%
      dplyr::rename(war_last_season = war),
    by = "player_id"
  )

franchiseWARPlayer <- roster %>%
  dplyr::left_join(franchiseWARPlayerRaw %>% dplyr::select(player_id, points, war, war_last_season), by = "player_id", multiple = "any") %>%
  dplyr::mutate(
    war_per_fpt = war / points
  ) %>%
  group_by(franchise_id) %>%
  arrange(desc(war_per_fpt)) %>%
  dplyr::ungroup()

rm(franchiseWARPlayerRaw)

# Official Stats
#pbp <- nflfastR::load_pbp(var.lastSeason)
#playerStatsOffense <- nflfastR::calculate_player_stats(pbp)
#playerStatsDefense <- nflfastR::calculate_player_stats_def(pbp)

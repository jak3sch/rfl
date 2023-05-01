library(tidyverse)
library(gt)

last_season <- 2022

create_pos_rank <- function(df) {
    df %>%
    dplyr::select(player_id, pos, war) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(war = round(mean(war), 2)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(pos) %>%
    dplyr::arrange(dplyr::desc(war)) %>%
    dplyr::mutate(pos_rank = paste0(pos, " #", dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_id, war, pos_rank)
}

# base data ----
war <- purrr::map_df(2016:last_season, function(x) {
  readr::read_delim(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-{x}.csv"), delim = "; "
  ) %>%
  dplyr::mutate(season = x)
})

trade_baits <- jsonlite::read_json("https://www45.myfantasyleague.com/2023/export?TYPE=tradeBait&L=63018&APIKEY=aRNp3s%2BWvuWsx12mPlrBYDoeErox&INCLUDE_DRAFT_PICKS=0&JSON=1")$tradeBaits$tradeBait %>%
dplyr::tibble() %>%
tidyr::unnest_wider(1) %>%

# split col willGiveUp on , and create a new row for each player
tidyr::separate_rows(willGiveUp, sep = ",") %>%
dplyr::mutate(willGiveUp = as.numeric(willGiveUp)) %>%

# war last season
dplyr::left_join(
    war %>%
    dplyr::filter(season == last_season) %>%
    create_pos_rank() %>%
    dplyr::rename(last_season_war = war, last_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id")
) %>%

# war last 2 seasons
dplyr::left_join(
    war %>%
    dplyr::filter(season <= last_season & season >= last_season - 1) %>%
    create_pos_rank() %>%
    dplyr::rename(two_season_war = war, two_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id"),
    multiple = "first"
) %>%

# war last 3 seasons
dplyr::left_join(
    war %>%
    dplyr::filter(season <= last_season & season >= last_season - 2) %>%
    create_pos_rank() %>%
    dplyr::rename(three_season_war = war, three_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id"),
    multiple = "first"
) %>%

# war since 2016 seasons
dplyr::left_join(
    war %>%
    create_pos_rank() %>%
    dplyr::rename(all_season_war = war, all_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id"),
    multiple = "first"
) %>%
dplyr::left_join(war %>% dplyr::select(player_id, player_name, pos), by = c("willGiveUp" = "player_id"), multiple = "first") %>%
dplyr::select(-timestamp, -willGiveUp)

trade_baits %>%
dplyr::group_by(franchise_id) %>%
gt::gt() %>%
gt::cols_hide(inExchangeFor) %>%
gt::sub_missing(missing_text = "---") %>%
gt::tab_spanner(label = last_season, columns = c(last_season_war, last_season_pos_rank)) %>%
gt::tab_spanner(label = paste(last_season - 1, last_season, sep = "-"), columns = c(two_season_war, two_season_pos_rank)) %>%
gt::tab_spanner(label = paste(last_season - 2, last_season, sep = "-"), columns = c(three_season_war, three_season_pos_rank)) %>%
gt::tab_spanner(label = paste(2016, last_season, sep = "-"), columns = c(all_season_war, all_season_pos_rank)) %>%
gt::cols_label(
    last_season_war = "WAR",
    last_season_pos_rank = "Pos. Rank",two_season_war = "WAR",
    two_season_pos_rank = "Pos. Rank",
    three_season_war = "WAR",
    three_season_pos_rank = "Pos. Rank",
    all_season_war = "WAR",
    all_season_pos_rank = "Pos. Rank"
) %>%
gt::cols_move_to_start(columns = player_name) %>%
#gt::rm_stubhead() %>%
gt::data_color(
    columns = c(last_season_war, two_season_war, three_season_war, all_season_war),
    colors = c("white", "green")
)

# by position
trade_baits %>%
dplyr::group_by(pos) %>%
dplyr::arrange(desc(last_season_war)) %>%
gt::gt()

# league data ----
league <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", var.season, "/export?TYPE=league&L=63018&APIKEY=&JSON=1")) %>%
  purrr::pluck("league")

## franchise data ----
franchises <- league %>%
  purrr::pluck("franchises", "franchise") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    franchise_name = name,
    franchise_id = id
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("divisions", "division") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        division_name = name,
        division_id = id,
        conference_id = conference
      ),
    by = c("division" = "division_id")
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("conferences", "conference") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        conference_name = name
      ),
    by = c("conference_id" = "id")
  )

## starter data ----
starter <- purrr::map_df(2016:var.season, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv")
  )
})

## roster data ----
roster <- jsonlite::read_json(paste0("https://www55.myfantasyleague.com/", var.season, "/export?TYPE=rosters&L=63018&APIKEY=&FRANCHISE=&W=&JSON=1")) %>%
  purrr::pluck("rosters", "franchise") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::unnest(2) %>%
  dplyr::rename(franchise_id = id) %>%
  tidyr::unnest_wider(2) %>%
  dplyr::rename(player_id = id) %>%
  dplyr::left_join(
    players %>%
      select(player_id, player_name, pos),
    by = "player_id"
  )

# war data ----
war <- purrr::map_df(2016:var.season, function(x) {
  readr::read_delim(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-{x}.csv"), delim = "; "
  ) %>%
    dplyr::mutate(season = x)
})
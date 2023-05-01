load_starter <- function(seasons = 2016:nflreadr::most_recent_season()) {
  starter <- purrr::map_df(seasons, function(x) {
    readr::read_csv(
      glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv")
    )
  })

  return(starter)
}

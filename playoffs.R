# https://www.informationisbeautifulawards.com/showcase/3310-a-history-of-the-fifa-world-cup

library(tidyverse)
library(ggplot2)

# function to read rds from url with a season argument
read_data <- function(season) {
  readRDS(url(paste0("https://github.com/jak3sch/rfl/blob/main/data/schedule/rfl-results-postseason-", season, ".rds?raw=true"))) %>%
  dplyr::mutate(season = as.integer(season))
}

postseason_raw <- rbind(read_data(2016), read_data(2017), read_data(2018), read_data(2019), read_data(2020), read_data(2021))

postseason <- postseason_raw %>%
    dplyr::mutate(
        season = as.integer(season),
        week = as.numeric(week),
        bowl = ifelse(postseason %in% c("SBC", "SC"), "SB", postseason)
    ) %>%
    # für jedes team nur eine zeile pro woche
    tidyr::gather(key, team_id, c(away_id, home_id)) %>%
    #filter(team_id == "0005") %>%
    dplyr::group_by(bowl, season, team_id) %>%
    dplyr::arrange(season, week) %>%
    # für jedes jahr eine zeile pro bowl erstellen
    dplyr::mutate(
        first_week = dplyr::case_when(
            season == 2016 ~ 15,
            season <= 2020 ~ 12,
            TRUE ~ 13
        ),
        round = week - first_week,
        max_round = max(round),
        wl = ifelse(
            key == "home_id" & home_points > away_points | key == "away_id" & away_points > home_points, "W", "L"),
        result = dplyr::case_when(
            # nach 2016
            # Finale
            season > 2016 & postseason == "SBC" & wl == "W" ~ 1,
            season > 2016 & postseason == "SBC" & wl == "L" ~ 2,

            # Spiel um Platz 3
            season > 2016 & postseason == "SB" & round == 3 & wl == "W" ~ 3,
            season > 2016 & postseason == "SB" & round == 3 & wl == "L" ~ 4,

            # Halbfinale
            season > 2016 & postseason == "SB" & round == 2 & wl == "L" ~ 5,

            # Finale
            season > 2016 & postseason == "SB" & round == 1 & wl == "L" ~ 6,

            # andere Bowls
            bowl %in% c("PB", "TB") & round == 4 & wl == "W" ~ 1,
            bowl %in% c("PB", "TB") & round == 4 & wl == "L" ~ 2,

            bowl %in% c("PB", "TB") & round == 3 & wl == "L" ~ 4,

            bowl %in% c("PB", "TB") & round == 2 & wl == "L" ~ 5,

            bowl %in% c("PB", "TB") & round == 1 & wl == "L" ~ 6,

            # 2016
            season == 2016 & round == 3 & wl == "W" ~ 1,
            season == 2016 & round == 3 & wl == "L" ~ 2,
            season == 2016 & round == 2 & wl == "L" ~ 4,
            season == 2016 & round == 1 & wl == "L" ~ 5,
        )
    ) %>%
    dplyr::summarise(
        max_round = max(max_round, na.rm = TRUE),
        result = min(result, na.rm = TRUE),
        appearance = 1,
        .groups = "drop"
    ) %>%
    dplyr::group_by(team_id, bowl) %>%
    dplyr::arrange(season) %>%
    dplyr::mutate(
        total_appearances = sum(appearance),
        next_appearance = dplyr::lead(season)
    ) %>%
    dplyr::select(-appearance)

super_bowl <- postseason %>%
    dplyr::filter(
        bowl == "SB" & total_appearances > 1
    ) %>%
    dplyr::group_by(team_id, season) %>%
    dplyr::arrange(desc(result)) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::group_by(team_id) %>%
    dplyr::arrange(season) %>%
    dplyr::mutate(
        next_appearance = lead(season)
    ) %>%
    dplyr::select(season, team_id, total_appearances, result, next_appearance)

ggplot(super_bowl, aes(x = season, fill = result, y = 0.1)) +
    facet_wrap(~team_id, scales = "free_y", ncol = 2) +
    geom_point(aes(size = result * -1)) +
    geom_curve(aes(xend = next_appearance), yend = 0.1, curvature = -0.5) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

other_bowls <- postseason %>%
    dplyr::filter(
        bowl == "SB" & total_appearances == 1 | bowl != "SB"
    )

ggplot(other_bowls, aes(x = season, y = team_id, size = result * -1, color = bowl)) +
    geom_point()

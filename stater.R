#https://www.informationisbeautifulawards.com/showcase/5246-quarantine-visual-diary

library(tidyverse)
library(treemapify)
library(ggplot2)

group <- paste("Group", 1:9)
subgroup <- c("A", "C", "B", "A", "A",
              "C", "C", "B", "B")
value <- c(7, 25, 50, 5, 16,
           18, 30, 12, 41)

df <- data.frame(group, subgroup, value) 

ggplot(df, aes(area = value, fill = group)) +
  geom_treemap()


starter <- purrr::map_df(2016:2022, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv")
  )
})

clean_data <- starter %>%
    dplyr::filter(franchise_id == "0007" & starter_status == "starter" & !pos %in% c("QB", "PK")) %>%
    dplyr::group_by(franchise_id, season, week, pos) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    tidyr::spread(pos, count) %>%
    dplyr::mutate(
        dplyr::across(
            c("RB", "WR", "TE", "DT", "DE", "LB", "CB", "S"),
            ~ ifelse(is.na(.), 0, .)),
        offense = paste0(RB,WR,TE),
        defense_parent = paste0(DT+DE,LB,CB+S),
        defense = paste0(DT, DE, LB, CB, S)
    ) %>%
    tidyr::gather(key, personnell, c(offense, defense)) %>%
    dplyr::group_by(season, key, personnell, defense_parent) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(
        unit = ifelse(key == "offense", "offense", "defense")
    )
    tidyr::spread(personnell, count)

plot_personnell_groupings <- function(data, subgroup = unit) {
    ggplot(data, aes(area = count, fill = personnell, label = personnell, subgroup = {{subgroup}})) +
        facet_wrap(~season) +
        geom_treemap() +
        geom_treemap_text(color = "white", place = "centre") +
        geom_treemap_subgroup_border(color = "red")
}

plot_personnell_groupings(
    clean_data %>%
        dplyr::filter(unit == "offense") %>%
        dplyr::group_by(season, personnell) %>%
        dplyr::summarise(count = sum(count), .groups = "drop"),
        personnell
)

plot_personnell_groupings(
    clean_data %>%
    dplyr::filter(unit != "offense") %>%
    dplyr::mutate(
        unit = defense_parent
    )
) +
    geom_treemap_text(aes(label = unit), color = "red", place = "bottom")

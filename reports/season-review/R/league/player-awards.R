# data ----
league_awards_data <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/awards/rfl-player-awards.csv") %>%
  dplyr::left_join(nflreadr::load_players() %>% dplyr::select(gsis_id, headshot), by = "gsis_id")

## MVPs ----
league_awards <- league_awards_data %>%
  dplyr::mutate(
    points_total_pctl = dplyr::percent_rank(points),
    war_total_pctl = dplyr::percent_rank(war)
  ) %>%
  dplyr::group_by(award) %>%
  dplyr::mutate(
    points_award_pctl = dplyr::percent_rank(points),
    war_award_pctl = dplyr::percent_rank(war)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(
    points_season_pctl = dplyr::percent_rank(points),
    war_season_pctl = dplyr::percent_rank(war)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(season == var_season_last & rank == 1) %>%
  tidyr::gather(category, pctl, dplyr::ends_with("pctl")) %>%
  dplyr::mutate(
    group = dplyr::case_when(
      grepl("season", category) ~ paste0("All award candidates ", var_season_last),
      grepl("award", category) ~ paste0("All candidates for the same award ", var_season_first, "-", var_season_last),
      TRUE ~ paste0("All award candidates ", var_season_first, "-", var_season_last),
      #grepl("season", category) ~ paste0("Alle Spieler der Saison ", var_season_last),
      #grepl("award", category) ~ paste0("Alle Spieler des gleichen Awards ", var_season_first, "-", var_season_last),
      #TRUE ~ paste0("Alle Spieler ", var_season_first, "-", var_season_last)
    ),
    category_f= factor(
      category,
      levels = c("points_total_pctl", "points_award_pctl", "points_season_pctl", "war_season_pctl", "war_award_pctl", "war_total_pctl"),
    #labels =  c(
    #  paste0("Liga\n", substr(var_season_first, 3, 4), "-", substr(var_season_last, 3, 4)),
    #  paste0("Pos\n", substr(var_season_first, 3, 4), "-", substr(var_season_last, 3, 4)),
    #  var_season_last,
    #  var_season_last,
    #  paste0("Pos\n", substr(var_season_first, 3, 4), "-", substr(var_season_last, 3, 4)),
    #  paste0("Liga\n", substr(var_season_first, 3, 4), "-", substr(var_season_last, 3, 4))
    #)
    ),
    award_f = factor(award, c("MVP", "OPOY", "DPOY", "CPOY", "APOY", "GPOY", "OROY", "DROY")),
    subline = dplyr::case_when(
      award == "MVP" ~ "Most WAR",
      award == "OPOY" ~ "Most WAR: Offense non-MVP",
      award == "DPOY" ~ "Most WAR: Defense non-MVP",
      award == "CPOY" ~ "Highest WAR change to previous\nseason and ended season on IR",
      award == "APOY" ~ "Most Fantasy Points\nthrough the air (QB)",
      award == "GPOY" ~ "Fantasy Points\non the air",
      award == "OROY" ~ "Most WAR: Offensive Rookie",
      award == "DROY" ~ "Most WAR: Defensive Rookie",
      #award == "MVP" ~ "Die meisten WAR",
      #award == "OPOY" ~ "Die meisten WAR: Offense non-MVP",
      #award == "DPOY" ~ "Die meisten WAR: Defense non-MVP",
      #award == "CPOY" ~ "Die größte WAR Veränderung zum Vorjahr\nund Vorsaison auf IR beendet",
      #award == "APOY" ~ "Die meisten Fantasy Punkte\ndurch die Luft (QB)",
      #award == "GPOY" ~ "Die meisten Fantasy Punkte\nam Boden",
      #award == "OROY" ~ "Die meisten WAR: Offense Rookie",
      #award == "DROY" ~ "Die meisten WAR: Defense Rookie",
    ),
  )

# plots ----
## awards last season ----
segments <- data.frame(
  x1 = c(rep(0.4, 3), 1, 4),
  x2 = c(rep(6.5, 3), 3, 6),
  y1 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  y2 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  color = c(rep(color_muted, 3), rep(color_accent, 2))
)

#chart <-

ggplot(league_awards, aes(x = category_f, y = pctl)) +
  ggplot2::facet_wrap(~ award_f, ncol = 4) +
  geom_col(mapping = aes(fill = group), width = 0.97) +
  coord_polar(start = -3.1, clip = "off") +
  theme_void() +
  scale_y_continuous(limits = c(-1.5, 1.2)) +
  scale_x_discrete(expand = ggplot2::expansion(add = 4)) +
  geom_textpath(mapping = aes(label = category_f, y = 1), size = 3, vjust = 1)+
  geom_segment(data = segments, mapping = aes(x = x1, xend = x2, y = y1, yend = y2), linewidth = 0.3) +

  ggplot2::geom_point(aes(stroke = war_pct, size = war), color = color_accent, size = 35) +
  #geom_image(aes(image = cropcircles::circle_crop(headshot, border_size = 20, border_colour = color_bg)), asp = 1.13, size = 0.4) +

  ggplot2::geom_text(aes(label = display_name), x = 0.5, y = 3, hjust = 0.5, vjust = 1, color = color_accent, size = 4.7) +
  ggplot2::geom_text(aes(label = toupper(label)), x = 0.5, y = 2.55, hjust = 0.5, vjust = 1) +

  ggplot2::geom_text(aes(label = subline), x = 0.5, y = -0.7, hjust = 0.5, vjust = 1, color = color_light) +

  ggplot2::scale_x_continuous(limits = c(0,1)) +
  ggplot2::scale_y_continuous(limits = c(-1,3)) +
  ggplot2::labs(
    title = toupper(paste("Spieler-Awards", var_season_last)),
    subtitle = "Die wertvollsten Spieler nach Kategorie",
  ) +
  ggplot2::theme(
    plot.margin = margin(15, 15, 30, 15),
    legend.position = "top",
    panel.spacing = unit(1, "lines")
  )

output <- output %>%
  officer::add_slide(layout = "Bild") %>%
  officer::ph_with(value = chart, location = officer::ph_location_label("img"))

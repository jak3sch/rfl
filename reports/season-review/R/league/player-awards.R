# plot ----
plot_defaults_mvp_bar <- list(
  ggplot2::geom_text(mapping = ggplot2::aes(label = war), vjust = 1.7, size = 10, color = c_background, family = "accent"),
  ggplot2::geom_text(ggplot2::aes(label = sapply(display_name, function(x) paste(strwrap(x, width = 5), collapse = "\n"))), vjust = -0.5, hjust = 0.5, size = 7, family = "base", color = color_light, lineheight = 0.3),
  ggplot2::geom_hline(yintercept = 0, color = c_light, linewidth = 0.3),
  nflplotR::geom_nfl_headshots(aes(player_gsis = gsis_id), y = 0, height = 0.25, vjust = 0),
  ggplot2::scale_fill_manual(values = v_colors, guide = "none")
)

# data ----
league_awards_data <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/awards/rfl-player-awards.csv", col_types = "iiccccdddccc") %>%
  dplyr::left_join(nflreadr::load_players() %>% dplyr::select(gsis_id, headshot), by = "gsis_id")

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
  tidyr::gather(category, pctl, dplyr::ends_with("pctl")) %>%
  dplyr::mutate(
    group = dplyr::case_when(
      grepl("season", category) ~ paste0("Alle Award Anwärter der Saison ", v_season_last),
      grepl("award", category) ~ paste0("Alle Spieler des gleichen Awards ", v_season_first, "-", v_season_last),
      TRUE ~ paste0("Alle Award Anwärter ", v_season_first, "-", v_season_last)
    ),
    category_f= factor(
      category,
      levels = c("points_total_pctl", "points_award_pctl", "points_season_pctl", "war_season_pctl", "war_award_pctl", "war_total_pctl")
    ),
    award_f = factor(award, c("MVP", "OPOY", "DPOY", "CPOY", "APOY", "GPOY", "OROY", "DROY")),
    subline = dplyr::case_when(
      award == "MVP" ~ "Die meisten WAR",
      award == "OPOY" ~ "Die meisten WAR: Offense non-MVP",
      award == "DPOY" ~ "Die meisten WAR: Defense non-MVP",
      award == "CPOY" ~ "Die größte WAR Veränderung zum Vorjahr\nund Vorsaison auf IR beendet",
      award == "APOY" ~ "Die meisten Fantasy Punkte\ndurch die Luft (QB)",
      award == "GPOY" ~ "Die meisten Fantasy Punkte\nam Boden",
      award == "OROY" ~ "Die meisten WAR: Offense Rookie",
      award == "DROY" ~ "Die meisten WAR: Defense Rookie",
    ),
  )

league_awards_last_season <- league_awards %>%
  dplyr::filter(season == v_season_last & rank == 1)

# plots ----
## helper ----
player_annotaions <- function() {
  player_data <- league_awards_last_season %>%
    dplyr::group_by(gsis_id) %>%
    dplyr::filter(dplyr::row_number() == 1)

  player_annotations <- list(
    # award title
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = award),
      x = 3.5, y = 1.28, hjust = 0.5, color = c_accent, family = "accent", size = 8
    ),
    # player name
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = display_name),
      x = 3.5, y = -3.5, hjust = 0.5, color = c_light, family = "accent", size = 10
    ),
    # award description
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = subline),
      x = 3.5, y = -4.05, vjust = 1, hjust = 0.5, color = c_muted, family = "base", size = 8, lineheight = 0.35
    )
  )

  return(player_annotations)
}

## awards last season ----
segments <- data.frame(
  x1 = c(rep(0.5, 3), 1, 4),
  x2 = c(rep(6.5, 3), 3, 6),
  y1 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  y2 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  color = c(rep(c_background, 3), rep(c_accent, 2))
)

league_awards_plot <- ggplot2::ggplot(league_awards_last_season, aes(x = category_f, y = pctl)) +
  ggplot2::facet_wrap(~ award_f, ncol = 4) +
  ggplot2::coord_polar(start = -3.15, clip = "off") +

  ggplot2::geom_col(mapping = ggplot2::aes(alpha = group), fill = c_light, width = 0.97, position = "dodge") +

  # custom grid lines and annotations
  ggplot2::geom_text(label = "FPts", x = 5.85, y = -4.4, color = c_muted, size = 6, family = "base", vjust = 1, hjust = 0.5, lineheight = 0.35) +
  ggplot2::geom_text(label = "WAR", x = 6.15, y = 1.4, color = c_muted, size = 6, family = "base", vjust = 1, hjust = 0.5, lineheight = 0.35) +
  ggplot2::geom_segment(data = segments, mapping = aes(x = x1, xend = x2, y = y1, yend = y2, color = color), linewidth = 0.3) +
  ggplot2::scale_color_identity()


league_awards_plot

for (player in unique(league_awards_last_season$award)) {
  league_awards_plot <- league_awards_plot +
    player_annotaions()
}

league_awards_plot <- league_awards_plot +
  ggnewscale::new_scale_color() +

  # player background
  ggplot2::geom_point(mapping = aes(color = pos), y = -1.5, size = 21.5) +

  # player
  ggimage::geom_image(
    data = league_awards_last_season %>% dplyr::group_by(award) %>% dplyr::filter(dplyr::row_number() == 1),
    mapping = ggplot2::aes(image = cropcircles::circle_crop(headshot)),
    y = -1.5,
    asp = 1, size = 0.39
  ) +

  ggplot2::scale_color_manual(values = v_colors, guide = "none") +
  ggplot2::scale_alpha_manual(values = c(0.3, 1, 0.6)) +
  ggplot2::scale_y_continuous(limits = c(-1.5, 1.35)) +
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 2.5)) +

  ggplot2::labs(
    title = paste("Spieler Auszeichnungen", v_season_last),
    subtitle = "Die Grafik zeigt die besten Spieler in ihrer Kategorie. Die Balken stehen für die Perzentile,\nin denen der Spieler zwischen den Top-3 (Award Anwärter) ihrer Kategorie gerankt ist.",
    alpha = ""
  ) +

  default_plot_minimal +
  ggplot2::theme(
    legend.position = "top",
    strip.text = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(5, "mm"),
    panel.spacing.y = ggplot2::unit(0, "mm")
  )

league_awards_plot

output <- output %>% 
  officer::add_slide(layout = "Bild") %>% 
  officer::ph_with(value = league_awards_plot, location = officer::ph_location_label("img"))


# recent mvps ----
recent_mvps <- league_awards %>%
  dplyr::filter(award == "MVP" & rank == 1) %>%
  dplyr::select(season, gsis_id, display_name, pos, war) %>%
  dplyr::distinct()

recent_mvps_plot <- ggplot2::ggplot(recent_mvps, aes(x = season, y = war, fill = pos)) +
  ggplot2::geom_col() +

  plot_defaults_mvp_bar +

  ggplot2::scale_x_continuous(breaks = c(var_season_first:var_season_last)) +
  ggplot2::scale_y_continuous(limits = c(0, max(recent_mvps$war) + 1)) +

  ggplot2::labs(
    title = paste0("Liga MVPs ", var_season_first, "-", var_season_last),
    subtitle = "Die Grafik zeigt die Spieler mit den meisten WAR einer jeden Saison.",
    fill = ""
  ) +

  default_plot_minimal +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(color = color_light, family = "accent", size = 20),
  )

rm(player_annotaions, segments, player)

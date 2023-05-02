library(tidyverse)
library(patchwork)
library(camcorder)
library(ggtext)
library(ggplot2)
library(sysfonts)
library(showtext)
library(gtExtras)
library(webshot2)

# data ----
data <- readr::read_csv("data.csv") %>% 
  dplyr::group_by(unit) %>% 
  dplyr::group_modify(~ dplyr::add_row(.x, .before = 0)) %>% 
  dplyr::mutate(season = ifelse(dplyr::row_number() == 1, 2015, season))
annotations <- readr::read_csv("annotations.csv")

title <- "RFL Postseason
Appearances"
subtitle <- "Visualization of each teams appearances in
the RFL postseason between 2016 and 2022"

copy <- dplyr::tibble(
  content = "The Rocketbeans Football League (RFL) consists of 36 teams playing in 2
conferences with 3 divisions each.

The 3 division winners and the 3 next best teams play in the Superbowl for the league
title, while the next best 12 teams play in the Probowl and the 12 worst teams play
in the Toiletbowl for the honor.",
  x = 0,
  y = 0.59
)

caption <- 'Twitter: @JakobEschler - Website: jakob-eschler.de - Github: jak3sch'

# start recording ----
camcorder::gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 297, # width of saved image
  height = 210, # height of saved image
  units = "mm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# plot ----
color_bg <- "#222f3e"
color_light <- "#c8d6e5"
color_accent <- "#feca57"
colors <- c("#f9ca24", "#c8d6e5", "#f0932b", "#2e86de")

sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

plot_reset <- theme(
  plot.background = element_rect(color = NA, fill = color_bg),
  plot.margin = ggplot2::margin(c(0, 0, 0, 0))
)

## plot helper ----
plot_default <- list(
  ggplot2::scale_color_manual(values = colors),
  ggplot2::theme_void(),
  ggplot2::labs(
    color = ""
  ),
  ggplot2::theme(
    legend.position = "top"
  )
)

function(data) {
  ggplot2::ggplot(data, aes(area = count, fill = as.factor(grouping_order), subgroup = grouping, subgroup2 = personnel)) +
    ggplot2::facet_wrap(~season, ncol = 4) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup2_border(color = color_bg, size = 3) +
    
    ggplot2::geom_text(data = subset(data, season != 2015), aes(label = season), x = 0.05, y = 0.05, vjust = 0, hjust = 0, family = "base", fontface = "bold", color = color_bg, size = 4.5) +
    ggplot2::geom_text(data = subset(data, xmax - xmin > 0.1), aes(label = ifelse(count_above_league_avg > 0, paste0("+", count_above_league_avg), count_above_league_avg), x = xmax - 0.05, y = ymax - 0.05), hjust = 1, vjust = 1, family = "base", fontface = "bold", color = color_bg, size = 4.5) +
    
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_color_manual(values = colors_shape) +
    ggplot2::scale_shape_manual(values = c(17, 15, 19, 17)) +
    ggplot2::guides(fill = "none", color = "none", shape = guide_legend()) +
    ggplot2::coord_fixed() +
    
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    
    ggplot2::theme_void() +
    plot_reset +
    plot_default +
    ggplot2::theme (
      legend.position = "none",
      panel.spacing = ggplot2::unit(3, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = 22,
        color = color_accent,
        family = "accent"
      )
    )
}

# layout ----
## right ----
right <- ggplot2::ggplot(super_bowl, aes(x = season, color = result, y = 0.1)) +
  ggplot2::facet_wrap(~team_id, scales = "free_y", ncol = 2, strip.position = "left") +
  ggplot2::geom_curve(aes(xend = next_appearance, color = result_color), curvature = -0.5, yend = 0.1) +
  ggplot2::geom_point(aes(size = result, color = result_color)) +
  #ggplot2::geom_text(aes(label = season), y = 0) +
  
  ggplot2::scale_size_continuous(range = c(3, 1), guide = "none") +
  ggplot2::scale_y_continuous(limits = c(-0.5, 1), expand = c(0, 0)) +
  
  ggplot2::labs(
    title = "Superbowl Appearances",
    subtitle = "All teams with at least two appearances in the RFL Super Bowl",
  ) +
  
  plot_default +
  ggplot2::theme(
    plot.title = ggplot2::element_text(color = color_accent, family = "accent", hjust = 0.5, size = 30),
    plot.subtitle = ggplot2::element_text(color = color_light, family = "base", hjust = 0.5, size = 18, ggplot2::margin(b = 8, unit = "mm")),
    strip.text.y.left = ggplot2::element_text(angle = 90, color = color_light, family = "accent")
  )

right <- ggplot2::ggplot() +
  ggplot2::theme_void() +
  offense +
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  defense +
  patchwork::plot_layout(ncol = 1, heights = c(0.17, 0.4095, 0.0005, 0.4095)) +
  patchwork::inset_element(patchwork::wrap_elements(grid_subtitle("Offense")), top = 2.35, left = 0, right = 0.25, bottom = 2) +
  patchwork::inset_element(patchwork::wrap_elements(grid_subtitle("Defense")), top = 1.06, left = 0, right = 0.25, bottom = 0.5)

## left ----
### other bowls ----
other_bowls <- ggplot(data = subset(postseason, bowl == "SB" & total_appearances == 1 | bowl != "SB"), aes(x = season, y = team_id, size = result, color = bowl)) +
  ggplot2::geom_point(position = ggplot2::position_nudge(y = 0.5)) +
  ggplot2::scale_size_continuous(range = c(5, 1), guide = "none") +
  plot_default +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(color = color_light, size = 0.5)
  )

### combine ----
left <- patchwork::wrap_elements(
  ggplot(copy, aes(x = x, y = y, label = content)) +
    ggplot2::geom_text(
      size = 11,
      hjust = 0,
      vjust = 1,
      color = color_light,
      family = "base",
      lineheight = 0.4
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_void() +
    
    ggplot2::theme(
      plot.margin = ggplot2::margin(0),
      plot.title = ggplot2::element_text(
        size = 120,
        hjust = 0,
        family = "accent",
        color = color_accent,
        lineheight = 0.3,
        margin = ggplot2::margin(b = 8, unit = "mm")
      ),
      plot.subtitle = ggplot2::element_text(
        size = 42,
        family = "base",
        face = "bold",
        hjust = 0,
        lineheight = 0.4,
        color = color_light
      )
    ) +
    
    other_bowls +
    patchwork::plot_layout(ncol = 1, heights = c(1, 1.3)) &
    plot_reset
)

# final ----
page_layout <- "
  11111122222
"

#final <-

left + right +
  patchwork::plot_layout(design = page_layout) +
  patchwork::plot_layout(widths = c(rep(297, 2)), heights = 210) &
  ggplot2::theme(
    plot.margin = ggplot2::margin(c(6, 4.5, 5.7, 5.7), unit = "mm"),
    plot.background = element_rect(color = NA, fill = color_bg)
  )

final
# export ----
camcorder::gg_playback(
  name = "plot.gif",
  device = "png",
  first_image_duration = 5,
  last_image_duration = 10,
  frame_duration = .5,
  background = color_bg,
  image_resize = 800,
  stoprecording = TRUE
)

ggplot2::ggsave("plot.png", final, width = 297, height = 210, units = "mm")
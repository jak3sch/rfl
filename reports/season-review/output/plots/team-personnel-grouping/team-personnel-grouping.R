library(tidyverse)
library(patchwork)
library(camcorder)
library(ggtext)
library(ggplot2)
library(treemapify)
library(sysfonts)
library(showtext)

# data
data <- readr::read_csv("data.csv")
annotations <- readr::read_csv("annotations.csv")
  dplyr::mutate(shape = factor(shape, levels = c("QB", "RB", "WR", "TE", "PK", "DT", "DE", "LB", "CB", "S")))
  
title <- "Personnel Groupings
used by Jena Dragons"
subtitle <- "Visualization of the used fantasy football lineup
variations between 2016 and 2022"
  
copy <- dplyr::tibble(
  content = "Since the launch of the Rocketbeans Football League (RFL) in 2016, numerous
matchups have been played. The lineup rules call for 1 QB, 2 RB, 2 WR, 1 TE,
1 PK and 2 flex spots for the offense. For the defense, 2 DL (DT and DE), 2 LB,
and 2 DB (CB and S) positions are required, as well as 3 flex spots.

These requirements provide ample room for individual decisions, which I
wanted to visualize. The result will be part of the comprehensive league report
for last season.",
  x = 0,
  y = 0.6
)

caption <- "@JakobEschler jakob-eschler.de jak3sch"

# start recording
camcorder::gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 13, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

camcorder::gg_stop_recording()

# plot ----
color_bg <- "#222f3e"
color_light <- "#c8d6e5"
color_accent <- "#feca57"
colors <- c("#2e86de", "#ee5253", "#10ac84",  "#ff9f43")
colors_shape <- c("#ff9ff3", "#5f27cd", "#feca57", "#54a0ff")

plot_width <- 297
plot_height <- 210

sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

plot_reset <- theme(
  plot.background = element_rect(color = NA, fill = color_bg),
  plot.margin = ggplot2::margin(c(0, 0, 0, 0))
)

plot_theme <- list(
  ggplot2::theme_void(),
  theme(
    #plot.background = element_rect(fill = color_bg, color = NA)
  )
)

## plot helper ----
plot <- function(data, title = NULL) {
  ggplot2::ggplot(data, aes(area = count, fill = as.factor(grouping_order), subgroup = grouping, subgroup2 = personnel)) +
    ggplot2::facet_wrap(~season, ncol = 4) +

    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup2_border(color = color_bg, size = 3) +

    ggplot2::geom_text(aes(label = season), x = 0.05, y = 0.05, vjust = 0, hjust = 0, family = "base", fontface = "bold", color = color_bg, size = 3.2) +

    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_color_manual(values = colors_shape) +
    ggplot2::scale_shape_manual(values = c(17, 15, 19, 17)) +
    ggplot2::guides(fill = "none", color = "none", shape = guide_legend()) +

    ggplot2::theme_void() +
    plot_reset +
    ggplot2::theme (
      #plot.margin = ggplot2::margin(t = 1, unit = "cm"),
      #aspect.ratio = 1,
      legend.position = "none",
      panel.spacing = ggplot2::unit(1, "lines"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = 22,
        color = color_accent,
        family = "accent"
      )
    ) +
    
    ggplot2::coord_fixed() +

    ggplot2::labs(
      title = {{title}},
      x = NULL,
      y = NULL
    )
}

## add points helper ----
points <- function(data, pos) {
  geom_point <- ggplot2::geom_point(
    data = subset({{data}}, position == {{pos}} & is_to_big < 1),
    aes(x = xoffset, y = yoffset, shape = position, color = as.factor(grouping_order)),
    size = 1.4,
    stroke = 0.8,
    alpha = 0.9
  )
}

## plots ----
offense <- plot(data = subset(data, unit == "offense"))
defense <- plot(data = subset(data, unit == "defense"))

example_data <- data %>% 
  dplyr::filter(
    unit == "defense" & season == 2016
  )

example <- plot(subset(example_data, unit == "defense" & season == 2016)) +
  ggplot2::theme(
    plot.margin = ggplot2::margin(c(0, 0, 0, 0)),
    plot.background = element_blank()
  )

for(pos in as.vector(unique(annotations$position))) {
  offense <- offense +
    points(
      data = subset(annotations, unit == "offense"),
      pos = pos
    )
  
  defense <- defense +
    points(
      data = subset(annotations, unit == "defense"),
      pos = pos
    )
  
  example <- example +
    points(
      data = subset(annotations, unit == "defense" & season == 2016),
      pos = pos
    )
}

# layout ----
## right ----
grid_subtitle <- function(text) {
  grid::grobTree(
    grid::textGrob(
      label = {{text}},
      #y = 0.2,
      vjust = 0,
      gp = grid::gpar(col = color_accent, fontsize = 26, fontfamily = "accent", fill = color_bg, lwd = 0)
    )
  )
}

right <- wrap_elements(patchwork::plot_spacer()) +
  grid_subtitle("Offense")+
  offense +
  patchwork::plot_spacer() +
  grid_subtitle("Defense") +
  defense +
  patchwork::plot_layout(ncol = 1, heights = c(0.6, 0.05, 1, 0.005, 0.05, 1)) +
  plot_reset

right
## guide ----
example_annotation <- function(xstart, xend, ystart, yend, curve = -0.5, shape = 19, size = 2, stroke = 0, color = color_light, fill = NA, label = "Text", ...) {
  if(curve > 0 & xstart > xend) {
    hjust <- 1
    labelx <- xend - 0.07
  } else {
    hjust = 0
    labelx <- xend + 0.07
  }
  
  list(
    ggplot2::geom_point(aes(x = xstart, y = ystart), shape = shape, size = size, color = color, stroke = stroke),
    ggplot2::geom_curve(aes(x = xstart, xend = xend, y = ystart, yend = yend), curvature = curve, color = color_light, size = 0.5),
    ggplot2::geom_text(aes(x = labelx, y = yend), label = label, color = color_light, size = 4, lineheight = 0.8, family = "base", vjust = 0.5, hjust = hjust, ...)
  
  )
}

guides <- ggplot() +
  # grouping annotation
  example_annotation(
    xstart = 1.8,
    xend = 2,
    ystart = 0.65,
    yend = 0.85,
    label = "Colors stand for the grouping.\nFor offense that means number of started WR,\nfor the defense it is a 3 digit code consisting\nof the number of started DL, LB and DB (eg. 252)"
  ) +
  
  # personnel annotation
  example_annotation(
    xstart = 2.185,
    xend = 2.55,
    ystart = 0.5,
    yend = 0.55,
    size = 6,
    curve = -0.2,
    label = "Tiles stand for started personnel."
  ) +
  
  # season annotation
  example_annotation(
    xstart = 1.87,
    xend = 2.2,
    ystart = 0.39,
    yend = 0.2,
    curve = 0.4,
    label = "Number stands\nfor the played\nseason."
  ) +
  
  # symbols annotation
  example_annotation(
    xstart = 1.85,
    xend = 1.6,
    ystart = 0.48,
    yend = 0.6,
    curve = 0.3,
    label = "Symbols on the largest areas illustrate the personnel.\nFrom top to bottom:\nOffense: WR, TE, RB\nDefense: S, CB, LB, DE, DT\nThe number of symbols in one row stands for the\nnumber of started players of that position."
  ) +

  scale_x_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    caption = caption
  ) +
  theme_void() +
  theme(
    plot.caption = ggplot2::element_text(
      size = 16,
      vjust = 0,
      hjust = 0,
      color = color_light,
      family = "base",
    )
  ) +
  
  patchwork::inset_element(example, top = 0.7, bottom = 0.37, left = 0, right = 1, align_to = "plot", on_top = FALSE)

## left ----
left <- patchwork::wrap_elements(
  ggplot(copy, aes(x = x, y = y, label = content)) +
    ggplot2::geom_text(
      size = 6.5,
      hjust = 0,
      vjust = 1,
      color = color_light,
      family = "base",
      lineheight = 0.9
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 60,
        hjust = 0,
        family = "accent",
        color = color_accent,
        lineheight = 0.6,
        margin = ggplot2::margin(b = 4, unit = "mm")
      ),
      plot.subtitle = ggplot2::element_text(
        size = 24,
        family = "base",
        face = "bold",
        hjust = 0,
        lineheight = 0.8,
        color = color_light
      )
    ) +
    patchwork::plot_spacer() +
    guides +
    patchwork::plot_layout(ncol = 1, heights = c(1, 0.2, 1.2)) &
    plot_reset
)

# final ----
design <- "
  1111222
"

left + right +
  patchwork::plot_layout(design = design) +
  patchwork::plot_layout(widths = c(rep(plot_width, 2)), heights = plot_height) &
  theme(
    plot.margin = ggplot2::margin(c(6, 6, 6, 6), unit = "mm"),
    plot.background = element_rect(color = NA, fill = color_bg)
  )


preview_width <- 1270
preview_height <- 890
dpi <- 72

ggplot2::ggsave("mein_plot.jpg", final, width = plot_width, height = plot_height, units = "mm", dpi = 300) # Um den Plot als PDF zu speichern

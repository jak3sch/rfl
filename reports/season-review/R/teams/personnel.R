# Plots ----
team_personnel_offense <- plot_personnel(data = subset(rfl_personnel, franchise_id == i & unit == "offense")) +
  ggplot2::labs(
    title = "Offense"
  )

team_personnel_defense <- plot_personnel(data = subset(rfl_personnel, franchise_id == i & unit == "defense")) +
  ggplot2::labs(
    title = "Defense"
  )

## Symbols ----
for(pos in as.vector(unique(rfl_personnel_annotations$position))) {
  team_personnel_offense <- team_personnel_offense +
    plot_personnel_points(
      data = subset(rfl_personnel_annotations, franchise_id == i & unit == "offense"),
      pos = pos
    )

  team_personnel_defense <- team_personnel_defense +
    plot_personnel_points(
      data = subset(rfl_personnel_annotations, franchise_id == i & unit == "defense"),
      pos = pos
    )
}

season_diff <- (var_season_last - var_season_first) + 4

# Tables ----
table_data <- rfl_personnel %>%
  dplyr::filter(franchise_id == i) %>%
  dplyr::group_by(season, unit, grouping, personnel, grouping_order) %>%
  dplyr::summarise(count = sum(count), .groups = "drop") %>%
  dplyr::mutate(season = paste0("s_", season)) %>%
  tidyr::spread(season, count) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    unit = stringr::str_to_title(unit),
    grouping_clean = dplyr::case_when(
      unit == "Offense" ~ paste0(stringr::str_split(personnel, "")[[1]][1], "RB, ", stringr::str_split(personnel, "")[[1]][2], "WR, ", stringr::str_split(personnel, "")[[1]][3], "TE"),
      TRUE ~ paste0(stringr::str_split(grouping, "")[[1]][1], "DL, ", stringr::str_split(grouping, "")[[1]][2], "LB, ", stringr::str_split(grouping, "")[[1]][3], "DB")
    ),
    Total = sum(c_across(4:(ncol(.) - 2)), na.rm = TRUE),
    group = paste(unit, grouping_clean, sep = " - ")
  ) %>%
  dplyr::rename_with(~stringr::str_sub(., -2), dplyr::starts_with(("s_"))) %>%
  dplyr::group_by(group) %>%
  dplyr::arrange(dplyr::desc(unit), grouping_order)

## Helper ----
gt_default <- function(df) {
  df %>%
    gt::gt(rowname_col = "personnel") %>%
    gt::cols_hide(c("unit", "grouping", "grouping_clean", "grouping_order")) %>%
    gt::sub_missing(missing_text = "") %>%
    gt::cols_align(align = "center") %>%
    gt::tab_options(
      table.background.color = color_bg,
      table.font.names = "Open Sans",
      table.border.top.style = "hidden",
      table.border.right.style = "hidden",
      table.border.bottom.style = "hidden",
      table.border.left.style = "hidden",
    ) %>%
    gt::data_color(
      method = "numeric",
      palette = c(color_bg, color_accent)
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "left",
          color = color_light,
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_column_labels(
          columns = "Total"
        ),
        gt::cells_body(
          columns = "Total"
        )
      )
    )
}

table <- table_data %>%
  gt_default()

for (row in unique(table_data$group)) {
  group <- table_data %>%
    dplyr::filter(group == row) %>%
    dplyr::select(group, grouping_order) %>%
    dplyr::distinct()

  order <- as.numeric(group$grouping_order)

  table <- table %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = colors[order])
      ),
      locations = gt::cells_row_groups(group$group)
    )
}

gt_to_png <- function() {
  gt <- table

  tmp <- tempfile(fileext = '.png') #generate path to temp .png file
  gt::gtsave(gt, tmp, expand = 0, vwidth = 1108, vheight = 1675) #save gt table as png

  return(tmp)
}

table_img <- gt_to_png()

output <- output %>%
  officer::add_slide(layout = "Personnel Grouping") %>%
  officer::ph_with(value = "Personnel Groupings", location = officer::ph_location_label("title")) %>%
  officer::ph_with(value = paste("Visualisierung der gestarteten Fantasy Football Lineup Variationen zwischen", var_season_first, "und", var_season_last), location = officer::ph_location_label("caption")) %>%
  officer::ph_with(value = team_personnel_offense, location = officer::ph_location_label("img-left")) %>%
  officer::ph_with(value = team_personnel_defense, location = officer::ph_location_label("img-right")) %>%
  officer::ph_with(value = officer::external_img(table_img, width = 11.08, height = 16.75, unit = "cm"), location = officer::ph_location_label("table")) %>%
  officer::ph_with(value = paste(
    "Farben symbolisieren Gruppen. In der Offense ist das die Anzahl der gestarteten WR, in der Defense ist es ein dreistelliger Code, bestehend aus gestarteten DL, LB and DB (z.B. 252)",
    "Die Unterteilung der Kacheln sind verschiedene Variationen (Personnel) der Gruppen", sep = "\n"), location = officer::ph_location_label("legend"))

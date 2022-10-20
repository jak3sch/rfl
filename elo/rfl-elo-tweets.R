library(dplyr)
library(gt)
library(gtExtras)
library(ffscrapr)
library(svglite)
library(webshot2)

elo <- read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/app/data/rfl-elo.csv", colClasses=c("franchise_id" = "character", "opponent_id" = "character", "franchise_elo_postgame" = "numeric", "franchise_score" = "numeric")) %>% 
  dplyr::filter(season == max(season)) %>% 
  #dplyr::filter(week == max(week)) %>%
  dplyr::mutate(
    elo_shift = franchise_elo_postgame - franchise_elo_pregame,
    winloss = ifelse(score_differential > 0, 1, 0)
  ) %>% 
  dplyr::group_by(franchise_id) %>% 
  dplyr::arrange(week) %>% 
  dplyr::summarise(
    season = dplyr::first(season),
    week = dplyr::last(week),
    winloss = list(winloss),
    pf_sparkline = list(unique(franchise_score)),
    franchise_elo_postgame = dplyr::last(franchise_elo_postgame),
    elo_shift = dplyr::last(elo_shift),
    .groups = "drop"
  ) %>% 
  dplyr::left_join(
    read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/rfl-true-standing.csv", colClasses=c("franchise_id" = "character")) %>% 
      dplyr::group_by(franchise_id) %>% 
      dplyr::arrange(week) %>% 
      dplyr::summarise(
        pp_dist = list(unique(pp / 2) / week),
        dplyr::across(c(win, pf, pp, dplyr::ends_with("rank")), ~ dplyr::last(.x))
      ),
    by = "franchise_id"
  ) %>% 
  dplyr::arrange(desc(win), desc(pf)) %>% 
  dplyr::mutate(
    place = row_number(),
    loss = (2 * week) - win,
    pf = pf / 2,
    pp = pp / 2,
    across(pf_rank:coach_rank, ~ 37 - .x),
    elo_shift_norm = elo_shift - min(elo_shift)
  ) %>% 
  dplyr::left_join(
    ffscrapr::ff_franchises(ffscrapr::ff_connect("mfl", 63018, 2022)) %>% 
      dplyr::select(franchise_id, franchise_name, division_name, conference),
    by = "franchise_id"
  ) %>% 
  dplyr::mutate(
    conference = ifelse(conference == "00", "RFC", "BFC"),
    subline = paste(division_name, conference, sep = ", ")
  ) %>% 
  dplyr::select(place, season, week, franchise_name, win, loss, winloss, pf_sparkline, pp_dist, 12:18, 5:6, elo_shift, elo_shift_norm, subline)


elo %>% 
  dplyr::select(-season, -week) %>% 
  gt::gt(
    rowname_col = "place"
  ) %>% 
  gt::tab_header(
    title = paste("RFL Standings"),
    subtitle = paste("Woche", elo$week[1], elo$season[1])
  ) %>% 
  
  gtExtras::gt_merge_stack(
    franchise_name,
    subline,
    small_cap = F,
    palette = c("#222f3e", "#8395a7"),
    #font_size = c("14px", "10px"),
    font_weight = c("normal", "normal")
  ) %>%
  
  gt::tab_spanner(
    label = "Standings",
    columns = c(win, loss, winloss, pf_sparkline, pp_dist)
  ) %>% 
  
  gtExtras::gt_plt_winloss(
    winloss,
    palette = c("#10ac84", "#ee5253", "gray"),
    type = "pill"
  ) %>% 
  
  gtExtras::gt_plt_sparkline(
    pf_sparkline,
    type = "ref_iqr",
    fig_dim = c(5, 35),
    palette = c("#222f3e", "#222f3e", "#ee5253", "#10ac84", "#c8d6e5"),
    label = F,
    same_limit = F
  ) %>% 
  gtExtras::gt_plt_dist(
    pp_dist,
    type = "density",
    fig_dim = c(5, 20),
    line_color = "#222f3e",
    fill_color = "#c8d6e5",
    same_limit = TRUE
  ) %>% 

  gt::tab_spanner(
    label = "Power Rank",
    columns = dplyr::ends_with("_rank")
  ) %>% 
  
  gt::data_color(
    dplyr::ends_with("_rank"),
    colors = scales::col_numeric(
      palette = c("#c8d6e5", "white"),
      domain = c(1,36)
    )
  ) %>% 

  gt::tab_style(
    style = list(
      cell_text(weight = "bolder")
    ),
    locations = cells_body(
      columns = true_rank
    )
  ) %>%
  
  cols_merge(
    columns = c(franchise_elo_postgame, elo_shift),
    pattern = "{1} ({2})"
  ) %>%

  
  gtExtras::gt_add_divider(c(franchise_name, pp_dist, true_rank), color = "#c8d6e5", include_labels = F) %>% 
  
  gt::cols_label(
    franchise_name = "Team",
    franchise_elo_postgame = "ELO",
    elo_shift = "+/-",
    win = "W",
    loss = "L",
    winloss = "Ergebnisse",
    pf_sparkline = "Points For",
    pp_dist = "PP",
    pf_rank = "PF",
    pp_rank = "PP",
    record_rank = "Record",
    all_play_rank = "All-Play",
    coach_rank = "Coach",
    true_rank = "Ovrl"
  ) %>% 
  
  gt::cols_align(
    align = "center",
    columns = gt::everything()
  ) %>% 
  
  gt::cols_align(
    align = "left",
    columns = c(franchise_name)
  ) %>% 
  
  gt::tab_footnote(
    footnote = "Effizienz: PF - PP",
    locations = cells_column_labels(
      columns = coach_rank
    ),
    placement = "left"
  ) %>% 
  
  gt::tab_footnote(
    footnote = "In Klammern = ELO Veränderung zur Vorwoche",
    locations = cells_column_labels(
      columns = franchise_elo_postgame
    ),
    placement = "left"
  ) %>% 
    
  gt::tab_options(
    table.font.size = gt::px(14),
    table.font.color = "#222f3e",
    table.border.top.color = "white",
    heading.border.bottom.color = "white",
    heading.title.font.size = gt::px(40),
    heading.title.font.weight = "light",
    heading.subtitle.font.size = gt::px(16),
    
    column_labels.font.weight = "bold",
    column_labels.padding = gt::px(10),
    column_labels.padding.horizontal = gt::px(15),
    table_body.border.bottom.color = "#c8d6e5",
    column_labels.border.top.color = "red",
    stub.border.color = "#c8d6e5",
    column_labels.border.bottom.color = "#222f3e",
    
    footnotes.padding = gt::px(100),
    footnotes.padding.horizontal = gt::px(35),
    footnotes.border.bottom.color = "#222f3e"
  ) %>% 
  
  gt::cols_hide(elo_shift_norm)

# https://github.com/jak3sch/rfl/blob/main/data/schedule/rfl-results-postseason-2021.rds

library(dendextend)
library(circlize)
library(tidyverse)
library(ggplot2)

dend <- iris[1:40,-5] %>%
 dist %>%
 hclust %>%
 as.dendrogram %>%
    set("branches_k_color", k=3) %>% set("branches_lwd", c(5,2,1.5)) %>%
    set("branches_lty", c(1,1,3,1,1,2)) %>%
    set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
    set("nodes_pch", 19) %>% set("nodes_col", c("orange", "black", "plum", NA))

set.seed(2015-07-10)   
# In the following we get the dendrogram but can also get extra information on top of it
circos.initialize("foo", xlim = c(0, 40))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
   circos.rect(1:40-0.8, rep(0, 40), 1:40-0.2, runif(40), col = rand_color(40), border = NA)
}, bg.border = NA)
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
   circos.text(1:40-0.5, rep(0, 40), labels(dend), col = labels_colors(dend),
               facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA, track.height = 0.1)
max_height = attr(dend, "height")
circos.track(ylim = c(0, max_height), panel.fun = function(x, y) {
   circos.dendrogram(dend, max_height = max_height)
}, track.height = 0.5, bg.border = NA)


ggplot(dend, labels = FALSE)
   scale_y_reverse(expand = c(0.2, 0))
   coord_polar(theta="x")


read_data <- function(season) {
  readRDS(url(paste0("https://github.com/jak3sch/rfl/blob/main/data/schedule/rfl-results-postseason-", season, ".rds?raw=true"))) %>%
  dplyr::mutate(season = as.integer(season))
}

postseason <- read_data(2021) %>%
   dplyr::filter(postseason %in% c("SB", "SBC"))

league_data <- jsonlite::read_json("https://www55.myfantasyleague.com/2022/export?TYPE=league&L=63018&APIKEY=&JSON=1")$league

bracket <- postseason %>%
   dplyr::left_join(
      league_data$franchises$franchise %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::select(id, division),
      by = c("home_id" = "id")
   ) %>% 
   dplyr::left_join(
      league_data$divisions$division %>%
         dplyr::tibble() %>%
         tidyr::unnest_wider(1) %>%
         dplyr::select(id, conference),
      by = c("division" = "id")
   ) %>%
  # dplyr::mutate(
  #    winner = ifelse(home_score > away_score | away_score, home_id, away_id),
  # )
   dplyr::select(conference, division, week)

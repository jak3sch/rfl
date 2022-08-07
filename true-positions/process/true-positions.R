library(ffscrapr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(ggrepel)
library(here)
library(GGally)

var.season = 2021

# import position changes ----
# https://docs.google.com/spreadsheets/d/1rf5m-7DPzwfJViYNYZliBXVnaGuucrYAZhVCQlPTbI4/edit#gid=200822729
positionChanges <- data.frame(
  stringsAsFactors = FALSE,
  player_id = c(12751L,13755L,12218L,10272L,
         10861L,14238L,14900L,10806L,
         14164L,11859L,12791L,13989L,15819L,
         12698L,13434L,14161L,12360L,12697L,
         15093L,15412L,15132L,12740L,13777L,
         14162L,13839L,13448L,13693L,
         10457L,12288L,12225L,12744L,15433L,
         13212L,11707L,11872L,15812L,12361L,
         14975L,13258L,12221L,11830L,12278L,
         13703L,12489L,15851L,15378L,
         13256L,11790L,13681L,12215L,15861L,
         12270L,14149L,14167L,14888L,12686L,
         14275L,11721L,12734L,14250L,10753L,
         14151L,12763L,14225L,15349L,
         14249L,12301L,12795L,15385L,15806L,
         13245L,14940L,13214L,13704L,12075L,
         15379L,13700L,12234L,15405L,13785L,
         9633L,15177L,15381L,14224L),
  true_position = c("DE","DE","DE","DT","DT",
                    "DT","DT","DT","DT","DT","DT",
                    "DT","DT","DT","DT","DT","DT","DT",
                    "DT","DT","DT","DT","DT","DT",
                    "DT","DT","DT","DT","DT","DT",
                    "DT","DT","DT","DT","DT","DT","DT",
                    "DT","DT","DE","DE","DE","DE",
                    "DE","DE","DE","DE","DE","DE",
                    "DE","DE","DE","DE","DE","DE","DE",
                    "DE","DE","DE","DE","DE","DE",
                    "DE","DE","DE","DE","DE","DE",
                    "DE","DE","DE","DE","DE","DE","DE",
                    "DE","DE","DE","S","S","S","S",
                    "CB","CB"),
  mfl_position = c("DT","DT","DT","DE","DE",
                   "DE","DE","DE","DE","DE","DE",
                   "DE","DE","DE","DE","DE","DE","DE",
                   "DE","DE","DE","DE","DE","DE",
                   "DE","DE","DE","DE","DE","DE",
                   "DE","DE","DE","DE","DE","DE","DE",
                   "DE","DE","LB","LB","LB","LB",
                   "LB","LB","LB","LB","LB","LB",
                   "LB","LB","LB","LB","LB","LB","LB",
                   "LB","LB","LB","LB","LB","LB",
                   "LB","LB","LB","LB","LB","LB",
                   "LB","LB","LB","LB","LB","LB","LB",
                   "LB","LB","LB","CB","CB","CB",
                   "CB","S","S")
)

playerIDs <- as.vector(t(positionChanges %>% select(player_id)))

# work with data ----
mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2021, rate_limit = FALSE)

scores <- ffscrapr::ff_playerscores(mfl, season = var.season, week = 1:17)
availablePlayerIDs <- as.vector(t(scores %>% filter(is_available == 1) %>% select(player_id)))

scoresSeason <- scores %>% 
  filter(pos %in% c("DT", "DE", "LB", "CB", "S")) %>%
  group_by(player_id, player_name, season) %>% 
  summarise(
    points = sum(points),
    games = n(),
    points_per_game = points / games
  ) %>% 
  ungroup()
  
  
rankings <- scoresSeason %>% 
  left_join(scores %>% select(player_id, pos, team, season), by = c("player_id", "season")) %>% 
  distinct() %>% 
  mutate(player_id = as.integer(player_id)) %>% 
  dplyr::left_join(positionChanges, by = "player_id") %>% 
  mutate(
    true_position = ifelse(is.na(true_position), pos, true_position),
    true_position = case_when(
      true_position %in% c("DT", "DE") ~ "DL",
      true_position %in% c("S", "CB") ~ "DB",
      TRUE ~ true_position
    ),
    mfl_position = ifelse(is.na(mfl_position), pos, mfl_position),
    mfl_position = case_when(
      mfl_position %in% c("DT", "DE") ~ "DL",
      mfl_position %in% c("S", "CB") ~ "DB",
      TRUE ~ mfl_position
    )
  )

# plots ----
barChartDefaults <- function(df, nudge_y) {
  list(
    geom_col(aes(fill = team)),
    scale_fill_nfl(),
    geom_text(data = subset(df, rank <= 50 & !player_id %in% playerIDs), size = 2, angle = 45, hjust = 0, nudge_x = -0.1, nudge_y = nudge_y, color = "#000000"),
    geom_text(data = subset(df, rank <= 50 & player_id %in% playerIDs), size = 2, angle = 45, hjust = 0, nudge_x = -0.1, nudge_y = nudge_y, color = "red"),
    facet_wrap(vars(position), ncol = 1),
    ylim(c(0, 300))
  )
}

## total ----
### default mfl ----
rankingMFL <- rankings %>% 
  group_by(mfl_position) %>% 
  arrange(desc(points)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  rename(position = mfl_position) %>% 
  select(-true_position)

ggplot2::ggplot(subset(rankingMFL, rank <= 50), aes(x = rank, y = points, label = player_name)) +
  barChartDefaults(rankingMFL, 8) +
  labs(
    title = paste("MFL Position Rankings", var.season),
    subtitle = "Total Fantasy Points WK 1 - 17\nrot sind die Spieler, die einen Positionswechsel bekommen würden"
  )

ggsave(here("fantasy/true-positions/exports/rankings/", paste(var.season, "_default_mfl", ".jpg", sep = "")), width = 2400, height = 2400, units = "px")

### default/PPG gegenüberstellung ----
dlComparison <- rankingMFL %>% 
  left_join(rankings %>% select(player_id, true_position), by = "player_id") %>%
  filter(true_position == "DL") %>% 
  arrange(desc(points)) %>% 
  mutate(true_rank = row_number()) %>% 
  filter(true_rank <= 60) %>% 
  group_by(position) %>% 
  arrange(desc(points_per_game)) %>% 
  mutate(rank_ppg = row_number()) %>% 
  ungroup()

ggplot2::ggplot(dlComparison, aes(x = true_rank, y = points, label = player_name)) +
  geom_col(aes(fill = team)) +
  scale_fill_nfl() +
  geom_text(data = subset(dlComparison, !player_id %in% playerIDs), size = 2, angle = 45, hjust = 0, nudge_x = -0.1, nudge_y = 8, color = "#000000") +
  geom_text(data = subset(dlComparison, player_id %in% playerIDs), size = 2, angle = 45, hjust = 0, nudge_x = -0.1, nudge_y = 8, color = "red") +
  facet_wrap(vars(position), ncol = 1) +
  ylim(c(0, 300)) +
  labs(
    title = paste("MFL Position Rankings", var.season),
    subtitle = "Total Fantasy Points WK 1 - 17\nrot sind die Spieler, die einen Positionswechsel bekommen würden"
  ) +
  theme(
    axis.title.x = element_blank()
  )

ggsave(here("fantasy/true-positions/exports/rankings/", paste(var.season, "_dl-comparison", ".jpg", sep = "")), width = 2400, height = 2400, units = "px")

## ranking changes ----
rankingChangesPlotDefaults <- function(df) {
  list(
    facet_wrap(vars(true_position), ncol = 1),
    
    geom_point(aes(color = team)),
    #geom_point(data = subset(df, true_rank <= 60 & !player_id %in% availablePlayerIDs), aes(color = team)),
    #geom_point(data = subset(df, true_rank <= 60 & player_id %in% availablePlayerIDs), aes(color = team), shape = 15, size = 3),
    
    scale_color_nfl(),
    ggrepel::geom_text_repel(
      data = subset(df, !player_id %in% playerIDs & true_rank <= 60 & rank_diff >= 3 ),
      aes(label = paste(player_name, team, "\n", mfl_label, "->", true_label)),
      max.overlaps = Inf,
      size = 2.5, color = "#27ae60"
    ),
    ggrepel::geom_text_repel(
      data = subset(df, !player_id %in% playerIDs & true_rank <= 60 & rank_diff <= -3),
      aes(label = paste(player_name, team, "\n", mfl_label, "->", true_label)),
      max.overlaps = Inf,
      size = 2.5, color = "#c0392b"
    ),
    ggrepel::geom_text_repel(
      data = subset(df, player_id %in% playerIDs & true_rank <= 60),
      aes(label = paste(player_name, team, "\n", mfl_label, "->", true_label)),
      max.overlaps = Inf,
      size = 2.5, color = "#2980b9"
    )
  )
}

### total ----
rankingTrue <- rankings %>% 
  group_by(true_position) %>% 
  arrange(desc(points)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  rename(position = true_position) %>% 
  select(-mfl_position)

rankingChanges <- rankingMFL %>% 
  rename(
    mfl_position = position,
    mfl_rank = rank
  ) %>% 
  left_join(rankingTrue %>%  select(player_id, rank, position), by = "player_id") %>% 
  rename(
    true_position = position,
    true_rank = rank
  ) %>% 
  mutate(
    mfl_label = paste(mfl_position, mfl_rank, sep = " "),
    true_label = paste(true_position, true_rank, sep = " "),
    rank_diff = mfl_rank - true_rank
  )

ggplot2::ggplot(subset(rankingChanges, true_rank <= 60), aes(x = mfl_rank, y = true_rank)) +
  rankingChangesPlotDefaults(rankingChanges) +
  labs(
    title = paste("Ranking Changes", var.season),
    subtitle = "Beschriftet sind nur Spieler, die nach True Position Rankings mind. Platz 48 erreicht hätten und\nmindestens 3 Plätze gewonnen (grün) oder verloren (rot) haben\nblau sind alle Spieler mit einem Positionswechsel",
    x = "MFL Position Rank",
    y = "True Position Rank"
  )

ggsave(here("fantasy/true-positions/exports/rankings/", paste(var.season, "_position-changes", ".jpg", sep = "")), width = 2400, height = 6000, units = "px")

write.csv(rankingChanges, "truerankings.csv")


library(ffscrapr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(ggrepel)
library(here)
library(GGally)
library(nflfastR)

mfl <- ffscrapr::mfl_connect(league_id = "63018", season = 2021, rate_limit = FALSE)
scoresSince2021_3 <- ffscrapr::ff_playerscores(mfl, season = 2021, week = 15:18)

scores <- rbind(scoresSince2016, scoresSince2016_2, scoresSince2016_3, scoresSince2017_1, scoresSince2017_2, scoresSince2017_3,
                scoresSince2018_1, scoresSince2018_2, scoresSince2018_3, scoresSince2019_1, scoresSince2019_2, scoresSince2019_3,
                scoresSince2020_1, scoresSince2020_2, scoresSince2020_3, scoresSince2021_1, scoresSince2021_2, scoresSince2021_3)

scoresClean <- scores %>% 
  filter(pos %in% c("DT", "DE", "LB", "CB", "S")) %>% 
  mutate(
    pos = case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("S", "CB") ~ "DB",
      TRUE ~ pos
    )
  )

scoresPerGame <- scoresClean %>% 
  group_by(player_id, season) %>% 
  summarise(
    points = sum(points),
    games = n(),
    points_per_game = points / games
  ) %>% 
  ungroup() %>% 
  left_join(scoresClean %>% select(player_id, season, pos), by = c("player_id", "season")) %>% 
  distinct()

ranks2016 <- scoresPerGame %>% 
  filter(season == 2016) %>% 
  group_by(pos) %>% 
  arrange(desc(points)) %>% 
  mutate(rank_2016 = row_number()) %>% 
  arrange(desc(points_per_game)) %>% 
  mutate(rank_ppg_2016 = row_number()) %>% 
  ungroup() %>% 
  select(-season, -games, -pos) %>% 
  gather(point_type, points_2016, c(points, points_per_game))

ranks2017 <- scoresPerGame %>% 
  filter(season == 2017) %>% 
  group_by(pos) %>% 
  arrange(desc(points)) %>% 
  mutate(rank_2017 = row_number()) %>% 
  arrange(desc(points_per_game)) %>% 
  mutate(rank_ppg_2017 = row_number()) %>% 
  ungroup() %>% 
  select(-season, -games, -pos) %>% 
  gather(point_type, points_2017, c(points, points_per_game))

ranks2018 <- scoresPerGame %>% 
  filter(season == 2018) %>% 
  group_by(pos) %>% 
  arrange(desc(points)) %>% 
  mutate(rank_2018 = row_number()) %>% 
  arrange(desc(points_per_game)) %>% 
  mutate(rank_ppg_2018 = row_number()) %>% 
  ungroup() %>% 
  select(-season, -games, -pos) %>% 
  gather(point_type, points_2018, c(points, points_per_game))

ranks2019 <- scoresPerGame %>% 
  filter(season == 2019) %>% 
  group_by(pos) %>% 
  arrange(desc(points)) %>% 
  mutate(rank_2019 = row_number()) %>% 
  arrange(desc(points_per_game)) %>% 
  mutate(rank_ppg_2019 = row_number()) %>% 
  ungroup() %>% 
  select(-season, -games, -pos) %>% 
  gather(point_type, points_2019, c(points, points_per_game))

ranks2020 <- scoresPerGame %>% 
  filter(season == 2020) %>%
  group_by(pos) %>% 
  arrange(desc(points)) %>% 
  mutate(rank_2020 = row_number()) %>% 
  arrange(desc(points_per_game)) %>% 
  mutate(rank_ppg_2020 = row_number()) %>% 
  ungroup() %>% 
  select(-season, -games, -pos) %>% 
  gather(point_type, points_2020, c(points, points_per_game))

ranks2021 <- scoresPerGame %>% 
  filter(season == 2021) %>% 
  group_by(pos) %>% 
  arrange(desc(points)) %>% 
  mutate(rank_2021 = row_number()) %>% 
  arrange(desc(points_per_game)) %>% 
  mutate(rank_ppg_2021 = row_number()) %>% 
  ungroup() %>% 
  select(-season, -games, -pos) %>% 
  gather(point_type, points_2021, c(points, points_per_game))

ranks <- ranks2021 %>% 
  left_join(ranks2020, by = c("player_id", "point_type")) %>% 
  left_join(ranks2019, by = c("player_id", "point_type")) %>% 
  left_join(ranks2018, by = c("player_id", "point_type")) %>% 
  left_join(ranks2017, by = c("player_id", "point_type")) %>% 
  left_join(ranks2016, by = c("player_id", "point_type")) %>% 
  select(-starts_with("point")) %>% 
  mutate(position_change = ifelse(player_id %in% playerIDs, 1, 0)) %>% 
  left_join(scoresClean %>% filter(season == 2021) %>% select(player_id, player_name, team, pos), by = "player_id") %>% 
  distinct() %>% 
  left_join(scoresPerGame %>% select(player_id, games, points_per_game), by = "player_id") %>% 
  filter(
    points_per_game >= 5,
    games >= 5,
    pos == "LB"
  ) %>% 
  select(-games, -points_per_game) %>% 
  distinct()

ranksTotal <- ranks %>% 
  select(-starts_with("rank_ppg_"))

ranksTotal <- ranksTotal[, c(1, 9, 10, 11, 8, 7, 6, 5, 4, 3, 2)]

ranksPPG <- ranks %>% 
  select(-starts_with("rank_2"))

ranksPPG <- ranksPPG[, c(1, 9, 10, 11, 8, 7, 6, 5, 4, 3, 2)]

ggparcoord(
  ranksTotal,
  columns = 6:11,
  groupColumn = 5,
  scale="globalminmax",
  showPoints = T
) +
  scale_color_gradient(low = "#bdc3c7", high = "#c0392b") +
  #nflplotR::scale_color_nfl() +
  geom_hline(yintercept = 36, linetype="dashed", color = "black") +
  labs(
    title = "Volatilität der Fantasy Point Total Finishes LB",
    subtitle = "rot sind die Spieler, die einen Positionswechsel bekommen würde\nNur Datensaätze mit mind. 5 Spielen und mehr als 5 Pts. per Game; schwarze Linie = Rank 36"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) +
  scale_y_reverse() +
  ylim(c(0, 150))
  
ggsave(here("fantasy/true-positions/exports/volatility/", paste("2021_position-volatility_LB", ".jpg", sep = "")), width = 2400, height = 2400, units = "px")

library(ffscrapr)
library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(nflplotR)
library(ggrepel)

# als erstes muss die datei true-positions.R durchlaufen werden (am besten mit den Daten aus 2021)
# work with data ----
mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2021, rate_limit = FALSE)
roster <- ff_rosters(mfl) %>% 
  filter(pos %in% c("DT", "DE", "LB")) %>% 
  select(franchise_name, player_id, player_name)

mergedRoster <- roster %>% 
  mutate(player_id = as.integer(player_id)) %>% 
  left_join(rankingChanges %>% select(-player_name, -season, -games, -pos), by = "player_id") %>% 
  filter(true_position != "DB") %>% 
  group_by(franchise_name) %>%
  arrange(desc(points_per_game)) %>% 
  mutate(
    player_sort = row_number(),
    player_value_old = case_when(
      mfl_rank <= 12 ~ 1,
      mfl_rank <= 24 ~ 2,
      mfl_rank <= 36 ~ 3,
      TRUE <= 48 ~ 4
    ),
    player_value_new = case_when(
      true_rank <= 12 ~ 1,
      true_rank <= 24 ~ 2,
      true_rank <= 36 ~ 3,
      TRUE <= 48 ~ 4
    )
  )

ggplot(subset(mergedRoster, true_rank <= 100 | player_id %in% playerIDs), aes(y = true_rank, x = player_sort, color = team)) +
  facet_wrap(vars(franchise_name, true_position), ncol = 2) +
  
  geom_hline(yintercept = 12, linetype="dashed", color = "red") +
  geom_hline(yintercept = 24, linetype="dashed", color = "red") +
  geom_hline(yintercept = 36, linetype="dashed", color = "red") +
  
  geom_segment(aes(xend = player_sort, yend = mfl_rank)) +
  geom_point(aes(y = mfl_rank), size = 2) +
  geom_point(aes(y = true_rank), size = 5) +
  
  nflplotR::scale_color_nfl() +
  
  ggrepel::geom_label_repel(aes(label = paste(player_name, team, "\n", mfl_label, "->", true_label)), size = 1.5, nudge_y = 10, force = 3, force_pull = 5, segment.color = "black") +
  labs(
    title = "Ranking Implikationen f?r die RFL Teams",
    subtitle = "Es werden nur Spieler angezeigt, die entweder nach True Position Rankings\nPlatz 100 oder besser gewesen w?ren, oder einen Positionswechsel vollziehen w?rden\ngro?er Punkt = True Position Rank; kleiner Punkt = Original Rank"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
  )

ggsave(here("fantasy/true-positions/exports/rfl/", paste(var.season, "_team-changes.jpg", sep = "")), width = 2400, height = 32000, units = "px", limitsize = FALSE)

# roster score ----
rosterScoreOld <- mergedRoster %>% 
  group_by(franchise_name, mfl_position) %>% 
  summarise(
    points_old = sum(points)
  )

rosterScoreNew<- mergedRoster %>% 
  group_by(franchise_name, true_position) %>% 
  summarise(
    across(c(points, rank_diff), sum)
  ) %>% 
  ungroup() %>% 
  rename(
    points_new = points
  )

rosterScoreGather <- rosterScoreOld %>% 
  left_join(rosterScoreNew, by = c("franchise_name", "mfl_position" = "true_position")) %>% 
  mutate(point_diff = points_new - points_old) %>% 
  rename(pos = mfl_position) %>% 
  gather(points_cat, points, c(-franchise_name, -pos))

rosterScoreDL <- rosterScoreGather %>% 
  filter(pos == "DL")
#  mutate(points_cat = paste(points_cat, "_DL", sep = ""))

rosterScoreLB <- rosterScoreGather %>% 
  filter(pos == "LB")
  #mutate(points_cat = paste(points_cat, "_LB", sep = ""))

rosterScore <- rbind(rosterScoreDL, rosterScoreLB)
  mutate(
    points_cat = as.factor(points_cat, levels = unique("points_old_DL", "points_new_DL", "points_diff_DL", "rank_diff_DL", "points_old_LB", "points_new_LB", "points_diff_LB", "rank_diff_LB"))
  )

ggplot(rosterScore, aes(x = points_cat, y = franchise_name, fill = points)) +
  facet_wrap(vars(pos), ncol = 1) +
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  geom_text(aes(label = points))

ggsave(here("fantasy/true-positions/exports/rfl/", paste(var.season, "_team-changes_values.jpg", sep = "")), width = 2400, height = 6000, units = "px", limitsize = FALSE)

# verlierer der mfl umstellung im mai ----
franchiseLoser <- mergedRoster %>% 
  #filter(franchise_name == "M-V Lightning") %>% 
  filter(mfl_position != true_position) %>% 
  group_by(franchise_name, mfl_position, true_position) %>% 
  summarise(points = sum(points), .groups = "drop")

  ggplot(franchiseLoser, aes(y = reorder(franchise_name, desc(points)), x = points)) +
    geom_col() +
    labs(
      title = "Verlierer der MFL Umstellung im Mai",
      subtitle = "Alle Teams, die durch eine Positions?nderung betroffen sind.\nAbgebildet sind Total Points, die von der DLine zu den LBs wandern w?rden.",
      y = ""
    ) +
    theme()


library(jsonlite)
latestPlayerJson <- fromJSON("https://www55.myfantasyleague.com/2022/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=2022&PLAYERS=&JSON=1")
latestPlayer <- latestPlayerJson$players$player %>% 
  rename(latest_mfl_position = position) %>% 
  select(latest_mfl_position, id) %>% 
  mutate(id = as.integer(id))
  
  
LBtoDE <- mergedRoster %>% 
  left_join(latestPlayer, by = c("player_id" = "id")) %>% 
  filter(mfl_position == "LB" & latest_mfl_position == "DE") %>% 
  group_by(franchise_name, mfl_position, true_position) %>% 
  summarise(points = sum(points), .groups = "drop")

ggplot(LBtoDE, aes(y = reorder(franchise_name, desc(points)), x = points)) +
  geom_col() +
  labs(
    title = "MFL Umstellung im Mai: LB zu DE",
    subtitle = "Abgebildet sind Total Points, die von LBs in die DL gewandert sind.",
    y = ""
  ) +
  theme()


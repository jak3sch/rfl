library(tidyverse)
library(ffscrapr)
library(ggplot2)
library(stringdist)

# data ----
mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2017, rate_limit = FALSE)
draft2017 <- ff_draft(mfl) %>% 
  mutate(season = 2017)
stats2017 <- ff_playerscores(mfl, season = 2017, week = "YTD")

mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2018, rate_limit = FALSE)
draft2018 <- ff_draft(mfl) %>% 
  mutate(season = 2018)
stats2018 <- ff_playerscores(mfl, season = 2018, week = "YTD")

mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2019, rate_limit = FALSE)
draft2019 <- ff_draft(mfl) %>% 
  mutate(season = 2019)
stats2019 <- ff_playerscores(mfl, season = 2019, week = "YTD")


mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2020, rate_limit = FALSE)
draft2020 <- ff_draft(mfl) %>% 
  mutate(season = 2020)
stats2020 <- ff_playerscores(mfl, season = 2020, week = "YTD")


mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2021, rate_limit = FALSE)
draft2021 <- ff_draft(mfl) %>% 
  mutate(season = 2021)
stats2021 <- ff_playerscores(mfl, season = 2021, week = "YTD")
starter2021 <- ff_starters(mfl)

## avg count of starter by position ----
starterAvg <- starter2021 %>% 
  filter(starter_status == "starter") %>% 
  distinct() %>% 
  mutate(
    pos = case_when(
      pos %in% c("DE", "DT") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>% 
  group_by(franchise_id, pos, week) %>%
  summarise(count = n(), .groups = "drop") %>% 
  group_by(pos) %>% 
  summarise(count = mean(count))

starterAvg

## position changes ----
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

# combined ----
drafts <- rbind(draft2017,draft2018,draft2019,draft2020,draft2021) %>% 
  select(season, overall, player_id, player_name) %>% 
  group_by(player_id)

vbd <- rbind(stats2017,stats2018,stats2019,stats2020,stats2021) %>% 
  mutate(player_id = as.integer(player_id)) %>% 
  left_join(positionChanges, by = "player_id") %>% 
  mutate(pos = ifelse(is.na(true_position), pos, true_position)) %>% 
  group_by(season, pos) %>% 
  arrange(desc(points)) %>% 
  mutate(
    pos = case_when(
      pos %in% c("DE", "DT") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    ),
    vbd = case_when(
      pos %in% c("QB", "TE", "PK") ~ points - points[12],
      pos == "DL" ~ points - points[24],
      pos == "RB" ~ points - points[30],
      pos %in% c("WR", "DB") ~ points - points[36],
      TRUE ~ points - points[48]
    ),
    player_id = as.character(player_id)
  ) %>% 
  ungroup(pos, season) %>% 
  group_by(player_id) %>% 
  mutate(
    entries = n(),
    lowest = ifelse(vbd == min(vbd), 1, 0)
  ) %>% 
  filter(
    entries > 1 & lowest != 1
  ) %>% 
  select(season, player_id, player_name, pos, vbd)
  

vbdCareer <- vbd %>% 
  group_by(player_id) %>% 
  summarise(
    vbd_career = sum(vbd),
    vbd_career_avg = mean(vbd)
  ) %>% 
  left_join(vbd %>% select(player_id, player_name, pos), by = "player_id") %>% 
  distinct()

data <- drafts %>% 
  left_join(vbd, by = c("season", "player_id")) %>% 
  #left_join(vbdCareer, by = "player_id") %>%
  group_by(season) %>% 
  #mutate(vbd = ifelse(!is.na(vbd), vbd, min(vbd, na.rm = T))) %>% # spieler ohne vbd bekommen season min (zB verletzt oder nicht gespielt)
  filter(!is.na(vbd)) %>% 
  ungroup(season) %>% 
  group_by(overall) %>% 
  summarise(vbd = mean(vbd), .groups = "drop") 
  #filter(pos %in% c("DL", "LB"))

ggplot(data, aes(x = overall)) +
  geom_point(aes(y = vbd), alpha = 0.2) +
  geom_smooth(aes(y = vbd), method = loess, formula = y ~ x, se = F) +
  labs(
    y = "VBD avg"
  )

data.lo <- loess(vbd ~ overall, data)
data.pred <- predict(data.lo, data.frame(overall = seq(1, 252, 1)), se = TRUE)$fit

pickValues <- as.data.frame(data.pred) %>% 
  rename(pick_value_raw = data.pred) %>% 
  mutate(
    overall = row_number(),
    pick_value_clean = pick_value_raw - min(pick_value_raw),
    pick_value_pct = pick_value_clean / max(pick_value_clean)
  )

ggplot(pickValues, aes(x = overall, y = pick_value_clean)) +
  geom_vline(xintercept = 36) +
  geom_vline(xintercept = 72) +
  geom_vline(xintercept = 108) +
  geom_vline(xintercept = 144) +
  geom_vline(xintercept = 180) +
  geom_vline(xintercept = 216) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "RFL VBD",
    subtitle = "VBD is the players fantasy points minus the fantasy points of the baseline player,\nwhere the baseline player is the 12th-ranked QB, TE and PK the 24th-ranked DL,\nthe 30th-ranked RB, the 36th-ranked WR or DB, or the 48th-ranked LB",
    x = "Overall Pick",
    y = "Avg VBD seit 2017"
  )

# comp pick berechnung
filterPlayer <- vbdCareer %>% 
  mutate(vbd_career_avg = round(vbd_career_avg)) %>% 
  filter(
    vbd_career_avg >= 25 & vbd_career_avg <= 55 # threshold für comp pick; niedriger kriegt keinen (25 ca. zwischen 6. & 7. Runde), evtl auch max festlegen?
  )
playerVBD <- filterPlayer$vbd_career_avg

compPicks <- data.frame(
  vbd_career_avg = playerVBD,
  comp_pick = pickValues[amatch(playerVBD, round(pickValues$pick_value_clean), maxDist = 15),2])

playerData <- filterPlayer %>% 
  filter(pos %in% c("DL", "LB")) %>%
  left_join(compPicks, by = "vbd_career_avg") %>% 
  distinct() %>% 
  select(player_name, pos, vbd_career_avg, comp_pick)
  
write.csv(playerData, "apps/rfl-vbd/data/rfl_vbd_career.csv", row.names = F)
write.csv(pickValues, "apps/rfl-vbd/data/rfl_pick_vbd.csv", row.names = F)

playerData

# Libraries ----
library(ffscrapr)
library(tidyverse)
library(nflreadr)
library(ggplot2)
library(nflfastR)
library(scales)
#library(stringdist)

# Data ----
## Helper ----
playerList <- nflreadr::load_ff_playerids()

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

## Snap Count Data ----
plays <- nflfastR::load_pbp(2017:2021) %>% 
  filter(
    week <= 17,
    play_type %in% c("pass", "run", "qb_kneel", "qb_spike")
  ) %>% 
  group_by(season, week, game_id, posteam, defteam) %>% 
  summarise(
    plays = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    defteam = case_when(
      defteam == "GB" ~ "GBP",
      defteam == "JAX" ~ "JAC",
      defteam == "KC" ~ "KCC",
      defteam == "LA" ~ "LAR",
      defteam == "LV" ~ "LVR",
      defteam == "NE" ~ "NEP",
      defteam == "NO" ~ "NOS",
      defteam == "SF" ~ "SFO",
      defteam == "TB" ~ "TBB",
      TRUE ~ defteam
    )
  )

snapCounts <- nflreadr::load_snap_counts(2017:2021) %>% 
  left_join(playerList %>% select(pfr_id, mfl_id), by = c("pfr_player_id" = "pfr_id")) %>% 
  filter(
    game_type == "REG",
    position %in% c("NT", "DL", "DT", "DE", "LB", "OLB", "ILB", "CB", "SS", "FS", "DB")
  )

snapCountsNA <- snapCounts %>% 
  filter(is.na(mfl_id)) %>%
  select(-mfl_id) %>% 
  mutate(
    player = case_when(
      player == "Shaquil Barrett" ~ "Shaq Barrett",
      player == "Jonathan Bostic" ~ "Jon Bostic",
      TRUE ~ player
    )
  ) %>% 
  left_join(playerList %>% select(name, mfl_id), by = c("player" = "name"))

snapCountsData <- snapCounts %>% 
  filter(!is.na(mfl_id)) %>%
  rbind(snapCountsNA) %>% 
  left_join(plays %>% select(season, week, defteam, plays), by = c("season", "week", "team" = "defteam")) %>% 
  filter(season >= 2019) %>% 
  group_by(mfl_id, season) %>% 
  summarise(
    snaps = sum(defense_snaps, na.rm = T),
    snap_pct_available = mean(defense_pct, na.rm = T),
    .groups = "drop"
  ) %>% 
  mutate(mfl_id = as.integer(mfl_id))

rm(snapCountsNA)

## Stats und Drafts ----
statsList <- list()
statsAvgList <- list()
draftList <- list()

for (i in 2017:2021) {
  connection <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = i, rate_limit = FALSE)
  statsTotal <- ffscrapr::ff_playerscores(connection, season = i, week = "YTD")
  statsAvg <- ffscrapr::ff_playerscores(connection, season = i, week = "AVG") %>% select(player_id, season, points) %>% rename(ppg = points)
  draft <- ffscrapr::ff_draft(connection) %>% mutate(season = 2017)
  
  statsList[[i]] <- statsTotal
  statsAvgList[[i]] <- statsAvg
  draftList[[i]] <- draft
}

stats <- do.call(rbind, statsList) %>% 
  left_join(do.call(rbind, statsAvgList), by = c("player_id", "season")) %>% 
  mutate(player_id = as.integer(player_id)) %>% 
  left_join(positionChanges, by = "player_id") %>% 
  mutate(
    mfl_position = ifelse(!is.na(mfl_position), mfl_position, pos),
    mfl_position = case_when(
      mfl_position %in% c("DE", "DT") ~ "DL",
      mfl_position %in% c("CB", "S") ~ "DB",
      TRUE ~ mfl_position
    ),
    true_position = case_when(
      true_position %in% c("DE", "DT") ~ "DL",
      true_position %in% c("CB", "S") ~ "DB",
      TRUE ~ mfl_position
    )
  ) %>% 
  select(-pos)
  
drafts <- do.call(rbind, draftList)

rm(statsList, statsAvgList, draftList, draft, connection, i)

## Roster ----
roster <- ffscrapr::ff_rosters(ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2022, rate_limit = FALSE)) %>% 
  select(-franchise_name) %>% 
  left_join(ffscrapr::ff_franchises(ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2022, rate_limit = FALSE)) %>% select(franchise_id, franchise_name), by = "franchise_id")

#write.csv(roster, "fantasy/rfl/data/true-positions/raw/roster.csv", row.names = F)

# FAAB ----
## Player Value ----
### VBD ----
vbdPlayers <- stats %>% 
  # mfl vbd
  group_by(mfl_position, season) %>% 
  arrange(desc(points)) %>% 
  mutate(
    vbd_mfl = case_when(
      mfl_position %in% c("QB", "TE", "PK") ~ ppg - ppg[12],
      mfl_position == "DL" ~ ppg - ppg[24],
      mfl_position == "RB" ~ ppg - ppg[30],
      mfl_position %in% c("WR", "DB") ~ ppg - ppg[36],
      TRUE ~ ppg - ppg[48]
    )
  ) %>% 
  
  # true vbd
  group_by(true_position, season) %>% 
  arrange(desc(points)) %>% 
  mutate(
    vbd_true = case_when(
      true_position %in% c("QB", "TE", "PK") ~ ppg - ppg[12],
      true_position == "DL" ~ ppg - ppg[24],
      true_position == "RB" ~ ppg - ppg[30],
      true_position %in% c("WR", "DB") ~ ppg - ppg[36],
      TRUE ~ ppg - ppg[48]
    )
  ) %>% 
  ungroup() %>% 
  
  # spieler, die seit 2017 mehrere positionen hatten (z.B. montez sweat) bekommen für den gesamten zeitraum die position ihrer letzten position
  #left_join(positionHelper, by = c("player_id" = "mfl_id")) %>% 
  group_by(player_id) %>% 

  mutate(
    season_max_helper = max(season),
    last_mfl_pos_helper = ifelse(season == season_max_helper, mfl_position, NA),
    last_true_pos_helper = ifelse(season == season_max_helper, true_position, NA)
  ) %>% 
  arrange(last_mfl_pos_helper) %>% 
  tidyr::fill(last_mfl_pos_helper, last_true_pos_helper) %>% 
  mutate(
    mfl_position = last_mfl_pos_helper,
    true_position = last_true_pos_helper
  ) %>% 
  select(-ends_with("_helper")) %>% 
  ungroup() 

#write.csv(vbdPlayers, "fantasy/rfl/data/true-positions/raw/vbd_players.csv", row.names = F)

### Combined Data ----
playerValue <- stats %>% 
  filter(season >= 2019) %>% 
  
  left_join(vbdPlayers %>% select(player_id, season, starts_with("vbd_")), by = c("player_id", "season")) %>%
  left_join(snapCountsData, by = c("season", "player_id" = "mfl_id")) %>% 
  select(-week, -is_available) %>% 
  mutate(
    snaps = ifelse(is.na(snaps), 0, snaps),
    snap_pct_available = ifelse(is.na(snap_pct_available), 0, snap_pct_available),
    team = ifelse(team == "OAK", "LVR", team)
  ) %>% 
  
  left_join(plays %>% select(season, defteam, plays) %>% group_by(season, defteam) %>% summarise(plays = sum(plays), .groups = "drop"), by = c("season", "team" = "defteam")) %>% 
  mutate(snap_pct_total = snaps / plays) %>% 
  group_by(player_id, player_name) %>% 
  mutate(
    starter = ifelse(snap_pct_total >= 0.75 | snap_pct_available >= 0.75, 1, 0),
    starter = ifelse(is.na(snap_pct_total), 0, starter)
  ) %>% 
  summarise(
    starter = sum(starter),
    points = ifelse(starter > 0, max(points), points), .groups = "drop",
    ppg = mean(ppg),
    vbd_mfl_total = sum(vbd_mfl),
    vbd_mfl_avg = mean(vbd_mfl),
    vbd_true_total = sum(vbd_true),
    vbd_true_avg = mean(vbd_true)
  ) %>% 
  left_join(vbdPlayers %>% select(player_id, ends_with("_position")), by = "player_id") %>% 
  distinct() %>% 

  # tiers mfl
  group_by(mfl_position) %>% 
  arrange(desc(points)) %>% 
  mutate(
    rank_2021 = row_number(),
    tier_mfl = case_when(
      rank_2021 <= 12 ~ 1,
      rank_2021 <= 24 ~ 2,
      rank_2021 <= 36 ~ 3,
      rank_2021 <= 48 ~ 4,
      rank_2021 <= 60 ~ 5,
      TRUE ~ 6
    )
  ) %>% 
  
  # tiers true
  group_by(true_position) %>% 
  arrange(desc(points)) %>% 
  mutate(
    rank_true = row_number(),
    tier_true = case_when(
      rank_true <= 12 ~ 1,
      rank_true <= 24 ~ 2,
      rank_true <= 36 ~ 3,
      rank_true <= 48 ~ 4,
      rank_true <= 60 ~ 5,
      TRUE ~ 6
    )
  ) %>% 
  ungroup() %>% 
  select(-starts_with("rank")) %>% 
  distinct()

#write.csv(playerValue, "fantasy/rfl/data/true-positions/raw/values_player.csv", row.names = F)

## Factors ----
factors <- playerValue %>% 
  filter(true_position %in% c("DL", "LB") | mfl_position %in% c("DL", "LB")) %>% 
  gather(key, tier, starts_with("tier")) %>% 
  mutate(pos = ifelse(key == "tier_mfl", paste(key, tier, mfl_position, sep = "_"), paste(key, tier, true_position, sep = "_"))) %>% 
  group_by(pos) %>% 
  summarise(
    tier_avg = mean(points), # berechnung anhand von total points
    #tier_avg = case_when( # berechnung anhand von vbd
    #  key == "tier_mfl" ~ mean(vbd_mfl_avg),
    #  key == "tier_true" ~ mean(vbd_true_avg)
    #),
    .groups = "drop"
  ) %>% 
  distinct() %>% 
  separate(pos, c("key", "type", "tier", "pos"), "_") %>% 
  group_by(type, pos) %>% 
  #mutate(tier_avg = tier_avg - min(tier_avg)) %>% # normalisierung VBD
  ungroup() %>% 
  
  mutate(key = paste(type, pos, "tier_avg", sep = "_")) %>% 
  spread(key, tier_avg) %>% 
  group_by(tier) %>% 
  summarise(across(where(is.numeric), sum, na.rm = T), .groups = "drop") %>% 
  
  # tier value
  mutate(
    mfl_DL_tier_factor = mfl_DL_tier_avg / max(mfl_DL_tier_avg),
    true_DL_tier_factor = true_DL_tier_avg / max(true_DL_tier_avg),
    mfl_LB_tier_factor = mfl_LB_tier_avg / max(mfl_LB_tier_avg),
    true_LB_tier_factor = true_LB_tier_avg / max(true_LB_tier_avg),
  ) %>% 
  
  # positional value
  group_by(tier) %>% 
  mutate(
    mfl_DL_position_factor = mfl_DL_tier_avg / max(mfl_LB_tier_avg, mfl_DL_tier_avg),
    mfl_LB_position_factor = mfl_LB_tier_avg / max(mfl_LB_tier_avg, mfl_DL_tier_avg),
    true_DL_position_factor = true_DL_tier_avg / max(true_LB_tier_avg, true_DL_tier_avg),
    true_LB_position_factor = true_LB_tier_avg / max(true_LB_tier_avg, true_DL_tier_avg),
    tier = as.double(tier)
  ) %>% 
  ungroup() %>% 
  
  # prepare data to join
  select(tier, ends_with("_factor")) %>% 
  gather(key, factor, -tier) %>% 
  separate(key, c("pos_type", "pos", "type"), "_") %>% 
  mutate(key = paste(pos_type, pos, tier, sep = "_")) %>% 
  spread(type, factor) %>% 
  select(-pos_type, -pos) %>% 
  rename(factor_pos = position, factor_tier = tier) %>% 
  mutate(factor_pos = ifelse(is.na(factor_pos), 0, factor_pos))

#write.csv(factors, "fantasy/rfl/data/true-positions/raw/factors.csv", row.names = F)

## Roster Value ----
### Combined Data ----
rosterValue <- roster %>% 
  select(franchise_id, franchise_name, player_id) %>%
  mutate(player_id = as.integer(player_id)) %>%  
  left_join(playerValue, by = "player_id") %>% 
  filter(!is.na(player_name)) %>% 
  
  # prepare to join
  gather(key, tier, c(starts_with("tier"))) %>% 
  separate(key, c("key", "type"), "_") %>% 
  mutate(
    key = case_when(
      type == "mfl" ~ paste(type, mfl_position, tier, sep = "_"),
      type == "true" ~ paste(type, true_position, tier, sep = "_")
    )
  ) %>% 
  select(-tier) %>% 

  # join factors
  left_join(factors, by = "key") %>%
  
  # calc points
  mutate(
    factor_avg = (factor_pos + factor_tier) / 2,
    points_calc = points * factor_avg # berechnung anhand total points
    #points_calc = case_when( # berechnung anhand von vbd
    #  type == "mfl" ~ vbd_mfl_total * factor_avg,
    #  type == "true" ~ vbd_true_total * factor_avg
    #)
  ) %>% 
  filter(!is.na(points_calc))

#write.csv(rosterValue, "fantasy/rfl/data/true-positions/raw/values_roster_raw.csv", row.names = F)
  
rosterValueTotal <- rosterValue %>% 
  group_by(franchise_id, franchise_name, key) %>% 
  summarise(team_value = sum(points_calc), .groups = "drop") %>% 
  separate(key, c("type", "pos", "tier"), "_") %>% 
  group_by(franchise_id, franchise_name, type, pos) %>% 
  summarise(team_value = sum(team_value), .groups = "drop") %>% 
  spread(type, team_value) %>% 

  group_by(franchise_id, franchise_name) %>% 
  summarise(across(where(is.numeric), sum, rm.na = T), .groups = "drop") %>% 
  mutate(diff = true - mfl)

#write.csv(rosterValueTotal, "fantasy/rfl/data/true-positions/raw/values_roster_total.csv", row.names = F)

ggplot(rosterValueTotal, aes(x = diff, y = reorder(franchise_name, diff))) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Betroffenheit bei der Umstellung auf True Positions",
    subtitle = "Je höher der Wert, desto positiver die Umstellung auf True Positions",
    x = "",
    y = "Betroffenheit"
  )

## FAAB berechnung ----
var.faabMin = 50
var.faabMax = 150

faab <- rosterValueTotal %>% 
  mutate(
    faab_normal = diff - min(diff),
    faab_perc = 1 - faab_normal/max(faab_normal),
    faab = round((var.faabMax * faab_perc) + var.faabMin)
  )

ggplot(faab, aes(x = reorder(franchise_name, desc(faab)), y = faab)) +
  geom_col() +
  geom_text(aes(label = faab), vjust = 2, color = "white") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1, size = 12)
  )

#write.csv(faab, "fantasy/rfl/data/true-positions/faab.csv", row.names = F)

# Compensatory Picks ----
## Pick Value ----
vbdPicksRaw <- drafts %>% 
  select(overall, player_id, player_name) %>% 
  mutate(player_id = as.integer(player_id)) %>% 
    left_join(
      vbdPlayers %>%
        select(player_id, vbd_true) %>% 
        group_by(player_id) %>% 
        summarise(vbd_true = mean(vbd_true), .groups = "drop"),
        by = "player_id"
    ) %>% 
  filter(!is.na(vbd_true))

#write.csv(vbdPicksRaw, "fantasy/rfl/data/true-positions/raw/vbd_picks_raw.csv", row.names = F)

vbdPicks <- vbdPicksRaw %>% 
  group_by(overall) %>% 
  summarise(vbd_pick_avg = mean(vbd_true))

#write.csv(vbdPicks, "fantasy/rfl/data/true-positions/raw/vbd_picks.csv", row.names = F)

### Calc of smooth values
vbdPicks.lo <- loess(vbd_pick_avg ~ overall, vbdPicks)
vbdPicks.pred <- predict(vbdPicks.lo, data.frame(overall = seq(1, 252, 1)), se = TRUE)$fit

pickValue <- as.data.frame(vbdPicks.pred) %>% 
  rename(pick_value_raw = vbdPicks.pred) %>% 
  mutate(
    overall = row_number(),
    pick_value_raw = round(as.numeric(pick_value_raw), 4),
    #pick_value_clean = pick_value_raw - min(pick_value_raw),
    #pick_value_pct = pick_value_clean / max(pick_value_clean)
  )

rm(vbdPicks.lo, vbdPicks.pred)

#write.csv(pickValue, "fantasy/rfl/data/true-positions/raw/values_picks.csv", row.names = F)

## Calculate Compensatory Picks ----
# [1] ich habs bisher nicht in R hinbekommen, die comp picks den Spielern zuzuordnen. Deshalb hab ich das per google sheets gemacht und hier die daten importiert
# =array_constrain(filter(Sheet18!$A$2:A,ABS(C2-Sheet18!$A$2:A)=min(ABS(C2-Sheet18!$A$2:A))), 1, 1)
# https://infoinspired.com/google-docs/spreadsheet/find-closest-match-in-google-sheets/#:~:text=How%20to%20Find%20Closest%20Match%20in%20Google%20Sheets,-By&text=To%20find%20the%20closest%20match,replace%20Filter%20with%20Index%2DMatch
compPicks <- roster %>% 
  select(franchise_name, player_id) %>% 
  mutate(player_id = as.integer(player_id)) %>% 
  left_join(read.csv("fantasy/rfl/data/true-positions/player-and-pick-values.csv"), by = "player_id") %>% # [1]
  mutate(pick_value_raw = as.double(pick_value_raw)) %>% 
  left_join(pickValue, by = "pick_value_raw") %>% 
  rename(comp_pick = overall) %>% 
  filter(true_position %in% c("DL", "LB")) %>% 
  distinct()

#write.csv(compPicks, "fantasy/rfl/data/true-positions/comp_picks.csv", row.names = F)

ggplot(vbdPicks, aes(x = overall, y = vbd_pick_avg)) +
  geom_vline(xintercept = 36) +
  geom_vline(xintercept = 72) +
  geom_vline(xintercept = 108) +
  geom_vline(xintercept = 144) +
  geom_vline(xintercept = 180) +
  geom_vline(xintercept = 216) +
  geom_point(color = "black", alpha = 0.2) +
  geom_smooth(method = loess, formula = y ~ x, se = F) +
  
  geom_point(
    data = subset(compPicks, pick_value_raw <= -0.5 & pick_value_raw >= -6),
    aes(x = comp_pick, y = vbd_true_avg, color = true_position),
    size = 3
  ) +
  
  theme_minimal() +
  labs(
    title = "Pick Values",
    #title = "RFL VBD",
    #subtitle = "VBD is the players fantasy points minus the fantasy points of the baseline player,\nwhere the baseline player is the 12th-ranked QB, TE and PK the 24th-ranked DL,\nthe 30th-ranked RB, the 36th-ranked WR or DB, or the 48th-ranked LB",
    x = "Overall Pick",
    y = "Avg VBD seit 2017"
  )

# data export zu shiny app
write.csv(compPicks, "apps/rfl-vbd/data/rfl_comp-picks.csv", row.names = F)
write.csv(pickValue, "apps/rfl-vbd/data/rfl_pick-values.csv", row.names = F)
write.csv(faab, "apps/rfl-vbd/data/rfl_faab.csv", row.names = F)

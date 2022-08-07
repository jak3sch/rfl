library(ffscrapr)
library(tidyverse)
library(nflreadr)
library(ggplot2)
library(nflfastR)
library(scales)

mfl <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2021, rate_limit = FALSE)
mfl2020 <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2020, rate_limit = FALSE)
mfl2019 <- ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2019, rate_limit = FALSE)



# snap count daten ----
## plays aus pbp ----
plays <- nflfastR::load_pbp(2019:2021) %>% 
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

## snap counts von nflreader ----
snapCounts <- nflreadr::load_snap_counts() %>% 
  left_join(plays %>% select(season, week, defteam, plays), by = c("season", "week", "team" = "defteam")) %>% 
  filter(season >= 2019) %>% 
  group_by(pfr_player_id, season) %>% 
  summarise(
    snaps = sum(defense_snaps, na.rm = T),
    snap_pct_available = mean(defense_pct, na.rm = T),
    .groups = "drop"
  ) %>% 
  left_join(playerList, by = c("pfr_player_id" = "pfr_id"))

# scoring daten ----
scores2019 <- ffscrapr::ff_playerscores(mfl2019, season = 2019, week = "YTD")
scores2020 <- ffscrapr::ff_playerscores(mfl2020, season = 2020, week = "YTD")
scores2021 <- ffscrapr::ff_playerscores(mfl, season = 2021, week = "YTD")

scores <- rbind(scores2019, scores2020, scores2021) %>% 
  left_join(snapCounts %>% select(-pfr_player_id), by = c("season", "player_id")) %>% 
  filter(pos %in% c("DT", "DE", "LB")) %>% 
  select(-week, -is_available) %>% 
  
  mutate(team = ifelse(team == "OAK", "LVR", team)) %>% 
  left_join(plays %>% select(season, defteam, plays) %>% group_by(season, defteam) %>% summarise(plays = sum(plays), .groups = "drop"), by = c("season", "team" = "defteam")) %>% 
  mutate(
    snap_pct_total = snaps / plays
  ) %>%
  group_by(player_id) %>% 
  mutate(
    starter = ifelse(snap_pct_total >= 0.75 | snap_pct_available >= 0.75, 1, 0),
    points_best = ifelse(starter == 1, max(points), points)
  ) %>% 
  ungroup() %>% 
  filter(season == 2021) %>% 

  mutate(player_id = as.integer(player_id)) %>% 
  left_join(positionChanges, by = "player_id") %>% 
  rename(pos_mfl_2021 = pos) %>% 
  mutate(
    pos_mfl_2021 = ifelse(pos_mfl_2021 == "LB", "LB", "DL"),
    mfl_position = case_when(
      mfl_position == "LB" ~ "LB",
      mfl_position %in% c("DT", "DE") ~ "DL",
      TRUE ~ pos_mfl_2021
    ),
    true_position = case_when(
      true_position %in% c("DT", "DE") ~ "DL",
      true_position == "LB" ~ "LB",
      TRUE ~ pos_mfl_2021
    )
  ) %>% 

  group_by(pos_mfl_2021) %>% 
    arrange(desc(points_best)) %>% 
    mutate(
      rank_2021 = row_number(),
      tier_2021 = case_when(
        rank_2021 <= 12 ~ 1,
        rank_2021 <= 24 ~ 2,
        rank_2021 <= 36 ~ 3,
        rank_2021 <= 48 ~ 4,
        rank_2021 <= 60 ~ 5,
        TRUE ~ 6
      )
    ) %>% 
  group_by(mfl_position) %>% 
    arrange(desc(points_best)) %>% 
    mutate(
      rank_2022 = row_number(),
      tier_2022 = case_when(
        rank_2022 <= 12 ~ 1,
        rank_2022 <= 24 ~ 2,
        rank_2022 <= 36 ~ 3,
        rank_2022 <= 48 ~ 4,
        rank_2022 <= 60 ~ 5,
        TRUE ~ 6
      )
    ) %>% 
  group_by(true_position) %>% 
    arrange(desc(points_best)) %>% 
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
  
  select(-starts_with("rank"))

## avg ----
scoresTierAvg <- scores %>% 
  gather(key, tier, starts_with("tier")) %>% 
  mutate(
    key = case_when(
      key == "tier_2021" ~ paste(key, pos_mfl_2021, sep = "_"),
      key == "tier_2022" ~ paste(key, mfl_position, sep = "_"),
      key == "tier_true" ~ paste(key, true_position, sep = "_"),
    )
  ) %>% 
  
  mutate(
    key = sub("tier_", "multiplier_", key)
  ) %>% 
  
  group_by(key, tier) %>% 
  summarise(
    tier_avg = mean(points),
    #tier_med = median(points),
    .groups = "drop"
  ) %>% 
  group_by(key) %>% 
  mutate(tier_pct = tier_avg / max(tier_avg)) %>% 
  ungroup() %>% 
  select(-tier_avg) %>% 
  spread(key, tier_pct)

scoresPosAvg <- scores %>% 
  group_by(true_position, tier_true) %>% 
  mutate(true_avg = mean(points)) %>% 
  
  group_by(mfl_position, tier_2022) %>% 
  mutate(mfl_avg_2022 = mean(points)) %>% 
  
  group_by(pos_mfl_2021, tier_2021) %>% 
  mutate(mfl_avg_2021 = mean(points)) %>% 
  
  select(-points, -player_id) %>% 
  ungroup() %>% 
  distinct()

## pos value ----
posValueTrue <- data.frame(pos = c("DL", "LB")) %>% 
  left_join(scoresPosAvg %>% select(true_position, tier_true, true_avg), by = c("pos" = "true_position")) %>% 
  distinct() %>% 
  spread(pos, true_avg) %>%
  group_by(tier_true) %>% 
  mutate(
    pos_value_DL_true = DL / pmax(DL, LB),
    pos_value_LB_true = LB / pmax(DL, LB)
  ) %>% 
  select(-DL, -LB)

posValueMFL2022 <- data.frame(pos = c("DL", "LB")) %>% 
  left_join(scoresPosAvg %>% select(mfl_position, tier_2022, mfl_avg_2022), by = c("pos" = "mfl_position")) %>% 
  distinct() %>% 
  spread(pos, mfl_avg_2022) %>% 
  mutate(
    pos_value_DL_mfl_2022 = DL / pmax(DL, LB),
    pos_value_LB_mfl_2022 = LB / pmax(DL, LB)
  ) %>% 
  select(-DL, -LB)

posValueMFL2021 <- data.frame(pos = c("DL", "LB")) %>% 
  left_join(scoresPosAvg %>% select(pos_mfl_2021, tier_2021, mfl_avg_2021), by = c("pos" = "pos_mfl_2021")) %>% 
  distinct() %>% 
  spread(pos, mfl_avg_2021) %>% 
  mutate(
    pos_value_DL_mfl_2021 = DL / pmax(DL, LB),
    pos_value_LB_mfl_2021 = LB / pmax(DL, LB)
  ) %>% 
  select(-DL, -LB)

# roster ----
roster <- ffscrapr::ff_rosters(ffscrapr::ff_connect(platform = "mfl", league_id = "63018", season = 2022, rate_limit = FALSE)) %>% 
  select(franchise_id, franchise_name, player_id) %>% 
  left_join(ffscrapr::ff_rosters(mfl) %>% select(-starts_with("franchise")), by = "player_id") %>% 
  filter(
    pos %in% c("DT", "DE", "LB"),
    draft_year < 2022
  ) %>% 
  mutate(
    season = 2021
  ) %>% 
  select(season, franchise_id, franchise_name, player_id, player_name, draft_year) %>% 
  left_join(playerList, by = "player_id") %>% 
  left_join(snapCounts %>% select(-pfr_player_id), by = c("player_id", "season")) %>% 
  mutate(rookie = ifelse(draft_year == season, 1, 0)) %>% 
  mutate(player_id = as.integer(player_id)) %>% 
  left_join(scores %>% select(-player_name,-snaps,-snap_pct_available,-season), by = "player_id") %>% 
  
  rename(
    mfl_2021 = pos_mfl_2021,
    mfl_2022 = mfl_position,
    true = true_position
  ) %>% 
  distinct()

# zusammengefasst nach team
rflTeams <- roster %>% 
  
  #filter(franchise_id == "0007") %>% 
  
  group_by(franchise_id, mfl_2021, tier_2021) %>% 
  mutate(points_mfl_2021 = sum(points, na.rm = T)) %>% 
  
  group_by(franchise_id, mfl_2022, tier_2022) %>% 
  mutate(points_mfl_2022 = sum(points, na.rm = T)) %>% 
    
  group_by(franchise_id, true, tier_true) %>% 
  mutate(points_true = sum(points, na.rm = T)) %>% 
  ungroup() %>% 
  
  gather(key, pos, c(mfl_2021, mfl_2022, true)) %>% 
  mutate(
    points = case_when(
      key == "mfl_2021" ~ points_mfl_2021,
      key == "mfl_2022" ~ points_mfl_2022,
      key == "true" ~ points_true
    )
  ) %>% 
  select(franchise_id, key, pos, starts_with("tier"), points) %>%
  left_join(roster %>% select(franchise_id, franchise_name), by = "franchise_id") %>% 
  distinct() %>% 
  mutate(tier = case_when(
    key == "mfl_2021" ~ tier_2021,
    key == "mfl_2022" ~ tier_2022,
    key == "true" ~ tier_true
  )) %>% 
  select(-starts_with("tier_")) %>% 
  distinct() %>% 
  filter(!is.na(tier))
  

ggplot(rflTeams, aes(x = tier, y = points, color = key, fill = key, linetype = key)) +
  facet_wrap(vars(franchise_name, pos)) +
  facet_wrap(vars(pos)) +
  geom_line(size = 2, alpha = 0.4) +
  geom_point(size = 4, stroke = 2) +
  scale_shape_manual(values = c(22, 25, 24)) +
  scale_color_manual(values = c("#e74c3c", "#3498db", "#f1c40f")) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(1,6)) +
  theme(
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Total Franchise Pts by Tier",
    subtitle = "Tier 1 = Top 12, Tier 2 = Top 24 etc.; Tier 6 > Top 60\nje mehr rote Linie zu sehen ist, desto betroffener ist das Team von den Positionsänderungen im Mai.",
    x = "Tier",
    y = "Total FPts"
  )

rflTeamsScoreRaw <- rflTeams %>% 
  mutate(
    label = paste(key, pos, sep = "_")
  ) %>% 
  spread(label, points) %>% 
  select(-pos, -key) %>% 
  group_by(franchise_name, tier) %>% 
  summarise(across(where(is.numeric), ~ mean(. , na.rm = T)), .groups = "drop")

rflTeamsScoreRaw[ rflTeamsScoreRaw == "NaN"] <- 0

## berechne team score; dabei wird jedes tier mit dem multiplikator multipliziert, der anhand des position avgs berechnet wurde
rflTeamsScore <- rflTeamsScoreRaw %>% 
  #filter(franchise_name == "Dexter Wolves") %>% 
  select(-ends_with("NA")) %>% 
  left_join(scoresTierAvg, by = "tier") %>% 
  left_join(posValueMFL2021, by = c("tier" = "tier_2021")) %>% 
  left_join(posValueMFL2022, by = c("tier" = "tier_2022")) %>% 
  left_join(posValueTrue, by = c("tier" = "tier_true")) %>% 
  filter(tier <= 5) %>% 

  mutate(
    changes_mai_DL = (mfl_2022_DL * pos_value_DL_mfl_2022 * multiplier_2022_DL) - (mfl_2021_DL * pos_value_DL_mfl_2021 * multiplier_2021_DL),
    changes_mai_LB = (mfl_2022_LB * pos_value_LB_mfl_2022 * multiplier_2022_LB) - (mfl_2021_LB * pos_value_LB_mfl_2021 * multiplier_2021_LB),
    changes_true_DL = (true_DL * pos_value_DL_true * multiplier_true_DL) - (mfl_2022_DL * pos_value_DL_mfl_2022 * multiplier_2022_DL),
    changes_true_LB = (true_LB * pos_value_LB_true * multiplier_true_LB) - (mfl_2022_LB * pos_value_LB_mfl_2022 * multiplier_2022_LB),
    changes_mai = changes_mai_DL + changes_mai_LB,
    changes_true = changes_true_DL + changes_true_LB
  ) %>% 
  select(franchise_name, tier, starts_with("changes")) %>% 
  gather(key, value, starts_with("changes")) %>% 
  filter(!is.na(value))

ggplot(rflTeamsScore, aes(x = tier, y = value, color = key)) +
  facet_wrap(vars(franchise_name)) +
  geom_line(size = 1)

## totals ----
rflTeamsScoreTotal <- rflTeamsScore %>% 
  filter(key == "changes_true") %>% 
  #filter(key == "changes_mai") %>% 
  group_by(franchise_name, key) %>% 
  summarise(value = sum(value), .groups = "drop")

ggplot(rflTeamsScoreTotal, aes(x = key, y = reorder(franchise_name, value), fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 4, color = "white") +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Einfluss der Positionsänderungen auf True Positions",
    subtitle = "je größer der Wert, desto positiver der Einfluss",
    x = "",
    y = ""
  )

## DL Größe ----
dlSizeMfl <- roster %>% 
  filter(mfl_2022 == "DL") %>% 
  group_by(franchise_name) %>% 
  summarise(dlMfl = n())

dlSizeTrue <- roster %>% 
  filter(true == "DL") %>% 
  group_by(franchise_name) %>% 
  summarise(dlTrue = n())

dlSize <- dlSizeMfl %>% 
  left_join(dlSizeTrue, by = "franchise_name") %>% 
  mutate(
    diff = dlTrue - dlMfl
  )

## faab berechnung ----
var.faabMin = 10
var.faabMax = 140

faab <- rflTeamsScoreTotal %>%
  filter(key == "changes_true") %>% 
  spread(key, value) %>% 
  mutate(
    faab_normal = changes_true - min(changes_true),
    faab_perc = 1 - faab_normal/max(faab_normal),
    faab = round((var.faabMax * faab_perc) + var.faabMin)
  )

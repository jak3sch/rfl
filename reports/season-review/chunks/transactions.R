# zu- und abg?nge ----
trans1 <- ffscrapr::mfl_connect(league_id = 63018, season = var.lastSeason + 1)
trans2 <- ffscrapr::mfl_connect(league_id = 63018, season = var.lastSeason)

transactions1 <- ffscrapr::ff_transactions(trans1)
transactions2 <- ffscrapr::ff_transactions(trans2)

transactions <- rbind(transactions1, transactions2) %>%
  separate(timestamp, into = c("date", "time"), sep =" (?=[^ ]+$)") %>% 
  mutate(date = gsub("-", "", date)) %>% 
  filter(
    type %in% c("FREE_AGENT", "TRADE"),
    !is.na(player_name),
    date >= paste(var.lastSeason + 1, "0108", sep = "")
  ) %>% 
  mutate(
    transfer_dir = ifelse(type_desc %in% c("traded_for", "added"), "Zugang", "Abgang")
  ) %>% 
  left_join(franchiseWARPlayer %>% select(franchise_id, player_id, points, war), by = c("franchise_id", "player_id")) %>% 
  mutate(
    points = ifelse(is.na(points), 0, points),
    war = ifelse(is.na(war), 0, war)
  )

rm(trans1, trans2, transactions1, transactions2)

trades <- transactions %>% 
  filter(type == "TRADE")

# transaction summary ----
transactionsByFranchise <- transactions %>% 
  group_by(franchise_id, transfer_dir) %>% 
  summarise(
    war = sum(war),
    .groups = "drop"
  ) %>% 
  spread(transfer_dir, war) %>% 
  mutate(
    Zugang = ifelse(is.na(Zugang), 0, Zugang),
    Abgang = ifelse(is.na(Abgang), 0, Abgang),
    war_diff = Zugang - Abgang
  ) %>% 
  left_join(franchisesLatest %>% select(franchise_id, franchise_name), by = "franchise_id")

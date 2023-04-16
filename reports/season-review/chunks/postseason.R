# post season ----
postseasonRaw <- read.table(here("reports/rfl/data/postseason-matchups.csv"))
names(postseasonRaw) <- postseasonRaw %>% slice(1) %>% unlist() 
postseasonClean <- postseasonRaw %>% slice(-1)

postseason1 <- franchisesLatest %>% 
  select(franchise_id, franchise_name) %>% 
  left_join(postseasonClean %>% rename(franchise_points = away_points, opponent_id = home_id, opponent_points = home_points), by = c("franchise_id" = "away_id"))

postseason2 <- franchisesLatest %>% 
  select(franchise_id, franchise_name) %>% 
  left_join(postseasonClean %>% rename(franchise_points = home_points, opponent_id = away_id, opponent_points = away_points), by = c("franchise_id" = "home_id"))

postseason <- rbind(postseason1, postseason2) %>% 
  left_join(franchisesLatest %>% select(franchise_id, franchise_name) %>% rename(opponent_name= franchise_name), by = c("opponent_id" = "franchise_id"))

rm(postseasonRaw, postseasonClean)

library(tidyverse)
library(ffscrapr)
library(dplyr)
library(tidyr)
library(RCurl)
library(DT)

# sticky positions
sheet = RCurl::getForm("https://spreadsheets.google.com/spreadsheet/pub", 
                hl ="en_US", 
                key = "1rf5m-7DPzwfJViYNYZliBXVnaGuucrYAZhVCQlPTbI4", 
                output = "csv", 
                .opts = list(followlocation = TRUE, 
                             verbose = TRUE, 
                             ssl.verifypeer = FALSE)) 

stickyPostitions <- read.csv(textConnection(sheet)) %>% 
  rename(
    player_name = MFL.Name,
    player_id = X,
    true = TRUE.POSITION,
    mfl = MFL.POSITION
  )

# roster ----
player <- ffscrapr::mfl_players(ffscrapr::ff_connect(platform = "mfl", league_id = 63018, season = 2022)) %>% 
  select(player_id, pos) %>% 
  mutate(player_id = as.integer(player_id))

# abgleich ----
compare <- stickyPostitions %>% 
  left_join(player, by = "player_id") %>% 
  mutate(compare = ifelse(true == pos, 1, 0))
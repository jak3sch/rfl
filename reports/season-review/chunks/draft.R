# draft ----
## adp ----
# adp report: https://www77.myfantasyleague.com/2022/reports?R=ADP&POS=*&PERIOD=DRAFT&CUTOFF=5&FCOUNT=0&ROOKIES=1&INJURED=1&IS_PPR=2&IS_KEEPER=R&IS_MOCK=1&PAGE=ALL
adp <- data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
                     RANK = c(1L,2L,3L,4L,
                              5L,6L,7L,8L,9L,10L,11L,12L,13L,14L,15L,
                              16L,17L,18L,19L,20L,21L,22L,23L,24L,25L,
                              26L,27L,28L,29L,30L,31L,32L,33L,34L,35L,
                              36L,37L,38L,39L,40L,41L,42L,43L,44L,45L,
                              46L,47L,48L,49L,50L,51L,52L,53L,54L,55L,
                              56L,57L,58L,59L,60L,61L,62L,63L,64L,65L,
                              66L,67L,68L,69L,70L,71L,72L,73L,74L,75L,
                              76L,77L,78L,79L,80L,81L,82L,83L,84L,85L,
                              86L,87L,88L,89L,90L,91L,92L,93L,94L,95L,
                              96L,97L,98L,99L,100L,101L,102L,103L,104L,
                              105L,106L,107L,108L,109L,110L,111L,112L,
                              113L,114L,115L,116L,117L,118L,119L,120L,121L,
                              122L,123L,124L,125L,126L),
                   PLAYER = c("Hall, Breece NYJ RB","Walker III, Kenneth SEA RB",
                              "London, Drake ATL WR","Burks, Treylon TEN WR",
                              "Wilson, Garrett NYJ WR","Williams, Jameson DET WR",
                              "Olave, Chris NOS WR","Moore, Skyy KCC WR",
                              "Watson, Christian GBP WR","Cook, James BUF RB",
                              "Pickens, George PIT WR","Pickett, Kenny PIT QB",
                              "Dotson, Jahan WAS WR","Hutchinson, Aidan DET DE",
                              "Spiller, Isaiah LAC RB","Pierce, Dameon HOU RB",
                              "White, Rachaad TBB RB","Lloyd, Devin JAC LB",
                              "McBride, Trey ARI TE","Pierce, Alec IND WR",
                              "Willis, Malik TEN QB","Bell, David CLE WR",
                              "Metchie, John HOU WR","White, Zamir LVR RB",
                              "Thibodeaux, Kayvon NYG LB","Dean, Nakobe PHI LB",
                              "Allgeier, Tyler ATL RB","Ridder, Desmond ATL QB",
                              "Hamilton, Kyle BAL S","Robinson, Brian WAS RB",
                              "Walker, Quay GBP LB","Tolbert, Jalen DAL WR",
                              "Robinson, Wan'Dale NYG WR","Davis-Price, Tyrion SFO RB",
                              "Corral, Matt CAR QB","Woods, Jelani IND TE",
                              "Walker, Travon JAC DE","Karlaftis, George KCC DE",
                              "Johnson, Jermaine NYJ DE","Thornton, Tyquan NEP WR",
                              "Dulcich, Greg DEN TE","Shakir, Khalil BUF WR",
                              "Haskins, Hassan TEN RB","Andersen, Troy ATL LB",
                              "Ingram, Keaontay ARI RB","Ross, Justyn KCC WR",
                              "Strong Jr., Pierre NEP RB","Jones, Velus CHI WR",
                              "Howell, Sam WAS QB","Cine, Lewis MIN S",
                              "Doubs, Romeo GBP WR","Harris, Christian HOU LB",
                              "Williams, Kyren LAR RB","Austin III, Calvin PIT WR",
                              "Mafe, Boye SEA DE","Brisker, Jaquan CHI S",
                              "Gray, Danny SFO WR","Badie, Tyler BAL RB",
                              "Ford, Jerome CLE RB","Ruckert, Jeremy NYJ TE",
                              "Davis, Jordan PHI DT","Tindall, Channing MIA LB",
                              "Otton, Cade TBB TE","Ojabo, David BAL LB",
                              "Ebiketie, Arnold ATL LB","Pacheco, Isaih KCC RB",
                              "Chandler, Ty MIN RB","Hall, Logan TBB DE","Chenal, Leo KCC LB",
                              "Harris, Kevin NEP RB","Conner, Snoop JAC RB",
                              "Muma, Chad JAC LB","Bellinger, Daniel NYG TE",
                              "Stingley Jr., Derek HOU CB","Philips, Kyle TEN WR",
                              "Wyatt, Devonte GBP DE","Gardner, Ahmad NYJ CB",
                              "Kolar, Charlie BAL TE","Hill, Daxton CIN S",
                              "Strong, Carson PHI QB","Likely, Isaiah BAL TE",
                              "Smith, Abram NOS RB","Ferguson, Jake DAL TE",
                              "Asamoah, Brian MIN LB","Brooks, Kennedy PHI RB",
                              "Zappe, Bailey NEP QB","Okonkwo, Chigoziem TEN TE",
                              "Pitre, Jalen HOU S","Ezukanma, Erik MIA WR",
                              "Williams, Sam DAL DE","Toure, Samori GBP WR",
                              "York, Cade CLE PK","Bonitto, Nik DEN LB",
                              "Jackson, Drake SFO DE","Price, D'vonte IND RB",
                              "Melton, Bo SEA WR","Turner, Cole WAS TE",
                              "Nailor, Jalen MIN WR","White, ZaQuandre MIA RB",
                              "Cook, Bryan KCC S","Wydermyer, Jalen BUF TE",
                              "McDuffie, Trent KCC CB","Ealy, Jerrion KCC RB",
                              "Goodson, Tyler GBP RB","Thompson, Skylar MIA QB",
                              "Ebner, Trestan CHI RB","Bernard, Terrel BUF LB",
                              "Austin, Kevin JAC WR","Paschal, Josh DET DE",
                              "Corbin, Jashaun NYG RB","Calcaterra, Grant PHI TE",
                              "Elam, Kaiir BUF CB","Smith, Brandon CAR LB",
                              "Leal, DeMarvin PIT DT","Cross, Nick IND S",
                              "Rodriguez, Malcolm DET LB","Clark, Damone DAL LB",
                              "Booth, Andrew MIN CB","Jones, Travis BAL DT",
                              "McFadden, Micah NYG LB","Mathis, Phidarian WAS DT",
                              "Thomas, Cameron ARI DE","Gordon, Kyler CHI CB",
                              "Woods, JT LAC S","Winfrey, Perrion CLE DT",
                              "Sanders, Myjai ARI LB"),
                   STATUS = c(NA,NA,NA,NA,
                              NA,"Questionable",NA,"Questionable",NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              "Questionable",NA,"Questionable","Questionable",NA,
                              NA,NA,NA,NA,"Questionable",NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              "Questionable",NA,NA,"Questionable","Questionable",NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              NA,"Doubtful","Questionable",NA,NA,NA,
                              "Questionable",NA,NA,NA,NA),
                 AVG.PICK = c(1.18,2.88,3.24,
                              4.91,5.33,6.15,7.61,9.36,9.68,10.56,12.88,
                              13.75,13.9,16.77,17.39,17.46,17.72,18.59,
                              20.95,22.09,22.41,22.63,22.93,24.01,24.07,
                              24.92,27.23,27.71,27.74,27.96,28.65,28.81,
                              30.85,31,31.22,33.96,34.79,36.28,36.91,38.44,
                              38.67,38.76,40.23,40.73,40.97,41.29,41.48,
                              42.4,42.63,43.07,43.89,46.01,46.33,46.67,
                              48.55,50.12,50.85,50.92,51.05,51.23,51.71,
                              51.87,52.25,53.54,53.55,54.01,55.4,55.89,55.94,
                              56.27,56.42,56.47,56.64,56.77,56.87,57.44,
                              58.13,58.61,59.37,59.93,60.55,60.83,61.45,
                              61.68,61.75,63.36,63.58,63.79,63.91,65.04,
                              66.81,67.43,67.43,67.76,68.7,68.91,69.33,
                              69.84,70.05,70.54,70.58,70.83,71.56,71.8,71.93,
                              73.27,74.6,74.64,75.3,75.43,75.66,76.23,
                              77.13,78.38,81.95,81.96,82.72,83.13,85.39,
                              85.51,86.6,86.97,87.97,88.63,90,90.15),
                 MIN.PICK = c(1L,1L,1L,1L,
                              1L,1L,1L,1L,1L,1L,2L,1L,2L,1L,2L,3L,3L,
                              1L,1L,5L,1L,4L,6L,4L,1L,1L,6L,1L,3L,
                              4L,4L,5L,8L,2L,4L,6L,1L,9L,4L,4L,7L,8L,
                              7L,14L,4L,6L,8L,5L,11L,12L,11L,7L,11L,
                              14L,4L,13L,12L,6L,17L,16L,3L,17L,4L,10L,
                              12L,14L,20L,2L,15L,18L,22L,10L,21L,4L,
                              18L,9L,6L,5L,13L,15L,7L,19L,21L,8L,21L,
                              21L,14L,26L,14L,14L,24L,6L,30L,26L,17L,
                              24L,5L,26L,25L,28L,19L,4L,28L,35L,2L,26L,
                              35L,7L,22L,27L,21L,13L,25L,24L,29L,25L,
                              39L,36L,22L,34L,38L,39L,34L,44L,44L,45L),
                 MAX.PICK = c(17L,38L,18L,
                              19L,27L,37L,22L,40L,41L,42L,40L,62L,52L,
                              52L,67L,54L,58L,62L,80L,53L,73L,67L,55L,
                              65L,63L,69L,88L,83L,63L,79L,72L,68L,88L,
                              75L,96L,94L,78L,76L,80L,84L,82L,95L,99L,
                              81L,99L,130L,104L,96L,111L,81L,99L,93L,126L,
                              117L,89L,88L,100L,118L,139L,110L,122L,
                              98L,112L,105L,93L,121L,128L,92L,108L,177L,
                              133L,110L,127L,126L,125L,96L,119L,134L,107L,
                              187L,137L,146L,143L,105L,175L,157L,149L,
                              109L,140L,99L,173L,131L,108L,111L,189L,
                              158L,179L,169L,178L,128L,171L,121L,201L,167L,
                              195L,144L,108L,191L,119L,203L,182L,135L,
                              120L,127L,140L,127L,134L,127L,133L,119L,
                              150L,126L,140L,144L,148L,132L),
             `%.SELECTED` = c(91L,98L,94L,
                              92L,92L,99L,93L,100L,99L,98L,92L,99L,98L,
                              41L,91L,98L,99L,41L,99L,99L,93L,92L,93L,
                              92L,40L,41L,98L,94L,41L,96L,40L,98L,96L,
                              97L,91L,95L,40L,39L,40L,95L,94L,94L,95L,
                              36L,90L,88L,92L,91L,77L,39L,90L,34L,81L,
                              85L,33L,35L,78L,72L,74L,70L,28L,31L,68L,
                              31L,30L,64L,60L,28L,27L,46L,60L,28L,51L,
                              26L,49L,26L,23L,42L,27L,29L,36L,41L,36L,
                              24L,37L,21L,30L,23L,28L,21L,20L,13L,18L,
                              17L,6L,15L,17L,15L,11L,17L,11L,14L,9L,9L,
                              7L,13L,14L,6L,12L,7L,7L,12L,12L,11L,10L,
                              8L,9L,7L,6L,8L,6L,7L,7L,7L,5L,6L)
       )

adp <- adp %>% 
  rename(
    rank = RANK,
    player = PLAYER,
    pick_avg = AVG.PICK,
    pick_min = MIN.PICK,
    pick_max = MAX.PICK
  ) %>% 
  select(-STATUS,-"%.SELECTED")

## draft ----
draft <- ffscrapr::ff_draft(conn) %>% 
  mutate(
    overall_clean = ceiling(overall / 3), # runde overall pick immer zur n?chst h?heren, vollen zahl auf
    player = paste(player_name, team, pos) 
  ) %>% 
  left_join(adp, by = "player") %>% 
  select(-timestamp, -age) %>% 
  mutate(
    pick_diff = as.numeric(pick_avg - overall_clean)
  ) %>% 
  group_by(player_id) %>% 
  mutate(
    rfl_min = min(overall),
    rfl_max = max(overall),
    rfl_avg = mean(overall),
    rfl_clean_min = min(overall_clean),
    rfl_clean_max = max(overall_clean),
    rfl_clean_avg = mean(overall_clean)
  ) %>% 
  ungroup() %>% 
  mutate(
    rfl_diff = rfl_avg - overall,
    pick_value = rfl_diff + pick_diff,
    pick_cat = case_when(
      pick_diff >= 6 ~ "Leichter Reach",
      pick_diff <= -6 ~ "Leichter Steal",
      pick_diff >= 12 ~ "Reach",
      pick_diff <= -12 ~ "Steal",
      pick_diff >= 24 ~ "Großer Reach",
      pick_diff <= -24 ~ "Großer Steal",
      TRUE ~ "Value"
    )
  )

draftScore <- draft %>% 
  group_by(franchise_id) %>% 
  summarise(pick_diff = mean(pick_diff)) %>% 
  mutate(
    pick_cat = case_when(
      pick_diff >= 6 ~ "Leichter Reach",
      pick_diff <= -6 ~ "Leichter Steal",
      pick_diff >= 12 ~ "Reach",
      pick_diff <= -12 ~ "Steal",
      pick_diff >= 24 ~ "Großer Reach",
      pick_diff <= -24 ~ "Großer Steal",
      TRUE ~ "Value"
    )
  )

rm(adp)
# mfl ----
v_platform <- "mfl"
v_season_first <- 2016
v_season_last <- nflreadr::most_recent_season()
v_base_url <- "https://www45.myfantasyleague.com/"
v_mfl_league_id <- 63018
v_mfl_api_base <- paste0(v_base_url, v_season_last, "/")
conn <- ffscrapr::mfl_connect(league_id = v_mfl_league_id, season = v_season_last, rate_limit = FALSE)

# colors ----
v_colors <- c(
  "c_black" <- "#00080A",
  "c_pine_green" <- "#02272F",
  "c_mahogany_red" <- "#2B0618",
  "c_ruby_red" <- "#6A0F3B",
  "c_orange" <- "#FF5823",
  "c_yellow" <- "#EEBE40",
  "c_steel_blue" <- "#8AC6C7",
  "c_grey" <- "#CFDEDB",
  "c_white" <- "#ffffff"
)

c_background <- c_pine_green
c_accent <- c_yellow
c_light <- c_grey
c_muted <- c_steel_blue

var.colorBlue <- "#00183C"
var.colorLightblue <- "#263C56"
var.colorRed <- "#D03324"
var.colorYellow <- "#eb9b03"

color_warning <- "#ee5253"

v_colors_positions <- c(
  "QB" <- "#FC2B6D",
  "RB" <- "#20CEB8",
  "WR" <- "#59A7FF",
  "TE" <- "#FEAE58",
  "PK" <- "#c8d6e5",
  "DL" <- "#BD66FF",
  "LB" <- "#A3ACFF",
  "DB" <- "#FD7DB6"
)

c_palette <- c("#2e86de", "#ee5253", "#10ac84",  "#ff9f43", "#f368e0", "#0abde3", "#feca57", "#1dd1a1", "#8395a7", "#5f27cd")
c_palette_contrast <- c("#ff9ff3", "#5f27cd", "#feca57", "#1dd1a1", "#54a0ff", "#c8d6e5", "#10ac84", "#ff9f43", "#00d2d3", "#ff6b6b")

# fonts ----
var.fontText <- "Libre Franklin"
var.fontTextBold <- "Libre Franklin ExtraBold"
var.fontHeadline <- "Lora"
var.fontAccent <- "Lora"

sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

# other ----
var.posOrder <- c("QB", "RB", "WR", "TE", "PK", "DT", "DE", "LB", "CB", "S")

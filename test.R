library(tidyverse)
library(nflreadr)

starter <- read.csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-2022.csv", colClasses = c("franchise_id" = "character"))
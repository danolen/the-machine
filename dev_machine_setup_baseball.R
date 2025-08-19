library(tidyverse)
library(RMariaDB)
setwd("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine")
readRenviron(".Renviron")
source("dannyverse/dannyverse.R")

con <- dbConnect(
  RMariaDB::MariaDB(),
  user = Sys.getenv("MYSQL_USER"),
  password = Sys.getenv("MYSQL_PWD"),
  dbname = Sys.getenv("MYSQL_DBNAME"),
  host = Sys.getenv("MYSQL_HOST")
)

#### d_rosters ####

daily_rosters <- read.csv(paste0("Baseball Machine/Daily Files/2024/daily_rosters_2024.csv"))




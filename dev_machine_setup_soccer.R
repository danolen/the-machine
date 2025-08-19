library(tidyverse)
library(worldfootballR)
library(RMariaDB)
library(stringi)
readRenviron(".Renviron")

con <- dbConnect(
  RMariaDB::MariaDB(),
  user = Sys.getenv("MYSQL_USER"),
  password = Sys.getenv("MYSQL_PWD"),
  dbname = Sys.getenv("MYSQL_DBNAME"),
  host = Sys.getenv("MYSQL_HOST")
)

#### d_competitions ####

comps <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv")

dbWriteTable(con, "d_competitions", comps, overwrite = TRUE, append = FALSE, row.names = FALSE)

rm(list=setdiff(ls(), "con"))

#### d_player_mapping ####

player_dict <- player_dictionary_mapping() 

cleaned_player_dict <- player_dict %>% 
  mutate(PlayerFBref = stri_trans_general(PlayerFBref, "Latin-ASCII"))

dbWriteTable(con, "d_player_mapping", cleaned_player_dict, overwrite = TRUE, append = FALSE, row.names = FALSE)

rm(list=setdiff(ls(), "con"))

#### d_seasons_with_xg ####

seasons_w_xg <- read.csv("Soccer Machine/d_seasons_with_xg.csv")

dbWriteTable(con, "d_seasons_with_xg", seasons_w_xg, overwrite = TRUE, append = FALSE, row.names = FALSE)

rm(list=setdiff(ls(), "con"))

#### d_team_seasons ####

seasons_query <- "
  select distinct c.seasons_urls, 
  from dev_machine.d_competitions c
  inner join dev_machine.d_seasons_with_xg s
  	on c.comp_url = s.comp_url
  where s.first_season <= c.seasons
    and c.competition_name = 'Premier League'
"

seasons_result <- dbGetQuery(con, seasons_query)

team_seasons <- data.frame(season_url = character(), team_url = character(), stringsAsFactors = FALSE)

for (url in seasons_result$seasons_urls) {
  processed_teams <- fb_teams_urls(url)
  temp_df <- data.frame(season_url = url, team_url = processed_teams)
  team_seasons <- bind_rows(team_seasons,temp_df)
  print(paste0(url,": complete"))
}

dbWriteTable(con, "d_team_seasons", team_seasons, overwrite = TRUE, append = FALSE, row.names = FALSE)

rm(list=setdiff(ls(), "con"))

#### d_player_seasons ####

teams_query <- "
  select distinct team_url
  from dev_machine.d_team_seasons
"

teams_result <- dbGetQuery(con, teams_query)

player_seasons <- data.frame(team_url = character(), player_url = character(), stringsAsFactors = FALSE)

for (url in teams_result$team_url) {
  processed_players <- fb_player_urls(url)
  temp_df <- data.frame(team_url = url, player_url = processed_players)
  player_seasons <- bind_rows(player_seasons,temp_df)
  print(paste0(url,": complete"))
}

dbWriteTable(con, "d_player_seasons", player_seasons, overwrite = TRUE, append = FALSE, row.names = FALSE)

rm(list=setdiff(ls(), "con"))

#### f_player_wages ####

teams_query <- "
  select distinct team_url
  from dev_machine.d_team_seasons
"

teams_result <- dbGetQuery(con, teams_query)

player_wages <- data.frame()

extract_first_element <- function(lst) {
  map(lst, 1)
}

extract_first_element_to_numeric <- function(col) {
  if (is.list(col)) {
    return(sapply(col, function(x) as.numeric(x[[1]])))
  } else {
    return(col)
  }
}

for (url in teams_result$team_url) {
  processed_wages <- fb_squad_wages(url)
  wages_updated <- processed_wages %>%
    mutate(across(where(is.list), extract_first_element)) %>%
    unnest(cols = where(is.numeric)) %>%
    mutate(across(where(is.list), extract_first_element_to_numeric)) %>%
    mutate(across(where(is.list), as.numeric))
  player_wages <- bind_rows(player_wages,wages_updated)
  print(paste0(url,": complete"))
}

dbWriteTable(con, "f_player_wages", player_wages, overwrite = TRUE, append = FALSE, row.names = FALSE)

rm(list=setdiff(ls(), "con"))




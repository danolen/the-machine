library(tidyverse)

#### Baseball ####

library(baseballr)

mlb_roster_types()
mlb_rosters(109, 2019, "2019-06-01", "active")
mlb_teams

baseballr::mlb_probables(game_pk = 719359)

today_pks <- get_game_pks_mlb(Sys.Date())
tomorrow_pks <- get_game_pks_mlb(Sys.Date()+1)

baseballr::get_chadwick_lu()

# Get Projections 

library(rvest)

# URL for DepthCharts projections
depth_url <- "https://www.fangraphs.com/projections?statgroup=fantasy&type=fangraphsdc"

# URL for ATC projections
atc_url <- "https://www.fangraphs.com/projections?statgroup=fantasy&type=atc"

# URL for The BAT projections
the_bat_url <- "https://www.fangraphs.com/projections?statgroup=fantasy&type=thebat"

# URL for The BAT X projections
the_batx_url <- "https://www.fangraphs.com/projections?statgroup=fantasy&type=thebatx"

# Scrape the DepthCharts projections
depth_html <- read_html(depth_url)
depth_data <- depth_html %>%
  html_node("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE)
  html_nodes(xpath = '//table[@id="ProjectionBoard1_dg1_ctl00"]') %>%
  html_table(fill = TRUE)

# Scrape the ATC projections
atc_html <- read_html(atc_url)
atc_data <- atc_html %>%
  html_nodes(xpath = '//table[@id="ProjectionBoard1_dg1_ctl00"]') %>%
  html_table(fill = TRUE)

# Scrape the The BAT projections
the_bat_html <- read_html(the_bat_url)
the_bat_data <- the_bat_html %>%
  html_nodes(xpath = '//table[@id="ProjectionBoard1_dg1_ctl00"]') %>%
  html_table(fill = TRUE)

# Scrape the The BAT X projections
the_batx_html <- read_html(the_batx_url)
the_batx_data <- the_batx_html %>%
  html_nodes(xpath = '//table[@id="ProjectionBoard1_dg1_ctl00"]') %>%
  html_table(fill = TRUE)

#### Odds ####

library(oddsapiR)

Sys.getenv("ODDS_API_KEY")
# Sys.setenv(ODDS_API_KEY = "933d79f947f3de884dd9d79e5cae4609")

odds <- toa_sports_odds(sport_key = 'americanfootball_ncaaf', 
                          regions = 'us', 
                          markets = 'spreads', 
                          odds_format = 'american',
                          date_format = 'iso')

my_books <- odds %>% 
  filter(bookmaker_key %in% c('draftkings', 'fanduel', 'williamhill_us', 
                              'pointsbetus', 'betmgm', 'wynnbet', 'betrivers'))

#### College Basketball ####

library(hoopR)
library(toRvik) ##depricated
library(reticulate)
library(cbbdata)

cbbdata::cbd_create_account(username = 'danolen', email = 'dnolen@smu.edu', password = 'P@nyUp2@2@', confirm_password = 'P@nyUp2@2@')
cbbdata::cbd_login(username = 'danolen', password = 'P@nyUp2@2@')

current_kp_ratings <- kp_pomeroy_ratings(min_year = most_recent_mbb_season(), max_year = most_recent_mbb_season())
current_torvik_ratings <- cbd_torvik_ratings_archive()

hasla <- read.csv("Haslametrics.csv") %>% 
  mutate(Home = str_trim(gsub('[[:digit:]]+', '', Home)),
         Away = str_trim(gsub('[[:digit:]]+', '', Away)),
         Htotal = HScore + Ascore)

kp_team_schedule("LSU")

kenpom <- kp_fanmatch('2022-11-10') %>% 
  select(prediction, date) %>% 
  mutate()

torvik <- bart_game_prediction(date = '20221110')
bart_pregame()
tomorrow_games <- bart_season_schedule(2023) %>% 
  filter(date == as.character(Sys.Date()))

torvik <- data.frame()
i <- 1
for (i in 1:nrow(tomorrow_games)) {
  game = bart_game_prediction(team = tomorrow_games$home[i],
                              opp = tomorrow_games$away[i],
                              date = gsub('-','',tomorrow_games$date[i]),
                              location = case_when(tomorrow_games$neutral[i] ~ 'N',
                                                   TRUE ~ 'H'))
  
  torvik <- torvik %>% 
    bind_rows(game)
  
  i <- i + 1
}



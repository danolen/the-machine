library(tidyverse)

#### Baseball ####

library(baseballr)

mlb_roster_types()
mlb_rosters(109, 2019, "2019-06-01", "active")
mlb_teams

baseballr::mlb_probables(game_pk = 663178)

today_pks <- get_game_pks_mlb(Sys.Date())
tomorrow_pks <- get_game_pks_mlb(Sys.Date()+1)

#### Odds ####

library(oddsapiR)

Sys.setenv(ODDS_API_KEY = "933d79f947f3de884dd9d79e5cae4609")

odds <- toa_sports_odds(sport_key = 'americanfootball_ncaaf', 
                          regions = 'us', 
                          markets = 'spreads', 
                          odds_format = 'american',
                          date_format = 'iso')

my_books <- odds %>% 
  filter(bookmaker_key %in% c('draftkings', 'fanduel', 'williamhill_us', 
                              'pointsbetus', 'betmgm', 'wynnbet', 'betrivers'))

#### Soccer ####

library(worldfootballR)

champ_23 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "2nd") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away, Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>% 
  filter(Day != "") %>% 
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         League = "MLS",
         Season = as.character(Season))

#### College Basketball ####

library(hoopR)
library(toRvik)
library(reticulate)

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



library(tidyverse)
library(baseballr)
library(worldfootballR)
library(oddsapiR)

baseballr::mlb_probables(game_pk = 661333)

Sys.setenv(ODDS_API_KEY = "933d79f947f3de884dd9d79e5cae4609")

odds <- toa_sports_odds(sport_key = 'americanfootball_ncaaf', 
                          regions = 'us', 
                          markets = 'spreads', 
                          odds_format = 'american',
                          date_format = 'iso')

my_books <- odds %>% 
  filter(bookmaker_key %in% c('draftkings', 'fanduel', 'williamhill_us', 
                              'pointsbetus', 'betmgm', 'wynnbet', 'betrivers'))

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

epl_results <- understat_league_match_results(league = "EPL", season_start_year = 2020)

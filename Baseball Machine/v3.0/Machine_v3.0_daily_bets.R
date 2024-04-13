### The Machine v3.0
### Daily Bets
library(tidyverse)
library(xtable)
library(RDCOMClient)
setwd("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine")
source("dannyverse/dannyverse.R")

#### Setup ####

## Season start and end date
season = 2024
season_info = baseballr::mlb_seasons_all()
startDate = as.Date(season_info[season_info$season_id==as.character(season),"regular_season_start_date"]$regular_season_start_date)
endDate = as.Date(season_info[season_info$season_id==as.character(season),"regular_season_end_date"]$regular_season_end_date)

## Info for today's games
todays_games <- get_mlb_daily_scores(Sys.Date(), Sys.Date(), file_type = "scores") %>% 
  distinct(game_pk, home_team_name, away_team_name, home_team_season, home_team_league_name,
           home_team_division_name, away_team_league_name, away_team_division_name) %>% 
  inner_join(get_mlb_daily_scores(Sys.Date(), Sys.Date(), file_type = "pks") %>% 
               filter(seriesDescription == 'Regular Season' &
                        scheduledInnings == 9) %>% 
               select(game_pk, officialDate, doubleHeader, gameNumber, dayNight, venue.name)) %>% 
  select(-game_pk) %>% 
  dplyr::mutate(officialDate = as.Date(officialDate))
tmrw_games <- get_mlb_daily_scores(Sys.Date()+1, Sys.Date()+1, file_type = "scores") %>% 
  distinct(game_pk, home_team_name, away_team_name, home_team_season, home_team_league_name,
           home_team_division_name, away_team_league_name, away_team_division_name) %>% 
  inner_join(get_mlb_daily_scores(Sys.Date()+1, Sys.Date()+1, file_type = "pks") %>% 
               filter(seriesDescription == 'Regular Season' &
                        scheduledInnings == 9) %>% 
               select(game_pk, officialDate, doubleHeader, gameNumber, dayNight, venue.name)) %>% 
  select(-game_pk) %>% 
  dplyr::mutate(officialDate = as.Date(officialDate))

## Team names
team_names <- read.csv("Baseball Machine/team_names.csv") %>% 
  left_join(baseballr::teams_lu_table %>%
              filter(sport.name == "Major League Baseball") %>%
              distinct(id, name),
            by = c("Full.Name" = "name"))

## Load model objects
models_dir <- "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/"
full_game_pls <- readRDS(paste0(models_dir,"full_game_pls.rds"))
full_game_rf <- readRDS(paste0(models_dir,"full_game_rf.rds"))
full_game_cub <- readRDS(paste0(models_dir,"full_game_cub.rds"))
full_game_gbm <- readRDS(paste0(models_dir,"full_game_gbm.rds"))
F5_pls <- readRDS(paste0(models_dir,"F5_pls.rds"))
F5_rf <- readRDS(paste0(models_dir,"F5_rf.rds"))
F5_cub <- readRDS(paste0(models_dir,"F5_cub.rds"))
F5_gbm <- readRDS(paste0(models_dir,"F5_gbm.rds"))
F1_pls <- readRDS(paste0(models_dir,"F1_pls.rds"))
F1_rf <- readRDS(paste0(models_dir,"F1_rf.rds"))
F1_cub <- readRDS(paste0(models_dir,"F1_cub.rds"))
F1_gbm <- readRDS(paste0(models_dir,"F1_gbm.rds"))
FG_ML_gbm <- readRDS(paste0(models_dir,"FG_ML_gbm.rds"))
FG_ML_pls <- readRDS(paste0(models_dir,"FG_ML_pls.rds"))
FG_ML_xgb <- readRDS(paste0(models_dir,"FG_ML_xgb.rds"))
FG_Minus_1.5_gbm <- readRDS(paste0(models_dir,"FG_Minus_1.5_gbm.rds"))
FG_Minus_1.5_pls <- readRDS(paste0(models_dir,"FG_Minus_1.5_pls.rds"))
FG_Minus_1.5_xgb <- readRDS(paste0(models_dir,"FG_Minus_1.5_xgb.rds"))
FG_Minus_2.5_gbm <- readRDS(paste0(models_dir,"FG_Minus_2.5_gbm.rds"))
FG_Minus_2.5_pls <- readRDS(paste0(models_dir,"FG_Minus_2.5_pls.rds"))
FG_Minus_2.5_xgb <- readRDS(paste0(models_dir,"FG_Minus_2.5_xgb.rds"))
FG_Plus_1.5_gbm <- readRDS(paste0(models_dir,"FG_Plus_1.5_gbm.rds"))
FG_Plus_1.5_pls <- readRDS(paste0(models_dir,"FG_Plus_1.5_pls.rds"))
FG_Plus_1.5_xgb <- readRDS(paste0(models_dir,"FG_Plus_1.5_xgb.rds"))
FG_Plus_2.5_gbm <- readRDS(paste0(models_dir,"FG_Plus_2.5_gbm.rds"))
FG_Plus_2.5_pls <- readRDS(paste0(models_dir,"FG_Plus_2.5_pls.rds"))
FG_Plus_2.5_xgb <- readRDS(paste0(models_dir,"FG_Plus_2.5_xgb.rds"))
FG_Total_6.5_gbm <- readRDS(paste0(models_dir,"FG_Total_6.5_gbm.rds"))
FG_Total_6.5_pls <- readRDS(paste0(models_dir,"FG_Total_6.5_pls.rds"))
FG_Total_6.5_xgb <- readRDS(paste0(models_dir,"FG_Total_6.5_xgb.rds"))
FG_Total_7_gbm <- readRDS(paste0(models_dir,"FG_Total_7_gbm.rds"))
FG_Total_7_pls <- readRDS(paste0(models_dir,"FG_Total_7_pls.rds"))
FG_Total_7_xgb <- readRDS(paste0(models_dir,"FG_Total_7_xgb.rds"))
FG_Total_7.5_gbm <- readRDS(paste0(models_dir,"FG_Total_7.5_gbm.rds"))
FG_Total_7.5_pls <- readRDS(paste0(models_dir,"FG_Total_7.5_pls.rds"))
FG_Total_7.5_xgb <- readRDS(paste0(models_dir,"FG_Total_7.5_xgb.rds"))
FG_Total_8_gbm <- readRDS(paste0(models_dir,"FG_Total_8_gbm.rds"))
FG_Total_8_pls <- readRDS(paste0(models_dir,"FG_Total_8_pls.rds"))
FG_Total_8_xgb <- readRDS(paste0(models_dir,"FG_Total_8_xgb.rds"))
FG_Total_8.5_gbm <- readRDS(paste0(models_dir,"FG_Total_8.5_gbm.rds"))
FG_Total_8.5_pls <- readRDS(paste0(models_dir,"FG_Total_8.5_pls.rds"))
FG_Total_8.5_xgb <- readRDS(paste0(models_dir,"FG_Total_8.5_xgb.rds"))
FG_Total_9_gbm <- readRDS(paste0(models_dir,"FG_Total_9_gbm.rds"))
FG_Total_9_pls <- readRDS(paste0(models_dir,"FG_Total_9_pls.rds"))
FG_Total_9_xgb <- readRDS(paste0(models_dir,"FG_Total_9_xgb.rds"))
FG_Total_9.5_gbm <- readRDS(paste0(models_dir,"FG_Total_9.5_gbm.rds"))
FG_Total_9.5_pls <- readRDS(paste0(models_dir,"FG_Total_9.5_pls.rds"))
FG_Total_9.5_xgb <- readRDS(paste0(models_dir,"FG_Total_9.5_xgb.rds"))
FG_Total_10_gbm <- readRDS(paste0(models_dir,"FG_Total_10_gbm.rds"))
FG_Total_10_pls <- readRDS(paste0(models_dir,"FG_Total_10_pls.rds"))
FG_Total_10_xgb <- readRDS(paste0(models_dir,"FG_Total_10_xgb.rds"))
FG_Total_10.5_gbm <- readRDS(paste0(models_dir,"FG_Total_10.5_gbm.rds"))
FG_Total_10.5_pls <- readRDS(paste0(models_dir,"FG_Total_10.5_pls.rds"))
FG_Total_10.5_xgb <- readRDS(paste0(models_dir,"FG_Total_10.5_xgb.rds"))
FG_Total_11_gbm <- readRDS(paste0(models_dir,"FG_Total_11_gbm.rds"))
FG_Total_11_pls <- readRDS(paste0(models_dir,"FG_Total_11_pls.rds"))
FG_Total_11_xgb <- readRDS(paste0(models_dir,"FG_Total_11_xgb.rds"))
FG_Total_11.5_gbm <- readRDS(paste0(models_dir,"FG_Total_11.5_gbm.rds"))
FG_Total_11.5_pls <- readRDS(paste0(models_dir,"FG_Total_11.5_pls.rds"))
FG_Total_11.5_xgb <- readRDS(paste0(models_dir,"FG_Total_11.5_xgb.rds"))
FG_TT_2.5_gbm <- readRDS(paste0(models_dir,"FG_TT_2.5_gbm.rds"))
FG_TT_2.5_pls <- readRDS(paste0(models_dir,"FG_TT_2.5_pls.rds"))
FG_TT_2.5_xgb <- readRDS(paste0(models_dir,"FG_TT_2.5_xgb.rds"))
FG_TT_3_gbm <- readRDS(paste0(models_dir,"FG_TT_3_gbm.rds"))
FG_TT_3_pls <- readRDS(paste0(models_dir,"FG_TT_3_pls.rds"))
FG_TT_3_xgb <- readRDS(paste0(models_dir,"FG_TT_3_xgb.rds"))
FG_TT_3.5_gbm <- readRDS(paste0(models_dir,"FG_TT_3.5_gbm.rds"))
FG_TT_3.5_pls <- readRDS(paste0(models_dir,"FG_TT_3.5_pls.rds"))
FG_TT_3.5_xgb <- readRDS(paste0(models_dir,"FG_TT_3.5_xgb.rds"))
FG_TT_4_gbm <- readRDS(paste0(models_dir,"FG_TT_4_gbm.rds"))
FG_TT_4_pls <- readRDS(paste0(models_dir,"FG_TT_4_pls.rds"))
FG_TT_4_xgb <- readRDS(paste0(models_dir,"FG_TT_4_xgb.rds"))
FG_TT_4.5_gbm <- readRDS(paste0(models_dir,"FG_TT_4.5_gbm.rds"))
FG_TT_4.5_pls <- readRDS(paste0(models_dir,"FG_TT_4.5_pls.rds"))
FG_TT_4.5_xgb <- readRDS(paste0(models_dir,"FG_TT_4.5_xgb.rds"))
FG_TT_5_gbm <- readRDS(paste0(models_dir,"FG_TT_5_gbm.rds"))
FG_TT_5_pls <- readRDS(paste0(models_dir,"FG_TT_5_pls.rds"))
FG_TT_5_xgb <- readRDS(paste0(models_dir,"FG_TT_5_xgb.rds"))
FG_TT_5.5_gbm <- readRDS(paste0(models_dir,"FG_TT_5.5_gbm.rds"))
FG_TT_5.5_pls <- readRDS(paste0(models_dir,"FG_TT_5.5_pls.rds"))
FG_TT_5.5_xgb <- readRDS(paste0(models_dir,"FG_TT_5.5_xgb.rds"))
F5_ML_gbm <- readRDS(paste0(models_dir,"F5_ML_gbm.rds"))
F5_ML_pls <- readRDS(paste0(models_dir,"F5_ML_pls.rds"))
F5_ML_xgb <- readRDS(paste0(models_dir,"F5_ML_xgb.rds"))
F5_Minus_1.5_gbm <- readRDS(paste0(models_dir,"F5_Minus_1.5_gbm.rds"))
F5_Minus_1.5_pls <- readRDS(paste0(models_dir,"F5_Minus_1.5_pls.rds"))
F5_Minus_1.5_xgb <- readRDS(paste0(models_dir,"F5_Minus_1.5_xgb.rds"))
F5_Minus_0.5_gbm <- readRDS(paste0(models_dir,"F5_Minus_0.5_gbm.rds"))
F5_Minus_0.5_pls <- readRDS(paste0(models_dir,"F5_Minus_0.5_pls.rds"))
F5_Minus_0.5_xgb <- readRDS(paste0(models_dir,"F5_Minus_0.5_xgb.rds"))
F5_Plus_1.5_gbm <- readRDS(paste0(models_dir,"F5_Plus_1.5_gbm.rds"))
F5_Plus_1.5_pls <- readRDS(paste0(models_dir,"F5_Plus_1.5_pls.rds"))
F5_Plus_1.5_xgb <- readRDS(paste0(models_dir,"F5_Plus_1.5_xgb.rds"))
F5_Plus_0.5_gbm <- readRDS(paste0(models_dir,"F5_Plus_0.5_gbm.rds"))
F5_Plus_0.5_pls <- readRDS(paste0(models_dir,"F5_Plus_0.5_pls.rds"))
F5_Plus_0.5_xgb <- readRDS(paste0(models_dir,"F5_Plus_0.5_xgb.rds"))
F5_Total_3.5_gbm <- readRDS(paste0(models_dir,"F5_Total_3.5_gbm.rds"))
F5_Total_3.5_pls <- readRDS(paste0(models_dir,"F5_Total_3.5_pls.rds"))
F5_Total_3.5_xgb <- readRDS(paste0(models_dir,"F5_Total_3.5_xgb.rds"))
F5_Total_4_gbm <- readRDS(paste0(models_dir,"F5_Total_4_gbm.rds"))
F5_Total_4_pls <- readRDS(paste0(models_dir,"F5_Total_4_pls.rds"))
F5_Total_4_xgb <- readRDS(paste0(models_dir,"F5_Total_4_xgb.rds"))
F5_Total_4.5_gbm <- readRDS(paste0(models_dir,"F5_Total_4.5_gbm.rds"))
F5_Total_4.5_pls <- readRDS(paste0(models_dir,"F5_Total_4.5_pls.rds"))
F5_Total_4.5_xgb <- readRDS(paste0(models_dir,"F5_Total_4.5_xgb.rds"))
F5_Total_5_gbm <- readRDS(paste0(models_dir,"F5_Total_5_gbm.rds"))
F5_Total_5_pls <- readRDS(paste0(models_dir,"F5_Total_5_pls.rds"))
F5_Total_5_xgb <- readRDS(paste0(models_dir,"F5_Total_5_xgb.rds"))
F5_Total_5.5_gbm <- readRDS(paste0(models_dir,"F5_Total_5.5_gbm.rds"))
F5_Total_5.5_pls <- readRDS(paste0(models_dir,"F5_Total_5.5_pls.rds"))
F5_Total_5.5_xgb <- readRDS(paste0(models_dir,"F5_Total_5.5_xgb.rds"))
F5_TT_0.5_gbm <- readRDS(paste0(models_dir,"F5_TT_0.5_gbm.rds"))
F5_TT_0.5_pls <- readRDS(paste0(models_dir,"F5_TT_0.5_pls.rds"))
F5_TT_0.5_xgb <- readRDS(paste0(models_dir,"F5_TT_0.5_xgb.rds"))
F5_TT_1_gbm <- readRDS(paste0(models_dir,"F5_TT_1_gbm.rds"))
F5_TT_1_pls <- readRDS(paste0(models_dir,"F5_TT_1_pls.rds"))
F5_TT_1_xgb <- readRDS(paste0(models_dir,"F5_TT_1_xgb.rds"))
F5_TT_1.5_gbm <- readRDS(paste0(models_dir,"F5_TT_1.5_gbm.rds"))
F5_TT_1.5_pls <- readRDS(paste0(models_dir,"F5_TT_1.5_pls.rds"))
F5_TT_1.5_xgb <- readRDS(paste0(models_dir,"F5_TT_1.5_xgb.rds"))
F5_TT_2_gbm <- readRDS(paste0(models_dir,"F5_TT_2_gbm.rds"))
F5_TT_2_pls <- readRDS(paste0(models_dir,"F5_TT_2_pls.rds"))
F5_TT_2_xgb <- readRDS(paste0(models_dir,"F5_TT_2_xgb.rds"))
F5_TT_2.5_gbm <- readRDS(paste0(models_dir,"F5_TT_2.5_gbm.rds"))
F5_TT_2.5_pls <- readRDS(paste0(models_dir,"F5_TT_2.5_pls.rds"))
F5_TT_2.5_xgb <- readRDS(paste0(models_dir,"F5_TT_2.5_xgb.rds"))
F5_TT_3_gbm <- readRDS(paste0(models_dir,"F5_TT_3_gbm.rds"))
F5_TT_3_pls <- readRDS(paste0(models_dir,"F5_TT_3_pls.rds"))
F5_TT_3_xgb <- readRDS(paste0(models_dir,"F5_TT_3_xgb.rds"))
F5_TT_3.5_gbm <- readRDS(paste0(models_dir,"F5_TT_3.5_gbm.rds"))
F5_TT_3.5_pls <- readRDS(paste0(models_dir,"F5_TT_3.5_pls.rds"))
F5_TT_3.5_xgb <- readRDS(paste0(models_dir,"F5_TT_3.5_xgb.rds"))
F1_Total_0.5_gbm <- readRDS(paste0(models_dir,"F1_Total_0.5_gbm.rds"))
F1_Total_0.5_pls <- readRDS(paste0(models_dir,"F1_Total_0.5_pls.rds"))
F1_Total_0.5_xgb <- readRDS(paste0(models_dir,"F1_Total_0.5_xgb.rds"))
F1_TT_0.5_gbm <- readRDS(paste0(models_dir,"F1_TT_0.5_gbm.rds"))
F1_TT_0.5_pls <- readRDS(paste0(models_dir,"F1_TT_0.5_pls.rds"))
F1_TT_0.5_xgb <- readRDS(paste0(models_dir,"F1_TT_0.5_xgb.rds"))

## Daily stats
pitchers_s2d <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/pitchers_s2d_",season,".csv"))
team_batting_L7 <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_batting_L7_",season,".csv"))
team_batting_L14 <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_batting_L14_",season,".csv"))
team_batting_L30 <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_batting_L30_",season,".csv"))
team_batting_s2d <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_batting_s2d_",season,".csv"))
team_bullpen_L7 <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L7_",season,".csv"))
team_bullpen_L14 <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L14_",season,".csv"))
team_bullpen_L30 <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L30_",season,".csv"))
team_bullpen_s2d <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_s2d_",season,".csv"))
pitchers_s2d_startDate <- max(as.Date(pitchers_s2d$Date)) + 1
team_batting_L7_startDate <- max(as.Date(team_batting_L7$Date)) + 1
team_batting_L14_startDate <- max(as.Date(team_batting_L14$Date)) + 1
team_batting_L30_startDate <- max(as.Date(team_batting_L30$Date)) + 1
team_batting_s2d_startDate <- max(as.Date(team_batting_s2d$Date)) + 1
team_bullpen_L7_startDate <- max(as.Date(team_bullpen_L7$Date)) + 1
team_bullpen_L14_startDate <- max(as.Date(team_bullpen_L14$Date)) + 1
team_bullpen_L30_startDate <- max(as.Date(team_bullpen_L30$Date)) + 1
team_bullpen_s2d_startDate <- max(as.Date(team_bullpen_s2d$Date)) + 1
pitchers_s2d_today <- scrape_fangraphs(position = "starting pitchers",
                                       season = season,
                                       start_date = pitchers_s2d_startDate,
                                       end_date = Sys.Date(),
                                       time_frame = "s2d")
team_batting_L7_today <- scrape_fangraphs(position = "team batting",
                                          season = season,
                                          start_date = team_batting_L7_startDate,
                                          end_date = Sys.Date(),
                                          time_frame = "L7")
team_batting_L14_today <- scrape_fangraphs(position = "team batting",
                                           season = season,
                                           start_date = team_batting_L14_startDate,
                                           end_date = Sys.Date(),
                                           time_frame = "L14")
team_batting_L30_today <- scrape_fangraphs(position = "team batting",
                                           season = season,
                                           start_date = team_batting_L30_startDate,
                                           end_date = Sys.Date(),
                                           time_frame = "L30")
team_batting_s2d_today <- scrape_fangraphs(position = "team batting",
                                           season = season,
                                           start_date = team_batting_s2d_startDate,
                                           end_date = Sys.Date(),
                                           time_frame = "s2d")
team_bullpen_L7_today <- scrape_fangraphs(position = "team bullpen",
                                          season = season,
                                          start_date = team_bullpen_L7_startDate,
                                          end_date = Sys.Date(),
                                          time_frame = "L7")
team_bullpen_L14_today <- scrape_fangraphs(position = "team bullpen",
                                           season = season,
                                           start_date = team_bullpen_L14_startDate,
                                           end_date = Sys.Date(),
                                           time_frame = "L14")
team_bullpen_L30_today <- scrape_fangraphs(position = "team bullpen",
                                           season = season,
                                           start_date = team_bullpen_L30_startDate,
                                           end_date = Sys.Date(),
                                           time_frame = "L30")
team_bullpen_s2d_today <- scrape_fangraphs(position = "team bullpen",
                                           season = season,
                                           start_date = team_bullpen_s2d_startDate,
                                           end_date = Sys.Date(),
                                           time_frame = "s2d")

## Scores
game_pks <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/game_pks_",season,".csv"))
game_pks_max_date <- max(as.Date(game_pks$officialDate))
pks <- get_mlb_daily_scores(start_date = game_pks_max_date + 1,
                            end_date = Sys.Date()-1,
                            file_type = "pks")
game_scores <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/game_scores_",season,".csv"))
game_scores_max_date <- max(as.Date(game_pks$officialDate))
scores <- get_mlb_daily_scores(start_date = game_pks_max_date + 1,
                            end_date = Sys.Date()-1,
                            file_type = "scores")

## Starting pitchers and rosters for today
starting_pitchers <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/starting_pitchers_",season,".csv")) %>% 
  filter(game_date < Sys.Date())
starting_pitchers_max_date <- max(as.Date(starting_pitchers$game_date))
probables <- get_probable_pitchers(starting_pitchers_max_date+1, Sys.Date())
tmrw_probables <- get_probable_pitchers(Sys.Date()+1, Sys.Date()+1)
daily_rosters <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/daily_rosters_",season,".csv"))
daily_rosters_max_date <- max(as.Date(daily_rosters$date))
rosters <- get_daily_rosters(daily_rosters_max_date+1, Sys.Date())
tmrw_rosters <- get_daily_rosters(Sys.Date()+1, Sys.Date()+1)

## Bovada Odds
bovada_odds <- get_bovada_odds("baseball") #%>% 
  # dplyr::mutate(AwayStartingPitcher = case_when(str_detect(AwayStartingPitcher, "Shohei Ohtani") ~ "Shohei Ohtani",
  #                                        str_detect(AwayStartingPitcher, "Nestor Cortes") ~ "Nestor Cortes",
  #                                        TRUE ~ AwayStartingPitcher),
  #        HomeStartingPitcher = case_when(str_detect(HomeStartingPitcher, "Shohei Ohtani") ~ "Shohei Ohtani",
  #                                        str_detect(HomeStartingPitcher, "Nestor Cortes") ~ "Nestor Cortes",
  #                                        TRUE ~ HomeStartingPitcher))

## Overwrite files with updates
pitchers_s2d_update <- pitchers_s2d %>% 
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(pitchers_s2d_today)
team_batting_L7_update <- team_batting_L7 %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_batting_L7_today)
team_batting_L14_update <- team_batting_L14 %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_batting_L14_today)
team_batting_L30_update <- team_batting_L30 %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_batting_L30_today)
team_batting_s2d_update <- team_batting_s2d %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_batting_s2d_today)
team_bullpen_L7_update <- team_bullpen_L7 %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_L7_today)
team_bullpen_L14_update <- team_bullpen_L14 %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_L14_today)
team_bullpen_L30_update <- team_bullpen_L30 %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_L30_today)
team_bullpen_s2d_update <- team_bullpen_s2d %>%  
  dplyr::mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_s2d_today)

pks_update <- game_pks %>% 
  dplyr::mutate(season = as.character(season),
         seasonDisplay = as.character(seasonDisplay),
         status.codedGameState = as.character(status.codedGameState),
         status.statusCode = as.character(status.statusCode),
         status.abstractGameCode = as.character(status.abstractGameCode),
         teams.away.leagueRecord.pct = as.character(teams.away.leagueRecord.pct),
         teams.home.leagueRecord.pct = as.character(teams.home.leagueRecord.pct)) %>% 
  bind_rows(pks) %>% 
  distinct()
scores_update <- game_scores %>% 
  dplyr::mutate(home_team_id = as.character(home_team_id),
         away_team_id = as.character(away_team_id),
         home_team_season = as.character(home_team_season),
         home_team_venue_id = as.character(home_team_venue_id),
         home_team_first_year_of_play = as.character(home_team_first_year_of_play),
         home_team_league_id = as.character(home_team_league_id),
         home_team_division_id = as.character(home_team_division_id),
         home_team_sport_id = as.character(home_team_sport_id),
         home_team_record_games_played = as.character(home_team_record_games_played),
         home_team_record_league_record_wins = as.character(home_team_record_league_record_wins),
         home_team_record_league_record_losses = as.character(home_team_record_league_record_losses),
         home_team_record_league_record_ties = as.character(home_team_record_league_record_ties),
         home_team_record_league_record_pct = as.character(home_team_record_league_record_pct),
         home_team_record_division_leader = as.character(home_team_record_division_leader),
         home_team_record_wins = as.character(home_team_record_wins),
         home_team_record_losses = as.character(home_team_record_losses),
         home_team_record_winning_percentage = as.character(home_team_record_winning_percentage),
         home_team_active = as.character(home_team_active),
         away_team_season = as.character(away_team_season),
         away_team_venue_id = as.character(away_team_venue_id),
         away_team_first_year_of_play = as.character(away_team_first_year_of_play),
         away_team_league_id = as.character(away_team_league_id),
         away_team_division_id = as.character(away_team_division_id),
         away_team_sport_id = as.character(away_team_sport_id),
         away_team_record_games_played = as.character(away_team_record_games_played),
         away_team_record_league_record_wins = as.character(away_team_record_league_record_wins),
         away_team_record_league_record_losses = as.character(away_team_record_league_record_losses),
         away_team_record_league_record_ties = as.character(away_team_record_league_record_ties),
         away_team_record_league_record_pct = as.character(away_team_record_league_record_pct),
         away_team_record_division_leader = as.character(away_team_record_division_leader),
         away_team_record_wins = as.character(away_team_record_wins),
         away_team_record_losses = as.character(away_team_record_losses),
         away_team_record_winning_percentage = as.character(away_team_record_winning_percentage),
         away_team_active = as.character(away_team_active)) %>% 
  bind_rows(scores %>% 
              filter(!game_pk %in% unique(game_scores$game_pk)))

probables_update <- starting_pitchers %>% 
  union(probables)
rosters_update <- daily_rosters %>% 
  union(rosters)

#### Data Prep ####
results <- scores_update %>% 
  distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
           home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
           home_team_division_name, away_team_league_name, away_team_division_name) %>%
  inner_join(pks_update %>% 
               filter(status.detailedState %in% c('Final', 'Completed Early') &
                        # is.na(resumeDate) &
                        # is.na(resumedFrom) &
                        seriesDescription == 'Regular Season' &
                        scheduledInnings == 9) %>% 
               select(game_pk, officialDate, doubleHeader, gameNumber, dayNight, scheduledInnings, venue.name)) %>% 
  filter(!is.na(num)) %>% 
  replace(is.na(.),0) %>% 
  group_by(game_pk) %>% 
  dplyr::mutate(home_runs = cumsum(home_runs),
         home_hits = cumsum(home_hits),
         home_errors = cumsum(home_errors),
         away_runs = cumsum(away_runs),
         away_hits = cumsum(away_hits),
         away_errors = cumsum(away_errors)) %>% 
  filter(num <= 9) %>%
  select(-num) %>%  
  pivot_wider(names_from = ordinal_num, values_from = c(home_runs, home_hits, home_errors, away_runs, away_hits, away_errors)) %>% 
  left_join(scores_update %>% 
              distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
                       home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
                       home_team_division_name, away_team_league_name, away_team_division_name) %>%
              filter(!is.na(num)) %>% 
              replace(is.na(.),0) %>% 
              group_by(game_pk) %>% 
              dplyr::summarise(home_runs_final = sum(home_runs),
                        home_hits_final = sum(home_hits),
                        home_errors_final = sum(home_errors),
                        away_runs_final = sum(away_runs),
                        away_hits_final = sum(away_hits),
                        away_errors_final = sum(away_errors)))

team_batting_L7_today$Date <- as.Date(team_batting_L7_today$Date)
team_batting_L14_today$Date <- as.Date(team_batting_L14_today$Date)
team_batting_L30_today$Date <- as.Date(team_batting_L30_today$Date)
team_batting_s2d_today$Date <- as.Date(team_batting_s2d_today$Date)

daily_team_batting <- team_batting_L7_today %>% 
  select(-contains('url')) %>% 
  full_join(team_batting_L14_today %>% 
              select(-contains('url')), by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_today %>% 
              select(-contains('url')), by = c("Team", "Date"))
colnames(daily_team_batting)[57:83] <- paste0(colnames(daily_team_batting)[57:83],'_L30')
daily_team_batting <- daily_team_batting %>% 
  full_join(team_batting_s2d_today %>% 
              select(-contains('url')), by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name22, BR),
            by = c("Team" = "BR")) %>% 
  dplyr::mutate(Team = Full.Name22) %>% 
  select(-Full.Name22)

team_bullpen_L7_today$Date <- as.Date(team_bullpen_L7_today$Date)
team_bullpen_L14_today$Date <- as.Date(team_bullpen_L14_today$Date)
team_bullpen_L30_today$Date <- as.Date(team_bullpen_L30_today$Date)
team_bullpen_s2d_today$Date <- as.Date(team_bullpen_s2d_today$Date)

daily_team_bullpen <- team_bullpen_L7_today %>% 
  select(-contains('url')) %>% 
  full_join(team_bullpen_L14_today %>% 
              select(-contains('url')), by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_today %>% 
              select(-contains('url')), by = c("Team", "Date"))
colnames(daily_team_bullpen)[51:74] <- paste0(colnames(daily_team_bullpen)[51:74],'_L30')
daily_team_bullpen <- daily_team_bullpen %>% 
  full_join(team_bullpen_s2d_today %>% 
              select(-contains('url')), by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name22, BR),
            by = c("Team" = "BR")) %>% 
  dplyr::mutate(Team = Full.Name22) %>% 
  select(-Full.Name22)

pitchers_s2d_today$Date <- as.Date(pitchers_s2d_today$Date)
dupe_SPs_today <- pitchers_s2d_today %>% 
  filter(Date == max(pitchers_s2d_today$Date)) %>% 
  group_by(Name) %>% 
  dplyr::summarise(records = n()) %>% 
  filter(records > 1) %>% 
  left_join(pitchers_s2d_today %>% distinct(Name, Team))
dupe_SPs_today$Name
daily_pitchers <- pitchers_s2d_today %>% 
  select(-contains('url')) %>% 
  dplyr::mutate(Name = case_when(Name == "Luis Garcia" & Team == "HOU" ~ "Luis Garcia (HOU)",
                          # Name == "Luis Castillo" & Team == "DET" ~ "Luis Castillo (DET)",
                          Name == "Luis Ortiz" & Team == "PIT" ~ "Luis Ortiz (PIT)",
                          Name == "Hyun-Jin Ryu" ~ "Hyun Jin Ryu",
                          TRUE ~ Name))

PECOTA_pitching_24 <- readxl::read_xlsx(paste0("Baseball Machine/PECOTA/",season,"/pecota2024_pitching_mar23.xlsx"), sheet = "50") %>% 
  select(mlbid, name, ip, warp) %>% 
  dplyr::mutate(WARP200 = (warp/ip)*200)
PECOTA_hitting_24 <- readxl::read_xlsx(paste0("Baseball Machine/PECOTA/",season,"/pecota2024_hitting_mar23.xlsx"), sheet = "50") %>% 
  select(mlbid, name, pa, warp) %>% 
  dplyr::mutate(WARP600 = (as.numeric(warp)/as.numeric(pa))*600)

upcoming_games_today <- bovada_odds %>% 
  filter(gamedate == Sys.Date()) %>% 
  left_join(todays_games %>% 
              bind_rows(tmrw_games),
            by = c("AwayTeam" = "away_team_name",
                   "HomeTeam" = "home_team_name",
                   "gamedate" = "officialDate")) %>% 
  left_join(probables %>% 
              bind_rows(tmrw_probables) %>% 
              select(game_date, fullName, id, team) %>% 
              dplyr::mutate(game_date = as.Date(game_date)),
            by = c("AwayTeam" = "team", "gamedate" = "game_date")) %>% 
  left_join(probables %>% 
              bind_rows(tmrw_probables) %>% 
              select(game_date, fullName, id, team) %>%
              dplyr::mutate(game_date = as.Date(game_date)),
          by = c("HomeTeam" = "team", "gamedate" = "game_date"),
          suffix = c("_AwaySP", "_HomeSP")) %>% 
  left_join(PECOTA_pitching_24 %>% 
              select(mlbid, WARP200),
            by = c("id_AwaySP" = "mlbid")) %>% 
  left_join(PECOTA_pitching_24 %>% 
            select(mlbid, WARP200),
          by = c("id_HomeSP" = "mlbid"),
          suffix = c("_AwaySP", "_HomeSP")) %>% 
  left_join(rosters_update %>% 
              bind_rows(tmrw_rosters) %>% 
              filter(position_type != "Pitcher") %>% 
              left_join(PECOTA_hitting_24 %>% 
                          select(mlbid, WARP600),
                        by = c("person_id" = "mlbid")) %>% 
              group_by(team_id, date) %>% 
              dplyr::summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
              left_join(team_names %>% 
                          select(Full.Name22, id),
                        by = c("team_id" = "id")) %>% 
              ungroup() %>% 
              select(-team_id) %>% 
              dplyr::mutate(date = as.Date(date)),
            by = c("gamedate" = "date",
                   "HomeTeam" = "Full.Name22")) %>% 
  left_join(rosters_update %>% 
              bind_rows(tmrw_rosters) %>% 
              filter(position_type != "Pitcher") %>% 
              left_join(PECOTA_hitting_24 %>% 
                          select(mlbid, WARP600),
                        by = c("person_id" = "mlbid")) %>% 
              group_by(team_id, date) %>% 
              dplyr::summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
              left_join(team_names %>% 
                          select(Full.Name22, id),
                        by = c("team_id" = "id")) %>% 
              ungroup() %>% 
              select(-team_id) %>% 
              dplyr::mutate(date = as.Date(date)),
            by = c("gamedate" = "date",
                   "AwayTeam" = "Full.Name22"),
            suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  filter(!is.na(fullName_HomeSP) & !is.na(fullName_AwaySP)) %>% 
  left_join(daily_pitchers, by = c("gamedate" = "Date", "fullName_HomeSP" = "Name")) %>% 
  left_join(daily_pitchers, by = c("gamedate" = "Date", "fullName_AwaySP" = "Name"), suffix = c("_HomeSP", "_AwaySP")) %>% 
  left_join(daily_team_batting, by = c("gamedate" = "Date", "HomeTeam" = "Team")) %>% 
  left_join(daily_team_batting, by = c("gamedate" = "Date", "AwayTeam" = "Team"), suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  left_join(daily_team_bullpen, by = c("gamedate" = "Date", "HomeTeam" = "Team")) %>% 
  left_join(daily_team_bullpen, by = c("gamedate" = "Date", "AwayTeam" = "Team"), suffix = c("_HomeBullpen", "_AwayBullpen")) %>% 
  dplyr::rename(AwaySP_fullName = fullName_AwaySP,
                HomeSP_fullName = fullName_HomeSP) %>% 
  dplyr::mutate(WARP200_HomeSP = case_when(!is.nan(WARP200_HomeSP) ~ WARP200_HomeSP),
         WARP200_AwaySP = case_when(!is.nan(WARP200_AwaySP) ~ WARP200_AwaySP)) %>% 
  dplyr::mutate(IP_AwaySP = as.integer(IP_AwaySP) + (IP_AwaySP %% 1 * 3.33),
         Start.IP_AwaySP = as.integer(Start.IP_AwaySP) + (Start.IP_AwaySP %% 1 * 3.33),
         Relief.IP_AwaySP = as.integer(Relief.IP_AwaySP) + (Relief.IP_AwaySP %% 1 * 3.33),
         IP_HomeSP = as.integer(IP_HomeSP) + (IP_HomeSP %% 1 * 3.33),
         Start.IP_HomeSP = as.integer(Start.IP_HomeSP) + (Start.IP_HomeSP %% 1 * 3.33),
         Relief.IP_HomeSP = as.integer(Relief.IP_HomeSP) + (Relief.IP_HomeSP %% 1 * 3.33),
         IP_L7_AwayBullpen = as.integer(IP_L7_AwayBullpen) + (IP_L7_AwayBullpen %% 1 * 3.33),
         IP_L14_AwayBullpen = as.integer(IP_L14_AwayBullpen) + (IP_L14_AwayBullpen %% 1 * 3.33),
         IP_L30_AwayBullpen = as.integer(IP_L30_AwayBullpen) + (IP_L30_AwayBullpen %% 1 * 3.33),
         IP_AwayBullpen = as.integer(IP_AwayBullpen) + (IP_AwayBullpen %% 1 * 3.33),
         IP_L7_HomeBullpen = as.integer(IP_L7_HomeBullpen) + (IP_L7_HomeBullpen %% 1 * 3.33),
         IP_L14_HomeBullpen = as.integer(IP_L14_HomeBullpen) + (IP_L14_HomeBullpen %% 1 * 3.33),
         IP_L30_HomeBullpen = as.integer(IP_L30_HomeBullpen) + (IP_L30_HomeBullpen %% 1 * 3.33),
         IP_HomeBullpen = as.integer(IP_HomeBullpen) + (IP_HomeBullpen %% 1 * 3.33)) %>% 
  dplyr::mutate(WAR200_AwaySP = (WAR_AwaySP / IP_AwaySP) * 200,
         WAR200_HomeSP = (WAR_HomeSP / IP_HomeSP) * 200,
         HR_L7_AwayBatters = (HR_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         SB_L7_AwayBatters = (SB_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         BsR_L7_AwayBatters = (BsR_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         Off_L7_AwayBatters = (Off_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         Def_L7_AwayBatters = (Def_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         WAR_L7_AwayBatters = (WAR_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         HR_L14_AwayBatters = (HR_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         SB_L14_AwayBatters = (SB_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         BsR_L14_AwayBatters = (BsR_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         Off_L14_AwayBatters = (Off_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         Def_L14_AwayBatters = (Def_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         WAR_L14_AwayBatters = (WAR_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         HR_L30_AwayBatters = (HR_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         SB_L30_AwayBatters = (SB_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         BsR_L30_AwayBatters = (BsR_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         Off_L30_AwayBatters = (Off_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         Def_L30_AwayBatters = (Def_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         WAR_L30_AwayBatters = (WAR_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         HR_AwayBatters = (HR_AwayBatters / PA_AwayBatters) * 500,
         SB_AwayBatters = (SB_AwayBatters / PA_AwayBatters) * 500,
         BsR_AwayBatters = (BsR_AwayBatters / PA_AwayBatters) * 500,
         Off_AwayBatters = (Off_AwayBatters / PA_AwayBatters) * 500,
         Def_AwayBatters = (Def_AwayBatters / PA_AwayBatters) * 500,
         WAR_AwayBatters = (WAR_AwayBatters / PA_AwayBatters) * 500,
         HR_L7_HomeBatters = (HR_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         SB_L7_HomeBatters = (SB_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         BsR_L7_HomeBatters = (BsR_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         Off_L7_HomeBatters = (Off_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         Def_L7_HomeBatters = (Def_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         WAR_L7_HomeBatters = (WAR_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         HR_L14_HomeBatters = (HR_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         SB_L14_HomeBatters = (SB_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         BsR_L14_HomeBatters = (BsR_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         Off_L14_HomeBatters = (Off_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         Def_L14_HomeBatters = (Def_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         WAR_L14_HomeBatters = (WAR_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         HR_L30_HomeBatters = (HR_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         SB_L30_HomeBatters = (SB_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         BsR_L30_HomeBatters = (BsR_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         Off_L30_HomeBatters = (Off_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         Def_L30_HomeBatters = (Def_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         WAR_L30_HomeBatters = (WAR_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         HR_HomeBatters = (HR_HomeBatters / PA_HomeBatters) * 500,
         SB_HomeBatters = (SB_HomeBatters / PA_HomeBatters) * 500,
         BsR_HomeBatters = (BsR_HomeBatters / PA_HomeBatters) * 500,
         Off_HomeBatters = (Off_HomeBatters/ PA_HomeBatters) * 500,
         Def_HomeBatters = (Def_HomeBatters / PA_HomeBatters) * 500,
         WAR_HomeBatters = (WAR_HomeBatters / PA_HomeBatters) * 500,
         WAR_L7_AwayBullpen = (WAR_L7_AwayBullpen / IP_L7_AwayBullpen) * 200,
         WAR_L14_AwayBullpen = (WAR_L14_AwayBullpen / IP_L14_AwayBullpen) * 200,
         WAR_L30_AwayBullpen = (WAR_L30_AwayBullpen / IP_L30_AwayBullpen) * 200,
         WAR_AwayBullpen = (WAR_AwayBullpen / IP_AwayBullpen) * 200,
         WAR_L7_HomeBullpen = (WAR_L7_HomeBullpen / IP_L7_HomeBullpen) * 200,
         WAR_L14_HomeBullpen = (WAR_L14_HomeBullpen / IP_L14_HomeBullpen) * 200,
         WAR_L30_HomeBullpen = (WAR_L30_HomeBullpen / IP_L30_HomeBullpen) * 200,
         WAR_HomeBullpen = (WAR_HomeBullpen / IP_HomeBullpen) * 200,
         IPperStart_AwaySP = Start.IP_AwaySP / GS_AwaySP,
         IPperG_AwaySP = IP_AwaySP / G_AwaySP,
         IPperStart_HomeSP = Start.IP_HomeSP / GS_HomeSP,
         IPperG_HomeSP = IP_HomeSP / G_HomeSP) %>% 
  dplyr::mutate(month = lubridate::month(gamedate, label = TRUE, abbr = FALSE),
         doubleHeader = case_when(doubleHeader == 'S' ~ 'Y',
                                  TRUE ~ doubleHeader)) %>% 
  dplyr::mutate(home_team_season = as.numeric(home_team_season))

upcoming_games_tomorrow <- bovada_odds %>% 
  filter(gamedate == Sys.Date()+1) %>% 
  dplyr::mutate(yesterday_date = gamedate - 1) %>% 
  left_join(todays_games %>% 
              bind_rows(tmrw_games),
            by = c("AwayTeam" = "away_team_name",
                   "HomeTeam" = "home_team_name",
                   "gamedate" = "officialDate")) %>% 
  left_join(probables %>% 
              bind_rows(tmrw_probables) %>% 
              select(game_date, fullName, id, team) %>% 
              dplyr::mutate(game_date = as.Date(game_date)),
            by = c("AwayTeam" = "team", "gamedate" = "game_date")) %>% 
  left_join(probables %>% 
              bind_rows(tmrw_probables) %>% 
              select(game_date, fullName, id, team) %>%
              dplyr::mutate(game_date = as.Date(game_date)),
            by = c("HomeTeam" = "team", "gamedate" = "game_date"),
            suffix = c("_AwaySP", "_HomeSP")) %>% 
  left_join(PECOTA_pitching_23 %>% 
              select(mlbid, WARP200),
            by = c("id_AwaySP" = "mlbid")) %>% 
  left_join(PECOTA_pitching_23 %>% 
              select(mlbid, WARP200),
            by = c("id_HomeSP" = "mlbid"),
            suffix = c("_AwaySP", "_HomeSP")) %>% 
  left_join(rosters_update %>% 
              bind_rows(tmrw_rosters) %>% 
              filter(position_type != "Pitcher") %>% 
              left_join(PECOTA_hitting_23 %>% 
                          select(mlbid, WARP600),
                        by = c("person_id" = "mlbid")) %>% 
              group_by(team_id, date) %>% 
              dplyr::summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
              left_join(team_names %>% 
                          select(Full.Name22, id),
                        by = c("team_id" = "id")) %>% 
              ungroup() %>% 
              select(-team_id) %>% 
              dplyr::mutate(date = as.Date(date)),
            by = c("gamedate" = "date",
                   "HomeTeam" = "Full.Name22")) %>% 
  left_join(rosters_update %>% 
              bind_rows(tmrw_rosters) %>% 
              filter(position_type != "Pitcher") %>% 
              left_join(PECOTA_hitting_23 %>% 
                          select(mlbid, WARP600),
                        by = c("person_id" = "mlbid")) %>% 
              group_by(team_id, date) %>% 
              dplyr::summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
              left_join(team_names %>% 
                          select(Full.Name22, id),
                        by = c("team_id" = "id")) %>% 
              ungroup() %>% 
              select(-team_id) %>% 
              dplyr::mutate(date = as.Date(date)),
            by = c("gamedate" = "date",
                   "AwayTeam" = "Full.Name22"),
            suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  filter(!is.na(fullName_HomeSP) & !is.na(fullName_AwaySP)) %>% 
  left_join(daily_pitchers, by = c("yesterday_date" = "Date", "fullName_HomeSP" = "Name")) %>% 
  left_join(daily_pitchers, by = c("yesterday_date" = "Date", "fullName_AwaySP" = "Name"), suffix = c("_HomeSP", "_AwaySP")) %>% 
  left_join(daily_team_batting, by = c("yesterday_date" = "Date", "HomeTeam" = "Team")) %>% 
  left_join(daily_team_batting, by = c("yesterday_date" = "Date", "AwayTeam" = "Team"), suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  left_join(daily_team_bullpen, by = c("yesterday_date" = "Date", "HomeTeam" = "Team")) %>% 
  left_join(daily_team_bullpen, by = c("yesterday_date" = "Date", "AwayTeam" = "Team"), suffix = c("_HomeBullpen", "_AwayBullpen")) %>% 
  dplyr::rename(AwaySP_fullName = fullName_AwaySP,
                HomeSP_fullName = fullName_HomeSP) %>% 
  dplyr::mutate(WARP200_HomeSP = case_when(!is.nan(WARP200_HomeSP) ~ WARP200_HomeSP),
         WARP200_AwaySP = case_when(!is.nan(WARP200_AwaySP) ~ WARP200_AwaySP)) %>% 
  dplyr::mutate(IP_AwaySP = as.integer(IP_AwaySP) + (IP_AwaySP %% 1 * 3.33),
         Start.IP_AwaySP = as.integer(Start.IP_AwaySP) + (Start.IP_AwaySP %% 1 * 3.33),
         Relief.IP_AwaySP = as.integer(Relief.IP_AwaySP) + (Relief.IP_AwaySP %% 1 * 3.33),
         IP_HomeSP = as.integer(IP_HomeSP) + (IP_HomeSP %% 1 * 3.33),
         Start.IP_HomeSP = as.integer(Start.IP_HomeSP) + (Start.IP_HomeSP %% 1 * 3.33),
         Relief.IP_HomeSP = as.integer(Relief.IP_HomeSP) + (Relief.IP_HomeSP %% 1 * 3.33),
         IP_L7_AwayBullpen = as.integer(IP_L7_AwayBullpen) + (IP_L7_AwayBullpen %% 1 * 3.33),
         IP_L14_AwayBullpen = as.integer(IP_L14_AwayBullpen) + (IP_L14_AwayBullpen %% 1 * 3.33),
         IP_L30_AwayBullpen = as.integer(IP_L30_AwayBullpen) + (IP_L30_AwayBullpen %% 1 * 3.33),
         IP_AwayBullpen = as.integer(IP_AwayBullpen) + (IP_AwayBullpen %% 1 * 3.33),
         IP_L7_HomeBullpen = as.integer(IP_L7_HomeBullpen) + (IP_L7_HomeBullpen %% 1 * 3.33),
         IP_L14_HomeBullpen = as.integer(IP_L14_HomeBullpen) + (IP_L14_HomeBullpen %% 1 * 3.33),
         IP_L30_HomeBullpen = as.integer(IP_L30_HomeBullpen) + (IP_L30_HomeBullpen %% 1 * 3.33),
         IP_HomeBullpen = as.integer(IP_HomeBullpen) + (IP_HomeBullpen %% 1 * 3.33)) %>% 
  dplyr::mutate(WAR200_AwaySP = (WAR_AwaySP / IP_AwaySP) * 200,
         WAR200_HomeSP = (WAR_HomeSP / IP_HomeSP) * 200,
         HR_L7_AwayBatters = (HR_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         SB_L7_AwayBatters = (SB_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         BsR_L7_AwayBatters = (BsR_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         Off_L7_AwayBatters = (Off_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         Def_L7_AwayBatters = (Def_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         WAR_L7_AwayBatters = (WAR_L7_AwayBatters / PA_L7_AwayBatters) * 500,
         HR_L14_AwayBatters = (HR_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         SB_L14_AwayBatters = (SB_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         BsR_L14_AwayBatters = (BsR_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         Off_L14_AwayBatters = (Off_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         Def_L14_AwayBatters = (Def_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         WAR_L14_AwayBatters = (WAR_L14_AwayBatters / PA_L14_AwayBatters) * 500,
         HR_L30_AwayBatters = (HR_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         SB_L30_AwayBatters = (SB_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         BsR_L30_AwayBatters = (BsR_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         Off_L30_AwayBatters = (Off_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         Def_L30_AwayBatters = (Def_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         WAR_L30_AwayBatters = (WAR_L30_AwayBatters / PA_L30_AwayBatters) * 500,
         HR_AwayBatters = (HR_AwayBatters / PA_AwayBatters) * 500,
         SB_AwayBatters = (SB_AwayBatters / PA_AwayBatters) * 500,
         BsR_AwayBatters = (BsR_AwayBatters / PA_AwayBatters) * 500,
         Off_AwayBatters = (Off_AwayBatters / PA_AwayBatters) * 500,
         Def_AwayBatters = (Def_AwayBatters / PA_AwayBatters) * 500,
         WAR_AwayBatters = (WAR_AwayBatters / PA_AwayBatters) * 500,
         HR_L7_HomeBatters = (HR_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         SB_L7_HomeBatters = (SB_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         BsR_L7_HomeBatters = (BsR_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         Off_L7_HomeBatters = (Off_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         Def_L7_HomeBatters = (Def_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         WAR_L7_HomeBatters = (WAR_L7_HomeBatters / PA_L7_HomeBatters) * 500,
         HR_L14_HomeBatters = (HR_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         SB_L14_HomeBatters = (SB_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         BsR_L14_HomeBatters = (BsR_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         Off_L14_HomeBatters = (Off_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         Def_L14_HomeBatters = (Def_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         WAR_L14_HomeBatters = (WAR_L14_HomeBatters / PA_L14_HomeBatters) * 500,
         HR_L30_HomeBatters = (HR_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         SB_L30_HomeBatters = (SB_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         BsR_L30_HomeBatters = (BsR_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         Off_L30_HomeBatters = (Off_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         Def_L30_HomeBatters = (Def_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         WAR_L30_HomeBatters = (WAR_L30_HomeBatters / PA_L30_HomeBatters) * 500,
         HR_HomeBatters = (HR_HomeBatters / PA_HomeBatters) * 500,
         SB_HomeBatters = (SB_HomeBatters / PA_HomeBatters) * 500,
         BsR_HomeBatters = (BsR_HomeBatters / PA_HomeBatters) * 500,
         Off_HomeBatters = (Off_HomeBatters/ PA_HomeBatters) * 500,
         Def_HomeBatters = (Def_HomeBatters / PA_HomeBatters) * 500,
         WAR_HomeBatters = (WAR_HomeBatters / PA_HomeBatters) * 500,
         WAR_L7_AwayBullpen = (WAR_L7_AwayBullpen / IP_L7_AwayBullpen) * 200,
         WAR_L14_AwayBullpen = (WAR_L14_AwayBullpen / IP_L14_AwayBullpen) * 200,
         WAR_L30_AwayBullpen = (WAR_L30_AwayBullpen / IP_L30_AwayBullpen) * 200,
         WAR_AwayBullpen = (WAR_AwayBullpen / IP_AwayBullpen) * 200,
         WAR_L7_HomeBullpen = (WAR_L7_HomeBullpen / IP_L7_HomeBullpen) * 200,
         WAR_L14_HomeBullpen = (WAR_L14_HomeBullpen / IP_L14_HomeBullpen) * 200,
         WAR_L30_HomeBullpen = (WAR_L30_HomeBullpen / IP_L30_HomeBullpen) * 200,
         WAR_HomeBullpen = (WAR_HomeBullpen / IP_HomeBullpen) * 200,
         IPperStart_AwaySP = Start.IP_AwaySP / GS_AwaySP,
         IPperG_AwaySP = IP_AwaySP / G_AwaySP,
         IPperStart_HomeSP = Start.IP_HomeSP / GS_HomeSP,
         IPperG_HomeSP = IP_HomeSP / G_HomeSP) %>% 
  dplyr::mutate(month = lubridate::month(gamedate, label = TRUE, abbr = FALSE),
         doubleHeader = case_when(doubleHeader == 'S' ~ 'Y',
                                  TRUE ~ doubleHeader)) %>% 
  dplyr::mutate(home_team_season = as.numeric(home_team_season))

upcoming_games <- upcoming_games_today %>% 
  bind_rows(upcoming_games_tomorrow) %>% 
  filter(venue.name != 'Estadio Alfredo Harp Helu' &
           doubleHeader == 'N')

pred_data_home <- upcoming_games %>% 
  ungroup() %>% 
  select(gamedate, AwayTeam, HomeTeam,# AwayStartingPitcher, HomeStartingPitcher,
         AwaySP_fullName, HomeSP_fullName,
         home_team_season, month, home_team_league_name, home_team_division_name,
         doubleHeader, gameNumber, dayNight, venue.name, contains('_AwaySP'), contains('_HomeBatters'),
         -Def_L7_HomeBatters, -Def_L14_HomeBatters, -Def_L30_HomeBatters, -Def_HomeBatters,
         Def_L7_AwayBatters, Def_L14_AwayBatters, Def_L30_AwayBatters, Def_AwayBatters,
         contains('_AwayBullpen')) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  dplyr::mutate(home_or_away = 'Home')

pred_data_away <- upcoming_games %>% 
  ungroup() %>% 
  select(gamedate, AwayTeam, HomeTeam,# AwayStartingPitcher, HomeStartingPitcher,
         AwaySP_fullName, HomeSP_fullName,
         home_team_season, month, away_team_league_name, away_team_division_name,
         doubleHeader, gameNumber, dayNight, venue.name, contains('_HomeSP'), contains('_AwayBatters'),
         Def_L7_HomeBatters, Def_L14_HomeBatters, Def_L30_HomeBatters, Def_HomeBatters,
         -Def_L7_AwayBatters, -Def_L14_AwayBatters, -Def_L30_AwayBatters, -Def_AwayBatters,
         contains('_HomeBullpen')) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("away_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  dplyr::mutate(home_or_away = 'Away')
  
pred_data <- pred_data_home %>% 
  bind_rows(pred_data_away) %>% 
  filter(!is.na(IPSP) &
           !is.na(PA_L7Batters) &
           !is.na(IP_L7Bullpen) &
           !is.na(PA_L30Batters) &
           !is.na(IPBullpen)) %>% 
  replace(is.na(.), 0) %>% 
  distinct() %>% 
  dplyr::mutate(Team = case_when(home_or_away == "Home" ~ HomeTeam,
                          TRUE ~ AwayTeam),
         Opponent = case_when(home_or_away == "Away" ~ HomeTeam,
                              TRUE ~ AwayTeam))

singles <- pred_data
singles$pR_FG_pls <- predict(full_game_pls, pred_data)
singles$pR_FG_rf <- predict(full_game_rf, pred_data)
singles$pR_FG_cub <- predict(full_game_cub, pred_data)
singles$pR_FG_gbm <- predict(full_game_gbm, pred_data)
singles$pR_F5_pls <- predict(F5_pls, pred_data)
singles$pR_F5_rf <- predict(F5_rf, pred_data)
singles$pR_F5_cub <- predict(F5_cub, pred_data)
singles$pR_F5_gbm <- predict(F5_gbm, pred_data)
singles$pR_F1_pls <- predict(F1_pls, pred_data)
singles$pR_F1_rf <- predict(F1_rf, pred_data)
singles$pR_F1_cub <- predict(F1_cub, pred_data)
singles$pR_F1_gbm <- predict(F1_gbm, pred_data)
singles$pFG_TT_2.5_gbm <- predict(FG_TT_2.5_gbm, pred_data, type = "prob")
singles$pFG_TT_2.5_pls <- predict(FG_TT_2.5_pls, pred_data, type = "prob")
singles$pFG_TT_2.5_xgb <- predict(FG_TT_2.5_xgb, pred_data, type = "prob")
singles$pFG_TT_3_gbm <- predict(FG_TT_3_gbm, pred_data, type = "prob")
singles$pFG_TT_3_pls <- predict(FG_TT_3_pls, pred_data, type = "prob")
singles$pFG_TT_3_xgb <- predict(FG_TT_3_xgb, pred_data, type = "prob")
singles$pFG_TT_3.5_gbm <- predict(FG_TT_3.5_gbm, pred_data, type = "prob")
singles$pFG_TT_3.5_pls <- predict(FG_TT_3.5_pls, pred_data, type = "prob")
singles$pFG_TT_3.5_xgb <- predict(FG_TT_3.5_xgb, pred_data, type = "prob")
singles$pFG_TT_4_gbm <- predict(FG_TT_4_gbm, pred_data, type = "prob")
singles$pFG_TT_4_pls <- predict(FG_TT_4_pls, pred_data, type = "prob")
singles$pFG_TT_4_xgb <- predict(FG_TT_4_xgb, pred_data, type = "prob")
singles$pFG_TT_4.5_gbm <- predict(FG_TT_4.5_gbm, pred_data, type = "prob")
singles$pFG_TT_4.5_pls <- predict(FG_TT_4.5_pls, pred_data, type = "prob")
singles$pFG_TT_4.5_xgb <- predict(FG_TT_4.5_xgb, pred_data, type = "prob")
singles$pFG_TT_5_gbm <- predict(FG_TT_5_gbm, pred_data, type = "prob")
singles$pFG_TT_5_pls <- predict(FG_TT_5_pls, pred_data, type = "prob")
singles$pFG_TT_5_xgb <- predict(FG_TT_5_xgb, pred_data, type = "prob")
singles$pFG_TT_5.5_gbm <- predict(FG_TT_5.5_gbm, pred_data, type = "prob")
singles$pFG_TT_5.5_pls <- predict(FG_TT_5.5_pls, pred_data, type = "prob")
singles$pFG_TT_5.5_xgb <- predict(FG_TT_5.5_xgb, pred_data, type = "prob")
singles$pF5_TT_0.5_gbm <- predict(F5_TT_0.5_gbm, pred_data, type = "prob")
singles$pF5_TT_0.5_pls <- predict(F5_TT_0.5_pls, pred_data, type = "prob")
singles$pF5_TT_0.5_xgb <- predict(F5_TT_0.5_xgb, pred_data, type = "prob")
singles$pF5_TT_1_gbm <- predict(F5_TT_1_gbm, pred_data, type = "prob")
singles$pF5_TT_1_pls <- predict(F5_TT_1_pls, pred_data, type = "prob")
singles$pF5_TT_1_xgb <- predict(F5_TT_1_xgb, pred_data, type = "prob")
singles$pF5_TT_1.5_gbm <- predict(F5_TT_1.5_gbm, pred_data, type = "prob")
singles$pF5_TT_1.5_pls <- predict(F5_TT_1.5_pls, pred_data, type = "prob")
singles$pF5_TT_1.5_xgb <- predict(F5_TT_1.5_xgb, pred_data, type = "prob")
singles$pF5_TT_2_gbm <- predict(F5_TT_2_gbm, pred_data, type = "prob")
singles$pF5_TT_2_pls <- predict(F5_TT_2_pls, pred_data, type = "prob")
singles$pF5_TT_2_xgb <- predict(F5_TT_2_xgb, pred_data, type = "prob")
singles$pF5_TT_2.5_gbm <- predict(F5_TT_2.5_gbm, pred_data, type = "prob")
singles$pF5_TT_2.5_pls <- predict(F5_TT_2.5_pls, pred_data, type = "prob")
singles$pF5_TT_2.5_xgb <- predict(F5_TT_2.5_xgb, pred_data, type = "prob")
singles$pF5_TT_3_gbm <- predict(F5_TT_3_gbm, pred_data, type = "prob")
singles$pF5_TT_3_pls <- predict(F5_TT_3_pls, pred_data, type = "prob")
singles$pF5_TT_3_xgb <- predict(F5_TT_3_xgb, pred_data, type = "prob")
singles$pF5_TT_3.5_gbm <- predict(F5_TT_3.5_gbm, pred_data, type = "prob")
singles$pF5_TT_3.5_pls <- predict(F5_TT_3.5_pls, pred_data, type = "prob")
singles$pF5_TT_3.5_xgb <- predict(F5_TT_3.5_xgb, pred_data, type = "prob")
singles$pF1_TT_0.5_gbm <- predict(F1_TT_0.5_gbm, pred_data, type = "prob")
singles$pF1_TT_0.5_pls <- predict(F1_TT_0.5_pls, pred_data, type = "prob")
singles$pF1_TT_0.5_xgb <- predict(F1_TT_0.5_xgb, pred_data, type = "prob")
singles <- singles %>% 
  dplyr::mutate(pR_FG = (pR_FG_gbm + pR_FG_cub + pR_FG_rf + pR_FG_pls) / 4,
         pR_F5 = (pR_F5_gbm + pR_F5_cub + pR_F5_rf + pR_F5_pls) / 4,
         pR_F1 = (pR_F1_gbm + pR_F1_cub + pR_F1_rf + pR_F1_pls) / 4,
         pFG_TT_2.5 = (pFG_TT_2.5_gbm + pFG_TT_2.5_pls + pFG_TT_2.5_xgb) / 3,
         pFG_TT_3 = (pFG_TT_3_gbm + pFG_TT_3_pls + pFG_TT_3_xgb) / 3,
         pFG_TT_3.5 = (pFG_TT_3.5_gbm + pFG_TT_3.5_pls + pFG_TT_3.5_xgb) / 3,
         pFG_TT_4 = (pFG_TT_4_gbm + pFG_TT_4_pls + pFG_TT_4_xgb) / 3,
         pFG_TT_4.5 = (pFG_TT_4.5_gbm + pFG_TT_4.5_pls + pFG_TT_4.5_xgb) / 3,
         pFG_TT_5 = (pFG_TT_5_gbm + pFG_TT_5_pls + pFG_TT_5_xgb) / 3,
         pFG_TT_5.5 = (pFG_TT_5.5_gbm + pFG_TT_5.5_pls + pFG_TT_5.5_xgb) / 3,
         pF5_TT_0.5 = (pF5_TT_0.5_gbm + pF5_TT_0.5_pls + pF5_TT_0.5_xgb) / 3,
         pF5_TT_1 = (pF5_TT_1_gbm + pF5_TT_1_pls + pF5_TT_1_xgb) / 3,
         pF5_TT_1.5 = (pF5_TT_1.5_gbm + pF5_TT_1.5_pls + pF5_TT_1.5_xgb) / 3,
         pF5_TT_2 = (pF5_TT_2_gbm + pF5_TT_2_pls + pF5_TT_2_xgb) / 3,
         pF5_TT_2.5 = (pF5_TT_2.5_gbm + pF5_TT_2.5_pls + pF5_TT_2.5_xgb) / 3,
         pF5_TT_3 = (pF5_TT_3_gbm + pF5_TT_3_pls + pF5_TT_3_xgb) / 3,
         pF5_TT_3.5 = (pF5_TT_3.5_gbm + pF5_TT_3.5_pls + pF5_TT_3.5_xgb) / 3,
         pF1_TT_0.5 = (pF1_TT_0.5_gbm + pF1_TT_0.5_pls + pF1_TT_0.5_xgb) / 3)

singles2 <- singles %>% 
  select(gamedate:HomeSP_fullName, Team, Opponent, home_or_away, pR_FG:pF1_TT_0.5) %>%
  left_join(singles %>% 
              select(gamedate:HomeSP_fullName, Team, Opponent, home_or_away, pR_FG:pF1_TT_0.5),
            by = c("gamedate", "Team" = "Opponent", "Opponent" = "Team", "AwaySP_fullName", 
                   "HomeSP_fullName"#, "AwayStartingPitcher", "HomeStartingPitcher"
                   ),
            suffix = c("_Home", "_Away")) %>% 
  select(gamedate:HomeSP_fullName, home_or_away_Home, pR_FG_Home:pF1_TT_0.5_Home, pR_FG_Away:pF1_TT_0.5_Away, -home_or_away_Away) %>% 
  # rename(Home = Team,
  #        Away = Opponent) %>% 
  filter(home_or_away_Home == "Home") %>% 
  select(-home_or_away_Home)

metrics_df <- upcoming_games %>% 
  select(-yesterday_date, -(bet_type:HUN.SpreadTotal)) %>% 
  ungroup() %>% 
  filter(!is.na(IP_HomeSP) &
           !is.na(IP_AwaySP) &
           !is.na(PA_L7_HomeBatters) &
           !is.na(PA_L7_AwayBatters) &
           !is.na(IP_L7_HomeBullpen) &
           !is.na(IP_L7_AwayBullpen) &
           !is.na(PA_L30_HomeBatters) &
           !is.na(PA_L30_AwayBatters) &
           !is.na(IP_HomeBullpen) &
           !is.na(IP_AwayBullpen)) %>% 
  replace(is.na(.), 0) %>% 
  distinct()
  
doubles <- metrics_df
doubles$pFG_ML_gbm <- predict(FG_ML_gbm, metrics_df, type = "prob")
doubles$pFG_ML_pls <- predict(FG_ML_pls, metrics_df, type = "prob")
doubles$pFG_ML_xgb <- predict(FG_ML_xgb, metrics_df, type = "prob")
doubles$pFG_Minus_1.5_gbm <- predict(FG_Minus_1.5_gbm, metrics_df, type = "prob")
doubles$pFG_Minus_1.5_pls <- predict(FG_Minus_1.5_pls, metrics_df, type = "prob")
doubles$pFG_Minus_1.5_xgb <- predict(FG_Minus_1.5_xgb, metrics_df, type = "prob")
doubles$pFG_Minus_2.5_gbm <- predict(FG_Minus_2.5_gbm, metrics_df, type = "prob")
doubles$pFG_Minus_2.5_pls <- predict(FG_Minus_2.5_pls, metrics_df, type = "prob")
doubles$pFG_Minus_2.5_xgb <- predict(FG_Minus_2.5_xgb, metrics_df, type = "prob")
doubles$pFG_Plus_1.5_gbm <- predict(FG_Plus_1.5_gbm, metrics_df, type = "prob")
doubles$pFG_Plus_1.5_pls <- predict(FG_Plus_1.5_pls, metrics_df, type = "prob")
doubles$pFG_Plus_1.5_xgb <- predict(FG_Plus_1.5_xgb, metrics_df, type = "prob")
doubles$pFG_Plus_2.5_gbm <- predict(FG_Plus_2.5_gbm, metrics_df, type = "prob")
doubles$pFG_Plus_2.5_pls <- predict(FG_Plus_2.5_pls, metrics_df, type = "prob")
doubles$pFG_Plus_2.5_xgb <- predict(FG_Plus_2.5_xgb, metrics_df, type = "prob")
doubles$pFG_Total_6.5_gbm <- predict(FG_Total_6.5_gbm, metrics_df, type = "prob")
doubles$pFG_Total_6.5_pls <- predict(FG_Total_6.5_pls, metrics_df, type = "prob")
doubles$pFG_Total_6.5_xgb <- predict(FG_Total_6.5_xgb, metrics_df, type = "prob")
doubles$pFG_Total_7_gbm <- predict(FG_Total_7_gbm, metrics_df, type = "prob")
doubles$pFG_Total_7_pls <- predict(FG_Total_7_pls, metrics_df, type = "prob")
doubles$pFG_Total_7_xgb <- predict(FG_Total_7_xgb, metrics_df, type = "prob")
doubles$pFG_Total_7.5_gbm <- predict(FG_Total_7.5_gbm, metrics_df, type = "prob")
doubles$pFG_Total_7.5_pls <- predict(FG_Total_7.5_pls, metrics_df, type = "prob")
doubles$pFG_Total_7.5_xgb <- predict(FG_Total_7.5_xgb, metrics_df, type = "prob")
doubles$pFG_Total_8_gbm <- predict(FG_Total_8_gbm, metrics_df, type = "prob")
doubles$pFG_Total_8_pls <- predict(FG_Total_8_pls, metrics_df, type = "prob")
doubles$pFG_Total_8_xgb <- predict(FG_Total_8_xgb, metrics_df, type = "prob")
doubles$pFG_Total_8.5_gbm <- predict(FG_Total_8.5_gbm, metrics_df, type = "prob")
doubles$pFG_Total_8.5_pls <- predict(FG_Total_8.5_pls, metrics_df, type = "prob")
doubles$pFG_Total_8.5_xgb <- predict(FG_Total_8.5_xgb, metrics_df, type = "prob")
doubles$pFG_Total_9_gbm <- predict(FG_Total_9_gbm, metrics_df, type = "prob")
doubles$pFG_Total_9_pls <- predict(FG_Total_9_pls, metrics_df, type = "prob")
doubles$pFG_Total_9_xgb <- predict(FG_Total_9_xgb, metrics_df, type = "prob")
doubles$pFG_Total_9.5_gbm <- predict(FG_Total_9.5_gbm, metrics_df, type = "prob")
doubles$pFG_Total_9.5_pls <- predict(FG_Total_9.5_pls, metrics_df, type = "prob")
doubles$pFG_Total_9.5_xgb <- predict(FG_Total_9.5_xgb, metrics_df, type = "prob")
doubles$pFG_Total_10_gbm <- predict(FG_Total_10_gbm, metrics_df, type = "prob")
doubles$pFG_Total_10_pls <- predict(FG_Total_10_pls, metrics_df, type = "prob")
doubles$pFG_Total_10_xgb <- predict(FG_Total_10_xgb, metrics_df, type = "prob")
doubles$pFG_Total_10.5_gbm <- predict(FG_Total_10.5_gbm, metrics_df, type = "prob")
doubles$pFG_Total_10.5_pls <- predict(FG_Total_10.5_pls, metrics_df, type = "prob")
doubles$pFG_Total_10.5_xgb <- predict(FG_Total_10.5_xgb, metrics_df, type = "prob")
doubles$pFG_Total_11_gbm <- predict(FG_Total_11_gbm, metrics_df, type = "prob")
doubles$pFG_Total_11_pls <- predict(FG_Total_11_pls, metrics_df, type = "prob")
doubles$pFG_Total_11_xgb <- predict(FG_Total_11_xgb, metrics_df, type = "prob")
doubles$pFG_Total_11.5_gbm <- predict(FG_Total_11.5_gbm, metrics_df, type = "prob")
doubles$pFG_Total_11.5_pls <- predict(FG_Total_11.5_pls, metrics_df, type = "prob")
doubles$pFG_Total_11.5_xgb <- predict(FG_Total_11.5_xgb, metrics_df, type = "prob")
doubles$pF5_ML_gbm <- predict(F5_ML_gbm, metrics_df, type = "prob")
doubles$pF5_ML_pls <- predict(F5_ML_pls, metrics_df, type = "prob")
doubles$pF5_ML_xgb <- predict(F5_ML_xgb, metrics_df, type = "prob")
doubles$pF5_Minus_0.5_gbm <- predict(F5_Minus_0.5_gbm, metrics_df, type = "prob")
doubles$pF5_Minus_0.5_pls <- predict(F5_Minus_0.5_pls, metrics_df, type = "prob")
doubles$pF5_Minus_0.5_xgb <- predict(F5_Minus_0.5_xgb, metrics_df, type = "prob")
doubles$pF5_Minus_1.5_gbm <- predict(F5_Minus_1.5_gbm, metrics_df, type = "prob")
doubles$pF5_Minus_1.5_pls <- predict(F5_Minus_1.5_pls, metrics_df, type = "prob")
doubles$pF5_Minus_1.5_xgb <- predict(F5_Minus_1.5_xgb, metrics_df, type = "prob")
doubles$pF5_Plus_0.5_gbm <- predict(F5_Plus_0.5_gbm, metrics_df, type = "prob")
doubles$pF5_Plus_0.5_pls <- predict(F5_Plus_0.5_pls, metrics_df, type = "prob")
doubles$pF5_Plus_0.5_xgb <- predict(F5_Plus_0.5_xgb, metrics_df, type = "prob")
doubles$pF5_Plus_1.5_gbm <- predict(F5_Plus_1.5_gbm, metrics_df, type = "prob")
doubles$pF5_Plus_1.5_pls <- predict(F5_Plus_1.5_pls, metrics_df, type = "prob")
doubles$pF5_Plus_1.5_xgb <- predict(F5_Plus_1.5_xgb, metrics_df, type = "prob")
doubles$pF5_Total_3.5_gbm <- predict(F5_Total_3.5_gbm, metrics_df, type = "prob")
doubles$pF5_Total_3.5_pls <- predict(F5_Total_3.5_pls, metrics_df, type = "prob")
doubles$pF5_Total_3.5_xgb <- predict(F5_Total_3.5_xgb, metrics_df, type = "prob")
doubles$pF5_Total_4_gbm <- predict(F5_Total_4_gbm, metrics_df, type = "prob")
doubles$pF5_Total_4_pls <- predict(F5_Total_4_pls, metrics_df, type = "prob")
doubles$pF5_Total_4_xgb <- predict(F5_Total_4_xgb, metrics_df, type = "prob")
doubles$pF5_Total_4.5_gbm <- predict(F5_Total_4.5_gbm, metrics_df, type = "prob")
doubles$pF5_Total_4.5_pls <- predict(F5_Total_4.5_pls, metrics_df, type = "prob")
doubles$pF5_Total_4.5_xgb <- predict(F5_Total_4.5_xgb, metrics_df, type = "prob")
doubles$pF5_Total_5_gbm <- predict(F5_Total_5_gbm, metrics_df, type = "prob")
doubles$pF5_Total_5_pls <- predict(F5_Total_5_pls, metrics_df, type = "prob")
doubles$pF5_Total_5_xgb <- predict(F5_Total_5_xgb, metrics_df, type = "prob")
doubles$pF5_Total_5.5_gbm <- predict(F5_Total_5.5_gbm, metrics_df, type = "prob")
doubles$pF5_Total_5.5_pls <- predict(F5_Total_5.5_pls, metrics_df, type = "prob")
doubles$pF5_Total_5.5_xgb <- predict(F5_Total_5.5_xgb, metrics_df, type = "prob")
doubles$pF1_Total_0.5_gbm <- predict(F1_Total_0.5_gbm, metrics_df, type = "prob")
doubles$pF1_Total_0.5_pls <- predict(F1_Total_0.5_pls, metrics_df, type = "prob")
doubles$pF1_Total_0.5_xgb <- predict(F1_Total_0.5_xgb, metrics_df, type = "prob")
doubles <- doubles %>% 
  dplyr::mutate(pFG_ML = (pFG_ML_gbm + pFG_ML_pls + pFG_ML_xgb) / 3,
         pFG_Minus_1.5 = (pFG_Minus_1.5_gbm + pFG_Minus_1.5_pls + pFG_Minus_1.5_xgb) / 3,
         pFG_Minus_2.5 = (pFG_Minus_2.5_gbm + pFG_Minus_2.5_pls + pFG_Minus_2.5_xgb) / 3,
         pFG_Plus_1.5 = (pFG_Plus_1.5_gbm + pFG_Plus_1.5_pls + pFG_Plus_1.5_xgb) / 3,
         pFG_Plus_2.5 = (pFG_Plus_2.5_gbm + pFG_Plus_2.5_pls + pFG_Plus_2.5_xgb) / 3,
         pFG_Total_6.5 = (pFG_Total_6.5_gbm + pFG_Total_6.5_pls + pFG_Total_6.5_xgb) / 3,
         pFG_Total_7 = (pFG_Total_7_gbm + pFG_Total_7_pls + pFG_Total_7_xgb) / 3,
         pFG_Total_7.5 = (pFG_Total_7.5_gbm + pFG_Total_7.5_pls + pFG_Total_7.5_xgb) / 3,
         pFG_Total_8 = (pFG_Total_8_gbm + pFG_Total_8_pls + pFG_Total_8_xgb) / 3,
         pFG_Total_8.5 = (pFG_Total_8.5_gbm + pFG_Total_8.5_pls + pFG_Total_8.5_xgb) / 3,
         pFG_Total_9 = (pFG_Total_9_gbm + pFG_Total_9_pls + pFG_Total_9_xgb) / 3,
         pFG_Total_9.5 = (pFG_Total_9.5_gbm + pFG_Total_9.5_pls + pFG_Total_9.5_xgb) / 3,
         pFG_Total_10 = (pFG_Total_10_gbm + pFG_Total_10_pls + pFG_Total_10_xgb) / 3,
         pFG_Total_10.5 = (pFG_Total_10.5_gbm + pFG_Total_10.5_pls + pFG_Total_10.5_xgb) / 3,
         pFG_Total_11 = (pFG_Total_11_gbm + pFG_Total_11_pls + pFG_Total_11_xgb) / 3,
         pFG_Total_11.5 = (pFG_Total_11.5_gbm + pFG_Total_11.5_pls + pFG_Total_11.5_xgb) / 3,
         pF5_ML = (pF5_ML_gbm + pF5_ML_pls + pF5_ML_xgb) / 3,
         pF5_Minus_0.5 = (pF5_Minus_0.5_gbm + pF5_Minus_0.5_pls + pF5_Minus_0.5_xgb) / 3,
         pF5_Minus_1.5 = (pF5_Minus_1.5_gbm + pF5_Minus_1.5_pls + pF5_Minus_1.5_xgb) / 3,
         pF5_Plus_0.5 = (pF5_Plus_0.5_gbm + pF5_Plus_0.5_pls + pF5_Plus_0.5_xgb) / 3,
         pF5_Plus_1.5 = (pF5_Plus_1.5_gbm + pF5_Plus_1.5_pls + pF5_Plus_1.5_xgb) / 3,
         pF5_Total_3.5 = (pF5_Total_3.5_gbm + pF5_Total_3.5_pls + pF5_Total_3.5_xgb) / 3,
         pF5_Total_4 = (pF5_Total_4_gbm + pF5_Total_4_pls + pF5_Total_4_xgb) / 3,
         pF5_Total_4.5 = (pF5_Total_4.5_gbm + pF5_Total_4.5_pls + pF5_Total_4.5_xgb) / 3,
         pF5_Total_5 = (pF5_Total_5_gbm + pF5_Total_5_pls + pF5_Total_5_xgb) / 3,
         pF5_Total_5.5 = (pF5_Total_5.5_gbm + pF5_Total_5.5_pls + pF5_Total_5.5_xgb) / 3,
         pF1_Total_0.5 = (pF1_Total_0.5_gbm + pF1_Total_0.5_pls + pF1_Total_0.5_xgb) / 3)

doubles2 <- doubles %>% 
  select(gamedate:HomeTeam,#HomeStartingPitcher,
         pFG_ML:pF1_Total_0.5)

FinalPreds <- doubles2 %>% 
  left_join(singles2,
            by = c("gamedate", "AwayTeam" = "AwayTeam_Home", 
                   "HomeTeam" = "HomeTeam_Home"
                   # , 
                   # "AwayStartingPitcher", "HomeStartingPitcher"
                   ))

#### Predictions ####

upcoming_df <- upcoming_games %>% 
  select(gamedate:HomeTeam,#HomeStartingPitcher,
         AwaySP_fullName, HomeSP_fullName, bet_type:HUN.SpreadTotal) %>% 
  left_join(FinalPreds)

# mismatched_pitchers <- upcoming_df %>% 
#   filter(trimws(tolower(AwayStartingPitcher)) != trimws(tolower(AwaySP_fullName)) |
#            trimws(tolower(HomeStartingPitcher)) != trimws(tolower(HomeSP_fullName)))

bets <- upcoming_df %>% 
  # filter(trimws(tolower(AwayStartingPitcher)) == trimws(tolower(AwaySP_fullName)) &
  #          trimws(tolower(HomeStartingPitcher)) == trimws(tolower(HomeSP_fullName))) %>%
  dplyr::mutate(AOY_ImpliedOdds = if_else(AOY.Odds > 0, 100 / (AOY.Odds + 100), abs(AOY.Odds) / (abs(AOY.Odds) + 100)),
         HUN_ImpliedOdds = if_else(HUN.Odds > 0, 100 / (HUN.Odds + 100), abs(HUN.Odds) / (abs(HUN.Odds) + 100))) %>% 
  dplyr::rename(Home_pred_FG = pR_FG_Home,
         Away_pred_FG = pR_FG_Away,
         Home_pred_F5 = pR_F5_Home,
         Away_pred_F5 = pR_F5_Away,
         Home_pred_F1 = pR_F1_Home,
         Away_pred_F1 = pR_F1_Away) %>% 
  dplyr::mutate(bet_type_full = bet_type,
         bet_type = case_when(bet_type_full == "Moneyline - Game" ~ "FG ML",
                              bet_type_full == "Moneyline - First 5 Innings" ~ "F5 ML",
                              bet_type_full == "Runline - Game" ~ "FG RL",
                              bet_type_full == "Runline - First 5 Innings" ~ "F5 RL",
                              bet_type_full == "Total - Game" ~ "FG Total",
                              bet_type_full == "Total - First 5 Innings" ~ "F5 Total",
                              bet_type_full == "Will there be a run scored in the 1st inning - Game" ~ "RFI",
                              bet_type_full == "Alternate Runline - Game - 1.5" ~ "FG Alt RL",
                              bet_type_full == "Alternate Runline - Game - 2.5" ~ "FG Alt RL",
                              bet_type_full == "Alternate Runline - First 5 Innings - 0.5" ~ "F5 Alt RL",
                              bet_type_full == "Alternate Runline - First 5 Innings - 1.5" ~ "F5 Alt RL",
                              str_detect(bet_type_full, "Alternate Total - Game") ~ "FG Alt Total",
                              str_detect(bet_type_full, "Alternate Total - First 5 Innings") ~ "F5 Alt Total",
                              str_detect(bet_type_full, "To Score - 1st Inning") ~ "Team RFI",
                              str_detect(bet_type_full, "Team Total") & 
                                str_detect(bet_type_full, "First 5 Innings") & 
                                (AOY.Odds <= -160 | HUN.Odds <= -160) ~ "F5 Alt TT",
                              str_detect(bet_type_full, "Team Total") & 
                                str_detect(bet_type_full, "Game") &
                                (AOY.Odds <= -160 | HUN.Odds <= -160) ~ "FG Alt TT",
                              str_detect(bet_type_full, "Team Total") & 
                                str_detect(bet_type_full, "First 5 Innings") ~ "F5 TT",
                              str_detect(bet_type_full, "Team Total") & 
                                str_detect(bet_type_full, "Game") ~ "FG TT",
                              TRUE ~ "Other"))

bets2 <- bets %>% 
  rowwise() %>% 
  dplyr::mutate(Away_pred = case_when(str_detect(bet_type, "FG") ~ Away_pred_FG,
                                      str_detect(bet_type, "F5") ~ Away_pred_F5,
                                      str_detect(bet_type, "RFI") ~ Away_pred_F1,
                                      TRUE ~ Away_pred_FG),
                Home_pred = case_when(str_detect(bet_type, "FG") ~ Home_pred_FG,
                                      str_detect(bet_type, "F5") ~ Home_pred_F5,
                                      str_detect(bet_type, "RFI") ~ Home_pred_F1,
                                      TRUE ~ Home_pred_FG),
                AOY_ProjOdds1 = case_when(bet_type == "FG ML" ~ simulate_game(homeScorePred = Home_pred_FG,
                                                                              awayScorePred = Away_pred_FG,
                                                                              homeORaway = "away",
                                                                              max_score = 20,
                                                                              drawAllowed = FALSE),
                                          bet_type == "F5 ML" ~ simulate_game(homeScorePred = Home_pred_F5,
                                                                              awayScorePred = Away_pred_F5,
                                                                              homeORaway = "away",
                                                                              max_score = 20,
                                                                              drawAllowed = FALSE),
                                          bet_type == "FG RL" |
                                            bet_type == "FG Alt RL" ~ simulate_spread(homeScorePred = Home_pred_FG,
                                                                              awayScorePred = Away_pred_FG,
                                                                              homeORaway = "away",
                                                                              max_score = 20,
                                                                              spread = AOY.SpreadTotal),
                                          bet_type == "F5 RL" |
                                            bet_type == "F5 Alt RL" ~ simulate_spread(homeScorePred = Home_pred_F5,
                                                                              awayScorePred = Away_pred_F5,
                                                                              homeORaway = "away",
                                                                              max_score = 20,
                                                                              spread = AOY.SpreadTotal),
                                          bet_type == "FG Total" |
                                            bet_type == "FG Alt Total" ~ simulate_total(homeScorePred = Home_pred_FG,
                                                                              awayScorePred = Away_pred_FG,
                                                                              overORunder = "over",
                                                                              max_score = 20,
                                                                              total = AOY.SpreadTotal),
                                          bet_type == "F5 Total" |
                                            bet_type == "F5 Alt Total" ~ simulate_total(homeScorePred = Home_pred_F5,
                                                                              awayScorePred = Away_pred_F5,
                                                                              overORunder = "over",
                                                                              max_score = 20,
                                                                              total = AOY.SpreadTotal),
                                          bet_type == "RFI" ~ simulate_total(homeScorePred = Home_pred_F1,
                                                                             awayScorePred = Away_pred_F1,
                                                                             overORunder = "over",
                                                                             max_score = 20,
                                                                             total = 0.5),
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, AwayTeam) ~ simulate_team_total(ScorePred = Away_pred_F1,
                                                                                                      overORunder = "over",
                                                                                                      team_total = 0.5,
                                                                                                      max_score = 20),
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, HomeTeam) ~ simulate_team_total(ScorePred = Home_pred_F1,
                                                                                                      overORunder = "over",
                                                                                                      team_total = 0.5,
                                                                                                      max_score = 20),
                                          (bet_type == "F5 TT" |
                                            bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) ~ simulate_team_total(ScorePred = Away_pred_F5,
                                                                                                      overORunder = "over",
                                                                                                      team_total = AOY.SpreadTotal,
                                                                                                      max_score = 20),
                                          (bet_type == "F5 TT" |
                                            bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) ~ simulate_team_total(ScorePred = Home_pred_F5,
                                                                                                      overORunder = "over",
                                                                                                      team_total = AOY.SpreadTotal,
                                                                                                      max_score = 20),
                                          (bet_type == "FG TT" |
                                            bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) ~ simulate_team_total(ScorePred = Away_pred_FG,
                                                                                                      overORunder = "over",
                                                                                                      team_total = AOY.SpreadTotal,
                                                                                                      max_score = 20),
                                          (bet_type == "FG TT" |
                                            bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) ~ simulate_team_total(ScorePred = Home_pred_FG,
                                                                                                      overORunder = "over",
                                                                                                      team_total = AOY.SpreadTotal,
                                                                                                      max_score = 20)),
                HUN_ProjOdds1 = case_when(bet_type == "FG ML" ~ simulate_game(homeScorePred = Home_pred_FG,
                                                                              awayScorePred = Away_pred_FG,
                                                                              homeORaway = "home",
                                                                              max_score = 20,
                                                                              drawAllowed = FALSE),
                                          bet_type == "F5 ML" ~ simulate_game(homeScorePred = Home_pred_F5,
                                                                              awayScorePred = Away_pred_F5,
                                                                              homeORaway = "home",
                                                                              max_score = 20,
                                                                              drawAllowed = FALSE),
                                          bet_type == "FG RL" |
                                            bet_type == "FG Alt RL" ~ simulate_spread(homeScorePred = Home_pred_FG,
                                                                                      awayScorePred = Away_pred_FG,
                                                                                      homeORaway = "home",
                                                                                      max_score = 20,
                                                                                      spread = HUN.SpreadTotal),
                                          bet_type == "F5 RL" |
                                            bet_type == "F5 Alt RL" ~ simulate_spread(homeScorePred = Home_pred_F5,
                                                                                      awayScorePred = Away_pred_F5,
                                                                                      homeORaway = "home",
                                                                                      max_score = 20,
                                                                                      spread = HUN.SpreadTotal),
                                          bet_type == "FG Total" |
                                            bet_type == "FG Alt Total" ~ simulate_total(homeScorePred = Home_pred_FG,
                                                                                        awayScorePred = Away_pred_FG,
                                                                                        overORunder = "under",
                                                                                        max_score = 20,
                                                                                        total = HUN.SpreadTotal),
                                          bet_type == "F5 Total" |
                                            bet_type == "F5 Alt Total" ~ simulate_total(homeScorePred = Home_pred_F5,
                                                                                        awayScorePred = Away_pred_F5,
                                                                                        overORunder = "under",
                                                                                        max_score = 20,
                                                                                        total = HUN.SpreadTotal),
                                          bet_type == "RFI" ~ simulate_total(homeScorePred = Home_pred_F1,
                                                                             awayScorePred = Away_pred_F1,
                                                                             overORunder = "under",
                                                                             max_score = 20,
                                                                             total = 0.5),
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, AwayTeam) ~ simulate_team_total(ScorePred = Away_pred_F1,
                                                                                                      overORunder = "under",
                                                                                                      team_total = 0.5,
                                                                                                      max_score = 20),
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, HomeTeam) ~ simulate_team_total(ScorePred = Home_pred_F1,
                                                                                                      overORunder = "under",
                                                                                                      team_total = 0.5,
                                                                                                      max_score = 20),
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) ~ simulate_team_total(ScorePred = Away_pred_F5,
                                                                                                      overORunder = "under",
                                                                                                      team_total = HUN.SpreadTotal,
                                                                                                      max_score = 20),
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) ~ simulate_team_total(ScorePred = Home_pred_F5,
                                                                                                      overORunder = "under",
                                                                                                      team_total = HUN.SpreadTotal,
                                                                                                      max_score = 20),
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) ~ simulate_team_total(ScorePred = Away_pred_FG,
                                                                                                      overORunder = "under",
                                                                                                      team_total = HUN.SpreadTotal,
                                                                                                      max_score = 20),
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) ~ simulate_team_total(ScorePred = Home_pred_FG,
                                                                                                      overORunder = "under",
                                                                                                      team_total = HUN.SpreadTotal,
                                                                                                      max_score = 20)),
                AOY_ProjOdds2 = case_when(bet_type == "FG ML" ~ pFG_ML$Lose,
                                          bet_type == "F5 ML" ~ pF5_ML$Lose,
                                          (bet_type == "FG RL" |
                                            bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == -1.5 ~ pFG_Minus_1.5$Lose,
                                          (bet_type == "FG RL" |
                                             bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == 1.5 ~ pFG_Plus_1.5$Lose,
                                          (bet_type == "FG RL" |
                                             bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == -2.5 ~ pFG_Minus_2.5$Lose,
                                          (bet_type == "FG RL" |
                                             bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == 2.5 ~ pFG_Plus_2.5$Lose,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == -1.5 ~ pF5_Minus_1.5$Lose,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == 1.5 ~ pF5_Plus_1.5$Lose,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == -0.5 ~ pF5_Minus_0.5$Lose,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == 0.5 ~ pF5_Plus_0.5$Lose,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 6.5 ~ pFG_Total_6.5$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 7 ~ pFG_Total_7$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 7.5 ~ pFG_Total_7.5$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 8 ~ pFG_Total_8$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 8.5 ~ pFG_Total_8.5$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 9 ~ pFG_Total_9$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 9.5 ~ pFG_Total_9.5$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 10 ~ pFG_Total_10$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 10.5 ~ pFG_Total_10.5$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 11 ~ pFG_Total_11$Over,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            AOY.SpreadTotal == 11.5 ~ pFG_Total_11.5$Over,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            AOY.SpreadTotal == 3.5 ~ pF5_Total_3.5$Over,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            AOY.SpreadTotal == 4 ~ pF5_Total_4$Over,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            AOY.SpreadTotal == 4.5 ~ pF5_Total_4.5$Over,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            AOY.SpreadTotal == 5 ~ pF5_Total_5$Over,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            AOY.SpreadTotal == 5.5 ~ pF5_Total_5.5$Over,
                                          bet_type == "RFI" ~ pF1_Total_0.5$Over,
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, AwayTeam) ~ pF1_TT_0.5_Away$Over,
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, HomeTeam) ~ pF1_TT_0.5_Home$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 2.5 ~ pFG_TT_2.5_Away$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 2.5 ~ pFG_TT_2.5_Home$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 3 ~ pFG_TT_3_Away$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 3 ~ pFG_TT_3_Home$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 3.5 ~ pFG_TT_3.5_Away$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 3.5 ~ pFG_TT_3.5_Home$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 4 ~ pFG_TT_4_Away$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 4 ~ pFG_TT_4_Home$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 4.5 ~ pFG_TT_4.5_Away$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 4.5 ~ pFG_TT_4.5_Home$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 5 ~ pFG_TT_5_Away$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 5 ~ pFG_TT_5_Home$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 5.5 ~ pFG_TT_5.5_Away$Over,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 5.5 ~ pFG_TT_5.5_Home$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 0.5 ~ pF5_TT_0.5_Away$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 0.5 ~ pF5_TT_0.5_Home$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 1 ~ pF5_TT_1_Away$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 1 ~ pF5_TT_1_Home$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 1.5 ~ pF5_TT_1.5_Away$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 1.5 ~ pF5_TT_1.5_Home$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 2 ~ pF5_TT_2_Away$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 2 ~ pF5_TT_2_Home$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 2.5 ~ pF5_TT_2.5_Away$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 2.5 ~ pF5_TT_2.5_Home$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 3 ~ pF5_TT_3_Away$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 3 ~ pF5_TT_3_Home$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            AOY.SpreadTotal == 3.5 ~ pF5_TT_3.5_Away$Over,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            AOY.SpreadTotal == 3.5 ~ pF5_TT_3.5_Home$Over),
                HUN_ProjOdds2 = case_when(bet_type == "FG ML" ~ pFG_ML$Win,
                                          bet_type == "F5 ML" ~ pF5_ML$Win,
                                          (bet_type == "FG RL" |
                                             bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == -1.5 ~ pFG_Minus_1.5$Win,
                                          (bet_type == "FG RL" |
                                             bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == 1.5 ~ pFG_Plus_1.5$Win,
                                          (bet_type == "FG RL" |
                                             bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == -2.5 ~ pFG_Minus_2.5$Win,
                                          (bet_type == "FG RL" |
                                             bet_type == "FG Alt RL") &
                                            HUN.SpreadTotal == 2.5 ~ pFG_Plus_2.5$Win,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == -1.5 ~ pF5_Minus_1.5$Win,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == 1.5 ~ pF5_Plus_1.5$Win,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == -0.5 ~ pF5_Minus_0.5$Win,
                                          (bet_type == "F5 RL" |
                                             bet_type == "F5 Alt RL") &
                                            HUN.SpreadTotal == 0.5 ~ pF5_Plus_0.5$Win,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 6.5 ~ pFG_Total_6.5$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 7 ~ pFG_Total_7$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 7.5 ~ pFG_Total_7.5$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 8 ~ pFG_Total_8$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 8.5 ~ pFG_Total_8.5$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 9 ~ pFG_Total_9$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 9.5 ~ pFG_Total_9.5$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 10 ~ pFG_Total_10$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 10.5 ~ pFG_Total_10.5$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 11 ~ pFG_Total_11$Under,
                                          (bet_type == "FG Total" |
                                             bet_type == "FG Alt Total") &
                                            HUN.SpreadTotal == 11.5 ~ pFG_Total_11.5$Under,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            HUN.SpreadTotal == 3.5 ~ pF5_Total_3.5$Under,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            HUN.SpreadTotal == 4 ~ pF5_Total_4$Under,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            HUN.SpreadTotal == 4.5 ~ pF5_Total_4.5$Under,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            HUN.SpreadTotal == 5 ~ pF5_Total_5$Under,
                                          (bet_type == "F5 Total" |
                                             bet_type == "F5 Alt Total") &
                                            HUN.SpreadTotal == 5.5 ~ pF5_Total_5.5$Under,
                                          bet_type == "RFI" ~ pF1_Total_0.5$Under,
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, AwayTeam) ~ pF1_TT_0.5_Away$Under,
                                          bet_type == "Team RFI" &
                                            str_detect(bet_type_full, HomeTeam) ~ pF1_TT_0.5_Home$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 2.5 ~ pFG_TT_2.5_Away$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 2.5 ~ pFG_TT_2.5_Home$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 3 ~ pFG_TT_3_Away$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 3 ~ pFG_TT_3_Home$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 3.5 ~ pFG_TT_3.5_Away$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 3.5 ~ pFG_TT_3.5_Home$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 4 ~ pFG_TT_4_Away$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 4 ~ pFG_TT_4_Home$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 4.5 ~ pFG_TT_4.5_Away$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 4.5 ~ pFG_TT_4.5_Home$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 5 ~ pFG_TT_5_Away$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 5 ~ pFG_TT_5_Home$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 5.5 ~ pFG_TT_5.5_Away$Under,
                                          (bet_type == "FG TT" |
                                             bet_type == "FG Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 5.5 ~ pFG_TT_5.5_Home$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 0.5 ~ pF5_TT_0.5_Away$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 0.5 ~ pF5_TT_0.5_Home$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 1 ~ pF5_TT_1_Away$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 1 ~ pF5_TT_1_Home$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 1.5 ~ pF5_TT_1.5_Away$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 1.5 ~ pF5_TT_1.5_Home$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 2 ~ pF5_TT_2_Away$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 2 ~ pF5_TT_2_Home$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 2.5 ~ pF5_TT_2.5_Away$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 2.5 ~ pF5_TT_2.5_Home$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 3 ~ pF5_TT_3_Away$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 3 ~ pF5_TT_3_Home$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, AwayTeam) &
                                            HUN.SpreadTotal == 3.5 ~ pF5_TT_3.5_Away$Under,
                                          (bet_type == "F5 TT" |
                                             bet_type == "F5 Alt TT") &
                                            str_detect(bet_type_full, HomeTeam) &
                                            HUN.SpreadTotal == 3.5 ~ pF5_TT_3.5_Home$Under))

bets3 <- bets2 %>% 
  filter(!is.na(AOY_ProjOdds2) &
           !is.na(HUN_ProjOdds2)) %>% 
  dplyr::mutate(AOY_ProjOdds = (AOY_ProjOdds1 + AOY_ProjOdds2) / 2,
         HUN_ProjOdds = (HUN_ProjOdds1 + HUN_ProjOdds2) / 2,
         AOY.Odds_Diff = AOY_ProjOdds - AOY_ImpliedOdds,
         HUN.Odds_Diff = HUN_ProjOdds - HUN_ImpliedOdds,
         Pick = case_when(AOY.Odds_Diff > HUN.Odds_Diff ~ AwayTeam,
                          TRUE ~ HomeTeam),
         Pick_Odds = case_when(Pick == AwayTeam ~ AOY.Odds,
                               Pick == HomeTeam ~ HUN.Odds),
         Pick_SpreadTotal = case_when(Pick == AwayTeam ~ AOY.SpreadTotal,
                                      Pick == HomeTeam ~ HUN.SpreadTotal),
         Pick_WinProb = case_when(Pick == AwayTeam ~ AOY_ProjOdds,
                                  Pick == HomeTeam ~ HUN_ProjOdds),
         Pick_LoseProb = case_when(Pick == AwayTeam ~ HUN_ProjOdds,
                                   TRUE ~ AOY_ProjOdds),
         Pick_Edge = case_when(Pick == AwayTeam ~ AOY.Odds_Diff,
                               Pick == HomeTeam ~ HUN.Odds_Diff),
         Fract_Odds = (100 / abs(Pick_Odds))^if_else(Pick_Odds < 0, 1, -1),
         Pushable = case_when(Pick_WinProb + Pick_LoseProb < 0.999 ~ 'Y',
                              TRUE ~ 'N'),
         Kelly_Criteria = if_else(Pushable == 'Y',
                                  ((Pick_WinProb / (Pick_WinProb + Pick_LoseProb)) * (Fract_Odds + 1) - 1) / Fract_Odds,
                                  (Pick_WinProb * (Fract_Odds + 1) - 1) / Fract_Odds),
         EV = case_when(Pick_Odds < 0 ~ (10*Pick_WinProb) - ((abs(Pick_Odds)/10)*Pick_LoseProb),
                        TRUE ~ ((Pick_Odds/10*Pick_WinProb) - (10*Pick_LoseProb)))
         ) %>% 
  dplyr::mutate(Pick = case_when(str_detect(bet_type_full, "Total") ~ 
                            case_when(Pick == AwayTeam ~ 'Over',
                                      TRUE ~ 'Under'),
                          str_detect(bet_type, 'RFI') ~ 
                            case_when(Pick == AwayTeam ~ 'Yes',
                                      TRUE ~ 'No'),
                          TRUE ~ Pick),
         Machine_Odds = round(if_else(Pushable == 'Y',
                                      if_else((Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2)) < 0.5,
                                              (100 / (Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2))) - 100,
                                              -1 * (100 * (Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2))) / 
                                                (1 - (Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2)))),
                                      if_else(Pick_WinProb < 0.5,
                                              (100 / Pick_WinProb) - 100,
                                              -1 * (100 * Pick_WinProb) / (1 - Pick_WinProb))),
                              0),
         KC_tier = as.factor(round_any(Kelly_Criteria, 0.05, floor)),
         EV_tier = as.factor(round_any(EV, 1, floor))) %>% 
  filter(!is.na(Pick)) %>% 
  select(#-AwaySP_fullName, -HomeSP_fullName, 
         -(pFG_ML:pF1_TT_0.5_Away), -bet_type_full, -Pushable, bet_type_full) %>% 
  dplyr::mutate(run_timestamp = as.POSIXct(Sys.time()-hours(6)),
         OddsType = case_when(as.Date(substr(run_timestamp,1,10)) == gamedate ~ 'Day Of',
                              as.Date(substr(run_timestamp,1,10)) < as.Date(gamedate) ~ 'Overnight',
                              TRUE ~ 'Other')) %>% 
  dplyr::rename(AwayStartingPitcher = AwaySP_fullName,
         HomeStartingPitcher = HomeSP_fullName)

write.csv(bets3, "Baseball Machine/v3.0/upcoming_bets.csv", row.names = FALSE, na = "")

#### Analyze Performance ####

history <- readRDS("Baseball Machine/v3.0/PicksHistory.rds") %>% 
  bind_rows(bets3) %>% 
  distinct()

saveRDS(history, "Baseball Machine/v3.0/PicksHistory.rds")

history2 <- history %>% 
  inner_join(results %>% 
               filter(doubleHeader == 'N') %>% 
               mutate(officialDate = as.Date(officialDate)),
             by = c("gamedate" = "officialDate",
                    "AwayTeam" = "away_team_name",
                    "HomeTeam" = "home_team_name")) %>% 
  # filter(!paste0(gamedate, AwayTeam, HomeTeam) %in% c('2023-07-17San Francisco GiantsCincinnati Reds','2023-07-18San Francisco GiantsCincinnati Reds',
  #                                                     '2023-05-13New York MetsWashington Nationals','2023-05-03Toronto Blue JaysBoston Red Sox',
  #                                                     '2023-05-19Chicago CubsPhiladelphia Phillies','2023-05-20Baltimore OriolesToronto Blue Jays',
  #                                                     '2023-06-02Los Angeles AngelsHouston Astros','2023-06-13Miami MarlinsSeattle Mariners',
  #                                                     '2023-06-18Philadelphia PhilliesOakland Athletics')) %>% 
  dplyr::mutate(Winner = case_when(bet_type == 'FG ML' ~ 
                              case_when(away_runs_final > home_runs_final ~ AwayTeam,
                                        TRUE ~ HomeTeam),
                            bet_type == 'F5 ML' ~
                              case_when(away_runs_5th > home_runs_5th ~ AwayTeam,
                                        away_runs_5th == home_runs_5th ~ "Push",
                                        TRUE ~ HomeTeam),
                            bet_type %in% c('FG RL', 'FG Alt RL') ~ 
                              case_when(Pick == AwayTeam ~ 
                                          case_when(away_runs_final + Pick_SpreadTotal > home_runs_final ~ AwayTeam,
                                                    away_runs_final + Pick_SpreadTotal == home_runs_final ~ "Push",
                                                    TRUE ~ HomeTeam),
                                        Pick == HomeTeam ~ 
                                          case_when(home_runs_final + Pick_SpreadTotal > away_runs_final ~ HomeTeam,
                                                    home_runs_final + Pick_SpreadTotal == away_runs_final ~ "Push",
                                                    TRUE ~ AwayTeam)),
                            bet_type %in% c('F5 RL', 'F5 Alt RL') ~ 
                              case_when(Pick == AwayTeam ~ 
                                          case_when(away_runs_5th + Pick_SpreadTotal > home_runs_5th ~ AwayTeam,
                                                    away_runs_5th + Pick_SpreadTotal == home_runs_5th ~ "Push",
                                                    TRUE ~ HomeTeam),
                                        Pick == HomeTeam ~ 
                                          case_when(home_runs_5th + Pick_SpreadTotal > away_runs_5th ~ HomeTeam,
                                                    home_runs_5th + Pick_SpreadTotal == away_runs_5th ~ "Push",
                                                    TRUE ~ AwayTeam)),
                            bet_type %in% c('FG Total', 'FG Alt Total') ~
                              case_when(away_runs_final + home_runs_final > Pick_SpreadTotal ~ "Over",
                                        away_runs_final + home_runs_final == Pick_SpreadTotal ~ "Push",
                                        TRUE ~ "Under"),
                            bet_type %in% c('F5 Total', 'F5 Alt Total') ~
                              case_when(away_runs_5th + home_runs_5th > Pick_SpreadTotal ~ "Over",
                                        away_runs_5th + home_runs_5th == Pick_SpreadTotal ~ "Push",
                                        TRUE ~ "Under"),
                            bet_type %in% c('FG TT', 'FG Alt TT') ~
                              case_when(str_detect(bet_type_full, AwayTeam) ~ 
                                          case_when(away_runs_final > Pick_SpreadTotal ~ "Over",
                                                    away_runs_final == Pick_SpreadTotal ~ "Push",
                                                    TRUE ~ "Under"),
                                        str_detect(bet_type_full, HomeTeam) ~ 
                                          case_when(home_runs_final > Pick_SpreadTotal ~ "Over",
                                                    home_runs_final == Pick_SpreadTotal ~ "Push",
                                                    TRUE ~ "Under")),
                            bet_type %in% c('F5 TT', 'F5 Alt TT') ~
                              case_when(str_detect(bet_type_full, AwayTeam) ~ 
                                          case_when(away_runs_5th > Pick_SpreadTotal ~ "Over",
                                                    away_runs_5th == Pick_SpreadTotal ~ "Push",
                                                    TRUE ~ "Under"),
                                        str_detect(bet_type_full, HomeTeam) ~ 
                                          case_when(home_runs_5th > Pick_SpreadTotal ~ "Over",
                                                    home_runs_5th == Pick_SpreadTotal ~ "Push",
                                                    TRUE ~ "Under")),
                            bet_type == 'RFI' ~
                              case_when(away_runs_1st + home_runs_1st > 0 ~ "Yes",
                                        TRUE ~ "No"),
                            bet_type == 'Team RFI' ~
                              case_when(str_detect(bet_type_full, AwayTeam) ~
                                          case_when(away_runs_1st > 0 ~ "Yes",
                                                    TRUE ~ "No"),
                                        str_detect(bet_type_full, HomeTeam) ~
                                          case_when(home_runs_1st > 0 ~ "Yes",
                                                    TRUE ~ "No"))),
                Pick_Correct = if_else(Winner == Pick, 1, 0),
                Units = if_else(Pick_Correct == 1, Fract_Odds, -1),
                Kelly_Bet = Kelly_Criteria * 100,
                Kelly_Profit = Units * Kelly_Bet) %>%
  arrange(desc(run_timestamp)) %>%
  group_by(gamedate, AwayTeam, HomeTeam, bet_type_full, OddsType) %>%
  dplyr::mutate(partition = row_number()) %>% 
  select(-(game_pk:away_errors_final))

saveRDS(history2, "Baseball Machine/v3.0/PicksHistory_Outcomes.rds")

types <- history2 %>%
  ungroup() %>% 
  filter(Kelly_Criteria > 0 &
           Pick_Odds >= -180 &
           Pick_WinProb >= 0.3 &
           partition == 1 &
           OddsType == 'Day Of' &
           !str_detect(bet_type, "RFI") &
           bet_type != "F5 Alt TT") %>% 
  mutate(Units = if_else(Pick_Correct == 1,
                         if_else(Pick_Odds > 0, Pick_Odds / 100, 1),
                         if_else(Pick_Odds > 0, -1, Pick_Odds / 100))) %>% 
  select(gamedate, AwayTeam, HomeTeam, bet_type, Pick_Odds, Pick_WinProb, Pick_LoseProb, Fract_Odds,
         Kelly_Criteria, EV, KC_tier, Winner, Pick_Correct, Units, Kelly_Bet, Kelly_Profit, run_timestamp) %>% 
  dplyr::mutate(WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
                Odds_tier = round_any(Pick_Odds, 10, floor),
                EV_tier = round_any(EV, 1, floor),
                bets = as.integer(1),
                EV_Grade = case_when(EV_tier >= 3 ~ 3,
                                     EV_tier == 2 ~ 2,
                                     EV_tier == 1 ~ 1,
                                     EV_tier < 1 ~ 0),
                KC_Grade = case_when(as.numeric(as.character(KC_tier)) >= 0.35 ~ 4,
                                     as.numeric(as.character(KC_tier)) >= 0.3 ~ 3,
                                     as.numeric(as.character(KC_tier)) >= 0.2 ~ 2.5,
                                     as.numeric(as.character(KC_tier)) == 0.15 ~ 1,
                                     as.numeric(as.character(KC_tier)) < 0.15 ~ 0),
                New_Grade = (KC_Grade + EV_Grade) / 2)

grades <- types %>%
  arrange(gamedate, AwayTeam, HomeTeam, desc(EV)) %>%
  group_by(gamedate, AwayTeam, HomeTeam) %>%
  dplyr::mutate(EV_Rank = row_number()) %>%
  arrange(gamedate, AwayTeam, HomeTeam, desc(Kelly_Criteria)) %>%
  group_by(gamedate, AwayTeam, HomeTeam) %>%
  dplyr::mutate(KC_Rank = row_number(),
                Rank = (KC_Rank + EV_Rank) / 2) %>%
  arrange(gamedate, gamedate, AwayTeam, HomeTeam, Rank) %>%
  dplyr::mutate(Final_Rank = row_number()) %>%
  filter(Final_Rank == 1) %>%
  ungroup() %>% 
  dplyr::mutate(`Bet Grade` = case_when(New_Grade > 3 ~ 'A+',
                                        New_Grade >= 2.5 ~ 'A',
                                        New_Grade >= 1.75 ~ 'B',
                                        # New_Grade >= 1 ~ 'C',
                                        New_Grade < 1.75 ~ 'C'),
                `Bet Grade` = case_when(Kelly_Criteria >= 0.4 ~ 'C',
                                        TRUE ~ `Bet Grade`),
                `Graded Risk` = case_when(`Bet Grade` == 'A+' ~ 2,
                                          `Bet Grade` == 'A' ~ 1,
                                          `Bet Grade` == 'B' ~ 0,
                                          `Bet Grade` == 'C' ~ 0,
                                          `Bet Grade` == 'D' ~ 0),
                `Graded Profit` = Units*`Graded Risk`,
                `Bet Grade` = factor(`Bet Grade`, levels = c('A+', 'A', 'B', 'C', 'D'))) %>% 
  filter(Winner != "Push")

#### Final Email Tables ####

email_table_1 <- grades %>%
  group_by(`Bet Grade`,
           `Suggested Wager` = case_when(`Bet Grade` == 'A+' ~ '2 units',
                                         `Bet Grade` == 'A' ~ '1 unit',
                                         # `Bet Grade` == 'B' ~ '0 unit',
                                         # `Bet Grade` == 'C' ~ '0 unit',
                                         TRUE ~ 'No bet')) %>%
  dplyr::summarise(`Hit Rate` = mean(Pick_Correct),
                   `Average Implied Odds` = mean(if_else(Pick_Odds > 0, 100 / (Pick_Odds + 100), abs(Pick_Odds) / (abs(Pick_Odds) + 100))),
                   `Average Odds` = if_else(`Average Implied Odds` < 0.5,
                                            (100 / `Average Implied Odds`) - 100,
                                            -1 * (100 * `Average Implied Odds`) / (1 - `Average Implied Odds`)),
                   Bets = sum(bets),
                   `Profit: 1 Unit Wagers` = sum(Units),
                   `Profit: Suggested Wagers` = sum(`Graded Profit`)) %>% 
  dplyr::mutate(ROI = `Profit: 1 Unit Wagers` / Bets) %>%
  dplyr::mutate(`Hit Rate` = paste0(round_any(`Hit Rate`*100, 1), '%'),
                `Average Odds` = as.integer(round_any(`Average Odds`,1)),
                `Average Implied Odds` = paste0(round_any(`Average Implied Odds`*100, 1), '%'),
                Bets = formatC(Bets, format="d", big.mark=","),
                `Profit: 1 Unit Wagers` = paste0(round_any(`Profit: 1 Unit Wagers`, 0.1), ' units'),
                `Profit: Suggested Wagers` = paste0(round_any(`Profit: Suggested Wagers`, 0.1), ' units'),
                ROI = paste0(round_any(ROI*100, 0.01), '%')) %>%
  print()

df_html_1 <- print(xtable(email_table_1), type = "html", print.results = FALSE)

plot_data <- grades %>% 
  filter(`Bet Grade` %in% c('A+','A')) %>%
  select(gamedate, Units, `Graded Profit`) %>% 
  dplyr::rename(`Profit: 1 Unit Wagers` = Units,
         `Profit: Suggested Wagers` = `Graded Profit`) %>% 
  melt("gamedate", c("Profit: 1 Unit Wagers", "Profit: Suggested Wagers")) %>% 
  group_by(gamedate, variable) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  dplyr::mutate(cumulative_value = cumsum(value))

plot <- ggplot(plot_data) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Game Date",
    y = "Cumulative Profit (Units)",
    color = ""
  ) +
  theme(legend.position = "bottom")

plot_html <- blastula::add_ggplot(plot_object = plot)

bets_table <- bets3 %>% 
  filter(Kelly_Criteria > 0 &
           Pick_Odds >= -180 &
           Pick_WinProb > 0.3 &
           !str_detect(bet_type, "RFI") &
           bet_type != "F5 Alt TT" &
           gamedate == Sys.Date()) %>% 
  arrange(gamedate, AwayTeam, HomeTeam, desc(EV)) %>% 
  group_by(gamedate, AwayTeam, HomeTeam) %>% 
  dplyr::mutate(EV_Rank = row_number()) %>% 
  arrange(gamedate, AwayTeam, HomeTeam, desc(Kelly_Criteria)) %>% 
  group_by(gamedate, AwayTeam, HomeTeam) %>% 
  dplyr::mutate(KC_Rank = row_number(),
                Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, gamedate, AwayTeam, HomeTeam, Rank) %>% 
  dplyr::mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1) %>%
  arrange(gamedate, desc(Kelly_Criteria)) %>% 
  ungroup() %>% 
  dplyr::mutate(Pick = case_when(is.na(Pick_SpreadTotal) | Pick_SpreadTotal == 0 ~ paste0(Pick),
                                 str_detect(bet_type, "RL") & Pick_SpreadTotal > 0 ~ paste0(Pick, " +", Pick_SpreadTotal),
                                 str_detect(bet_type_full, HomeTeam) ~ paste0(HomeTeam, " ", Pick, " ", Pick_SpreadTotal),
                                 str_detect(bet_type_full, AwayTeam) ~ paste0(AwayTeam, " ", Pick, " ", Pick_SpreadTotal),
                                 TRUE ~ paste0(Pick, " ", Pick_SpreadTotal))) %>% 
  select(gamedate, AwayTeam, HomeTeam, bet_type_full, Pick, Pick_Odds, Machine_Odds, KC_tier, EV_tier) %>% 
  dplyr::mutate(gamedate = as.character(gamedate)) %>% 
  dplyr::rename(`Game Date` = gamedate,
                `Home Team` = HomeTeam,
                `Away Team` = AwayTeam,
                `Bet Type` = bet_type_full,
                `Current Pick Odds` = Pick_Odds,
                `Odds Should Be` = Machine_Odds,
                KC = KC_tier,
                EV = EV_tier) %>% 
  dplyr::mutate(`Current Pick Odds` = as.integer(`Current Pick Odds`),
                `Odds Should Be` = as.integer(`Odds Should Be`),
                EV = as.integer(as.character(EV)),
                EV_Grade = case_when(EV >= 3 ~ 3,
                                     EV == 2 ~ 2,
                                     EV == 1 ~ 1,
                                     EV < 1 ~ 0),
                KC_Grade = case_when(as.numeric(as.character(KC)) >= 0.35 ~ 4,
                                     as.numeric(as.character(KC)) >= 0.3 ~ 3,
                                     as.numeric(as.character(KC)) >= 0.2 ~ 2.5,
                                     as.numeric(as.character(KC)) == 0.15 ~ 1,
                                     as.numeric(as.character(KC)) < 0.15 ~ 0),
                New_Grade = (KC_Grade + EV_Grade) / 2,
                `Bet Grade` = case_when(New_Grade > 3 ~ 'A+',
                                        New_Grade >= 2.5 ~ 'A',
                                        New_Grade >= 1.75 ~ 'B',
                                        # New_Grade >= 1 ~ 'C',
                                        New_Grade < 1.75 ~ 'C'),
                `Bet Grade` = case_when(as.numeric(as.character(KC)) >= 0.4 ~ 'C',
                                        TRUE ~ `Bet Grade`),
                `Bet Grade` = factor(`Bet Grade`, levels = c('A+', 'A', 'B', 'C', 'D'))) %>% 
  select(-EV_Grade, -KC_Grade, -New_Grade) %>% 
  arrange(`Game Date`, `Bet Grade`, desc(KC)) %>%
  select(-KC, -EV)

df_html_bets <- if_else(nrow(bets_table)==0,
                        "<b>At the odds currently available, no bets are recommended</b>",
                        print(xtable(bets_table), type = "html", print.results = FALSE))

ITTs <- bets2 %>% 
  filter(#trimws(tolower(AwayStartingPitcher)) == trimws(tolower(AwaySP_fullName)) &
           # trimws(tolower(HomeStartingPitcher)) == trimws(tolower(HomeSP_fullName)) &
           !is.na(AOY_ProjOdds2) &
           !is.na(HUN_ProjOdds2) &
           gamedate == Sys.Date()) %>% 
  distinct(AwayTeam, HomeTeam, AwaySP_fullName, HomeSP_fullName,# AwayStartingPitcher, HomeStartingPitcher,
           Away_pred_FG, Home_pred_FG) %>% 
  select(AwayTeam, HomeTeam, AwaySP_fullName, HomeSP_fullName,# AwayStartingPitcher, HomeStartingPitcher,
         Away_pred_FG, Home_pred_FG) %>% 
  dplyr::rename(`Away Runs` = Away_pred_FG,
                `Home Runs` = Home_pred_FG) %>% 
  dplyr::rename(AwayStartingPitcher = AwaySP_fullName,
                HomeStartingPitcher = HomeSP_fullName)

itts_html <- print(xtable(ITTs), type = "html", print.results = FALSE)
  

Outlook <- COMCreate("Outlook.Application")

Email = Outlook$CreateItem(0)
Email[["to"]] = "dnolen@smu.edu"
Email[["bcc"]] = paste("jamesorler@gmail.com",
                       "asnolen@crimson.ua.edu",
                       "jamestodd425@gmail.com",
                       "jordanreticker@gmail.com",
                       "brentcaminiti@gmail.com",
                       "dougmyers4987@gmail.com",
                       "ralphmstudley@gmail.com",
                       "johnpavese@gmail.com",
                       # "amishra1293@gmail.com",
                       "rfinstra@gmail.com",
                       "james_bueck@yahoo.com",
                       "mattgodelman@gmail.com",
                       "dnassar15@gmail.com",
                       "vtloncto@gmail.com",
                       "bcap15@yahoo.com",
                       "mshin0630@gmail.com",
                       "chrisjhogan@gmail.com",
                       "jasonarata@yahoo.com",
                       sep = ";",
                       collapse = NULL)
Email[["subject"]] = paste0("Baseball Machine Picks: ", Sys.Date())
Email[["HTMLbody"]] = sprintf("
The Basebal Machine is now up and running! The Machine will suggest one bet per game, although I wouldn't actually suggest betting on every game. Each bet is given a grade. Last season had decent results with the A+ and A grades, so I would stick to those. We will refine our approach as we see how The Machine performs, so this might change. Here are the results so far grouped by bet grade:
</p><br></p>
%s
</p><br></p>
Below are the top picks for each game today, along with the predicted team totals for each team. Good luck! (FYI - If a game is not listed below it means The Machine was not able to make predictions on that game. Usually this is because odds are not currently posted for that game or the starting pitcher for either team has not been announced.)
</p><br></p>
%s
</p><br></p>
%s
</p><br></p>
Below are the historical results for both a flat betting strategy (1 unit wagers), and the suggested wager sizes from the bet grades above:
</p><br></p>
%s
</p><br></p>
", df_html_1, df_html_bets, itts_html, plot_html)
Email[["attachments"]]$Add("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine/Baseball Machine/v3.0/upcoming_bets.csv")

Email$Send()

write.csv(pitchers_s2d_update, paste0("Baseball Machine/Daily Files/",season,"/pitchers_s2d_",season,".csv"), row.names = FALSE)
write.csv(team_batting_L7_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_L7_",season,".csv"), row.names = FALSE)
write.csv(team_batting_L14_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_L14_",season,".csv"), row.names = FALSE)
write.csv(team_batting_L30_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_L30_",season,".csv"), row.names = FALSE)
write.csv(team_batting_s2d_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_s2d_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_L7_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L7_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_L14_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L14_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_L30_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L30_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_s2d_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_s2d_",season,".csv"), row.names = FALSE)
write.csv(probables_update, paste0("Baseball Machine/Daily Files/",season,"/starting_pitchers_",season,".csv"), row.names = FALSE)
write.csv(rosters_update, paste0("Baseball Machine/Daily Files/",season,"/daily_rosters_",season,".csv"), row.names = FALSE)
write.csv(scores_update, paste0("Baseball Machine/Daily Files/",season,"/game_scores_",season,".csv"), row.names = FALSE)
write.csv(pks_update # %>% select(-dates)
          , paste0("Baseball Machine/Daily Files/",season,"/game_pks_",season,".csv"), row.names = FALSE)


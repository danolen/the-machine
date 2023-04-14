### The Machine v3.0
### Daily Bets
library(tidyverse)
source("dannyverse/dannyverse.R")

#### Setup ####

## Season start and end date
season = 2023
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
  mutate(officialDate = as.Date(officialDate))
tmrw_games <- get_mlb_daily_scores(Sys.Date()+1, Sys.Date()+1, file_type = "scores") %>% 
  distinct(game_pk, home_team_name, away_team_name, home_team_season, home_team_league_name,
           home_team_division_name, away_team_league_name, away_team_division_name) %>% 
  inner_join(get_mlb_daily_scores(Sys.Date()+1, Sys.Date()+1, file_type = "pks") %>% 
               filter(seriesDescription == 'Regular Season' &
                        scheduledInnings == 9) %>% 
               select(game_pk, officialDate, doubleHeader, gameNumber, dayNight, venue.name)) %>% 
  select(-game_pk) %>% 
  mutate(officialDate = as.Date(officialDate))

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
starting_pitchers <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/starting_pitchers_",season,".csv"))
starting_pitchers_max_date <- max(as.Date(starting_pitchers$game_date))
probables <- get_probable_pitchers(starting_pitchers_max_date+1, Sys.Date())
tmrw_probables <- get_probable_pitchers(Sys.Date()+1, Sys.Date()+1)
daily_rosters <- read.csv(paste0("Baseball Machine/Daily Files/",season,"/daily_rosters_",season,".csv"))
daily_rosters_max_date <- max(as.Date(daily_rosters$date))
rosters <- get_daily_rosters(daily_rosters_max_date+1, Sys.Date())
tmrw_rosters <- get_daily_rosters(Sys.Date()+1, Sys.Date()+1)

## Bovada Odds
bovada_odds <- get_bovada_odds("baseball", "mlb") %>% 
  mutate(AwayStartingPitcher = case_when(str_detect(AwayStartingPitcher, "Shohei Ohtani") ~ "Shohei Ohtani",
                                         TRUE ~ AwayStartingPitcher),
         HomeStartingPitcher = case_when(str_detect(HomeStartingPitcher, "Shohei Ohtani") ~ "Shohei Ohtani",
                                         TRUE ~ HomeStartingPitcher))

## Overwrite files with updates
pitchers_s2d_update <- pitchers_s2d %>% 
  mutate(Date = as.Date(Date)) %>% 
  union(pitchers_s2d_today)
team_batting_L7_update <- team_batting_L7 %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_batting_L7_today)
team_batting_L14_update <- team_batting_L14 %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_batting_L14_today)
team_batting_L30_update <- team_batting_L30 %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_batting_L30_today)
team_batting_s2d_update <- team_batting_s2d %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_batting_s2d_today)
team_bullpen_L7_update <- team_bullpen_L7 %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_L7_today)
team_bullpen_L14_update <- team_bullpen_L14 %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_L14_today)
team_bullpen_L30_update <- team_bullpen_L30 %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_L30_today)
team_bullpen_s2d_update <- team_bullpen_s2d %>%  
  mutate(Date = as.Date(Date)) %>% 
  union(team_bullpen_s2d_today)
write.csv(pitchers_s2d_update, paste0("Baseball Machine/Daily Files/",season,"/pitchers_s2d_",season,".csv"), row.names = FALSE)
write.csv(team_batting_L7_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_L7_",season,".csv"), row.names = FALSE)
write.csv(team_batting_L14_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_L14_",season,".csv"), row.names = FALSE)
write.csv(team_batting_L30_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_L30_",season,".csv"), row.names = FALSE)
write.csv(team_batting_s2d_update, paste0("Baseball Machine/Daily Files/",season,"/team_batting_s2d_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_L7_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L7_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_L14_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L14_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_L30_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_L30_",season,".csv"), row.names = FALSE)
write.csv(team_bullpen_s2d_update, paste0("Baseball Machine/Daily Files/",season,"/team_bullpen_s2d_",season,".csv"), row.names = FALSE)

pks_update <- game_pks %>% 
  mutate(season = as.character(season),
         seasonDisplay = as.character(seasonDisplay),
         status.codedGameState = as.character(status.codedGameState),
         status.statusCode = as.character(status.statusCode),
         status.abstractGameCode = as.character(status.abstractGameCode),
         teams.away.leagueRecord.pct = as.character(teams.away.leagueRecord.pct),
         teams.home.leagueRecord.pct = as.character(teams.home.leagueRecord.pct)) %>% 
  bind_rows(pks)
scores_update <- game_scores %>% 
  mutate(home_team_id = as.character(home_team_id),
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
  bind_rows(scores)
write.csv(pks_update, paste0("Baseball Machine/Daily Files/",season,"/game_pks_",season,".csv"), row.names = FALSE)
write.csv(scores_update, paste0("Baseball Machine/Daily Files/",season,"/game_scores_",season,".csv"), row.names = FALSE)

probables_update <- starting_pitchers %>% 
  union(probables)
rosters_update <- daily_rosters %>% 
  union(rosters)
write.csv(probables_update, paste0("Baseball Machine/Daily Files/",season,"/starting_pitchers_",season,".csv"), row.names = FALSE)
write.csv(rosters_update, paste0("Baseball Machine/Daily Files/",season,"/daily_rosters_",season,".csv"), row.names = FALSE)


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
  mutate(home_runs = cumsum(home_runs),
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
  mutate(Team = Full.Name22) %>% 
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
  mutate(Team = Full.Name22) %>% 
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
  mutate(Name = case_when(Name == "Luis Garcia" & Team == "HOU" ~ "Luis Garcia (HOU)",
                          # Name == "Luis Castillo" & Team == "DET" ~ "Luis Castillo (DET)",
                          # Name == "Luis Ortiz" & Team == "SFG" ~ "Luis Ortiz (SFG)",
                          # Name == "Hyun-Jin Ryu" ~ "Hyun Jin Ryu",
                          TRUE ~ Name))

PECOTA_pitching_23 <- readxl::read_xlsx(paste0("Baseball Machine/PECOTA/",season,"/pecota2023_pitching_mar29.xlsx"), sheet = "50") %>% 
  select(mlbid, name, ip, warp) %>% 
  mutate(WARP200 = (warp/ip)*200)
PECOTA_hitting_23 <- readxl::read_xlsx(paste0("Baseball Machine/PECOTA/",season,"/pecota2023_hitting_mar29.xlsx"), sheet = "50") %>% 
  select(mlbid, name, pa, warp) %>% 
  mutate(WARP600 = (as.numeric(warp)/as.numeric(pa))*600)

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
              mutate(game_date = as.Date(game_date)),
            by = c("AwayTeam" = "team", "gamedate" = "game_date")) %>% 
  left_join(probables %>% 
              bind_rows(tmrw_probables) %>% 
              select(game_date, fullName, id, team) %>%
              mutate(game_date = as.Date(game_date)),
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
              mutate(date = as.Date(date)),
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
              mutate(date = as.Date(date)),
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
  mutate(WARP200_HomeSP = case_when(!is.nan(WARP200_HomeSP) ~ WARP200_HomeSP),
         WARP200_AwaySP = case_when(!is.nan(WARP200_AwaySP) ~ WARP200_AwaySP)) %>% 
  mutate(IP_AwaySP = as.integer(IP_AwaySP) + (IP_AwaySP %% 1 * 3.33),
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
  mutate(WAR200_AwaySP = (WAR_AwaySP / IP_AwaySP) * 200,
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
  mutate(month = lubridate::month(gamedate, label = TRUE, abbr = FALSE),
         doubleHeader = case_when(doubleHeader == 'S' ~ 'Y',
                                  TRUE ~ doubleHeader)) %>% 
  mutate(home_team_season = as.numeric(home_team_season))

upcoming_games_tomorrow <- bovada_odds %>% 
  filter(gamedate == Sys.Date()+1) %>% 
  mutate(yesterday_date = gamedate - 1) %>% 
  left_join(todays_games %>% 
              bind_rows(tmrw_games),
            by = c("AwayTeam" = "away_team_name",
                   "HomeTeam" = "home_team_name",
                   "gamedate" = "officialDate")) %>% 
  left_join(probables %>% 
              bind_rows(tmrw_probables) %>% 
              select(game_date, fullName, id, team) %>% 
              mutate(game_date = as.Date(game_date)),
            by = c("AwayTeam" = "team", "gamedate" = "game_date")) %>% 
  left_join(probables %>% 
              bind_rows(tmrw_probables) %>% 
              select(game_date, fullName, id, team) %>%
              mutate(game_date = as.Date(game_date)),
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
              mutate(date = as.Date(date)),
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
              mutate(date = as.Date(date)),
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
  mutate(WARP200_HomeSP = case_when(!is.nan(WARP200_HomeSP) ~ WARP200_HomeSP),
         WARP200_AwaySP = case_when(!is.nan(WARP200_AwaySP) ~ WARP200_AwaySP)) %>% 
  mutate(IP_AwaySP = as.integer(IP_AwaySP) + (IP_AwaySP %% 1 * 3.33),
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
  mutate(WAR200_AwaySP = (WAR_AwaySP / IP_AwaySP) * 200,
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
  mutate(month = lubridate::month(gamedate, label = TRUE, abbr = FALSE),
         doubleHeader = case_when(doubleHeader == 'S' ~ 'Y',
                                  TRUE ~ doubleHeader)) %>% 
  mutate(home_team_season = as.numeric(home_team_season))

upcoming_games <- upcoming_games_today %>% 
  bind_rows(upcoming_games_tomorrow)

pred_data_home <- upcoming_games %>% 
  ungroup() %>% 
  select(gamedate, AwayTeam, HomeTeam, AwayStartingPitcher, HomeStartingPitcher, AwaySP_fullName, HomeSP_fullName,
         home_team_season, month, home_team_league_name, home_team_division_name,
         doubleHeader, gameNumber, dayNight, venue.name, contains('_AwaySP'), contains('_HomeBatters'),
         -Def_L7_HomeBatters, -Def_L14_HomeBatters, -Def_L30_HomeBatters, -Def_HomeBatters,
         Def_L7_AwayBatters, Def_L14_AwayBatters, Def_L30_AwayBatters, Def_AwayBatters,
         contains('_AwayBullpen')) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Home')

pred_data_away <- upcoming_games %>% 
  ungroup() %>% 
  select(gamedate, AwayTeam, HomeTeam, AwayStartingPitcher, HomeStartingPitcher, AwaySP_fullName, HomeSP_fullName,
         home_team_season, month, away_team_league_name, away_team_division_name,
         doubleHeader, gameNumber, dayNight, venue.name, contains('_HomeSP'), contains('_AwayBatters'),
         Def_L7_HomeBatters, Def_L14_HomeBatters, Def_L30_HomeBatters, Def_HomeBatters,
         -Def_L7_AwayBatters, -Def_L14_AwayBatters, -Def_L30_AwayBatters, -Def_AwayBatters,
         contains('_HomeBullpen')) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("away_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Away')
  
pred_data <- pred_data_home %>% 
  bind_rows(pred_data_away) %>% 
  filter(!is.na(IPSP) &
           !is.na(PA_L7Batters) &
           !is.na(IP_L7Bullpen) &
           !is.na(PA_L30Batters) &
           !is.na(IPBullpen)) %>% 
  replace(is.na(.), 0) %>% 
  distinct() %>% 
  mutate(Team = case_when(home_or_away == "Home" ~ HomeTeam,
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
  mutate(pR_FG = (pR_FG_gbm + pR_FG_cub + pR_FG_rf + pR_FG_pls) / 4,
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
                   "HomeSP_fullName", "AwayStartingPitcher", "HomeStartingPitcher"),
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
  mutate(pFG_ML = (pFG_ML_gbm + pFG_ML_pls + pFG_ML_xgb) / 3,
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
  select(gamedate:HomeStartingPitcher, pFG_ML:pF1_Total_0.5)

FinalPreds <- doubles2 %>% 
  left_join(singles2,
            by = c("gamedate", "AwayTeam" = "AwayTeam_Home", 
                   "HomeTeam" = "HomeTeam_Home", 
                   "AwayStartingPitcher", "HomeStartingPitcher"))

upcoming_df <- upcoming_games %>% 
  select(gamedate:HomeStartingPitcher, AwaySP_fullName, HomeSP_fullName, bet_type:HUN.SpreadTotal) %>% 
  left_join(FinalPreds)

bets <- upcoming_df %>% 
  mutate(AOY_ImpliedOdds = if_else(AOY.Odds > 0, 100 / (AOY.Odds + 100), abs(AOY.Odds) / (abs(AOY.Odds) + 100)),
         HUN_ImpliedOdds = if_else(HUN.Odds > 0, 100 / (HUN.Odds + 100), abs(HUN.Odds) / (abs(HUN.Odds) + 100))) %>% 
  dplyr::rename(Home_pred_FG = pR_FG_Home,
         Away_pred_FG = pR_FG_Away,
         Home_pred_F5 = pR_F5_Home,
         Away_pred_F5 = pR_F5_Away,
         Home_pred_F1 = pR_F1_Home,
         Away_pred_F1 = pR_F1_Away) %>% 
  mutate(bet_type_full = bet_type,
         bet_type = case_when(bet_type == "Moneyline - Game" ~ "FG ML",
                              bet_type == "Moneyline - 5 Inning Line" ~ "F5 ML",
                              bet_type == "Runline - Game" ~ "FG RL",
                              bet_type == "Runline - 5 Inning Line" ~ "F5 RL",
                              bet_type == "Total - Game" ~ "FG Total",
                              bet_type == "Total - 5 Inning Line" ~ "F5 Total",
                              bet_type == "Will there be a run scored in the 1st inning - Game" ~ "RFI",
                              bet_type == "Alternate Runline - Game - 1.5" ~ "FG Alt RL",
                              bet_type == "Alternate Runline - Game - 2.5" ~ "FG Alt RL",
                              bet_type == "Alternate Runline - 5 Inning Line - 0.5" ~ "FG Alt RL",
                              bet_type == "Alternate Runline - 5 Inning Line - 1.5" ~ "FG Alt RL",
                              str_detect(bet_type, )
                              
                              bet_type == "Goal Spread" ~ "Spread",
                              grepl("Alternate Spread", bet_type) ~ "Alt Spread",
                              bet_type == "Total" ~ "Total",
                              grepl("Alternate Total", bet_type) ~ "Alt Total",
                              bet_type == "Both Teams To Score" ~ "BTTS",
                              grepl("Total Goals O/U", bet_type) & (Pick_Odds > -250) & (Pick_Odds < 210) ~ "TT",
                              grepl("Total Goals O/U", bet_type) & ((Pick_Odds <= -250) | (Pick_Odds >= 210)) ~ "Alt TT",
                              bet_type == "Draw No Bet" ~ "Draw No Bet",
                              TRUE ~ "Other"))

bets2 <- bets %>% 
  mutate(bet_type_full)


### The Machine v2.0
### Daily Bets

library("pacman")
p_load("jsonlite", "rlist", "stringr", "tidyjson", "reshape2", "RDCOMClient", "zoo",
       "h2o", "bit64", "readr", "DataCombine", "readxl", "rvest", "xml2", "janitor", 
       "lubridate", "plyr", "xtable", "tidyverse")
#install.packages("devtools")
#devtools::install_github("beanumber/mlbgm")
library("mlbgm")

setwd("C:/Users/danie/Desktop/Baseball Stuff/MLB Team Projections")

## Daily Probable Pitchers
# Scrape Probable Starting Pitchers from FanGraphs for today, tomorrow, and the next day

i <- 1
startdate <- as.Date(Sys.Date())
enddate <- startdate + 1
urls <- list()
while (startdate <= enddate) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=8&season=2021&month=0&season1=2021&ind=1&team=0,to&rost=1&age=0&filter=&players=p',startdate,'&startdate=&enddate=&page=1_60')
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1) %>%
    select(Name, Team)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

prob_sp <- ldply(tbl, data.frame)

# Replace team names with full names

team_names <- read.csv("team_names.csv")

prob_sp <- prob_sp %>%
  FindReplace(Var = "Team", replaceData = team_names,
              from = "BR", to = "FG")

## Get odds from Bovada API

url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/baseball/mlb-spring-training"

bovada_odds <- fromJSON(url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("AwayTeam", "HomeTeam"), " @ ") %>%
  select(gamedate, AwayTeam, HomeTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  select(gamedate, AwayTeam, HomeTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE) %>%
  mutate(bet_type = paste0(description, " - ", period$description)) %>%
  select(gamedate, AwayTeam, HomeTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "A" | type == "O", "AO", "HU")) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, AwayTeam, HomeTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  filter(bet_type %in% c("Moneyline - Game", "Runline - Game", "Total - Game",
                         "Moneyline - 5 Inning Line", "Runline - 5 Inning Line", "Total - 5 Inning Line",
                         "Spread - Game", "Total Runs O/U - Game", "Total Runs O/U - 5 Inning Line",
                         paste0("Total Runs O/U - ", AwayTeam, " - Game"), paste0("Total Runs O/U - ", HomeTeam, " - Game"))) %>%
  mutate(bet_type = if_else(bet_type == "Spread - Game", 
                            paste0("Alternate Runline - Game - ", abs(as.numeric(SpreadTotal))), 
                            bet_type)) %>%
  filter(bet_type %in% c("Moneyline - Game", "Runline - Game", "Total - Game",
                         "Moneyline - 5 Inning Line", "Runline - 5 Inning Line", "Total - 5 Inning Line",
                         "Alternate Runline - Game - 1.5",
                         "Total Runs O/U - Game", "Total Runs O/U - 5 Inning Line",
                         paste0("Total Runs O/U - ", AwayTeam, " - Game"), paste0("Total Runs O/U - ", HomeTeam, " - Game"))) %>%
  mutate(bet_type = if_else(bet_type == "Total Runs O/U - Game", 
                            paste0("Alternate Total - Game - ", abs(as.numeric(SpreadTotal))), 
                            if_else(bet_type == "Total Runs O/U - 5 Inning Line",
                                    paste0("Alternate Total - 5 Inning Line - ", abs(as.numeric(SpreadTotal))),
                                    if_else(bet_type == paste0("Total Runs O/U - ", AwayTeam, " - Game"),
                                            paste0("Team Total - ", AwayTeam, " - ", abs(as.numeric(SpreadTotal))),
                                            if_else(bet_type == paste0("Total Runs O/U - ", HomeTeam, " - Game"),
                                                    paste0("Team Total - ", HomeTeam, " - ", abs(as.numeric(SpreadTotal))),
                                                    bet_type))))) %>%
  melt(id.vars = c("gamedate", "AwayTeam", "HomeTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + AwayTeam + HomeTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AO.Odds = round(AO.Odds, 0),
         HU.Odds = round(HU.Odds, 0)) %>%
  filter((AO.Odds >= 100 | AO.Odds <= -100) & (HU.Odds >= 100 | HU.Odds <= -100))

# Replace team names

bovada_odds <- bovada_odds %>%
  FindReplace(Var = "AwayTeam", replaceData = team_names,
              from = "Full.Name", to = "FG") %>%
  FindReplace(Var = "HomeTeam", replaceData = team_names,
              from = "Full.Name", to = "FG")

## Join SPs to team odds

bovada_odds <- left_join(bovada_odds, prob_sp, by = c("AwayTeam" = "Team", "gamedate" = "Date")) %>%
  left_join(prob_sp, by = c("HomeTeam" = "Team", "gamedate" = "Date"))
colnames(bovada_odds)[9:10] <- c("AwaySP", "HomeSP")
bovada_odds <- select(bovada_odds, colnames(bovada_odds)[1:2], AwaySP, colnames(bovada_odds)[3], HomeSP, colnames(bovada_odds)[4:8])

## Bring in Model Predictors

# Last 7 days team batting stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-26")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2021&month=1000&season1=2021&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   startdate <- startdate + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_batting_L7 <- ldply(tbl, data.frame)
# clms <- c(7,8,21:28)
# for (i in 1:length(clms)) {
#   team_batting_L7[,clms[i]] <- substr(team_batting_L7[,clms[i]], 1, nchar(team_batting_L7[,clms[i]])-1)
# }
# 
# team_batting_L7[,2:28] <- lapply(team_batting_L7[,2:28], as.numeric)
# team_batting_L7[,c(7,8,21:28)] <- team_batting_L7[,c(7,8,21:28)] / 100
# 
# write.csv(team_batting_L7, "team_batting_L7_2021.csv", row.names = FALSE)

team_batting_L7 <- read.csv("team_batting_L7_2021.csv")
team_batting_L7$Date <- as.Date(team_batting_L7$Date)

i <- 1
startdate <- as.Date(Sys.Date()-7)
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2021&month=1000&season1=2021&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_L7_today <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L7_today[,clms[i]] <- substr(team_batting_L7_today[,clms[i]], 1, nchar(team_batting_L7_today[,clms[i]])-1)
}

team_batting_L7_today[,2:28] <- lapply(team_batting_L7_today[,2:28], as.numeric)
team_batting_L7_today[,c(7,8,21:28)] <- team_batting_L7_today[,c(7,8,21:28)] / 100

team_batting_L7 <- filter(team_batting_L7, Date < Sys.Date()) %>%
  union(team_batting_L7_today)

write.csv(team_batting_L7, "team_batting_L7_2021.csv", row.names = FALSE)

# Last 14 days team batting stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-19")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2020&month=1000&season1=2020&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   startdate <- startdate + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_batting_L14 <- ldply(tbl, data.frame)
# clms <- c(7,8,21:28)
# for (i in 1:length(clms)) {
#   team_batting_L14[,clms[i]] <- substr(team_batting_L14[,clms[i]], 1, nchar(team_batting_L14[,clms[i]])-1)
# }
# 
# team_batting_L14[,2:28] <- lapply(team_batting_L14[,2:28], as.numeric)
# team_batting_L14[,c(7,8,21:28)] <- team_batting_L14[,c(7,8,21:28)] / 100
# 
# write.csv(team_batting_L14, "team_batting_L14_2021.csv", row.names = FALSE)

team_batting_L14 <- read.csv("team_batting_L14_2021.csv")
team_batting_L14$Date <- as.Date(team_batting_L14$Date)

i <- 1
startdate <- as.Date(Sys.Date()-14)
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2021&month=1000&season1=2021&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_L14_today <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L14_today[,clms[i]] <- substr(team_batting_L14_today[,clms[i]], 1, nchar(team_batting_L14_today[,clms[i]])-1)
}

team_batting_L14_today[,2:28] <- lapply(team_batting_L14_today[,2:28], as.numeric)
team_batting_L14_today[,c(7,8,21:28)] <- team_batting_L14_today[,c(7,8,21:28)] / 100

team_batting_L14 <- filter(team_batting_L14, Date < Sys.Date()) %>%
  union(team_batting_L14_today)

write.csv(team_batting_L14, "team_batting_L14_2021.csv", row.names = FALSE)

# Last 30 days team batting stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-03")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2020&month=1000&season1=2020&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   startdate <- startdate + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_batting_L30 <- ldply(tbl, data.frame)
# clms <- c(7,8,21:28)
# for (i in 1:length(clms)) {
#   team_batting_L30[,clms[i]] <- substr(team_batting_L30[,clms[i]], 1, nchar(team_batting_L30[,clms[i]])-1)
# }
# 
# team_batting_L30[,2:28] <- lapply(team_batting_L30[,2:28], as.numeric)
# team_batting_L30[,c(7,8,21:28)] <- team_batting_L30[,c(7,8,21:28)] / 100
# 
# write.csv(team_batting_L30, "team_batting_L30_2021.csv", row.names = FALSE)

team_batting_L30 <- read.csv("team_batting_L30_2021.csv")
team_batting_L30$Date <- as.Date(team_batting_L30$Date)

i <- 1
startdate <- as.Date(Sys.Date()-30)
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2021&month=1000&season1=2021&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_L30_today <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L30_today[,clms[i]] <- substr(team_batting_L30_today[,clms[i]], 1, nchar(team_batting_L30_today[,clms[i]])-1)
}

team_batting_L30_today[,2:28] <- lapply(team_batting_L30_today[,2:28], as.numeric)
team_batting_L30_today[,c(7,8,21:28)] <- team_batting_L30_today[,c(7,8,21:28)] / 100

team_batting_L30 <- filter(team_batting_L30, Date < Sys.Date()) %>%
  union(team_batting_L30_today)

write.csv(team_batting_L30, "team_batting_L30_2021.csv", row.names = FALSE)

# Season-to-date team batting stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-31")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2020&month=1000&season1=2020&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_batting_s2d <- ldply(tbl, data.frame)
# clms <- c(7,8,21:28)
# for (i in 1:length(clms)) {
#   team_batting_s2d[,clms[i]] <- substr(team_batting_s2d[,clms[i]], 1, nchar(team_batting_s2d[,clms[i]])-1)
# }
# 
# team_batting_s2d[,2:28] <- lapply(team_batting_s2d[,2:28], as.numeric)
# team_batting_s2d[,c(7,8,21:28)] <- team_batting_s2d[,c(7,8,21:28)] / 100
# 
# write.csv(team_batting_s2d, "team_batting_s2d_2021.csv", row.names = FALSE)

team_batting_s2d <- read.csv("team_batting_s2d_2021.csv")
team_batting_s2d$Date <- as.Date(team_batting_s2d$Date)

i <- 1
startdate <- as.Date("2021-03-31")
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2020&month=1000&season1=2020&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_s2d_today <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_s2d_today[,clms[i]] <- substr(team_batting_s2d_today[,clms[i]], 1, nchar(team_batting_s2d_today[,clms[i]])-1)
}

team_batting_s2d_today[,2:28] <- lapply(team_batting_s2d_today[,2:28], as.numeric)
team_batting_s2d_today[,c(7,8,21:28)] <- team_batting_s2d_today[,c(7,8,21:28)] / 100

team_batting_s2d <- filter(team_batting_s2d, Date < Sys.Date()) %>%
  union(team_batting_s2d_today)

write.csv(team_batting_s2d, "team_batting_s2d_2021.csv", row.names = FALSE)

# Season-to-date starting pitcher stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-31")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c%2C7%2C8%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C55%2C57%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate,'&page=1_1000')
#   urls[[i]] = url
#   i <- i + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# pitchers_s2d <- ldply(tbl, data.frame)
# clms <- c(6:7,10:14,24:29)
# for (i in 1:length(clms)) {
#   pitchers_s2d[,clms[i]] <- substr(pitchers_s2d[,clms[i]], 1, nchar(pitchers_s2d[,clms[i]])-1)
# }
# 
# pitchers_s2d[,3:29] <- lapply(pitchers_s2d[,3:29], as.numeric)
# pitchers_s2d[,c(6:7,10:14,24:29)] <- pitchers_s2d[,c(6:7,10:14,24:29)] / 100
# 
# write.csv(pitchers_s2d, "pitchers_s2d_2021.csv", row.names = FALSE)

pitchers_s2d <- read.csv("pitchers_s2d_2021.csv")
pitchers_s2d$Date <- as.Date(pitchers_s2d$Date)

i <- 1
startdate <- as.Date("2021-03-31")
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c%2C7%2C8%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C55%2C57%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0&rost=1&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate,'&page=1_1000')
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

pitchers_s2d_today <- ldply(tbl, data.frame)
clms <- c(6:7,10:14,24:29)
for (i in 1:length(clms)) {
  pitchers_s2d_today[,clms[i]] <- substr(pitchers_s2d_today[,clms[i]], 1, nchar(pitchers_s2d_today[,clms[i]])-1)
}

pitchers_s2d_today[,3:29] <- lapply(pitchers_s2d_today[,3:29], as.numeric)
pitchers_s2d_today[,c(6:7,10:14,24:29)] <- pitchers_s2d_today[,c(6:7,10:14,24:29)] / 100

pitchers_s2d <- filter(pitchers_s2d, Date < Sys.Date()) %>%
  union(pitchers_s2d_today)

write.csv(pitchers_s2d, "pitchers_s2d_2021.csv", row.names = FALSE)

# Last 7 days team bullpen stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-26")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   startdate <- startdate + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_bullpen_L7 <- ldply(tbl, data.frame)
# clms <- c(4,5,8:12,20:25)
# for (i in 1:length(clms)) {
#   team_bullpen_L7[,clms[i]] <- substr(team_bullpen_L7[,clms[i]], 1, nchar(team_bullpen_L7[,clms[i]])-1)
# }
# 
# team_bullpen_L7[,2:25] <- lapply(team_bullpen_L7[,2:25], as.numeric)
# team_bullpen_L7[,c(4,5,8:12,20:25)] <- team_bullpen_L7[,c(4,5,8:12,20:25)] / 100
# 
# write.csv(team_bullpen_L7, "team_bullpen_L7_2021.csv", row.names = FALSE)

team_bullpen_L7 <- read.csv("team_bullpen_L7_2021.csv")
team_bullpen_L7$Date <- as.Date(team_bullpen_L7$Date)

i <- 1
startdate <- as.Date(Sys.Date()-7)
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L7_today <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L7_today[,clms[i]] <- substr(team_bullpen_L7_today[,clms[i]], 1, nchar(team_bullpen_L7_today[,clms[i]])-1)
}

team_bullpen_L7_today[,2:25] <- lapply(team_bullpen_L7_today[,2:25], as.numeric)
team_bullpen_L7_today[,c(4,5,8:12,20:25)] <- team_bullpen_L7_today[,c(4,5,8:12,20:25)] / 100

team_bullpen_L7 <- filter(team_bullpen_L7, Date < Sys.Date()) %>%
  union(team_bullpen_L7_today)

write.csv(team_bullpen_L7, "team_bullpen_L7_2021.csv", row.names = FALSE)

# Last 14 days team bullpen stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-19")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   startdate <- startdate + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_bullpen_L14 <- ldply(tbl, data.frame)
# clms <- c(4,5,8:12,20:25)
# for (i in 1:length(clms)) {
#   team_bullpen_L14[,clms[i]] <- substr(team_bullpen_L14[,clms[i]], 1, nchar(team_bullpen_L14[,clms[i]])-1)
# }
# 
# team_bullpen_L14[,2:25] <- lapply(team_bullpen_L14[,2:25], as.numeric)
# team_bullpen_L14[,c(4,5,8:12,20:25)] <- team_bullpen_L14[,c(4,5,8:12,20:25)] / 100
# 
# write.csv(team_bullpen_L14, "team_bullpen_L14_2021.csv", row.names = FALSE)

team_bullpen_L14 <- read.csv("team_bullpen_L14_2021.csv")
team_bullpen_L14$Date <- as.Date(team_bullpen_L14$Date)

i <- 1
startdate <- as.Date(Sys.Date()-14)
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L14_today <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L14_today[,clms[i]] <- substr(team_bullpen_L14_today[,clms[i]], 1, nchar(team_bullpen_L14_today[,clms[i]])-1)
}

team_bullpen_L14_today[,2:25] <- lapply(team_bullpen_L14_today[,2:25], as.numeric)
team_bullpen_L14_today[,c(4,5,8:12,20:25)] <- team_bullpen_L14_today[,c(4,5,8:12,20:25)] / 100

team_bullpen_L14 <- filter(team_bullpen_L14, Date < Sys.Date()) %>%
  union(team_bullpen_L14_today)

write.csv(team_bullpen_L14, "team_bullpen_L14_2021.csv", row.names = FALSE)

# Last 30 days team bullpen stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-03")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   startdate <- startdate + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_bullpen_L30 <- ldply(tbl, data.frame)
# clms <- c(4,5,8:12,20:25)
# for (i in 1:length(clms)) {
#   team_bullpen_L30[,clms[i]] <- substr(team_bullpen_L30[,clms[i]], 1, nchar(team_bullpen_L30[,clms[i]])-1)
# }
# 
# team_bullpen_L30[,2:25] <- lapply(team_bullpen_L30[,2:25], as.numeric)
# team_bullpen_L30[,c(4,5,8:12,20:25)] <- team_bullpen_L30[,c(4,5,8:12,20:25)] / 100
# 
# write.csv(team_bullpen_L30, "team_bullpen_L30_2021.csv", row.names = FALSE)

team_bullpen_L30 <- read.csv("team_bullpen_L30_2021.csv")
team_bullpen_L30$Date <- as.Date(team_bullpen_L30$Date)

i <- 1
startdate <- as.Date(Sys.Date()-30)
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L30_today <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L30_today[,clms[i]] <- substr(team_bullpen_L30_today[,clms[i]], 1, nchar(team_bullpen_L30_today[,clms[i]])-1)
}

team_bullpen_L30_today[,2:25] <- lapply(team_bullpen_L30_today[,2:25], as.numeric)
team_bullpen_L30_today[,c(4,5,8:12,20:25)] <- team_bullpen_L30_today[,c(4,5,8:12,20:25)] / 100

team_bullpen_L30 <- filter(team_bullpen_L30, Date < Sys.Date()) %>%
  union(team_bullpen_L30_today)

write.csv(team_bullpen_L30, "team_bullpen_L30_2021.csv", row.names = FALSE)

# Season-to-date team bullpen stats (2021)

# i <- 1
# startdate <- as.Date("2021-03-31")
# enddate <- as.Date("2021-04-01")
# urls <- list()
# while (enddate < as.Date(Sys.Date())) {
#   url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
#   urls[[i]] = url
#   i <- i + 1
#   enddate <- enddate + 1
# }
# 
# tbl <- list()
# dates <- as.Date("2021-04-02")
# j = 1
# for (j in seq_along(urls)) {
#   tbl[[j]] = urls[[j]] %>%
#     read_html() %>%
#     html_nodes("table") %>%
#     .[17] %>%
#     html_table(fill = TRUE) %>%
#     data.frame(stringsAsFactors = FALSE) %>%
#     .[-c(1,3),] %>%
#     .[,-1] %>% row_to_names(row_number = 1)
#   tbl[[j]]$Date = dates
#   j = j+1
#   dates = dates+1
# }
# 
# team_bullpen_s2d <- ldply(tbl, data.frame)
# clms <- c(4,5,8:12,20:25)
# for (i in 1:length(clms)) {
#   team_bullpen_s2d[,clms[i]] <- substr(team_bullpen_s2d[,clms[i]], 1, nchar(team_bullpen_s2d[,clms[i]])-1)
# }
# 
# team_bullpen_s2d[,2:25] <- lapply(team_bullpen_s2d[,2:25], as.numeric)
# team_bullpen_s2d[,c(4,5,8:12,20:25)] <- team_bullpen_s2d[,c(4,5,8:12,20:25)] / 100
# 
# write.csv(team_bullpen_s2d, "team_bullpen_s2d_2021.csv", row.names = FALSE)

team_bullpen_s2d <- read.csv("team_bullpen_s2d_2021.csv")
team_bullpen_s2d$Date <- as.Date(team_bullpen_s2d$Date)

i <- 1
startdate <- as.Date("2021-03-31")
enddate <- as.Date(Sys.Date()-1)
urls <- list()
while (enddate < as.Date(Sys.Date())) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(Sys.Date())
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_s2d_today <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_s2d_today[,clms[i]] <- substr(team_bullpen_s2d_today[,clms[i]], 1, nchar(team_bullpen_s2d_today[,clms[i]])-1)
}

team_bullpen_s2d_today[,2:25] <- lapply(team_bullpen_s2d_today[,2:25], as.numeric)
team_bullpen_s2d_today[,c(4,5,8:12,20:25)] <- team_bullpen_s2d_today[,c(4,5,8:12,20:25)] / 100

team_bullpen_s2d <- filter(team_bullpen_s2d, Date < Sys.Date()) %>%
  union(team_bullpen_s2d_today)

write.csv(team_bullpen_s2d, "team_bullpen_s2d_2021.csv", row.names = FALSE)

## Create daily batting, SP, and bullpen tables

team_batting_L7_2021 <- read.csv("team_batting_L7_2021.csv")
team_batting_L7_2021$Date <- as.Date(team_batting_L7_2021$Date)
team_batting_L14_2021 <- read.csv("team_batting_L14_2021.csv")
team_batting_L14_2021$Date <- as.Date(team_batting_L14_2021$Date)
team_batting_L30_2021 <- read.csv("team_batting_L30_2021.csv")
team_batting_L30_2021$Date <- as.Date(team_batting_L30_2021$Date)
team_batting_s2d_2021 <- read.csv("team_batting_s2d_2021.csv")
team_batting_s2d_2021$Date <- as.Date(team_batting_s2d_2021$Date)

daily_team_batting_2021 <- full_join(team_batting_L7_2021, team_batting_L14_2021, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_2021, by = c("Team", "Date"))
colnames(daily_team_batting_2021)[57:83] <- paste0(colnames(daily_team_batting_2021)[57:83],'_L30')
daily_team_batting_2021 <- full_join(daily_team_batting_2021, team_batting_s2d_2021, by = c("Team", "Date"))
daily_team_batting_2021 <- as.data.frame(daily_team_batting_2021) %>%
  FindReplace(Var = "Team", replaceData = team_names,
              from = "BR", to = "FG")

team_bullpen_L7_2021 <- read.csv("team_bullpen_L7_2021.csv")
team_bullpen_L7_2021$Date <- as.Date(team_bullpen_L7_2021$Date)
team_bullpen_L14_2021 <- read.csv("team_bullpen_L14_2021.csv")
team_bullpen_L14_2021$Date <- as.Date(team_bullpen_L14_2021$Date)
team_bullpen_L30_2021 <- read.csv("team_bullpen_L30_2021.csv")
team_bullpen_L30_2021$Date <- as.Date(team_bullpen_L30_2021$Date)
team_bullpen_s2d_2021 <- read.csv("team_bullpen_s2d_2021.csv")
team_bullpen_s2d_2021$Date <- as.Date(team_bullpen_s2d_2021$Date)

daily_team_bullpen_2021 <- full_join(team_bullpen_L7_2021, team_bullpen_L14_2021, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_2021, by = c("Team", "Date"))
colnames(daily_team_bullpen_2021)[51:74] <- paste0(colnames(daily_team_bullpen_2021)[51:74],'_L30')
daily_team_bullpen_2021 <- full_join(daily_team_bullpen_2021, team_bullpen_s2d_2021, by = c("Team", "Date"))
daily_team_bullpen_2021 <- as.data.frame(daily_team_bullpen_2021) %>%
  FindReplace(Var = "Team", replaceData = team_names,
              from = "BR", to = "FG")

daily_pitchers_2021 <- read.csv("pitchers_s2d_2021.csv")
daily_pitchers_2021$Date <- as.Date(daily_pitchers_2021$Date)
daily_pitchers_2021 <- as.data.frame(daily_pitchers_2021) %>%
  FindReplace(Var = "Team", replaceData = team_names,
              from = "BR", to = "FG")


## Join daily data to odds data

bovada_odds <- left_join(bovada_odds, daily_pitchers_2021, 
                    by = c("gamedate" = "Date", "AwayTeam" = "Team", "AwaySP" = "Name"))
colnames(bovada_odds)[11:37] <- paste0("VisitingSP_", colnames(bovada_odds)[11:37])
bovada_odds <- left_join(bovada_odds, daily_pitchers_2021,
                    by = c("gamedate" = "Date", "HomeTeam" = "Team", "HomeSP" = "Name"))
colnames(bovada_odds)[38:64] <- paste0("HomeSP_", colnames(bovada_odds)[38:64])
bovada_odds <- left_join(bovada_odds, daily_team_batting_2021,
                    by = c("gamedate" = "Date", "AwayTeam" = "Team"))
colnames(bovada_odds)[65:172] <- paste0("VisitingBatters_", colnames(bovada_odds)[65:172])
bovada_odds <- left_join(bovada_odds, daily_team_batting_2021,
                    by = c("gamedate" = "Date", "HomeTeam" = "Team"))
colnames(bovada_odds)[173:280] <- paste0("HomeBatters_", colnames(bovada_odds)[173:280])
bovada_odds <- left_join(bovada_odds, daily_team_bullpen_2021,
                    by = c("gamedate" = "Date", "AwayTeam" = "Team"))
colnames(bovada_odds)[281:376] <- paste0("VisitingBullpen_", colnames(bovada_odds)[281:376])
bovada_odds <- left_join(bovada_odds, daily_team_bullpen_2021,
                    by = c("gamedate" = "Date", "HomeTeam" = "Team"))
colnames(bovada_odds)[377:472] <- paste0("HomeBullpen_", colnames(bovada_odds)[377:472])

bovada_odds <- filter(bovada_odds, !is.na(AwaySP) & !is.na(HomeSP))

## Add in 2021 pre-season projections

DC_batting_21 <- read.csv("DepthCharts/2021/DepthCharts_batting.csv")
DC_pitching_21 <- read.csv("DepthCharts/2021/DepthCharts_pitching.csv")
PECOTA_batting_21 <- read_excel("PECOTA/2021/pecota2021_hitting_apr01.xlsx", sheet = "50")
PECOTA_pitching_21 <- read_excel("PECOTA/2021/pecota2021_pitching_apr01.xlsx", sheet = "50")
PECOTA_batting_21$warp <- as.numeric(PECOTA_batting_21$warp)
PECOTA_pitching_21$warp <- as.numeric(PECOTA_pitching_21$warp)
DC_batting_21 <- as.data.frame(DC_batting_21) %>%
  FindReplace(Var = "Team", replaceData = team_names,
              from = "BR", to = "FG")
DC_pitching_21 <- as.data.frame(DC_pitching_21) %>%
  FindReplace(Var = "Team", replaceData = team_names,
              from = "BR", to = "FG")
PECOTA_batting_21 <- as.data.frame(PECOTA_batting_21) %>%
  FindReplace(Var = "team", replaceData = team_names,
              from = "BP_2021", to = "FG")
PECOTA_pitching_21 <- as.data.frame(PECOTA_pitching_21) %>%
  FindReplace(Var = "team", replaceData = team_names,
              from = "BP_2021", to = "FG")

player_lkup <- lkup_players
player_map <- read.csv("player_id_map.csv")

player_id_map <- left_join(player_lkup, player_map[,c(1,19)], by = c("mlbam_id" = "mlb_id"))
player_id_map <- mutate(player_id_map, 
                        full_name = paste0(name_first, " ", name_last))

PECOTA_batting_21 <- left_join(PECOTA_batting_21, player_id_map[,c(4,16)], by = c("mlbid" = "mlbam_id"))
PECOTA_batting_21 <- mutate(PECOTA_batting_21, name = if_else(fg_name == "" | is.na(fg_name), name, fg_name))
PECOTA_pitching_21 <- left_join(PECOTA_pitching_21, player_id_map[,c(4,16)], by = c("mlbid" = "mlbam_id"))
PECOTA_pitching_21 <- mutate(PECOTA_pitching_21, name = if_else(fg_name == "" | is.na(fg_name), name, fg_name))

## Aggregate WAR projections for 2021 and join to odds table
# Batters

bat_WAR_21 <- DC_batting_21 %>% 
  group_by(Team) %>%
  dplyr::summarize(bat_WAR500_proj = (sum(WAR) / sum(PA)) * 500)

BWARP_21 <- PECOTA_batting_21 %>%
  filter(dc_fl == TRUE) %>%
  group_by(team) %>%
  dplyr::summarize(BWARP500_proj = (sum(warp) / sum(pa)) * 500)

proj_batting_21 <- left_join(BWARP_21, bat_WAR_21, by = c("team" = "Team"))

bovada_odds <- left_join(bovada_odds, proj_batting_21, by = c("AwayTeam" = "team"))
colnames(bovada_odds)[473:474] <- paste0("Visiting_", colnames(bovada_odds)[473:474])
bovada_odds <- left_join(bovada_odds, proj_batting_21, by = c("HomeTeam" = "team"))
colnames(bovada_odds)[475:476] <- paste0("Home_", colnames(bovada_odds)[475:476])

# Starting pitchers

pit_WAR_21 <- DC_pitching_21 %>%
  group_by(?..Name) %>%
  dplyr::summarize(pit_WAR200_proj = (sum(WAR) / sum(IP)) * 200)

PWARP_21 <- PECOTA_pitching_21 %>%
  group_by(name) %>%
  dplyr::summarize(PWARP200_proj = (sum(warp) / sum(as.numeric(ip))) * 200)

proj_SP_21 <- full_join(pit_WAR_21, PWARP_21, by = c("?..Name" = "name"))

bovada_odds <- left_join(bovada_odds, proj_SP_21, by = c("AwaySP" = "?..Name"))
colnames(bovada_odds)[477:478] <- paste0("Visiting_", colnames(bovada_odds)[477:478])
bovada_odds <- left_join(bovada_odds, proj_SP_21, by = c("HomeSP" = "?..Name"))
colnames(bovada_odds)[479:480] <- paste0("Home_", colnames(bovada_odds)[479:480])

## Adjust IP columns to use .333 instead of .1 for a third of an inning

bovada_odds <- mutate(bovada_odds,
                      VisitingSP_IP = as.integer(VisitingSP_IP) + (VisitingSP_IP %% 1 * 3.33),
                      VisitingSP_Start.IP = as.integer(VisitingSP_Start.IP) + (VisitingSP_Start.IP %% 1 * 3.33),
                      VisitingSP_Relief.IP = as.integer(VisitingSP_Relief.IP) + (VisitingSP_Relief.IP %% 1 * 3.33),
                      HomeSP_IP = as.integer(HomeSP_IP) + (HomeSP_IP %% 1 * 3.33),
                      HomeSP_Start.IP = as.integer(HomeSP_Start.IP) + (HomeSP_Start.IP %% 1 * 3.33),
                      HomeSP_Relief.IP = as.integer(HomeSP_Relief.IP) + (HomeSP_Relief.IP %% 1 * 3.33),
                      VisitingBullpen_IP_L7 = as.integer(VisitingBullpen_IP_L7) + (VisitingBullpen_IP_L7 %% 1 * 3.33),
                      VisitingBullpen_IP_L14 = as.integer(VisitingBullpen_IP_L14) + (VisitingBullpen_IP_L14 %% 1 * 3.33),
                      VisitingBullpen_IP_L30 = as.integer(VisitingBullpen_IP_L30) + (VisitingBullpen_IP_L30 %% 1 * 3.33),
                      VisitingBullpen_IP = as.integer(VisitingBullpen_IP) + (VisitingBullpen_IP %% 1 * 3.33),
                      HomeBullpen_IP_L7 = as.integer(HomeBullpen_IP_L7) + (HomeBullpen_IP_L7 %% 1 * 3.33),
                      HomeBullpen_IP_L14 = as.integer(HomeBullpen_IP_L14) + (HomeBullpen_IP_L14 %% 1 * 3.33),
                      HomeBullpen_IP_L30 = as.integer(HomeBullpen_IP_L30) + (HomeBullpen_IP_L30 %% 1 * 3.33),
                      HomeBullpen_IP = as.integer(HomeBullpen_IP) + (HomeBullpen_IP %% 1 * 3.33))

## Adjust counting stats to rate stats
# pitching stats converted to per 200 IP
# batting stats converted to per 500 PA
## Also add SP IP per start and per appearance

bovada_odds <- mutate(bovada_odds,
                      VisitingSP_WAR = (VisitingSP_WAR / VisitingSP_IP) * 200,
                      HomeSP_WAR = (HomeSP_WAR / HomeSP_IP) * 200,
                      VisitingBatters_HR_L7 = (VisitingBatters_HR_L7 / VisitingBatters_PA_L7) * 500,
                      VisitingBatters_SB_L7 = (VisitingBatters_SB_L7 / VisitingBatters_PA_L7) * 500,
                      VisitingBatters_BsR_L7 = (VisitingBatters_BsR_L7 / VisitingBatters_PA_L7) * 500,
                      VisitingBatters_Off_L7 = (VisitingBatters_Off_L7 / VisitingBatters_PA_L7) * 500,
                      VisitingBatters_Def_L7 = (VisitingBatters_Def_L7 / VisitingBatters_PA_L7) * 500,
                      VisitingBatters_WAR_L7 = (VisitingBatters_WAR_L7 / VisitingBatters_PA_L7) * 500,
                      VisitingBatters_HR_L14 = (VisitingBatters_HR_L14 / VisitingBatters_PA_L14) * 500,
                      VisitingBatters_SB_L14 = (VisitingBatters_SB_L14 / VisitingBatters_PA_L14) * 500,
                      VisitingBatters_BsR_L14 = (VisitingBatters_BsR_L14 / VisitingBatters_PA_L14) * 500,
                      VisitingBatters_Off_L14 = (VisitingBatters_Off_L14 / VisitingBatters_PA_L14) * 500,
                      VisitingBatters_Def_L14 = (VisitingBatters_Def_L14 / VisitingBatters_PA_L14) * 500,
                      VisitingBatters_WAR_L14 = (VisitingBatters_WAR_L14 / VisitingBatters_PA_L14) * 500,
                      VisitingBatters_HR_L30 = (VisitingBatters_HR_L30 / VisitingBatters_PA_L30) * 500,
                      VisitingBatters_SB_L30 = (VisitingBatters_SB_L30 / VisitingBatters_PA_L30) * 500,
                      VisitingBatters_BsR_L30 = (VisitingBatters_BsR_L30 / VisitingBatters_PA_L30) * 500,
                      VisitingBatters_Off_L30 = (VisitingBatters_Off_L30 / VisitingBatters_PA_L30) * 500,
                      VisitingBatters_Def_L30 = (VisitingBatters_Def_L30 / VisitingBatters_PA_L30) * 500,
                      VisitingBatters_WAR_L30 = (VisitingBatters_WAR_L30 / VisitingBatters_PA_L30) * 500,
                      VisitingBatters_HR = (VisitingBatters_HR / VisitingBatters_PA) * 500,
                      VisitingBatters_SB = (VisitingBatters_SB / VisitingBatters_PA) * 500,
                      VisitingBatters_BsR = (VisitingBatters_BsR / VisitingBatters_PA) * 500,
                      VisitingBatters_Off = (VisitingBatters_Off / VisitingBatters_PA) * 500,
                      VisitingBatters_Def = (VisitingBatters_Def / VisitingBatters_PA) * 500,
                      VisitingBatters_WAR = (VisitingBatters_WAR / VisitingBatters_PA) * 500,
                      HomeBatters_HR_L7 = (HomeBatters_HR_L7 / HomeBatters_PA_L7) * 500,
                      HomeBatters_SB_L7 = (HomeBatters_SB_L7 / HomeBatters_PA_L7) * 500,
                      HomeBatters_BsR_L7 = (HomeBatters_BsR_L7 / HomeBatters_PA_L7) * 500,
                      HomeBatters_Off_L7 = (HomeBatters_Off_L7 / HomeBatters_PA_L7) * 500,
                      HomeBatters_Def_L7 = (HomeBatters_Def_L7 / HomeBatters_PA_L7) * 500,
                      HomeBatters_WAR_L7 = (HomeBatters_WAR_L7 / HomeBatters_PA_L7) * 500,
                      HomeBatters_HR_L14 = (HomeBatters_HR_L14 / HomeBatters_PA_L14) * 500,
                      HomeBatters_SB_L14 = (HomeBatters_SB_L14 / HomeBatters_PA_L14) * 500,
                      HomeBatters_BsR_L14 = (HomeBatters_BsR_L14 / HomeBatters_PA_L14) * 500,
                      HomeBatters_Off_L14 = (HomeBatters_Off_L14 / HomeBatters_PA_L14) * 500,
                      HomeBatters_Def_L14 = (HomeBatters_Def_L14 / HomeBatters_PA_L14) * 500,
                      HomeBatters_WAR_L14 = (HomeBatters_WAR_L14 / HomeBatters_PA_L14) * 500,
                      HomeBatters_HR_L30 = (HomeBatters_HR_L30 / HomeBatters_PA_L30) * 500,
                      HomeBatters_SB_L30 = (HomeBatters_SB_L30 / HomeBatters_PA_L30) * 500,
                      HomeBatters_BsR_L30 = (HomeBatters_BsR_L30 / HomeBatters_PA_L30) * 500,
                      HomeBatters_Off_L30 = (HomeBatters_Off_L30 / HomeBatters_PA_L30) * 500,
                      HomeBatters_Def_L30 = (HomeBatters_Def_L30 / HomeBatters_PA_L30) * 500,
                      HomeBatters_WAR_L30 = (HomeBatters_WAR_L30 / HomeBatters_PA_L30) * 500,
                      HomeBatters_HR = (HomeBatters_HR / HomeBatters_PA) * 500,
                      HomeBatters_SB = (HomeBatters_SB / HomeBatters_PA) * 500,
                      HomeBatters_BsR = (HomeBatters_BsR / HomeBatters_PA) * 500,
                      HomeBatters_Off = (HomeBatters_Off / HomeBatters_PA) * 500,
                      HomeBatters_Def = (HomeBatters_Def / HomeBatters_PA) * 500,
                      HomeBatters_WAR = (HomeBatters_WAR / HomeBatters_PA) * 500,
                      VisitingBullpen_WAR_L7 = (VisitingBullpen_WAR_L7 / VisitingBullpen_IP_L7) * 200,
                      VisitingBullpen_WAR_L14 = (VisitingBullpen_WAR_L14 / VisitingBullpen_IP_L14) * 200,
                      VisitingBullpen_WAR_L30 = (VisitingBullpen_WAR_L30 / VisitingBullpen_IP_L30) * 200,
                      VisitingBullpen_WAR = (VisitingBullpen_WAR / VisitingBullpen_IP) * 200,
                      HomeBullpen_WAR_L7 = (HomeBullpen_WAR_L7 / HomeBullpen_IP_L7) * 200,
                      HomeBullpen_WAR_L14 = (HomeBullpen_WAR_L14 / HomeBullpen_IP_L14) * 200,
                      HomeBullpen_WAR_L30 = (HomeBullpen_WAR_L30 / HomeBullpen_IP_L30) * 200,
                      HomeBullpen_WAR = (HomeBullpen_WAR / HomeBullpen_IP) * 200,
                      VisitingSP_IPperStart = VisitingSP_Start.IP / VisitingSP_GS,
                      VisitingSP_IPperG = VisitingSP_IP / VisitingSP_G,
                      HomeSP_IPperStart = HomeSP_Start.IP / HomeSP_GS,
                      HomeSP_IPperG = HomeSP_IP / HomeSP_G)

## Make predictions

# VRS
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_BestOfFamily_AutoML_20210409_170242"
# HRS
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\GLM_1_AutoML_20210409_174344"
# VRS F5
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_AllModels_AutoML_20210409_182456"
# HRS F5
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_AllModels_AutoML_20210409_190602"

h2o.init(nthreads = -1)

Visitor_pred <- predict(h2o.loadModel("C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_AllModels_AutoML_20210911_221712"), 
                        as.h2o(bovada_odds)) %>% as.data.frame()
colnames(Visitor_pred) <- "Visitor_pred"

Home_pred <- predict(h2o.loadModel("C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_AllModels_AutoML_20210911_225938"), 
                     as.h2o(bovada_odds)) %>% as.data.frame()
colnames(Home_pred) <- "Home_pred"

Visitor_pred_F5 <- predict(h2o.loadModel("C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_AllModels_AutoML_20210911_234321"), 
                        as.h2o(bovada_odds)) %>% as.data.frame()
colnames(Visitor_pred_F5) <- "Visitor_pred_F5"

Home_pred_F5 <- predict(h2o.loadModel("C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_AllModels_AutoML_20210912_002507"), 
                     as.h2o(bovada_odds)) %>% as.data.frame()
colnames(Home_pred_F5) <- "Home_pred_F5"

## Join predictions to odds data

bovada_odds <- bovada_odds %>% cbind(Visitor_pred, Home_pred, Visitor_pred_F5, Home_pred_F5)

## Functions to simulate ML, RL, and totals probabilities
simulate_game <- function(awayScorePred, homeScorePred, homeORaway = c("home", "away"), max_score = 20) {
  score_matrix = dpois(0:max_score, awayScorePred) %o% dpois(0:max_score, homeScorePred)
  if_else(homeORaway == "home", 
          sum(score_matrix[upper.tri(score_matrix)]) + (sum(diag(score_matrix))/2), 
          sum(score_matrix[lower.tri(score_matrix)]) + (sum(diag(score_matrix))/2))
}

simulate_RL <- function(awayScorePred, homeScorePred, homeORaway = c("home", "away"), rl_spread, max_score = 20) {
  score_matrix = dpois(0:max_score, awayScorePred) %o% dpois(0:max_score, homeScorePred)
  colnames(score_matrix) = 0:max_score
  rownames(score_matrix) = 0:max_score
  score_matrix = rownames_to_column(as.data.frame(score_matrix)) %>% melt(id = "rowname")
  colnames(score_matrix) = c("away_score", "home_score", "prob")
  score_matrix = mutate(score_matrix,
                        away_score = as.numeric(away_score),
                        home_score = as.numeric(home_score) - 1,
                        away_margin = away_score - home_score,
                        home_margin = home_score - away_score)
  if_else(homeORaway == "home",
          sum(subset(score_matrix, away_margin < rl_spread)$prob),
          sum(subset(score_matrix, home_margin < rl_spread)$prob))
}

simulate_total <- function(awayScorePred, homeScorePred, overORunder = c("over", "under"), ou_total, max_score = 20) {
  score_matrix = dpois(0:max_score, awayScorePred) %o% dpois(0:max_score, homeScorePred)
  colnames(score_matrix) = 0:max_score
  rownames(score_matrix) = 0:max_score
  score_matrix = rownames_to_column(as.data.frame(score_matrix)) %>% melt(id = "rowname")
  colnames(score_matrix) = c("away_score", "home_score", "prob")
  score_matrix = mutate(score_matrix,
                        away_score = as.numeric(away_score),
                        home_score = as.numeric(home_score) - 1,
                        total_score = away_score + home_score)
  if_else(overORunder == "over",
          sum(subset(score_matrix, total_score > ou_total)$prob),
          sum(subset(score_matrix, total_score < ou_total)$prob))
}

simulate_team_total <- function(awayScorePred, homeScorePred, homeORaway = c("home", "away"), 
                                overORunder = c("over", "under"), team_total, max_score = 20) {
  score_prob = dpois(0:max_score, if_else(homeORaway == "away", awayScorePred, homeScorePred))
  score = 0:max_score
  score_matrix = cbind(score, score_prob) %>% as.data.frame()
  if_else(overORunder == "over",
          sum(subset(score_matrix, score > team_total)$score_prob),
          sum(subset(score_matrix, score < team_total)$score_prob))
  
}

## Add in implied odds, projected odds, edge, kelly criteria
bovada_odds <- mutate(bovada_odds,
                     AO_ImpliedOdds = if_else(AO.Odds > 0, 100 / (AO.Odds + 100), abs(AO.Odds) / (abs(AO.Odds) + 100)),
                     HU_ImpliedOdds = if_else(HU.Odds > 0, 100 / (HU.Odds + 100), abs(HU.Odds) / (abs(HU.Odds) + 100)))

bovada_odds <- bovada_odds %>% 
  rowwise() %>%
  dplyr::mutate(AO_ProjOdds = if_else(bet_type == "Moneyline - Game",
                               simulate_game(Visitor_pred, Home_pred, "away"),
                               if_else(bet_type == "Runline - Game" | grepl("Alternate Runline - Game", bet_type),
                                       simulate_RL(Visitor_pred, Home_pred, "away", rl_spread = AO.SpreadTotal),
                                       if_else(bet_type == "Total - Game" | grepl("Alternate Total - Game", bet_type),
                                               simulate_total(Visitor_pred, Home_pred, "over", ou_total = AO.SpreadTotal),
                                               if_else(bet_type == "Moneyline - 5 Inning Line",
                                                       simulate_game(Visitor_pred_F5, Home_pred_F5, "away"),
                                                       if_else(bet_type == "Runline - 5 Inning Line",
                                                               simulate_RL(Visitor_pred_F5, Home_pred_F5, "away", rl_spread = AO.SpreadTotal),
                                                               if_else(bet_type == "Total - 5 Inning Line" | grepl("Alternate Total - 5 Inning Line", bet_type),
                                                                       simulate_total(Visitor_pred_F5, Home_pred_F5, "over", ou_total = AO.SpreadTotal),
                                                                       if_else(grepl("Team Total", bet_type) & grepl(paste0(AwayTeam), bet_type),
                                                                               simulate_team_total(Visitor_pred, Home_pred, "away", "over", team_total = AO.SpreadTotal),
                                                                               if_else(grepl("Team Total", bet_type) & grepl(paste0(HomeTeam), bet_type),
                                                                                       simulate_team_total(Visitor_pred, Home_pred, "home", "over", team_total = AO.SpreadTotal),
                                                                                       0)))))))),
         HU_ProjOdds = if_else(bet_type == "Moneyline - Game",
                               simulate_game(Visitor_pred, Home_pred, "home"),
                               if_else(bet_type == "Runline - Game" | grepl("Alternate Runline - Game", bet_type),
                                       simulate_RL(Visitor_pred, Home_pred, "home", rl_spread = HU.SpreadTotal),
                                       if_else(bet_type == "Total - Game" | grepl("Alternate Total - Game", bet_type),
                                               simulate_total(Visitor_pred, Home_pred, "under", ou_total = HU.SpreadTotal),
                                               if_else(bet_type == "Moneyline - 5 Inning Line",
                                                       simulate_game(Visitor_pred_F5, Home_pred_F5, "home"),
                                                       if_else(bet_type == "Runline - 5 Inning Line",
                                                               simulate_RL(Visitor_pred_F5, Home_pred_F5, "home", rl_spread = HU.SpreadTotal),
                                                               if_else(bet_type == "Total - 5 Inning Line" | grepl("Alternate Total - 5 Inning Line", bet_type),
                                                                       simulate_total(Visitor_pred_F5, Home_pred_F5, "under", ou_total = HU.SpreadTotal),
                                                                       if_else(grepl("Team Total", bet_type) & grepl(paste0(AwayTeam), bet_type),
                                                                               simulate_team_total(Visitor_pred, Home_pred, "away", "under", team_total = HU.SpreadTotal),
                                                                               if_else(grepl("Team Total", bet_type) & grepl(paste0(HomeTeam), bet_type),
                                                                                       simulate_team_total(Visitor_pred, Home_pred, "home", "under", team_total = HU.SpreadTotal),
                                                                                       0)))))))))

bovada_odds <- mutate(bovada_odds,
                      AO.Odds_Diff = AO_ProjOdds - AO_ImpliedOdds,
                      HU.Odds_Diff = HU_ProjOdds - HU_ImpliedOdds,
                      Pick = if_else(AO.Odds_Diff > HU.Odds_Diff, AwayTeam, HomeTeam),
                      Pick_Odds = if_else(AO.Odds_Diff > HU.Odds_Diff, AO.Odds, HU.Odds),
                      Pick_SpreadTotal = if_else(AO.Odds_Diff > HU.Odds_Diff, AO.SpreadTotal, HU.SpreadTotal),
                      Pick_WinProb = if_else(AO.Odds_Diff > HU.Odds_Diff, AO_ProjOdds, HU_ProjOdds),
                      Pick_Edge = if_else(AO.Odds_Diff > HU.Odds_Diff, AO.Odds_Diff, HU.Odds_Diff),
                      Fract_Odds = (100 / abs(Pick_Odds))^if_else(Pick_Odds < 0, 1, -1),
                      Kelly_Criteria = (Pick_WinProb * (Fract_Odds + 1) - 1) / Fract_Odds)

bovada_odds <- mutate(bovada_odds,
                      Pick = if_else(grepl("Total", bet_type),
                                     if_else(Pick == AwayTeam, "Over", "Under"),
                                     Pick),
                      AwayRunsProj = if_else(grepl("5 Inning Line", bet_type),
                                             Visitor_pred_F5,
                                             Visitor_pred),
                      HomeRunsProj = if_else(grepl("5 Inning Line", bet_type),
                                             Home_pred_F5,
                                             Home_pred))

todays_bets <- select(bovada_odds, colnames(bovada_odds)[1:10], AwayRunsProj, HomeRunsProj, colnames(bovada_odds)[489:501])

write.csv(bovada_odds, "todays_data_full.csv", row.names = FALSE)
write.csv(todays_bets, "todays_bets.csv", row.names = FALSE)

## Adjust Win Prob based on bet type
# Analyze performance

scores <- read_excel("2021 MLB Score Tracker.xlsx")
history <- read_excel("Machine Picks History.xlsx")

history <- mutate(history, 
                  Pick_SpreadTotal = if_else(Pick == "Over" | Pick == paste0(AwayTeam),
                                             AO.SpreadTotal,
                                             HU.SpreadTotal))

history <- inner_join(history, scores, by = c("gamedate", "AwayTeam", "HomeTeam"))

history <- filter(history, Kelly_Criteria > 0 & Pick_Odds > -200) %>%
  mutate(VisitorRunsScored = if_else(grepl("5 Inning Line", bet_type),
                                     F5_VisitorRunsScored,
                                     VisitorRunsScored),
         HomeRunsScore = if_else(grepl("5 Inning Line", bet_type),
                                 F5_HomeRunsScored,
                                 HomeRunsScore),
         TotalScore = VisitorRunsScored + HomeRunsScore) %>%
  select(-F5_VisitorRunsScored, -F5_HomeRunsScored)

history$Pick_SpreadTotal <- as.numeric(history$Pick_SpreadTotal)

history <- history %>%
  rowwise() %>%
  mutate(Winner = if_else(grepl("Moneyline", bet_type),
                           if_else(VisitorRunsScored > HomeRunsScore,
                                   AwayTeam,
                                   if_else(VisitorRunsScored == HomeRunsScore,
                                           "Push",
                                           HomeTeam)),
                           if_else(grepl("Runline", bet_type),
                                   if_else(Pick == paste0(AwayTeam),
                                           if_else(VisitorRunsScored + Pick_SpreadTotal > HomeRunsScore,
                                                   AwayTeam,
                                                   HomeTeam),
                                           if_else(HomeRunsScore + Pick_SpreadTotal > VisitorRunsScored,
                                                   HomeTeam,
                                                   AwayTeam)),
                                   if_else(grepl("Team Total", bet_type) & grepl(paste0(AwayTeam), bet_type),
                                           if_else(VisitorRunsScored > Pick_SpreadTotal,
                                                   "Over",
                                                   if_else(VisitorRunsScored == Pick_SpreadTotal,
                                                           "Push",
                                                           "Under")),
                                           if_else(grepl("Team Total", bet_type) & grepl(paste0(HomeTeam), bet_type),
                                                   if_else(HomeRunsScore > Pick_SpreadTotal,
                                                           "Over",
                                                           if_else(HomeRunsScore == Pick_SpreadTotal,
                                                                   "Push",
                                                                   "Under")),
                                                   if_else(TotalScore > Pick_SpreadTotal,
                                                           "Over",
                                                           if_else(TotalScore == Pick_SpreadTotal,
                                                                   "Push",
                                                                   "Under")))))),
          Pick_Correct = if_else(Winner == Pick, 1, 0),
          Units = if_else(Pick_Correct == 1, 
                          if_else(Pick_Odds > 0, Pick_Odds / 100, 1),
                          if_else(Pick_Odds > 0, -1, Pick_Odds / 100)),
          Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
          Half_Kelly_Bet = if_else(Kelly_Criteria < 0.1, 5, Kelly_Criteria * 50),
          Kelly_Profit = Units * Kelly_Bet,
          Half_Kelly_Profit = Units * Half_Kelly_Bet)

write.csv(history, "machine_pa.csv", row.names = FALSE)

types <- filter(history, Winner != "Push")

types <- select(types, gamedate, bet_type, Pick_WinProb, Fract_Odds, Kelly_Criteria, Pick_Correct, Units, Pick_Odds) %>%
  mutate(bet_type = case_when(bet_type == "Moneyline - Game" ~ "ML",
                              bet_type == "Moneyline - 5 Inning Line" ~ "F5ML",
                              bet_type == "Runline - Game" ~ "RL",
                              bet_type == "Runline - 5 Inning Line" ~ "F5RL",
                              grepl("Alternate Runline - Game", bet_type) ~ "Alt RL",
                              bet_type == "Total - Game" ~ "Total",
                              bet_type == "Total - 5 Inning Line" ~ "F5 Total",
                              grepl("Alternate Total - Game", bet_type) ~ "Alt Total",
                              grepl("Alternate Total - 5 Inning Line", bet_type) ~ "Alt F5 Total",
                              (grepl("Team Total", bet_type)) & (Pick_Odds >= -145) & (Pick_Odds <= 130) ~ "TT",
                              (grepl("Team Total", bet_type)) & ((Pick_Odds <= -145) | (Pick_Odds >= 130)) ~ "Alt TT",
                              TRUE ~ "Other"))

types %>%
  filter(gamedate > as.Date("2021-09-11") & Kelly_Criteria >= 0) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter(gamedate > as.Date("2021-09-11") & Kelly_Criteria >= 0 & 
           bet_type != "F5 Total" & bet_type != "Total" & bet_type != "TT" &
           bet_type != "Alt F5 Total" & bet_type != "Alt Total" & bet_type != "Alt TT") %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter(gamedate > as.Date("2021-09-11") & bet_type == "TT") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

history_filtered <- filter(history, bet_type == "Moneyline - Game")

mean(history_filtered$Pick_Correct)
sum(history_filtered$Kelly_Profit)
sum(history_filtered$Half_Kelly_Profit)


# Update Win Prob

# types <- mutate(types, model_update = if_else(as.Date(gamedate) < as.Date("2021-05-12"),
#                                               "Pre 2021",
#                                               "May 2021"))
# 
# update_only <- filter(types, gamedate > as.Date("2021-05-12"))
# update_glm <- glm(Pick_Correct ~ bet_type + Pick_WinProb, family = binomial, data = update_only)
# summary(update_glm)
# update_only <- mutate(update_only, 
#                 CorrectProb = predict(update_glm, update_only, type = "response"),
#                 tier = as.factor(round_any(CorrectProb, 0.05, floor)),
#                 Kelly_Criteria_new = (CorrectProb * (Fract_Odds + 1) - 1) / Fract_Odds,
#                 KC_tier = as.factor(round_any(Kelly_Criteria_new, 0.05, floor)),
#                 Kelly_Bet = if_else(Kelly_Criteria_new < 0.1, 10, Kelly_Criteria_new * 100),
#                 Kelly_Profit = Units * Kelly_Bet,
#                 bets = 1)
# update_only %>%
#   filter(Kelly_Criteria_new > 0) %>%
#   group_by(KC_tier) %>% 
#   dplyr::summarise(bets = sum(bets), 
#                    HitRate = mean(Pick_Correct), 
#                    Units = sum(Units),
#                    Kelly_Profit = sum(Kelly_Profit)) %>%
#   mutate(Units_per_bet = Units / bets)
# 
# types_glm <- glm(Pick_Correct ~ bet_type + Pick_WinProb, family = binomial, data = types)
# summary(types_glm)
# 
# types <- mutate(types, 
#                 CorrectProb = predict(types_glm, types, type = "response"),
#                 tier = as.factor(round_any(CorrectProb, 0.05, floor)),
#                 Kelly_Criteria_new = (CorrectProb * (Fract_Odds + 1) - 1) / Fract_Odds,
#                 KC_tier = as.factor(round_any(Kelly_Criteria_new, 0.05, floor)),
#                 Kelly_Bet = if_else(Kelly_Criteria_new < 0.1, 10, Kelly_Criteria_new * 100),
#                 Kelly_Profit = Units * Kelly_Bet,
#                 bets = 1)                  

email_table_1 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & Kelly_Criteria >= 0) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         bets = 1) %>%
  group_by(bet_type) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets)

df_html_1 <- print(xtable(email_table_1), type = "html", print.results = FALSE)

email_table_2 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "F5 Total") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_2 <- print(xtable(email_table_2), type = "html", print.results = FALSE)

email_table_3 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "Total") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_3 <- print(xtable(email_table_3), type = "html", print.results = FALSE)

email_table_4 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "ML") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_4 <- print(xtable(email_table_4), type = "html", print.results = FALSE)

email_table_5 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "F5ML") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_5 <- print(xtable(email_table_5), type = "html", print.results = FALSE)

email_table_6 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "RL") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_6 <- print(xtable(email_table_6), type = "html", print.results = FALSE)

email_table_7 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "F5RL") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_7 <- print(xtable(email_table_7), type = "html", print.results = FALSE)

email_table_8 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "TT") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_8 <- print(xtable(email_table_8), type = "html", print.results = FALSE)

email_table_9 <- types %>%
  filter(gamedate > as.Date("2021-05-12") & bet_type == "Alt RL") %>%
  #filter(Kelly_Criteria >= 0.45) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         KC_tier = round_any(Kelly_Criteria, 0.05, floor),
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_9 <- print(xtable(email_table_9), type = "html", print.results = FALSE)

todays_bets <- read.csv("todays_bets.csv")

todays_bets <- mutate(todays_bets,
                      bet_type_full = bet_type,
                      bet_type = case_when(bet_type == "Moneyline - Game" ~ "ML",
                                           bet_type == "Moneyline - 5 Inning Line" ~ "F5ML",
                                           bet_type == "Runline - Game" ~ "RL",
                                           bet_type == "Runline - 5 Inning Line" ~ "F5RL",
                                           grepl("Alternate Runline - Game", bet_type) ~ "Alt RL",
                                           bet_type == "Total - Game" ~ "Total",
                                           bet_type == "Total - 5 Inning Line" ~ "F5 Total",
                                           grepl("Alternate Total - Game", bet_type) ~ "Alt Total",
                                           grepl("Alternate Total - 5 Inning Line", bet_type) ~ "Alt F5 Total",
                                           (grepl("Team Total", bet_type)) & (Pick_Odds >= -145) & (Pick_Odds <= 130) ~ "TT",
                                           (grepl("Team Total", bet_type)) & ((Pick_Odds <= -145) | (Pick_Odds >= 130)) ~ "Alt TT",
                                           TRUE ~ "Other"),
                      KC_tier = as.factor(round_any(Kelly_Criteria, 0.05, floor)))

write.csv(todays_bets, "tiered_bets.csv", row.names = FALSE)

## Send an email

Outlook <- COMCreate("Outlook.Application")

Email = Outlook$CreateItem(0)
Email[["to"]] = paste("dnolen@smu.edu", "jorler@smu.edu", sep = ";", collapse = NULL)
#Email[["to"]] = "dnolen@smu.edu"
Email[["subject"]] = paste0("MLB Machine Picks: ", Sys.Date())
Email[["HTMLbody"]] = sprintf("
Today's Machine picks are in! Find attached three documents. todays_bets.csv contains all of the pertinent betting information for today. todays_data_full.csv contains the same information plus all of the data points used by The Machine to produce its projections. tiered_bets.csv contains the machine picks grouped by bet type and Kelly Criteria tier. 
</p><br></p>
Below are the Machine results so far since the first in-season update on 5/12, grouped by bet type. The Kelly Profit uses a betting strategy where the KC is multiplied by 100 to get the wager amount (with any KC below 0.05 betting $5).
</p><br></p>
%s
</p><br></p>
Below are the results for each different bet type since 5/12, grouped by KC tier.
</p><br></p>
F5 Totals:
</p><br></p>
%s
</p><br></p>
Totals:
</p><br></p>
%s
</p><br></p>
ML:
</p><br></p>
%s
</p><br></p>
F5ML:
</p><br></p>
%s
</p><br></p>
RL:
</p><br></p>
%s
</p><br></p>
F5RL:
</p><br></p>
%s
</p><br></p>
TT:
</p><br></p>
%s
</p><br></p>
Alt RL:
</p><br></p>
%s
</p><br></p>
Happy Machine Season!
", df_html_1, df_html_2, df_html_3, df_html_4, df_html_5, df_html_6, df_html_7, df_html_8, df_html_9)
Email[["attachments"]]$Add("C:/Users/danie/Desktop/Baseball Stuff/MLB Team Projections/tiered_bets.csv")
Email[["attachments"]]$Add("C:/Users/danie/Desktop/Baseball Stuff/MLB Team Projections/todays_bets.csv")
Email[["attachments"]]$Add("C:/Users/danie/Desktop/Baseball Stuff/MLB Team Projections/todays_data_full.csv")

Email$Send()

h2o.shutdown()








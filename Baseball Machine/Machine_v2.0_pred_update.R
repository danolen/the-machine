### The Machine v2.0
### Predictions

library("pacman")
p_load("tidyverse", "h2o", "bit64", "zoo", "reshape", "readxl", "DataCombine")
#install.packages("devtools")
#devtools::install_github("beanumber/mlbgm")
library("mlbgm")

setwd("C:/Users/danie/Desktop/Baseball Stuff/MLB Team Projections")

gl2019_2020 <- read.csv("gl2019_2020.csv")
gl2019_2020$Date <- as.Date(gl2019_2020$Date)

## Adjust IP columns to use .333 instead of .1 for a third of an inning

gl2019_2020 <- mutate(gl2019_2020,
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

gl2019_2020 <- mutate(gl2019_2020,
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

## Identify response (y) and predictor (x) column names for component models

y_VisitorRunsScored <- 'VisitorRunsScored'
y_HomeRunsScore <- 'HomeRunsScore'
y_VisitorRunsScored_F5 <- 'F5_VisitorRunsScored'
y_HomeRunsScore_F5 <- 'F5_HomeRunsScored'

x_VisitorRunsScored <- select(gl2019_2020, 
                              HomeSP_K.:HomeSP_WAR, HomeSP_O.Swing.:HomeSP_HardHit., 
                              VisitingBatters_HR_L7, VisitingBatters_SB_L7:VisitingBatters_HardHit._L7,
                              VisitingBatters_HR_L14, VisitingBatters_SB_L14:VisitingBatters_HardHit._L14,
                              VisitingBatters_HR_L30, VisitingBatters_SB_L30:VisitingBatters_HardHit._L30,
                              VisitingBatters_HR, VisitingBatters_SB:VisitingBatters_HardHit.,
                              HomeBullpen_K._L7:HomeBullpen_HardHit._L7,
                              HomeBullpen_K._L14:HomeBullpen_HardHit._L14,
                              HomeBullpen_K._L30:HomeBullpen_HardHit._L30,
                              HomeBullpen_K.:HomeBullpen_HardHit.,
                              HomeSP_IPperStart, HomeSP_IPperG, 
                              Visiting_BWARP500_proj, Visiting_bat_WAR500_proj,
                              Home_pit_WAR200_proj, Home_PWARP200_proj) %>% colnames()

x_HomeRunsScore <- select(gl2019_2020, 
                              VisitingSP_K.:VisitingSP_WAR, VisitingSP_O.Swing.:VisitingSP_HardHit., 
                              HomeBatters_HR_L7, HomeBatters_SB_L7:HomeBatters_HardHit._L7,
                              HomeBatters_HR_L14, HomeBatters_SB_L14:HomeBatters_HardHit._L14,
                              HomeBatters_HR_L30, HomeBatters_SB_L30:HomeBatters_HardHit._L30,
                              HomeBatters_HR, HomeBatters_SB:HomeBatters_HardHit.,
                              VisitingBullpen_K._L7:VisitingBullpen_HardHit._L7,
                              VisitingBullpen_K._L14:VisitingBullpen_HardHit._L14,
                              VisitingBullpen_K._L30:VisitingBullpen_HardHit._L30,
                              VisitingBullpen_K.:VisitingBullpen_HardHit.,
                              VisitingSP_IPperStart, VisitingSP_IPperG,
                              Home_BWARP500_proj, Home_bat_WAR500_proj,
                              Visiting_pit_WAR200_proj, Visiting_PWARP200_proj) %>% colnames()

train <- select(gl2019_2020, x_VisitorRunsScored, x_HomeRunsScore,
                y_VisitorRunsScored, y_HomeRunsScore,
                y_VisitorRunsScored_F5, y_HomeRunsScore_F5)

## Bring in 2021 data
scores <- read_excel("2021 MLB Score Tracker.xlsx")
team_names <- read.csv("team_names.csv")

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

## Get SP's from picks history file
history <- read_excel("Machine Picks History.xlsx") %>%
  select(gamedate:HomeSP) %>%
  distinct()

scores <- left_join(scores, history, by = c("gamedate", "AwayTeam", "HomeTeam")) %>%
  filter(!is.na(AwaySP) | !is.na(HomeSP))

## Join daily data to scores data

scores <- left_join(scores, daily_pitchers_2021, 
                         by = c("gamedate" = "Date", "AwayTeam" = "Team", "AwaySP" = "Name"))
colnames(scores)[10:36] <- paste0("VisitingSP_", colnames(scores)[10:36])
scores <- left_join(scores, daily_pitchers_2021,
                         by = c("gamedate" = "Date", "HomeTeam" = "Team", "HomeSP" = "Name"))
colnames(scores)[37:63] <- paste0("HomeSP_", colnames(scores)[37:63])
scores <- left_join(scores, daily_team_batting_2021,
                         by = c("gamedate" = "Date", "AwayTeam" = "Team"))
colnames(scores)[64:171] <- paste0("VisitingBatters_", colnames(scores)[64:171])
scores <- left_join(scores, daily_team_batting_2021,
                         by = c("gamedate" = "Date", "HomeTeam" = "Team"))
colnames(scores)[172:279] <- paste0("HomeBatters_", colnames(scores)[172:279])
scores <- left_join(scores, daily_team_bullpen_2021,
                         by = c("gamedate" = "Date", "AwayTeam" = "Team"))
colnames(scores)[280:375] <- paste0("VisitingBullpen_", colnames(scores)[280:375])
scores <- left_join(scores, daily_team_bullpen_2021,
                         by = c("gamedate" = "Date", "HomeTeam" = "Team"))
colnames(scores)[376:471] <- paste0("HomeBullpen_", colnames(scores)[376:471])

scores <- filter(scores, !is.na(AwaySP) & !is.na(HomeSP))

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

scores <- left_join(scores, proj_batting_21, by = c("AwayTeam" = "team"))
colnames(scores)[472:473] <- paste0("Visiting_", colnames(scores)[472:473])
scores <- left_join(scores, proj_batting_21, by = c("HomeTeam" = "team"))
colnames(scores)[474:475] <- paste0("Home_", colnames(scores)[474:475])

# Starting pitchers

pit_WAR_21 <- DC_pitching_21 %>%
  group_by(ï..Name) %>%
  dplyr::summarize(pit_WAR200_proj = (sum(WAR) / sum(IP)) * 200)

PWARP_21 <- PECOTA_pitching_21 %>%
  group_by(name) %>%
  dplyr::summarize(PWARP200_proj = (sum(warp) / sum(as.numeric(ip))) * 200)

proj_SP_21 <- full_join(pit_WAR_21, PWARP_21, by = c("ï..Name" = "name"))

scores <- left_join(scores, proj_SP_21, by = c("AwaySP" = "ï..Name"))
colnames(scores)[476:477] <- paste0("Visiting_", colnames(scores)[476:477])
scores <- left_join(scores, proj_SP_21, by = c("HomeSP" = "ï..Name"))
colnames(scores)[478:479] <- paste0("Home_", colnames(scores)[478:479])

## Adjust IP columns to use .333 instead of .1 for a third of an inning

scores <- mutate(scores,
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

scores <- mutate(scores,
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

## Union all data
scores <- select(scores, x_VisitorRunsScored, x_HomeRunsScore,
                 y_VisitorRunsScored, y_HomeRunsScore,
                 y_VisitorRunsScored_F5, y_HomeRunsScore_F5)

train <- union(train, scores)

## For James
away_scores <- select(scores, x_VisitorRunsScored, y_VisitorRunsScored, y_VisitorRunsScored_F5)
home_scores <- select(scores, x_HomeRunsScore, y_HomeRunsScore, y_HomeRunsScore_F5)

write.csv(away_scores, "away_scores.csv", row.names = FALSE)
write.csv(home_scores, "home_scores.csv", row.names = FALSE)

## train models, save to disk, and predict
h2o.init(nthreads = -1)

train <- as.h2o(train)

model_VisitorRunsScored <- h2o.automl(x = x_VisitorRunsScored,
                                      y = y_VisitorRunsScored,
                                      training_frame = train,
                                      nfolds = 10)
VisitorRunsScored_lb <- model_VisitorRunsScored@leaderboard
VisitorRunsScored_leader <- model_VisitorRunsScored@leader
saved_path_VRS <- h2o.saveModel(object = VisitorRunsScored_leader, path = getwd(), force = TRUE)
print(saved_path_VRS) # Save this somewhere
loaded_model_VRS <- h2o.loadModel(saved_path_VRS)

model_HomeRunsScore <- h2o.automl(x = x_HomeRunsScore,
                                  y = y_HomeRunsScore,
                                  training_frame = train,
                                  nfolds = 10)
HomeRunsScore_lb <- model_HomeRunsScore@leaderboard
HomeRunsScore_leader <- model_HomeRunsScore@leader
saved_path_HRS <- h2o.saveModel(object = HomeRunsScore_leader, path = getwd(), force = TRUE)
print(saved_path_HRS) # Save this somewhere
loaded_model_HRS <- h2o.loadModel(saved_path_HRS)

model_VisitorRunsScored_F5 <- h2o.automl(x = x_VisitorRunsScored,
                                          y = y_VisitorRunsScored_F5,
                                          training_frame = train,
                                          nfolds = 10)
VisitorRunsScored_F5_lb <- model_VisitorRunsScored_F5@leaderboard
VisitorRunsScored_F5_leader <- model_VisitorRunsScored_F5@leader
saved_path_VRS_F5 <- h2o.saveModel(object = VisitorRunsScored_F5_leader, path = getwd(), force = TRUE)
print(saved_path_VRS_F5)

model_HomeRunsScore_F5 <- h2o.automl(x = x_HomeRunsScore,
                                      y = y_HomeRunsScore_F5,
                                      training_frame = train,
                                      nfolds = 10)
HomeRunsScore_F5_lb <- model_HomeRunsScore_F5@leaderboard
HomeRunsScore_F5_leader <- model_HomeRunsScore_F5@leader
saved_path_HRS_F5 <- h2o.saveModel(object = HomeRunsScore_F5_leader, path = getwd(), force = TRUE)
print(saved_path_HRS_F5)

h2o.shutdown(prompt = FALSE)

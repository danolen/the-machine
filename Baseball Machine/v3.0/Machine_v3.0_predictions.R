### The Machine v3.0
### Predictions

overallStart <- Sys.time()
library("pacman")
p_load(
  "tidyverse",
  "caret",
  "parallel",
  "doParallel"
  )

DF19 <- readRDS("Baseball Machine/Daily Files/2019/full_training_data.rds") %>%
  ungroup()
DF20 <- readRDS("Baseball Machine/Daily Files/2020/full_training_data.rds") %>%
  ungroup() %>% 
  rename(WARP200_HomeSP = HomeSP_WARP200,
         WARP200_AwaySP = AwaySP_WARP200)
DF21 <- readRDS("Baseball Machine/Daily Files/2021/full_training_data.rds") %>%
  ungroup() %>% 
  rename(WARP200_HomeSP = HomeSP_WARP200,
         WARP200_AwaySP = AwaySP_WARP200)
DF22 <- readRDS("Baseball Machine/Daily Files/2022/full_training_data.rds") %>%
  ungroup() %>%
  mutate(HomeSP_WARP200 = case_when(!is.nan(HomeSP_WARP200) ~ HomeSP_WARP200),
         AwaySP_WARP200 = case_when(!is.nan(AwaySP_WARP200) ~ AwaySP_WARP200)) %>% 
  rename(WARP200_HomeSP = HomeSP_WARP200,
         WARP200_AwaySP = AwaySP_WARP200)
DF23 <- readRDS("Baseball Machine/Daily Files/2023/full_training_data.rds") %>%
  ungroup() %>% 
  rename(WARP200_HomeSP = HomeSP_WARP200,
         WARP200_AwaySP = AwaySP_WARP200)

DF <- DF19 %>% 
  bind_rows(DF20, DF21, DF22, DF23) %>% 
  filter(doubleHeader == 'N')

## Adjust IP columns to use .333 instead of .1 for a third of an inning

DF_IP_Adj <- DF %>% 
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
    IP_HomeBullpen = as.integer(IP_HomeBullpen) + (IP_HomeBullpen %% 1 * 3.33))

## Adjust counting stats to rate stats
# pitching stats converted to per 200 IP
# batting stats converted to per 500 PA
## Also add SP IP per start and per appearance

DF_Rate_Adj <- DF_IP_Adj %>% 
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
  mutate(month = lubridate::month(officialDate, label = TRUE, abbr = FALSE),
         doubleHeader = case_when(doubleHeader == 'S' ~ 'Y',
                                  TRUE ~ doubleHeader)) %>% 
  select(-Team_HomeSP, -Team_AwaySP) %>% 
  mutate(Outcome_FG_ML = as.factor(case_when(home_runs_final > away_runs_final ~ 'Win',
                                             TRUE ~ 'Lose')),
         Outcome_FG_Minus_1.5 = as.factor(case_when(home_runs_final - 1.5 > away_runs_final ~ 'Win',
                                                    TRUE ~ 'Lose')),
         Outcome_FG_Minus_2.5 = as.factor(case_when(home_runs_final - 2.5 > away_runs_final ~ 'Win',
                                                    TRUE ~ 'Lose')),
         Outcome_FG_Plus_1.5 = as.factor(case_when(home_runs_final + 1.5 > away_runs_final ~ 'Win',
                                                    TRUE ~ 'Lose')),
         Outcome_FG_Plus_2.5 = as.factor(case_when(home_runs_final + 2.5 > away_runs_final ~ 'Win',
                                                    TRUE ~ 'Lose')),
         Outcome_FG_Total_6.5 = as.factor(case_when(home_runs_final + away_runs_final > 6.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_FG_Total_7 = as.factor(case_when(home_runs_final + away_runs_final > 7 ~ 'Over',
                                                  home_runs_final + away_runs_final == 7 ~ 'Push',
                                                  TRUE ~ 'Under')),
         Outcome_FG_Total_7.5 = as.factor(case_when(home_runs_final + away_runs_final > 7.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_FG_Total_8 = as.factor(case_when(home_runs_final + away_runs_final > 8 ~ 'Over',
                                                  home_runs_final + away_runs_final == 8 ~ 'Push',
                                                  TRUE ~ 'Under')),
         Outcome_FG_Total_8.5 = as.factor(case_when(home_runs_final + away_runs_final > 8.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_FG_Total_9 = as.factor(case_when(home_runs_final + away_runs_final > 9 ~ 'Over',
                                                  home_runs_final + away_runs_final == 9 ~ 'Push',
                                                  TRUE ~ 'Under')),
         Outcome_FG_Total_9.5 = as.factor(case_when(home_runs_final + away_runs_final > 9.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_FG_Total_10 = as.factor(case_when(home_runs_final + away_runs_final > 10 ~ 'Over',
                                                  home_runs_final + away_runs_final == 10 ~ 'Push',
                                                  TRUE ~ 'Under')),
         Outcome_FG_Total_10.5 = as.factor(case_when(home_runs_final + away_runs_final > 10.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_FG_Total_11 = as.factor(case_when(home_runs_final + away_runs_final > 11 ~ 'Over',
                                                  home_runs_final + away_runs_final == 11 ~ 'Push',
                                                  TRUE ~ 'Under')),
         Outcome_FG_Total_11.5 = as.factor(case_when(home_runs_final + away_runs_final > 11.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_F5_ML = as.factor(case_when(home_runs_5th > away_runs_5th ~ 'Win',
                                             home_runs_5th == away_runs_5th ~ 'Push',
                                             TRUE ~ 'Lose')),
         Outcome_F5_Minus_0.5 = as.factor(case_when(home_runs_5th - 0.5 > away_runs_5th ~ 'Win',
                                                    TRUE ~ 'Lose')),
         Outcome_F5_Minus_1.5 = as.factor(case_when(home_runs_5th - 1.5 > away_runs_5th ~ 'Win',
                                                    TRUE ~ 'Lose')),
         Outcome_F5_Plus_0.5 = as.factor(case_when(home_runs_5th + 0.5 > away_runs_5th ~ 'Win',
                                                   TRUE ~ 'Lose')),
         Outcome_F5_Plus_1.5 = as.factor(case_when(home_runs_5th + 1.5 > away_runs_5th ~ 'Win',
                                                   TRUE ~ 'Lose')),
         Outcome_F5_Total_3.5 = as.factor(case_when(home_runs_5th + away_runs_5th > 3.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_F5_Total_4 = as.factor(case_when(home_runs_5th + away_runs_5th > 4 ~ 'Over',
                                                  home_runs_5th + away_runs_5th == 4 ~ 'Push',
                                                  TRUE ~ 'Under')),
         Outcome_F5_Total_4.5 = as.factor(case_when(home_runs_5th + away_runs_5th > 4.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_F5_Total_5 = as.factor(case_when(home_runs_5th + away_runs_5th > 5 ~ 'Over',
                                                  home_runs_5th + away_runs_5th == 5 ~ 'Push',
                                                  TRUE ~ 'Under')),
         Outcome_F5_Total_5.5 = as.factor(case_when(home_runs_5th + away_runs_5th > 5.5 ~ 'Over',
                                                    TRUE ~ 'Under')),
         Outcome_F1_Total_0.5 = as.factor(case_when(home_runs_1st + away_runs_1st > 0.5 ~ 'Over',
                                                    TRUE ~ 'Under')))

## Create separate DFs for Home/Away Runs Scored as well as 1st Inning/F5/Full Game

train_full_game_home <- DF_Rate_Adj %>% 
  ungroup() %>% 
  select(home_runs_final, home_team_season, month, home_team_league_name, home_team_division_name,
         dayNight, venue.name, contains('_AwaySP'), contains('_HomeBatters'),
         -Def_L7_HomeBatters, -Def_L14_HomeBatters, -Def_L30_HomeBatters, -Def_HomeBatters,
         Def_L7_AwayBatters, Def_L14_AwayBatters, Def_L30_AwayBatters, Def_AwayBatters,
         contains('_AwayBullpen')) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Home')

train_full_game_away <- DF_Rate_Adj %>% 
  ungroup() %>% 
  select(away_runs_final, home_team_season, month, away_team_league_name, away_team_division_name,
         dayNight, venue.name, contains('_HomeSP'), contains('_AwayBatters'),
         Def_L7_HomeBatters, Def_L14_HomeBatters, Def_L30_HomeBatters, Def_HomeBatters,
         -Def_L7_AwayBatters, -Def_L14_AwayBatters, -Def_L30_AwayBatters, -Def_AwayBatters,
         contains('_HomeBullpen')) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("away_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Away')

train_full_game <- train_full_game_home %>% 
  bind_rows(train_full_game_away) %>% 
  filter(!is.na(IPSP) &
           !is.na(PA_L7Batters) &
           !is.na(IP_L7Bullpen) &
           !is.na(PA_L30Batters) &
           !is.na(IPBullpen)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Outcome_FG_TT_2.5 = as.factor(case_when(runs_final > 2.5 ~ 'Over',
                                                 TRUE ~ 'Under')),
         Outcome_FG_TT_3 = as.factor(case_when(runs_final > 3 ~ 'Over',
                                               runs_final == 3 ~ 'Push',
                                               TRUE ~ 'Under')),
         Outcome_FG_TT_3.5 = as.factor(case_when(runs_final > 3.5 ~ 'Over',
                                                 TRUE ~ 'Under')),
         Outcome_FG_TT_4 = as.factor(case_when(runs_final > 4 ~ 'Over',
                                               runs_final == 4 ~ 'Push',
                                               TRUE ~ 'Under')),
         Outcome_FG_TT_4.5 = as.factor(case_when(runs_final > 4.5 ~ 'Over',
                                                 TRUE ~ 'Under')),
         Outcome_FG_TT_5 = as.factor(case_when(runs_final > 5 ~ 'Over',
                                               runs_final == 5 ~ 'Push',
                                               TRUE ~ 'Under')),
         Outcome_FG_TT_5.5 = as.factor(case_when(runs_final > 5.5 ~ 'Over',
                                                 TRUE ~ 'Under')),
         Outcome_FG_TT_6 = as.factor(case_when(runs_final > 6 ~ 'Over',
                                               runs_final == 6 ~ 'Push',
                                               TRUE ~ 'Under')),
         Outcome_FG_TT_6.5 = as.factor(case_when(runs_final > 6.5 ~ 'Over',
                                                 TRUE ~ 'Under')))

train_F5_home <- DF_Rate_Adj %>% 
  ungroup() %>% 
  select(home_runs_5th, home_team_season, month, home_team_league_name, home_team_division_name,
         dayNight, venue.name, contains('_AwaySP'), contains('_HomeBatters'),
         -Def_L7_HomeBatters, -Def_L14_HomeBatters, -Def_L30_HomeBatters, -Def_HomeBatters,
         Def_L7_AwayBatters, Def_L14_AwayBatters, Def_L30_AwayBatters, Def_AwayBatters) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Home')

train_F5_away <- DF_Rate_Adj %>% 
  ungroup() %>% 
  select(away_runs_5th, home_team_season, month, away_team_league_name, away_team_division_name,
         dayNight, venue.name, contains('_HomeSP'), contains('_AwayBatters'),
         Def_L7_HomeBatters, Def_L14_HomeBatters, Def_L30_HomeBatters, Def_HomeBatters,
         -Def_L7_AwayBatters, -Def_L14_AwayBatters, -Def_L30_AwayBatters, -Def_AwayBatters) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("away_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Away')

train_F5 <- train_F5_home %>% 
  bind_rows(train_F5_away) %>% 
  filter(!is.na(IPSP) &
           !is.na(PA_L7Batters) &
           !is.na(PA_L30Batters)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Outcome_F5_TT_0.5 = as.factor(case_when(runs_5th > 0.5 ~ 'Over',
                                                 TRUE ~ 'Under')),
         Outcome_F5_TT_1 = as.factor(case_when(runs_5th > 1 ~ 'Over',
                                               runs_5th == 1 ~ 'Push',
                                               TRUE ~ 'Under')),
         Outcome_F5_TT_1.5 = as.factor(case_when(runs_5th > 1.5 ~ 'Over',
                                                 TRUE ~ 'Under')),
         Outcome_F5_TT_2 = as.factor(case_when(runs_5th > 2 ~ 'Over',
                                               runs_5th == 2 ~ 'Push',
                                               TRUE ~ 'Under')),
         Outcome_F5_TT_2.5 = as.factor(case_when(runs_5th > 2.5 ~ 'Over',
                                                 TRUE ~ 'Under')),
         Outcome_F5_TT_3 = as.factor(case_when(runs_5th > 3 ~ 'Over',
                                               runs_5th == 3 ~ 'Push',
                                               TRUE ~ 'Under')),
         Outcome_F5_TT_3.5 = as.factor(case_when(runs_5th > 3.5 ~ 'Over',
                                                 TRUE ~ 'Under')))

train_F1_home <- DF_Rate_Adj %>% 
  ungroup() %>% 
  select(home_runs_1st, home_team_season, month, home_team_league_name, home_team_division_name,
         dayNight, venue.name, contains('_AwaySP'), contains('_HomeBatters'),
         -Def_L7_HomeBatters, -Def_L14_HomeBatters, -Def_L30_HomeBatters, -Def_HomeBatters,
         Def_L7_AwayBatters, Def_L14_AwayBatters, Def_L30_AwayBatters, Def_AwayBatters) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Home')

train_F1_away <- DF_Rate_Adj %>% 
  ungroup() %>% 
  select(away_runs_1st, home_team_season, month, away_team_league_name, away_team_division_name,
         dayNight, venue.name, contains('_HomeSP'), contains('_AwayBatters'),
         Def_L7_HomeBatters, Def_L14_HomeBatters, Def_L30_HomeBatters, Def_HomeBatters,
         -Def_L7_AwayBatters, -Def_L14_AwayBatters, -Def_L30_AwayBatters, -Def_AwayBatters) %>%
  rename_all(~gsub("home_","",.)) %>% 
  rename_all(~gsub("away_","",.)) %>% 
  rename_all(~gsub("_Home","",.)) %>% 
  rename_all(~gsub("_Away","",.)) %>% 
  mutate(home_or_away = 'Away')

train_F1 <- train_F1_home %>% 
  bind_rows(train_F1_away) %>% 
  filter(!is.na(IPSP) &
           !is.na(PA_L7Batters) &
           !is.na(PA_L30Batters)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Outcome_F1_TT_0.5 = as.factor(case_when(runs_1st > 0.5 ~ 'Over',
                                                 TRUE ~ 'Under')))

## Create training DFs for classification models

doubles <- DF_Rate_Adj %>% 
  ungroup() %>% 
  select(home_team_season, month, home_team_league_name, home_team_division_name,
         dayNight, venue.name, contains('_AwaySP'),
         contains('_HomeSP'), contains('_HomeBatters'), contains('_AwayBatters'),
         contains('_AwayBullpen'), contains('_HomeBullpen'), contains('Outcome_')) %>% 
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
  replace(is.na(.), 0)

## Train regression models

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
full_game_gbm_mod <- train(runs_final ~ .,
                 data = train_full_game %>% select(-contains("Outcome_")),
                 method = "gbm",
                 trControl = fitControl)
set.seed(1234)
full_game_cub_mod <- train(runs_final ~ .,
                           data = train_full_game %>% select(-contains("Outcome_")),
                 method = "cubist",
                 trControl = fitControl,
                 tuneGrid = expand.grid(.committees=20,
                                        .neighbors=9))
set.seed(1234)
full_game_rf_mod <- train(runs_final ~ .,
                          data = train_full_game %>% select(-contains("Outcome_")),
                method = "ranger",
                trControl = fitControl,
                tuneGrid = expand.grid(.mtry = c(10,15,20),
                                       .splitrule = c("variance", "extratrees"),
                                       .min.node.size = c(5,10)))
set.seed(1234)
full_game_pls_mod <- train(runs_final ~ .,
                           data = train_full_game %>% select(-contains("Outcome_")),
                 method = "pls",
                 trControl = fitControl,
                 tuneLength = 15,
                 preProc = c("center", "scale"))

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Full game runs regression model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(full_game_gbm_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/full_game_gbm.rds")
saveRDS(full_game_cub_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/full_game_cub.rds")
saveRDS(full_game_rf_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/full_game_rf.rds")
saveRDS(full_game_pls_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/full_game_pls.rds")

set.seed(1234)
allResamples <- resamples(list("GBM" = full_game_gbm_mod,
                               "Cubist" = full_game_cub_mod,
                               "RF" = full_game_rf_mod,
                               "PLS" = full_game_pls_mod
                               ))

parallelplot(allResamples, metric = "RMSE")
parallelplot(allResamples)
parallelplot(allResamples, metric = "Rsquared")

eval <- train_full_game

eval$GBM <- predict(full_game_gbm_mod, train_full_game)
eval$CUB <- predict(full_game_cub_mod, train_full_game)
eval$RF <- predict(full_game_rf_mod, train_full_game)
eval$PLS <- predict(full_game_pls_mod, train_full_game)

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_gbm_mod <- train(runs_5th ~ .,
                           data = train_F5 %>% select(-contains("Outcome_")),
                           method = "gbm",
                           trControl = fitControl)
set.seed(1234)
F5_cub_mod <- train(runs_5th ~ .,
                           data = train_F5 %>% select(-contains("Outcome_")),
                           method = "cubist",
                           trControl = fitControl,
                           tuneGrid = expand.grid(.committees=20,
                                                  .neighbors=9))
set.seed(1234)
F5_rf_mod <- train(runs_5th ~ .,
                          data = train_F5 %>% select(-contains("Outcome_")),
                          method = "ranger",
                          trControl = fitControl,
                          tuneGrid = expand.grid(.mtry = c(10,15,20),
                                                 .splitrule = c("variance", "extratrees"),
                                                 .min.node.size = c(5,10)))
set.seed(1234)
F5_pls_mod <- train(runs_5th ~ .,
                           data = train_F5 %>% select(-contains("Outcome_")),
                           method = "pls",
                           trControl = fitControl,
                           tuneLength = 15,
                           preProc = c("center", "scale"))

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 runs regression model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_gbm_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_gbm.rds")
saveRDS(F5_cub_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_cub.rds")
saveRDS(F5_rf_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_rf.rds")
saveRDS(F5_pls_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_pls.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F1_gbm_mod <- train(runs_1st ~ .,
                    data = train_F1 %>% select(-contains("Outcome_")),
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
F1_cub_mod <- train(runs_1st ~ .,
                    data = train_F1 %>% select(-contains("Outcome_")),
                    method = "cubist",
                    trControl = fitControl,
                    tuneGrid = expand.grid(.committees=20,
                                           .neighbors=9))
set.seed(1234)
F1_rf_mod <- train(runs_1st ~ .,
                   data = train_F1 %>% select(-contains("Outcome_")),
                   method = "ranger",
                   trControl = fitControl,
                   tuneGrid = expand.grid(.mtry = c(10,15,20),
                                          .splitrule = c("variance", "extratrees"),
                                          .min.node.size = c(5,10)))
set.seed(1234)
F1_pls_mod <- train(runs_1st ~ .,
                    data = train_F1 %>% select(-contains("Outcome_")),
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F1 runs regression model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F1_gbm_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_gbm.rds")
saveRDS(F1_cub_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_cub.rds")
saveRDS(F1_rf_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_rf.rds")
saveRDS(F1_pls_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_pls.rds")

## Train classification models

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_ML_gbm <- train(Outcome_FG_ML ~ .,
                     data = doubles %>% select(-contains('Outcome_'), Outcome_FG_ML),
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
FG_ML_pls <- train(Outcome_FG_ML ~ .,
                   data = doubles %>% select(-contains('Outcome_'), Outcome_FG_ML),
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
FG_ML_xgb <- train(Outcome_FG_ML ~ .,
                   data = doubles %>% select(-contains('Outcome_'), Outcome_FG_ML),
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_ML_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_ML_gbm.rds")
saveRDS(FG_ML_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_ML_pls.rds")
saveRDS(FG_ML_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_ML_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Minus_1.5_gbm <- train(Outcome_FG_Minus_1.5 ~ .,
                   data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Minus_1.5),
                   method = "gbm",
                   trControl = fitControl)
set.seed(1234)
FG_Minus_1.5_pls <- train(Outcome_FG_Minus_1.5 ~ .,
                   data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Minus_1.5),
                   method = "pls",
                   trControl = fitControl,
                   tuneLength = 15,
                   preProc = c("center", "scale"))
set.seed(1234)
FG_Minus_1.5_xgb <- train(Outcome_FG_Minus_1.5 ~ .,
                   data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Minus_1.5),
                   method = "xgbTree",
                   trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Minus 1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Minus_1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Minus_1.5_gbm.rds")
saveRDS(FG_Minus_1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Minus_1.5_pls.rds")
saveRDS(FG_Minus_1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Minus_1.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Minus_2.5_gbm <- train(Outcome_FG_Minus_2.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Minus_2.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Minus_2.5_pls <- train(Outcome_FG_Minus_2.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Minus_2.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Minus_2.5_xgb <- train(Outcome_FG_Minus_2.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Minus_2.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Minus 2.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Minus_2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Minus_2.5_gbm.rds")
saveRDS(FG_Minus_2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Minus_2.5_pls.rds")
saveRDS(FG_Minus_2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Minus_2.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Plus_1.5_gbm <- train(Outcome_FG_Plus_1.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Plus_1.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Plus_1.5_pls <- train(Outcome_FG_Plus_1.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Plus_1.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Plus_1.5_xgb <- train(Outcome_FG_Plus_1.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Plus_1.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Plus 1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Plus_1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Plus_1.5_gbm.rds")
saveRDS(FG_Plus_1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Plus_1.5_pls.rds")
saveRDS(FG_Plus_1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Plus_1.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Plus_2.5_gbm <- train(Outcome_FG_Plus_2.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Plus_2.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Plus_2.5_pls <- train(Outcome_FG_Plus_2.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Plus_2.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Plus_2.5_xgb <- train(Outcome_FG_Plus_2.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Plus_2.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Plus 2.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Plus_2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Plus_2.5_gbm.rds")
saveRDS(FG_Plus_2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Plus_2.5_pls.rds")
saveRDS(FG_Plus_2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Plus_2.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_6.5_gbm <- train(Outcome_FG_Total_6.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_6.5),
                         method = "gbm",
                         trControl = fitControl)
set.seed(1234)
FG_Total_6.5_pls <- train(Outcome_FG_Total_6.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_6.5),
                         method = "pls",
                         trControl = fitControl,
                         tuneLength = 15,
                         preProc = c("center", "scale"))
set.seed(1234)
FG_Total_6.5_xgb <- train(Outcome_FG_Total_6.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_6.5),
                         method = "xgbTree",
                         trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 6.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_6.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_6.5_gbm.rds")
saveRDS(FG_Total_6.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_6.5_pls.rds")
saveRDS(FG_Total_6.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_6.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_7_gbm <- train(Outcome_FG_Total_7 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_7),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Total_7_pls <- train(Outcome_FG_Total_7 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_7),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Total_7_xgb <- train(Outcome_FG_Total_7 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_7),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 7 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_7_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_7_gbm.rds")
saveRDS(FG_Total_7_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_7_pls.rds")
saveRDS(FG_Total_7_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_7_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_7.5_gbm <- train(Outcome_FG_Total_7.5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_7.5),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
FG_Total_7.5_pls <- train(Outcome_FG_Total_7.5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_7.5),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
FG_Total_7.5_xgb <- train(Outcome_FG_Total_7.5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_7.5),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 7.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_7.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_7.5_gbm.rds")
saveRDS(FG_Total_7.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_7.5_pls.rds")
saveRDS(FG_Total_7.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_7.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_8_gbm <- train(Outcome_FG_Total_8 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_8),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
FG_Total_8_pls <- train(Outcome_FG_Total_8 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_8),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
FG_Total_8_xgb <- train(Outcome_FG_Total_8 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_8),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 8 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_8_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_8_gbm.rds")
saveRDS(FG_Total_8_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_8_pls.rds")
saveRDS(FG_Total_8_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_8_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_8.5_gbm <- train(Outcome_FG_Total_8.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_8.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Total_8.5_pls <- train(Outcome_FG_Total_8.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_8.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Total_8.5_xgb <- train(Outcome_FG_Total_8.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_8.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 8.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_8.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_8.5_gbm.rds")
saveRDS(FG_Total_8.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_8.5_pls.rds")
saveRDS(FG_Total_8.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_8.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_9_gbm <- train(Outcome_FG_Total_9 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_9),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
FG_Total_9_pls <- train(Outcome_FG_Total_9 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_9),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
FG_Total_9_xgb <- train(Outcome_FG_Total_9 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_9),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 9 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_9_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_9_gbm.rds")
saveRDS(FG_Total_9_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_9_pls.rds")
saveRDS(FG_Total_9_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_9_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_9.5_gbm <- train(Outcome_FG_Total_9.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_9.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Total_9.5_pls <- train(Outcome_FG_Total_9.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_9.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Total_9.5_xgb <- train(Outcome_FG_Total_9.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_9.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 9.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_9.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_9.5_gbm.rds")
saveRDS(FG_Total_9.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_9.5_pls.rds")
saveRDS(FG_Total_9.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_9.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_10_gbm <- train(Outcome_FG_Total_10 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_10),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
FG_Total_10_pls <- train(Outcome_FG_Total_10 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_10),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
FG_Total_10_xgb <- train(Outcome_FG_Total_10 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_10),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 10 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_10_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_10_gbm.rds")
saveRDS(FG_Total_10_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_10_pls.rds")
saveRDS(FG_Total_10_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_10_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_10.5_gbm <- train(Outcome_FG_Total_10.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_10.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Total_10.5_pls <- train(Outcome_FG_Total_10.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_10.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Total_10.5_xgb <- train(Outcome_FG_Total_10.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_10.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 10.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_10.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_10.5_gbm.rds")
saveRDS(FG_Total_10.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_10.5_pls.rds")
saveRDS(FG_Total_10.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_10.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_11_gbm <- train(Outcome_FG_Total_11 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_11),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
FG_Total_11_pls <- train(Outcome_FG_Total_11 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_11),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
FG_Total_11_xgb <- train(Outcome_FG_Total_11 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_11),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 11 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_11_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_11_gbm.rds")
saveRDS(FG_Total_11_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_11_pls.rds")
saveRDS(FG_Total_11_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_11_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_Total_11.5_gbm <- train(Outcome_FG_Total_11.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_11.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
FG_Total_11.5_pls <- train(Outcome_FG_Total_11.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_11.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
FG_Total_11.5_xgb <- train(Outcome_FG_Total_11.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), Outcome_FG_Total_11.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG Total 11.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_Total_11.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_11.5_gbm.rds")
saveRDS(FG_Total_11.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_11.5_pls.rds")
saveRDS(FG_Total_11.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_Total_11.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_ML_gbm <- train(Outcome_F5_ML ~ .,
                   data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_ML),
                   method = "gbm",
                   trControl = fitControl)
set.seed(1234)
F5_ML_pls <- train(Outcome_F5_ML ~ .,
                   data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_ML),
                   method = "pls",
                   trControl = fitControl,
                   tuneLength = 15,
                   preProc = c("center", "scale"))
set.seed(1234)
F5_ML_xgb <- train(Outcome_F5_ML ~ .,
                   data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_ML),
                   method = "xgbTree",
                   trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_ML_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_ML_gbm.rds")
saveRDS(F5_ML_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_ML_pls.rds")
saveRDS(F5_ML_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_ML_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Minus_0.5_gbm <- train(Outcome_F5_Minus_0.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Minus_0.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
F5_Minus_0.5_pls <- train(Outcome_F5_Minus_0.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Minus_0.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
F5_Minus_0.5_xgb <- train(Outcome_F5_Minus_0.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Minus_0.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Minus 0.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Minus_0.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Minus_0.5_gbm.rds")
saveRDS(F5_Minus_0.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Minus_0.5_pls.rds")
saveRDS(F5_Minus_0.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Minus_0.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Minus_1.5_gbm <- train(Outcome_F5_Minus_1.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Minus_1.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
F5_Minus_1.5_pls <- train(Outcome_F5_Minus_1.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Minus_1.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
F5_Minus_1.5_xgb <- train(Outcome_F5_Minus_1.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Minus_1.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Minus 1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Minus_1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Minus_1.5_gbm.rds")
saveRDS(F5_Minus_1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Minus_1.5_pls.rds")
saveRDS(F5_Minus_1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Minus_1.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Plus_0.5_gbm <- train(Outcome_F5_Plus_0.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Plus_0.5),
                         method = "gbm",
                         trControl = fitControl)
set.seed(1234)
F5_Plus_0.5_pls <- train(Outcome_F5_Plus_0.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Plus_0.5),
                         method = "pls",
                         trControl = fitControl,
                         tuneLength = 15,
                         preProc = c("center", "scale"))
set.seed(1234)
F5_Plus_0.5_xgb <- train(Outcome_F5_Plus_0.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Plus_0.5),
                         method = "xgbTree",
                         trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Plus 0.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Plus_0.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Plus_0.5_gbm.rds")
saveRDS(F5_Plus_0.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Plus_0.5_pls.rds")
saveRDS(F5_Plus_0.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Plus_0.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Plus_1.5_gbm <- train(Outcome_F5_Plus_1.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Plus_1.5),
                         method = "gbm",
                         trControl = fitControl)
set.seed(1234)
F5_Plus_1.5_pls <- train(Outcome_F5_Plus_1.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Plus_1.5),
                         method = "pls",
                         trControl = fitControl,
                         tuneLength = 15,
                         preProc = c("center", "scale"))
set.seed(1234)
F5_Plus_1.5_xgb <- train(Outcome_F5_Plus_1.5 ~ .,
                         data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Plus_1.5),
                         method = "xgbTree",
                         trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Plus 1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Plus_1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Plus_1.5_gbm.rds")
saveRDS(F5_Plus_1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Plus_1.5_pls.rds")
saveRDS(F5_Plus_1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Plus_1.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Total_3.5_gbm <- train(Outcome_F5_Total_3.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_3.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
F5_Total_3.5_pls <- train(Outcome_F5_Total_3.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_3.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
F5_Total_3.5_xgb <- train(Outcome_F5_Total_3.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_3.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Total 3.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Total_3.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_3.5_gbm.rds")
saveRDS(F5_Total_3.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_3.5_pls.rds")
saveRDS(F5_Total_3.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_3.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Total_4_gbm <- train(Outcome_F5_Total_4 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_4),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
F5_Total_4_pls <- train(Outcome_F5_Total_4 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_4),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
F5_Total_4_xgb <- train(Outcome_F5_Total_4 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_4),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Total 4 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Total_4_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_4_gbm.rds")
saveRDS(F5_Total_4_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_4_pls.rds")
saveRDS(F5_Total_4_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_4_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Total_4.5_gbm <- train(Outcome_F5_Total_4.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_4.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
F5_Total_4.5_pls <- train(Outcome_F5_Total_4.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_4.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
F5_Total_4.5_xgb <- train(Outcome_F5_Total_4.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_4.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Total 4.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Total_4.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_4.5_gbm.rds")
saveRDS(F5_Total_4.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_4.5_pls.rds")
saveRDS(F5_Total_4.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_4.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Total_5_gbm <- train(Outcome_F5_Total_5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_5),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
F5_Total_5_pls <- train(Outcome_F5_Total_5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_5),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
F5_Total_5_xgb <- train(Outcome_F5_Total_5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_5),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Total 5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Total_5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_5_gbm.rds")
saveRDS(F5_Total_5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_5_pls.rds")
saveRDS(F5_Total_5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_Total_5.5_gbm <- train(Outcome_F5_Total_5.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_5.5),
                          method = "gbm",
                          trControl = fitControl)
set.seed(1234)
F5_Total_5.5_pls <- train(Outcome_F5_Total_5.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_5.5),
                          method = "pls",
                          trControl = fitControl,
                          tuneLength = 15,
                          preProc = c("center", "scale"))
set.seed(1234)
F5_Total_5.5_xgb <- train(Outcome_F5_Total_5.5 ~ .,
                          data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F5_Total_5.5),
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 Total 5.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_Total_5.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_5.5_gbm.rds")
saveRDS(F5_Total_5.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_5.5_pls.rds")
saveRDS(F5_Total_5.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_Total_5.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F1_Total_0.5_gbm <- train(Outcome_F1_Total_0.5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F1_Total_0.5),
                        method = "gbm",
                        trControl = fitControl)
set.seed(1234)
F1_Total_0.5_pls <- train(Outcome_F1_Total_0.5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F1_Total_0.5),
                        method = "pls",
                        trControl = fitControl,
                        tuneLength = 15,
                        preProc = c("center", "scale"))
set.seed(1234)
F1_Total_0.5_xgb <- train(Outcome_F1_Total_0.5 ~ .,
                        data = doubles %>% select(-contains('Outcome_'), -contains('Bullpen'), Outcome_F1_Total_0.5),
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F1 Total 0.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F1_Total_0.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_Total_0.5_gbm.rds")
saveRDS(F1_Total_0.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_Total_0.5_pls.rds")
saveRDS(F1_Total_0.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_Total_0.5_xgb.rds")

## Team Totals

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_TT_2.5_gbm <- train(Outcome_FG_TT_2.5 ~ .,
                   data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_2.5),
                   method = "gbm",
                   trControl = fitControl)
set.seed(1234)
FG_TT_2.5_pls <- train(Outcome_FG_TT_2.5 ~ .,
                   data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_2.5),
                   method = "pls",
                   trControl = fitControl,
                   tuneLength = 15,
                   preProc = c("center", "scale"))
set.seed(1234)
FG_TT_2.5_xgb <- train(Outcome_FG_TT_2.5 ~ .,
                   data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_2.5),
                   method = "xgbTree",
                   trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_TT_2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_2.5_gbm.rds")
saveRDS(FG_TT_2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_2.5_pls.rds")
saveRDS(FG_TT_2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_2.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_TT_3_gbm <- train(Outcome_FG_TT_3 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_3),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
FG_TT_3_pls <- train(Outcome_FG_TT_3 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_3),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
FG_TT_3_xgb <- train(Outcome_FG_TT_3 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_3),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_TT_3_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_3_gbm.rds")
saveRDS(FG_TT_3_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_3_pls.rds")
saveRDS(FG_TT_3_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_3_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_TT_3.5_gbm <- train(Outcome_FG_TT_3.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_3.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
FG_TT_3.5_pls <- train(Outcome_FG_TT_3.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_3.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
FG_TT_3.5_xgb <- train(Outcome_FG_TT_3.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_3.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_TT_3.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_3.5_gbm.rds")
saveRDS(FG_TT_3.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_3.5_pls.rds")
saveRDS(FG_TT_3.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_3.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_TT_4_gbm <- train(Outcome_FG_TT_4 ~ .,
                     data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_4),
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
FG_TT_4_pls <- train(Outcome_FG_TT_4 ~ .,
                     data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_4),
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
FG_TT_4_xgb <- train(Outcome_FG_TT_4 ~ .,
                     data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_4),
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_TT_4_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_4_gbm.rds")
saveRDS(FG_TT_4_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_4_pls.rds")
saveRDS(FG_TT_4_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_4_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_TT_4.5_gbm <- train(Outcome_FG_TT_4.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_4.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
FG_TT_4.5_pls <- train(Outcome_FG_TT_4.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_4.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
FG_TT_4.5_xgb <- train(Outcome_FG_TT_4.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_4.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_TT_4.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_4.5_gbm.rds")
saveRDS(FG_TT_4.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_4.5_pls.rds")
saveRDS(FG_TT_4.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_4.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_TT_5_gbm <- train(Outcome_FG_TT_5 ~ .,
                     data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_5),
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
FG_TT_5_pls <- train(Outcome_FG_TT_5 ~ .,
                     data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_5),
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
FG_TT_5_xgb <- train(Outcome_FG_TT_5 ~ .,
                     data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_5),
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_TT_5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_5_gbm.rds")
saveRDS(FG_TT_5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_5_pls.rds")
saveRDS(FG_TT_5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
FG_TT_5.5_gbm <- train(Outcome_FG_TT_5.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_5.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
FG_TT_5.5_pls <- train(Outcome_FG_TT_5.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_5.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
FG_TT_5.5_xgb <- train(Outcome_FG_TT_5.5 ~ .,
                       data = train_full_game %>% select(-contains('Outcome_'), -runs_final, Outcome_FG_TT_5.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("FG ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(FG_TT_5.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_5.5_gbm.rds")
saveRDS(FG_TT_5.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_5.5_pls.rds")
saveRDS(FG_TT_5.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/FG_TT_5.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_TT_0.5_gbm <- train(Outcome_F5_TT_0.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_0.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
F5_TT_0.5_pls <- train(Outcome_F5_TT_0.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_0.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
F5_TT_0.5_xgb <- train(Outcome_F5_TT_0.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_0.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_TT_0.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_0.5_gbm.rds")
saveRDS(F5_TT_0.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_0.5_pls.rds")
saveRDS(F5_TT_0.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_0.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_TT_1_gbm <- train(Outcome_F5_TT_1 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_1),
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
F5_TT_1_pls <- train(Outcome_F5_TT_1 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_1),
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
F5_TT_1_xgb <- train(Outcome_F5_TT_1 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_1),
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_TT_1_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_1_gbm.rds")
saveRDS(F5_TT_1_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_1_pls.rds")
saveRDS(F5_TT_1_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_1_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_TT_1.5_gbm <- train(Outcome_F5_TT_1.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_1.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
F5_TT_1.5_pls <- train(Outcome_F5_TT_1.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_1.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
F5_TT_1.5_xgb <- train(Outcome_F5_TT_1.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_1.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_TT_1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_1.5_gbm.rds")
saveRDS(F5_TT_1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_1.5_pls.rds")
saveRDS(F5_TT_1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_1.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_TT_2_gbm <- train(Outcome_F5_TT_2 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_2),
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
F5_TT_2_pls <- train(Outcome_F5_TT_2 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_2),
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
F5_TT_2_xgb <- train(Outcome_F5_TT_2 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_2),
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_TT_2_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_2_gbm.rds")
saveRDS(F5_TT_2_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_2_pls.rds")
saveRDS(F5_TT_2_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_2_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_TT_2.5_gbm <- train(Outcome_F5_TT_2.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_2.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
F5_TT_2.5_pls <- train(Outcome_F5_TT_2.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_2.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
F5_TT_2.5_xgb <- train(Outcome_F5_TT_2.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_2.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_TT_2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_2.5_gbm.rds")
saveRDS(F5_TT_2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_2.5_pls.rds")
saveRDS(F5_TT_2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_2.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_TT_3_gbm <- train(Outcome_F5_TT_3 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_3),
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
F5_TT_3_pls <- train(Outcome_F5_TT_3 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_3),
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
F5_TT_3_xgb <- train(Outcome_F5_TT_3 ~ .,
                     data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_3),
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_TT_3_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_3_gbm.rds")
saveRDS(F5_TT_3_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_3_pls.rds")
saveRDS(F5_TT_3_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_3_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F5_TT_3.5_gbm <- train(Outcome_F5_TT_3.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_3.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
F5_TT_3.5_pls <- train(Outcome_F5_TT_3.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_3.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
F5_TT_3.5_xgb <- train(Outcome_F5_TT_3.5 ~ .,
                       data = train_F5 %>% select(-contains('Outcome_'), -runs_5th, Outcome_F5_TT_3.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F5 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F5_TT_3.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_3.5_gbm.rds")
saveRDS(F5_TT_3.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_3.5_pls.rds")
saveRDS(F5_TT_3.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F5_TT_3.5_xgb.rds")

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
F1_TT_0.5_gbm <- train(Outcome_F1_TT_0.5 ~ .,
                       data = train_F1 %>% select(-contains('Outcome_'), -runs_1st, Outcome_F1_TT_0.5),
                       method = "gbm",
                       trControl = fitControl)
set.seed(1234)
F1_TT_0.5_pls <- train(Outcome_F1_TT_0.5 ~ .,
                       data = train_F1 %>% select(-contains('Outcome_'), -runs_1st, Outcome_F1_TT_0.5),
                       method = "pls",
                       trControl = fitControl,
                       tuneLength = 15,
                       preProc = c("center", "scale"))
set.seed(1234)
F1_TT_0.5_xgb <- train(Outcome_F1_TT_0.5 ~ .,
                       data = train_F1 %>% select(-contains('Outcome_'), -runs_1st, Outcome_F1_TT_0.5),
                       method = "xgbTree",
                       trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("F1 ML classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(F1_TT_0.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_TT_0.5_gbm.rds")
saveRDS(F1_TT_0.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_TT_0.5_pls.rds")
saveRDS(F1_TT_0.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/BaseballModels/F1_TT_0.5_xgb.rds")

overallEnd <- Sys.time()
paste("Entire script took",overallEnd - overallStart,attr(overallEnd - overallStart,"units"))
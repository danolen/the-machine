### The Machine v3.0
### Data Processing

library("pacman")
p_load("tidyverse", "readr", "DataCombine", "readxl")
team_names <- read.csv("Baseball Machine/team_names.csv") %>% 
  left_join(baseballr::teams_lu_table %>%
              filter(sport.name == "Major League Baseball") %>%
              distinct(id, name),
            by = c("Full.Name" = "name"))

## Load and join data from FanGraphs

##2019

team_batting_L7_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_batting_L7_2019.csv")
team_batting_L7_2019$Date <- as.Date(team_batting_L7_2019$Date)
team_batting_L14_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_batting_L14_2019.csv")
team_batting_L14_2019$Date <- as.Date(team_batting_L14_2019$Date)
team_batting_L30_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_batting_L30_2019.csv")
team_batting_L30_2019$Date <- as.Date(team_batting_L30_2019$Date)
team_batting_s2d_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_batting_s2d_2019.csv")
team_batting_s2d_2019$Date <- as.Date(team_batting_s2d_2019$Date)

daily_team_batting_2019 <- full_join(team_batting_L7_2019, team_batting_L14_2019, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_2019, by = c("Team", "Date"))
colnames(daily_team_batting_2019)[57:83] <- paste0(colnames(daily_team_batting_2019)[57:83],'_L30')
daily_team_batting_2019 <- full_join(daily_team_batting_2019, team_batting_s2d_2019, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name, FG),
            by = c("Team" = "FG")) %>% 
  mutate(Team = Full.Name) %>% 
  select(-Full.Name)

team_bullpen_L7_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_bullpen_L7_2019.csv")
team_bullpen_L7_2019$Date <- as.Date(team_bullpen_L7_2019$Date)
team_bullpen_L14_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_bullpen_L14_2019.csv")
team_bullpen_L14_2019$Date <- as.Date(team_bullpen_L14_2019$Date)
team_bullpen_L30_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_bullpen_L30_2019.csv")
team_bullpen_L30_2019$Date <- as.Date(team_bullpen_L30_2019$Date)
team_bullpen_s2d_2019 <- read.csv("Baseball Machine/Daily Files/2019/team_bullpen_s2d_2019.csv")
team_bullpen_s2d_2019$Date <- as.Date(team_bullpen_s2d_2019$Date)

daily_team_bullpen_2019 <- full_join(team_bullpen_L7_2019, team_bullpen_L14_2019, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_2019, by = c("Team", "Date"))
colnames(daily_team_bullpen_2019)[51:74] <- paste0(colnames(daily_team_bullpen_2019)[51:74],'_L30')
daily_team_bullpen_2019 <- full_join(daily_team_bullpen_2019, team_bullpen_s2d_2019, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name, FG),
            by = c("Team" = "FG")) %>% 
  mutate(Team = Full.Name) %>% 
  select(-Full.Name)

daily_pitchers_2019 <- read.csv("Baseball Machine/Daily Files/2019/pitchers_s2d_2019.csv")
daily_pitchers_2019$Date <- as.Date(daily_pitchers_2019$Date)
dupe_SPs_19 <- daily_pitchers_2019 %>% 
  filter(Date == max(daily_pitchers_2019$Date)) %>% 
  group_by(Name) %>% 
  summarise(records = n()) %>% 
  filter(records > 1) %>% 
  left_join(daily_pitchers_2019 %>% distinct(Name, Team))
daily_pitchers_2019 <- daily_pitchers_2019 %>% 
  mutate(Name = case_when(Name == "Austin Adams" & Team == "Twins" ~ "Austin Adams (MIN)",
                          Name == "Hyun-Jin Ryu" ~ "Hyun Jin Ryu",
                          TRUE ~ Name))

##2020

team_batting_L7_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_batting_L7_2020.csv") %>% 
  FindReplace(Var = "Team", replaceData = team_names, 
              from = "BR", to = "FG")
team_batting_L7_2020$Date <- as.Date(team_batting_L7_2020$Date)
team_batting_L14_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_batting_L14_2020.csv")
team_batting_L14_2020$Date <- as.Date(team_batting_L14_2020$Date)
team_batting_L30_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_batting_L30_2020.csv")
team_batting_L30_2020$Date <- as.Date(team_batting_L30_2020$Date)
team_batting_s2d_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_batting_s2d_2020.csv")
team_batting_s2d_2020$Date <- as.Date(team_batting_s2d_2020$Date)

daily_team_batting_2020 <- full_join(team_batting_L7_2020, team_batting_L14_2020, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_2020, by = c("Team", "Date"))
colnames(daily_team_batting_2020)[57:83] <- paste0(colnames(daily_team_batting_2020)[57:83],'_L30')
daily_team_batting_2020 <- full_join(daily_team_batting_2020, team_batting_s2d_2020, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name, FG),
            by = c("Team" = "FG")) %>% 
  mutate(Team = Full.Name) %>% 
  select(-Full.Name)

team_bullpen_L7_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_bullpen_L7_2020.csv")
team_bullpen_L7_2020$Date <- as.Date(team_bullpen_L7_2020$Date)
team_bullpen_L14_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_bullpen_L14_2020.csv")
team_bullpen_L14_2020$Date <- as.Date(team_bullpen_L14_2020$Date)
team_bullpen_L30_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_bullpen_L30_2020.csv")
team_bullpen_L30_2020$Date <- as.Date(team_bullpen_L30_2020$Date)
team_bullpen_s2d_2020 <- read.csv("Baseball Machine/Daily Files/2020/team_bullpen_s2d_2020.csv")
team_bullpen_s2d_2020$Date <- as.Date(team_bullpen_s2d_2020$Date)

daily_team_bullpen_2020 <- full_join(team_bullpen_L7_2020, team_bullpen_L14_2020, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_2020, by = c("Team", "Date"))
colnames(daily_team_bullpen_2020)[51:74] <- paste0(colnames(daily_team_bullpen_2020)[51:74],'_L30')
daily_team_bullpen_2020 <- full_join(daily_team_bullpen_2020, team_bullpen_s2d_2020, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name, FG),
            by = c("Team" = "FG")) %>% 
  mutate(Team = Full.Name) %>% 
  select(-Full.Name)

daily_pitchers_2020 <- read.csv("Baseball Machine/Daily Files/2020/pitchers_s2d_2020.csv")
daily_pitchers_2020$Date <- as.Date(daily_pitchers_2020$Date)
dupe_SPs_20 <- daily_pitchers_2020 %>% 
  filter(Date == max(daily_pitchers_2020$Date)) %>% 
  group_by(Name) %>% 
  summarise(records = n()) %>% 
  filter(records > 1) %>% 
  left_join(daily_pitchers_2020 %>% distinct(Name, Team))
daily_pitchers_2020 <- daily_pitchers_2020 %>% 
  mutate(Name = case_when(Name == "Luis Garcia" & Team == "Astros" ~ "Luis Garcia (HOU)",
                          Name == "Hyun-Jin Ryu" ~ "Hyun Jin Ryu",
                          TRUE ~ Name))

##2021

team_batting_L7_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_batting_L7_2021.csv")
team_batting_L7_2021$Date <- as.Date(team_batting_L7_2021$Date)
team_batting_L14_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_batting_L14_2021.csv")
team_batting_L14_2021$Date <- as.Date(team_batting_L14_2021$Date)
team_batting_L30_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_batting_L30_2021.csv")
team_batting_L30_2021$Date <- as.Date(team_batting_L30_2021$Date)
team_batting_s2d_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_batting_s2d_2021.csv")
team_batting_s2d_2021$Date <- as.Date(team_batting_s2d_2021$Date)

daily_team_batting_2021 <- full_join(team_batting_L7_2021, team_batting_L14_2021, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_2021, by = c("Team", "Date"))
colnames(daily_team_batting_2021)[57:83] <- paste0(colnames(daily_team_batting_2021)[57:83],'_L30')
daily_team_batting_2021 <- full_join(daily_team_batting_2021, team_batting_s2d_2021, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name, BR),
            by = c("Team" = "BR")) %>% 
  mutate(Team = Full.Name) %>% 
  select(-Full.Name)

team_bullpen_L7_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_bullpen_L7_2021.csv")
team_bullpen_L7_2021$Date <- as.Date(team_bullpen_L7_2021$Date)
team_bullpen_L14_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_bullpen_L14_2021.csv")
team_bullpen_L14_2021$Date <- as.Date(team_bullpen_L14_2021$Date)
team_bullpen_L30_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_bullpen_L30_2021.csv")
team_bullpen_L30_2021$Date <- as.Date(team_bullpen_L30_2021$Date)
team_bullpen_s2d_2021 <- read.csv("Baseball Machine/Daily Files/2021/team_bullpen_s2d_2021.csv")
team_bullpen_s2d_2021$Date <- as.Date(team_bullpen_s2d_2021$Date)

daily_team_bullpen_2021 <- full_join(team_bullpen_L7_2021, team_bullpen_L14_2021, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_2021, by = c("Team", "Date"))
colnames(daily_team_bullpen_2021)[51:74] <- paste0(colnames(daily_team_bullpen_2021)[51:74],'_L30')
daily_team_bullpen_2021 <- full_join(daily_team_bullpen_2021, team_bullpen_s2d_2021, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name, BR),
            by = c("Team" = "BR")) %>% 
  mutate(Team = Full.Name) %>% 
  select(-Full.Name)

daily_pitchers_2021 <- read.csv("Baseball Machine/Daily Files/2021/pitchers_s2d_2021.csv")
daily_pitchers_2021$Date <- as.Date(daily_pitchers_2021$Date)
dupe_SPs_21 <- daily_pitchers_2021 %>% 
  filter(Date == max(daily_pitchers_2021$Date)) %>% 
  group_by(Name) %>% 
  summarise(records = n()) %>% 
  filter(records > 1) %>% 
  left_join(daily_pitchers_2021 %>% distinct(Name, Team))
daily_pitchers_2021 <- daily_pitchers_2021 %>% 
  mutate(Name = case_when(Name == "Luis Garcia" & Team == "HOU" ~ "Luis Garcia (HOU)",
                          Name == "Javy Guerra" & Team == "WSN" ~ "Javy Guerra (WSN)",
                          Name == "Hyun-Jin Ryu" ~ "Hyun Jin Ryu",
                          TRUE ~ Name))

##2022

team_batting_L7_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_batting_L7_2022.csv")
team_batting_L7_2022$Date <- as.Date(team_batting_L7_2022$Date)
team_batting_L14_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_batting_L14_2022.csv")
team_batting_L14_2022$Date <- as.Date(team_batting_L14_2022$Date)
team_batting_L30_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_batting_L30_2022.csv")
team_batting_L30_2022$Date <- as.Date(team_batting_L30_2022$Date)
team_batting_s2d_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_batting_s2d_2022.csv")
team_batting_s2d_2022$Date <- as.Date(team_batting_s2d_2022$Date)

daily_team_batting_2022 <- full_join(team_batting_L7_2022, team_batting_L14_2022, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_2022, by = c("Team", "Date"))
colnames(daily_team_batting_2022)[57:83] <- paste0(colnames(daily_team_batting_2022)[57:83],'_L30')
daily_team_batting_2022 <- full_join(daily_team_batting_2022, team_batting_s2d_2022, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name22, BR),
            by = c("Team" = "BR")) %>% 
  mutate(Team = Full.Name22) %>% 
  select(-Full.Name22)

team_bullpen_L7_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_L7_2022.csv")
team_bullpen_L7_2022$Date <- as.Date(team_bullpen_L7_2022$Date)
team_bullpen_L14_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_L14_2022.csv")
team_bullpen_L14_2022$Date <- as.Date(team_bullpen_L14_2022$Date)
team_bullpen_L30_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_L30_2022.csv")
team_bullpen_L30_2022$Date <- as.Date(team_bullpen_L30_2022$Date)
team_bullpen_s2d_2022 <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_s2d_2022.csv")
team_bullpen_s2d_2022$Date <- as.Date(team_bullpen_s2d_2022$Date)

daily_team_bullpen_2022 <- full_join(team_bullpen_L7_2022, team_bullpen_L14_2022, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_2022, by = c("Team", "Date"))
colnames(daily_team_bullpen_2022)[51:74] <- paste0(colnames(daily_team_bullpen_2022)[51:74],'_L30')
daily_team_bullpen_2022 <- full_join(daily_team_bullpen_2022, team_bullpen_s2d_2022, by = c("Team", "Date")) %>% 
  left_join(team_names %>% 
              select(Full.Name22, BR),
            by = c("Team" = "BR")) %>% 
  mutate(Team = Full.Name22) %>% 
  select(-Full.Name22)

daily_pitchers_2022 <- read.csv("Baseball Machine/Daily Files/2022/pitchers_s2d_2022.csv")
daily_pitchers_2022$Date <- as.Date(daily_pitchers_2022$Date)
dupe_SPs_22 <- daily_pitchers_2022 %>% 
  filter(Date == max(daily_pitchers_2022$Date)) %>% 
  group_by(Name) %>% 
  summarise(records = n()) %>% 
  filter(records > 1) %>% 
  left_join(daily_pitchers_2022 %>% distinct(Name, Team))
daily_pitchers_2022 <- daily_pitchers_2022 %>% 
  mutate(Name = case_when(Name == "Luis Garcia" & Team == "HOU" ~ "Luis Garcia (HOU)",
                          Name == "Luis Castillo" & Team == "DET" ~ "Luis Castillo (DET)",
                          Name == "Luis Ortiz" & Team == "SFG" ~ "Luis Ortiz (SFG)",
                          Name == "Hyun-Jin Ryu" ~ "Hyun Jin Ryu",
                          TRUE ~ Name))

## Load Game scorelines

pks_19 <- read.csv("Baseball Machine/Daily Files/2019/game_pks_2019.csv")
gms_19 <- read.csv("Baseball Machine/Daily Files/2019/games_scores_2019.csv")
pks_20 <- read.csv("Baseball Machine/Daily Files/2020/game_pks_2020.csv")
gms_20 <- read.csv("Baseball Machine/Daily Files/2020/games_scores_2020.csv")
pks_21 <- read.csv("Baseball Machine/Daily Files/2021/game_pks_2021.csv")
gms_21 <- read.csv("Baseball Machine/Daily Files/2021/games_scores_2021.csv")
pks_22 <- read.csv("Baseball Machine/Daily Files/2022/game_pks_2022.csv")
gms_22 <- read.csv("Baseball Machine/Daily Files/2022/games_scores_2022.csv")

scores_19 <- gms_19 %>% 
  distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
         home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
         home_team_division_name, away_team_league_name, away_team_division_name) %>%
  inner_join(pks_19 %>% 
               filter(status.detailedState %in% c('Final', 'Completed Early') &
                        is.na(resumeDate) &
                        is.na(resumedFrom) &
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
  left_join(gms_19 %>% 
              distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
                       home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
                       home_team_division_name, away_team_league_name, away_team_division_name) %>%
              filter(!is.na(num)) %>% 
              replace(is.na(.),0) %>% 
              group_by(game_pk) %>% 
              summarise(home_runs_final = sum(home_runs),
                        home_hits_final = sum(home_hits),
                        home_errors_final = sum(home_errors),
                        away_runs_final = sum(away_runs),
                        away_hits_final = sum(away_hits),
                        away_errors_final = sum(away_errors)))

scores_20 <- gms_20 %>% 
  distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
           home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
           home_team_division_name, away_team_league_name, away_team_division_name) %>%
  inner_join(pks_20 %>% 
               filter(status.detailedState %in% c('Final', 'Completed Early') &
                        is.na(resumeDate) &
                        is.na(resumedFrom) &
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
  left_join(gms_20 %>% 
              distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
                       home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
                       home_team_division_name, away_team_league_name, away_team_division_name) %>%
              filter(!is.na(num)) %>% 
              replace(is.na(.),0) %>% 
              group_by(game_pk) %>% 
              summarise(home_runs_final = sum(home_runs),
                        home_hits_final = sum(home_hits),
                        home_errors_final = sum(home_errors),
                        away_runs_final = sum(away_runs),
                        away_hits_final = sum(away_hits),
                        away_errors_final = sum(away_errors)))

scores_21 <- gms_21 %>% 
  distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
           home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
           home_team_division_name, away_team_league_name, away_team_division_name) %>%
  inner_join(pks_21 %>% 
               filter(status.detailedState %in% c('Final', 'Completed Early') &
                        is.na(resumeDate) &
                        is.na(resumedFrom) &
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
  left_join(gms_21 %>% 
              distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
                       home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
                       home_team_division_name, away_team_league_name, away_team_division_name) %>%
              filter(!is.na(num)) %>% 
              replace(is.na(.),0) %>% 
              group_by(game_pk) %>% 
              summarise(home_runs_final = sum(home_runs),
                        home_hits_final = sum(home_hits),
                        home_errors_final = sum(home_errors),
                        away_runs_final = sum(away_runs),
                        away_hits_final = sum(away_hits),
                        away_errors_final = sum(away_errors)))

scores_22 <- gms_22 %>% 
  distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
           home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
           home_team_division_name, away_team_league_name, away_team_division_name) %>%
  inner_join(pks_22 %>% 
               filter(status.detailedState %in% c('Final', 'Completed Early') &
                        is.na(resumeDate) &
                        is.na(resumedFrom) &
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
  left_join(gms_22 %>% 
              distinct(game_pk, home_team_name, away_team_name, num, ordinal_num, home_runs, home_hits,
                       home_errors, away_runs, away_hits, away_errors, home_team_season, home_team_league_name,
                       home_team_division_name, away_team_league_name, away_team_division_name) %>%
              filter(!is.na(num)) %>% 
              replace(is.na(.),0) %>% 
              group_by(game_pk) %>% 
              summarise(home_runs_final = sum(home_runs),
                        home_hits_final = sum(home_hits),
                        home_errors_final = sum(home_errors),
                        away_runs_final = sum(away_runs),
                        away_hits_final = sum(away_hits),
                        away_errors_final = sum(away_errors)))

## Read in PECOTA Projections
PECOTA_pitching_19 <- read_xlsx("Baseball Machine/PECOTA/2019/pecota_2019_03_20.xlsx", sheet = "Pitchers") %>% 
  select(MLBCODE, NAME, IP, WARP) %>% 
  mutate(WARP200 = (WARP/IP)*200)
PECOTA_pitching_20 <- read_xlsx("Baseball Machine/PECOTA/2020/pecota2020_pitching_jul25.xlsx", sheet = "50") %>% 
  select(mlbid, name, ip, warp) %>% 
  mutate(WARP200 = (warp/ip)*200)
PECOTA_pitching_21 <- read_xlsx("Baseball Machine/PECOTA/2021/pecota2021_pitching_apr01.xlsx", sheet = "50") %>% 
  select(mlbid, name, ip, warp) %>% 
  mutate(WARP200 = (as.numeric(warp)/as.numeric(ip))*200)
PECOTA_pitching_22 <- read_xlsx("Baseball Machine/PECOTA/2022/pecota2022_pitching_apr03.xlsx", sheet = "50") %>% 
  select(mlbid, name, ip, warp) %>% 
  mutate(WARP200 = (warp/ip)*200)

PECOTA_hitting_19 <- read_xlsx("Baseball Machine/PECOTA/2019/pecota_2019_03_20.xlsx", sheet = "Batters") %>% 
  select(MLBCODE, NAME, PA, WARP) %>% 
  mutate(WARP600 = (WARP/PA)*600)
PECOTA_hitting_20 <- read_xlsx("Baseball Machine/PECOTA/2020/pecota2020_hitting_jul25.xlsx", sheet = "50") %>% 
  select(mlbid, name, pa, warp) %>% 
  mutate(WARP600 = (warp/pa)*600)
PECOTA_hitting_21 <- read_xlsx("Baseball Machine/PECOTA/2021/pecota2021_hitting_apr01.xlsx", sheet = "50") %>% 
  select(mlbid, name, pa, warp) %>% 
  mutate(WARP600 = (as.numeric(warp)/as.numeric(pa))*600)
PECOTA_hitting_22 <- read_xlsx("Baseball Machine/PECOTA/2022/pecota2022_hitting_apr03.xlsx", sheet = "50") %>% 
  select(mlbid, name, pa, warp) %>% 
  mutate(WARP600 = (as.numeric(warp)/as.numeric(pa))*600)

## Get Starting Pitchers and Join WARP Projections

# SPs19 <- data.frame()
# for (i in seq_along(unique(scores_19$game_pk))) {
#   sps = baseballr::mlb_probables(unique(scores_19$game_pk)[i])
#   SPs19 = SPs19 %>% 
#     bind_rows(sps)
#   print(paste0("Retrieved SPs for game ", i,"/",length(unique(scores_19$game_pk))))
# }
# 
# write.csv(SPs19, "Baseball Machine/Daily Files/2019/starting_pitchers_2019.csv")
# 
# SPs20 <- data.frame()
# for (i in seq_along(unique(scores_20$game_pk))) {
#   sps = baseballr::mlb_probables(unique(scores_20$game_pk)[i])
#   SPs20 = SPs20 %>% 
#     bind_rows(sps)
#   print(paste0("Retrieved SPs for game ", i,"/",length(unique(scores_20$game_pk))))
# }
# 
# write.csv(SPs20, "Baseball Machine/Daily Files/2020/starting_pitchers_2020.csv")
# 
# SPs21 <- data.frame()
# for (i in seq_along(unique(scores_21$game_pk))) {
#   sps = baseballr::mlb_probables(unique(scores_21$game_pk)[i])
#   SPs21 = SPs21 %>% 
#     bind_rows(sps)
#   print(paste0("Retrieved SPs for game ", i,"/",length(unique(scores_21$game_pk))))
# }
# 
# write.csv(SPs21, "Baseball Machine/Daily Files/2021/starting_pitchers_2021.csv")
# 
# SPs22 <- data.frame()
# for (i in seq_along(unique(scores_22$game_pk))) {
#   sps = baseballr::mlb_probables(unique(scores_22$game_pk)[i])
#   SPs22 = SPs22 %>% 
#     bind_rows(sps)
#   print(paste0("Retrieved SPs for game ", i,"/",length(unique(scores_22$game_pk))))
# }
# 
# write.csv(SPs22, "Baseball Machine/Daily Files/2022/starting_pitchers_2022.csv")

SPs19 <- read.csv("Baseball Machine/Daily Files/2019/starting_pitchers_2019.csv") %>% 
  left_join(PECOTA_pitching_19 %>% 
              select(MLBCODE, WARP200),
            by = c("id" = "MLBCODE"))
SPs20 <- read.csv("Baseball Machine/Daily Files/2020/starting_pitchers_2020.csv") %>% 
  left_join(PECOTA_pitching_20 %>% 
              select(mlbid, WARP200),
            by = c("id" = "mlbid"))
SPs21 <- read.csv("Baseball Machine/Daily Files/2021/starting_pitchers_2021.csv") %>% 
  left_join(PECOTA_pitching_21 %>% 
              select(mlbid, WARP200),
            by = c("id" = "mlbid"))
SPs22 <- read.csv("Baseball Machine/Daily Files/2022/starting_pitchers_2022.csv") %>% 
  left_join(PECOTA_pitching_22 %>% 
              select(mlbid, WARP200),
            by = c("id" = "mlbid"))

## Get Daily Rosters and Join WARP Projections

# team_ids <- team_names$id
# dates_19 <- unique(scores_19$officialDate)
# dates_20 <- unique(scores_20$officialDate)
# dates_21 <- unique(scores_21$officialDate)
# dates_22 <- unique(scores_22$officialDate)
# 
# team_dates_19 <- merge(team_ids, dates_19)
# team_dates_20 <- merge(team_ids, dates_20)
# team_dates_21 <- merge(team_ids, dates_21)
# team_dates_22 <- merge(team_ids, dates_22)
# 
# rosters_19 <- data.frame()
# for (i in 1:nrow(team_dates_19)) {
#   roster = baseballr::mlb_rosters(team_id = team_dates_19[i,1], date = team_dates_19[i,2], roster_type = 'active') %>% 
#     select(person_id, person_full_name, position_type, position_abbreviation, team_id, date)
#   rosters_19 = rosters_19 %>% 
#     bind_rows(roster)
#   print(paste0("Retrieved Rosters for record ", i,"/",nrow(team_dates_19)))
# }
# 
# write.csv(rosters_19, "Baseball Machine/Daily Files/2019/daily_rosters_2019.csv")
# 
# rosters_20 <- data.frame()
# for (i in 1:nrow(team_dates_20)) {
#   roster = baseballr::mlb_rosters(team_id = team_dates_20[i,1], date = team_dates_20[i,2], roster_type = 'active') %>% 
#     select(person_id, person_full_name, position_type, position_abbreviation, team_id, date)
#   rosters_20 = rosters_20 %>% 
#     bind_rows(roster)
#   print(paste0("Retrieved Rosters for record ", i,"/",nrow(team_dates_20)))
# }
# 
# write.csv(rosters_20, "Baseball Machine/Daily Files/2020/daily_rosters_2020.csv")
# 
# rosters_21 <- data.frame()
# for (i in 1:nrow(team_dates_21)) {
#   roster = baseballr::mlb_rosters(team_id = team_dates_21[i,1], date = team_dates_21[i,2], roster_type = 'active') %>% 
#     select(person_id, person_full_name, position_type, position_abbreviation, team_id, date)
#   rosters_21 = rosters_21 %>% 
#     bind_rows(roster)
#   print(paste0("Retrieved Rosters for record ", i,"/",nrow(team_dates_21)))
# }
# 
# write.csv(rosters_21, "Baseball Machine/Daily Files/2021/daily_rosters_2021.csv")
# 
# rosters_22 <- data.frame()
# for (i in 1:nrow(team_dates_22)) {
#   roster = baseballr::mlb_rosters(team_id = team_dates_22[i,1], date = team_dates_22[i,2], roster_type = 'active') %>% 
#     select(person_id, person_full_name, position_type, position_abbreviation, team_id, date)
#   rosters_22 = rosters_22 %>% 
#     bind_rows(roster)
#   print(paste0("Retrieved Rosters for record ", i,"/",nrow(team_dates_22)))
# }
# 
# write.csv(rosters_22, "Baseball Machine/Daily Files/2022/daily_rosters_2022.csv")

rosters_19 <- read.csv("Baseball Machine/Daily Files/2019/daily_rosters_2019.csv") %>%
  select(-X) %>% 
  filter(position_type != "Pitcher") %>% 
  left_join(PECOTA_hitting_19 %>% 
              select(MLBCODE, WARP600),
            by = c("person_id" = "MLBCODE")) %>% 
  group_by(team_id, date) %>% 
  summarise(WARP600 = sum(WARP600, na.rm = T))
rosters_20 <- read.csv("Baseball Machine/Daily Files/2020/daily_rosters_2020.csv") %>%
  select(-X) %>% 
  filter(position_type != "Pitcher") %>% 
  left_join(PECOTA_hitting_20 %>% 
              select(mlbid, WARP600),
            by = c("person_id" = "mlbid")) %>% 
  group_by(team_id, date) %>% 
  summarise(WARP600 = sum(WARP600, na.rm = T))
rosters_21 <- read.csv("Baseball Machine/Daily Files/2021/daily_rosters_2021.csv") %>%
  select(-X) %>% 
  filter(position_type != "Pitcher") %>% 
  left_join(PECOTA_hitting_21 %>% 
              select(mlbid, WARP600),
            by = c("person_id" = "mlbid")) %>% 
  group_by(team_id, date) %>% 
  summarise(WARP600 = sum(WARP600, na.rm = T))
rosters_22 <- read.csv("Baseball Machine/Daily Files/2022/daily_rosters_2022.csv") %>%
  select(-X) %>% 
  filter(position_type != "Pitcher") %>% 
  left_join(PECOTA_hitting_22 %>% 
              select(mlbid, WARP600),
            by = c("person_id" = "mlbid")) %>% 
  group_by(team_id, date) %>% 
  summarise(WARP600 = sum(WARP600, na.rm = T))


## Combine all data
gamescores19 <- scores_19 %>% 
  left_join(SPs19 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "home_team_name" = "team")) %>% 
  rename(HomeSP_fullName = fullName,
         HomeSP_id = id,
         HomeSP_WARP200 = WARP200) %>% 
  left_join(SPs19 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "away_team_name" = "team")) %>% 
  rename(AwaySP_fullName = fullName,
         AwaySP_id = id,
         AwaySP_WARP200 = WARP200) %>% 
  mutate(officialDate = as.Date(officialDate)) %>% 
  filter(!is.na(HomeSP_fullName) & !is.na(AwaySP_fullName)) %>% 
  left_join(daily_pitchers_2019, by = c("officialDate" = "Date", "HomeSP_fullName" = "Name")) %>% 
  left_join(daily_pitchers_2019, by = c("officialDate" = "Date", "AwaySP_fullName" = "Name"), suffix = c("_HomeSP", "_AwaySP")) %>% 
  left_join(daily_team_batting_2019, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_batting_2019, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  left_join(daily_team_bullpen_2019, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_bullpen_2019, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBullpen", "_AwayBullpen"))

gamescores20 <- scores_20 %>% 
  left_join(SPs20 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "home_team_name" = "team")) %>% 
  rename(HomeSP_fullName = fullName,
         HomeSP_id = id,
         HomeSP_WARP200 = WARP200) %>% 
  left_join(SPs20 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "away_team_name" = "team")) %>% 
  rename(AwaySP_fullName = fullName,
         AwaySP_id = id,
         AwaySP_WARP200 = WARP200) %>% 
  mutate(officialDate = as.Date(officialDate)) %>% 
  filter(!is.na(HomeSP_fullName) & !is.na(AwaySP_fullName)) %>% 
  left_join(daily_pitchers_2020, by = c("officialDate" = "Date", "HomeSP_fullName" = "Name")) %>% 
  left_join(daily_pitchers_2020, by = c("officialDate" = "Date", "AwaySP_fullName" = "Name"), suffix = c("_HomeSP", "_AwaySP")) %>% 
  left_join(daily_team_batting_2020, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_batting_2020, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  left_join(daily_team_bullpen_2020, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_bullpen_2020, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBullpen", "_AwayBullpen"))


gamescores21 <- scores_21 %>% 
  left_join(SPs21 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "home_team_name" = "team")) %>% 
  rename(HomeSP_fullName = fullName,
         HomeSP_id = id,
         HomeSP_WARP200 = WARP200) %>% 
  left_join(SPs21 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "away_team_name" = "team")) %>% 
  rename(AwaySP_fullName = fullName,
         AwaySP_id = id,
         AwaySP_WARP200 = WARP200) %>% 
  mutate(officialDate = as.Date(officialDate)) %>% 
  filter(!is.na(HomeSP_fullName) & !is.na(AwaySP_fullName)) %>% 
  left_join(daily_pitchers_2021, by = c("officialDate" = "Date", "HomeSP_fullName" = "Name")) %>% 
  left_join(daily_pitchers_2021, by = c("officialDate" = "Date", "AwaySP_fullName" = "Name"), suffix = c("_HomeSP", "_AwaySP")) %>% 
  left_join(daily_team_batting_2021, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_batting_2021, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  left_join(daily_team_bullpen_2021, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_bullpen_2021, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBullpen", "_AwayBullpen"))


gamescores22 <- scores_22 %>% 
  left_join(SPs22 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "home_team_name" = "team")) %>% 
  rename(HomeSP_fullName = fullName,
         HomeSP_id = id,
         HomeSP_WARP200 = WARP200) %>% 
  left_join(SPs22 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "away_team_name" = "team")) %>% 
  rename(AwaySP_fullName = fullName,
         AwaySP_id = id,
         AwaySP_WARP200 = WARP200) %>% 
  mutate(officialDate = as.Date(officialDate)) %>% 
  filter(!is.na(HomeSP_fullName) & !is.na(AwaySP_fullName)) %>% 
  left_join(daily_pitchers_2022, by = c("officialDate" = "Date", "HomeSP_fullName" = "Name")) %>% 
  left_join(daily_pitchers_2022, by = c("officialDate" = "Date", "AwaySP_fullName" = "Name"), suffix = c("_HomeSP", "_AwaySP")) %>% 
  left_join(daily_team_batting_2022, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_batting_2022, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  left_join(daily_team_bullpen_2022, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_bullpen_2022, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBullpen", "_AwayBullpen"))


## Load Game Logs

load_gamelog <- function(season) {
  glheaders <- read.csv("game_log_header.txt") #from https://github.com/beanumber/baseball_R/blob/master/data/game_log_header.csv
  remote <- paste0("http://www.retrosheet.org/gamelogs/gl",
                   season, ".zip")
  local <- paste0("gl", season, ".zip")
  download.file(url = remote, destfile = local)
  unzip(local)
  local_txt <- gsub(".zip", ".txt", local) %>%
    toupper()
  gamelog <- read_csv(local_txt,
                      col_names = names(glheaders),
                      na = character())
  file.remove(local)
  file.remove(local_txt)
  return(gamelog)
}

gl2019 <- load_gamelog(2019)
gl2020 <- load_gamelog(2020)

## Clean and format data

gl2019$Date <- as.Date(as.character(gl2019$Date), "%Y%m%d")
gl2020$Date <- as.Date(as.character(gl2020$Date), "%Y%m%d")

team_names <- read.csv("team_names.csv")

gl2019 <- as.data.frame(gl2019) %>% 
  FindReplace(Var = "VisitingTeam", replaceData = team_names, 
              from = "BP_2019", to = "FG") %>%
  FindReplace(Var = "HomeTeam", replaceData = team_names, 
              from = "BP_2019", to = "FG")

gl2020 <- as.data.frame(gl2020) %>% 
  FindReplace(Var = "VisitingTeam", replaceData = team_names, 
              from = "BP_2019", to = "FG") %>%
  FindReplace(Var = "HomeTeam", replaceData = team_names, 
              from = "BP_2019", to = "FG")

## Get first 5 inning scores

F5_2019 <- filter(gl2019, VisitorRunsScored < 10 & HomeRunsScore < 10)
F5_2019$F5_VisitorRunsScored <- 
  sapply(strsplit(substr(as.character(F5_2019$VisitorLineScore),1,5), ""), function(x) sum(as.numeric(x)))
F5_2019$F5_HomeRunsScored <- 
  sapply(strsplit(substr(as.character(F5_2019$HomeLineScore),1,5), ""), function(x) sum(as.numeric(x)))

F5_2019[560,]$F5_HomeRunsScored <- 4

F5_2019 <- select(F5_2019, Date, VisitingTeam, VisitingTeamGameNumber, 
                  HomeTeam, HomeTeamGameNumber, F5_VisitorRunsScored, F5_HomeRunsScored)

F5_2020 <- filter(gl2020, VisitorRunsScored < 10 & HomeRunsScore < 10)
F5_2020$F5_VisitorRunsScored <- 
  sapply(strsplit(substr(as.character(F5_2020$VisitorLineScore),1,5), ""), function(x) sum(as.numeric(x)))
F5_2020$F5_HomeRunsScored <- 
  sapply(strsplit(substr(as.character(F5_2020$HomeLineScore),1,5), ""), function(x) sum(as.numeric(x)))

F5_2020 <- select(F5_2020, Date, VisitingTeam, VisitingTeamGameNumber, 
                  HomeTeam, HomeTeamGameNumber, F5_VisitorRunsScored, F5_HomeRunsScored)

## Join FanGraphs data to game logs

gl2019 <- left_join(gl2019, daily_pitchers_2019, 
                    by = c("Date" = "Date", "VisitingTeam" = "Team", "VisitorStartingPitcherName" = "Name"))
colnames(gl2019)[162:188] <- paste0("VisitingSP_", colnames(gl2019)[162:188])
gl2019 <- left_join(gl2019, daily_pitchers_2019,
                    by = c("Date" = "Date", "HomeTeam" = "Team", "HomeStartingPitcherName" = "Name"))
colnames(gl2019)[189:215] <- paste0("HomeSP_", colnames(gl2019)[189:215])
gl2019 <- left_join(gl2019, daily_team_batting_2019,
                    by = c("Date" = "Date", "VisitingTeam" = "Team"))
colnames(gl2019)[216:323] <- paste0("VisitingBatters_", colnames(gl2019)[216:323])
gl2019 <- left_join(gl2019, daily_team_batting_2019,
                    by = c("Date" = "Date", "HomeTeam" = "Team"))
colnames(gl2019)[324:431] <- paste0("HomeBatters_", colnames(gl2019)[324:431])
gl2019 <- left_join(gl2019, daily_team_bullpen_2019,
                    by = c("Date" = "Date", "VisitingTeam" = "Team"))
colnames(gl2019)[432:527] <- paste0("VisitingBullpen_", colnames(gl2019)[432:527])
gl2019 <- left_join(gl2019, daily_team_bullpen_2019,
                    by = c("Date" = "Date", "HomeTeam" = "Team"))
colnames(gl2019)[528:623] <- paste0("HomeBullpen_", colnames(gl2019)[528:623])
gl2019 <- left_join(gl2019, F5_2019,
                    by = c("Date", "VisitingTeam", "VisitingTeamGameNumber", "HomeTeam", "HomeTeamGameNumber"))

gl2020 <- left_join(gl2020, daily_pitchers_2020, 
                    by = c("Date" = "Date", "VisitingTeam" = "Team", "VisitorStartingPitcherName" = "Name"))
colnames(gl2020)[162:188] <- paste0("VisitingSP_", colnames(gl2020)[162:188])
gl2020 <- left_join(gl2020, daily_pitchers_2020,
                    by = c("Date" = "Date", "HomeTeam" = "Team", "HomeStartingPitcherName" = "Name"))
colnames(gl2020)[189:215] <- paste0("HomeSP_", colnames(gl2020)[189:215])
gl2020 <- left_join(gl2020, daily_team_batting_2020,
                    by = c("Date" = "Date", "VisitingTeam" = "Team"))
colnames(gl2020)[216:323] <- paste0("VisitingBatters_", colnames(gl2020)[216:323])
gl2020 <- left_join(gl2020, daily_team_batting_2020,
                    by = c("Date" = "Date", "HomeTeam" = "Team"))
colnames(gl2020)[324:431] <- paste0("HomeBatters_", colnames(gl2020)[324:431])
gl2020 <- left_join(gl2020, daily_team_bullpen_2020,
                    by = c("Date" = "Date", "VisitingTeam" = "Team"))
colnames(gl2020)[432:527] <- paste0("VisitingBullpen_", colnames(gl2020)[432:527])
gl2020 <- left_join(gl2020, daily_team_bullpen_2020,
                    by = c("Date" = "Date", "HomeTeam" = "Team"))
colnames(gl2020)[528:623] <- paste0("HomeBullpen_", colnames(gl2020)[528:623])
gl2020 <- left_join(gl2020, F5_2020,
                    by = c("Date", "VisitingTeam", "VisitingTeamGameNumber", "HomeTeam", "HomeTeamGameNumber"))

## Add in 2020 pre-season projections

DC_batting_20 <- read.csv("DepthCharts/2020/DepthCharts_batting_60.csv")
DC_pitching_20 <- read.csv("DepthCharts/2020/DepthCharts_pitching_60.csv")
PECOTA_batting_20 <- read_excel("PECOTA/2020/pecota2020_hitting_jul25.xlsx", sheet = "50")
PECOTA_pitching_20 <- read_excel("PECOTA/2020/pecota2020_pitching_jul25.xlsx", sheet = "50")

## Map ID's to MLB Names

player_id_map <- read.delim("http://crunchtimebaseball.com/master.txt")

DC_batting_20 <- left_join(DC_batting_20, player_id_map[,c(18,25,26)], by = c("playerid" = "fg_id"))
DC_batting_20 <- mutate(DC_batting_20, ?..Name = if_else(retro_name == "" | is.na(retro_name), ?..Name, retro_name))
DC_pitching_20 <- left_join(DC_pitching_20, player_id_map[,c(18,25,26)], by = c("playerid" = "fg_id"))
DC_pitching_20 <- mutate(DC_pitching_20, ?..Name = if_else(retro_name == "" | is.na(retro_name), ?..Name, retro_name))
PECOTA_batting_20 <- left_join(PECOTA_batting_20, player_id_map[,c(1,25,26)], by = c("mlbid" = "mlb_id"))
PECOTA_batting_20 <- mutate(PECOTA_batting_20, name = if_else(retro_name == "" | is.na(retro_name), name, retro_name))
PECOTA_pitching_20 <- left_join(PECOTA_pitching_20, player_id_map[,c(1,25,26)], by = c("mlbid" = "mlb_id"))
PECOTA_pitching_20 <- mutate(PECOTA_pitching_20, name = if_else(retro_name == "" | is.na(retro_name), name, retro_name))

DC_PECOTA_batting <- full_join(DC_batting_20[,c(1,24)], PECOTA_batting_20[,c(5,43)], by = c("?..Name" = "name"))

DC_batting_20 <- mutate(DC_batting_20, WAR_per_500PA = (WAR / PA) * 500)
PECOTA_batting_20 <- mutate(PECOTA_batting_20, warp_per_500pa = (warp / pa) * 500)

PECOTA_batting_20 <- as.data.frame(PECOTA_batting_20)
PECOTA_pitching_20 <- as.data.frame(PECOTA_pitching_20)
PECOTA_batting_20 <- FindReplace(PECOTA_batting_20, Var = "team", replaceData = team_names,
                                 from = "BP_2020", to = "FG")
PECOTA_pitching_20 <- FindReplace(PECOTA_pitching_20, Var = "team", replaceData = team_names,
                                  from = "BP_2020", to = "FG")

## Aggregate WAR projections for 2020 and join to game log
# Batters

bat_WAR_20 <- DC_batting_20 %>% 
  group_by(Team) %>%
  summarize(bat_WAR500_proj = (sum(WAR) / sum(PA)) * 500)

BWARP_20 <- PECOTA_batting_20 %>%
  filter(dc_fl == "T") %>%
  group_by(team) %>%
  summarize(BWARP500_proj = (sum(warp) / sum(pa)) * 500)

proj_batting_20 <- left_join(BWARP_20, bat_WAR_20, by = c("team" = "Team"))

gl2020 <- left_join(gl2020, proj_batting_20, by = c("VisitingTeam" = "team"))
colnames(gl2020)[626:627] <- paste0("Visiting_", colnames(gl2020)[626:627])
gl2020 <- left_join(gl2020, proj_batting_20, by = c("HomeTeam" = "team"))
colnames(gl2020)[628:629] <- paste0("Home_", colnames(gl2020)[628:629])

# Starting pitchers

pit_WAR_20 <- DC_pitching_20 %>%
  group_by(?..Name) %>%
  summarize(pit_WAR200_proj = (sum(WAR) / sum(IP)) * 200)

PWARP_20 <- PECOTA_pitching_20 %>%
  group_by(name) %>%
  summarize(PWARP200_proj = (sum(warp) / sum(ip)) * 200)

proj_SP_20 <- full_join(pit_WAR_20, PWARP_20, by = c("?..Name" = "name"))

gl2020 <- left_join(gl2020, proj_SP_20, by = c("VisitorStartingPitcherName" = "?..Name"))
colnames(gl2020)[630:631] <- paste0("Visiting_", colnames(gl2020)[630:631])
gl2020 <- left_join(gl2020, proj_SP_20, by = c("HomeStartingPitcherName" = "?..Name"))
colnames(gl2020)[632:633] <- paste0("Home_", colnames(gl2020)[632:633])

## Did not include preseason bullpen projections. I don't feel that there is much
## value to be added from bullpen WAR projections because they are so volatile
## and the work needed to include them is not worth my time right now

write.csv(gl2019, "gl2019.csv", row.names = FALSE)
write.csv(gl2020, "gl2020.csv", row.names = FALSE)

gl2019 <- mutate(gl2019, 
                 Visiting_BWARP500_proj = NA,
                 Visiting_bat_WAR500_proj = NA,
                 Home_BWARP500_proj = NA,
                 Home_bat_WAR500_proj = NA,
                 Visiting_pit_WAR200_proj = NA,
                 Visiting_PWARP200_proj = NA,
                 Home_pit_WAR200_proj = NA,
                 Home_PWARP200_proj = NA)

gl2019_2020 <- rbind(gl2019, gl2020)

write.csv(gl2019_2020, "gl2019_2020.csv", row.names = FALSE)




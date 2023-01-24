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
  summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
  left_join(team_names %>% 
              select(Full.Name, id),
            by = c("team_id" = "id"))
rosters_20 <- read.csv("Baseball Machine/Daily Files/2020/daily_rosters_2020.csv") %>%
  select(-X) %>% 
  filter(position_type != "Pitcher") %>% 
  left_join(PECOTA_hitting_20 %>% 
              select(mlbid, WARP600),
            by = c("person_id" = "mlbid")) %>% 
  group_by(team_id, date) %>% 
  summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
  left_join(team_names %>% 
              select(Full.Name, id),
            by = c("team_id" = "id"))
rosters_21 <- read.csv("Baseball Machine/Daily Files/2021/daily_rosters_2021.csv") %>%
  select(-X) %>% 
  filter(position_type != "Pitcher") %>% 
  left_join(PECOTA_hitting_21 %>% 
              select(mlbid, WARP600),
            by = c("person_id" = "mlbid")) %>% 
  group_by(team_id, date) %>% 
  summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
  left_join(team_names %>% 
              select(Full.Name, id),
            by = c("team_id" = "id"))
rosters_22 <- read.csv("Baseball Machine/Daily Files/2022/daily_rosters_2022.csv") %>%
  select(-X) %>% 
  filter(position_type != "Pitcher") %>% 
  left_join(PECOTA_hitting_22 %>% 
              select(mlbid, WARP600),
            by = c("person_id" = "mlbid")) %>% 
  group_by(team_id, date) %>% 
  summarise(WARP600 = sum(WARP600, na.rm = T)) %>% 
  left_join(team_names %>% 
              select(Full.Name22, id),
            by = c("team_id" = "id"))


## Combine all data
gamescores19 <- scores_19 %>% 
  left_join(SPs19 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "home_team_name" = "team")) %>% 
  rename(HomeSP_fullName = fullName,
         HomeSP_id = id,
         WARP200_HomeSP = WARP200) %>% 
  left_join(SPs19 %>% 
              select(-team_id, -home_plate_full_name, -home_plate_id),
            by = c("game_pk" = "game_pk",
                   "officialDate" = "game_date",
                   "away_team_name" = "team")) %>% 
  rename(AwaySP_fullName = fullName,
         AwaySP_id = id,
         WARP200_AwaySP = WARP200) %>% 
  left_join(rosters_19 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "home_team_name" = "Full.Name")) %>% 
  left_join(rosters_19 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "away_team_name" = "Full.Name"),
            suffix = c("_HomeBatters", "_AwayBatters")) %>% 
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
  left_join(rosters_20 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "home_team_name" = "Full.Name")) %>% 
  left_join(rosters_20 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "away_team_name" = "Full.Name"),
            suffix = c("_HomeBatters", "_AwayBatters")) %>% 
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
  left_join(rosters_21 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "home_team_name" = "Full.Name")) %>% 
  left_join(rosters_21 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "away_team_name" = "Full.Name"),
            suffix = c("_HomeBatters", "_AwayBatters")) %>% 
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
  left_join(rosters_22 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "home_team_name" = "Full.Name22")) %>% 
  left_join(rosters_22 %>% 
              ungroup() %>% 
              select(-team_id),
            by = c("officialDate" = "date",
                   "away_team_name" = "Full.Name22"),
            suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  mutate(officialDate = as.Date(officialDate)) %>% 
  filter(!is.na(HomeSP_fullName) & !is.na(AwaySP_fullName)) %>% 
  left_join(daily_pitchers_2022, by = c("officialDate" = "Date", "HomeSP_fullName" = "Name")) %>% 
  left_join(daily_pitchers_2022, by = c("officialDate" = "Date", "AwaySP_fullName" = "Name"), suffix = c("_HomeSP", "_AwaySP")) %>% 
  left_join(daily_team_batting_2022, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_batting_2022, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBatters", "_AwayBatters")) %>% 
  left_join(daily_team_bullpen_2022, by = c("officialDate" = "Date", "home_team_name" = "Team")) %>% 
  left_join(daily_team_bullpen_2022, by = c("officialDate" = "Date", "away_team_name" = "Team"), suffix = c("_HomeBullpen", "_AwayBullpen"))

## Get Pitcher Game Logs
player_ids <- baseballr::chadwick_player_lu()

## Save files

saveRDS(gamescores19, "Baseball Machine/Daily Files/2019/full_training_data.rds")
saveRDS(gamescores20, "Baseball Machine/Daily Files/2020/full_training_data.rds")
saveRDS(gamescores21, "Baseball Machine/Daily Files/2021/full_training_data.rds")
saveRDS(gamescores22, "Baseball Machine/Daily Files/2022/full_training_data.rds")


### The Machine v2.0
### Data Processing

library("pacman")
p_load("tidyverse", "readr", "DataCombine", "readxl")

## Load and join data from FanGraphs

##2019

team_batting_L7_2019 <- read.csv("team_batting_L7_2019.csv")
team_batting_L7_2019$Date <- as.Date(team_batting_L7_2019$Date)
team_batting_L14_2019 <- read.csv("team_batting_L14_2019.csv")
team_batting_L14_2019$Date <- as.Date(team_batting_L14_2019$Date)
team_batting_L30_2019 <- read.csv("team_batting_L30_2019.csv")
team_batting_L30_2019$Date <- as.Date(team_batting_L30_2019$Date)
team_batting_s2d_2019 <- read.csv("team_batting_s2d_2019.csv")
team_batting_s2d_2019$Date <- as.Date(team_batting_s2d_2019$Date)

daily_team_batting_2019 <- full_join(team_batting_L7_2019, team_batting_L14_2019, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_2019, by = c("Team", "Date"))
colnames(daily_team_batting_2019)[57:83] <- paste0(colnames(daily_team_batting_2019)[57:83],'_L30')
daily_team_batting_2019 <- full_join(daily_team_batting_2019, team_batting_s2d_2019, by = c("Team", "Date"))

team_bullpen_L7_2019 <- read.csv("team_bullpen_L7_2019.csv")
team_bullpen_L7_2019$Date <- as.Date(team_bullpen_L7_2019$Date)
team_bullpen_L14_2019 <- read.csv("team_bullpen_L14_2019.csv")
team_bullpen_L14_2019$Date <- as.Date(team_bullpen_L14_2019$Date)
team_bullpen_L30_2019 <- read.csv("team_bullpen_L30_2019.csv")
team_bullpen_L30_2019$Date <- as.Date(team_bullpen_L30_2019$Date)
team_bullpen_s2d_2019 <- read.csv("team_bullpen_s2d_2019.csv")
team_bullpen_s2d_2019$Date <- as.Date(team_bullpen_s2d_2019$Date)

daily_team_bullpen_2019 <- full_join(team_bullpen_L7_2019, team_bullpen_L14_2019, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_2019, by = c("Team", "Date"))
colnames(daily_team_bullpen_2019)[51:74] <- paste0(colnames(daily_team_bullpen_2019)[51:74],'_L30')
daily_team_bullpen_2019 <- full_join(daily_team_bullpen_2019, team_bullpen_s2d_2019, by = c("Team", "Date"))

daily_pitchers_2019 <- read.csv("pitchers_s2d_2019.csv")
daily_pitchers_2019$Date <- as.Date(daily_pitchers_2019$Date)

##2020

team_batting_L7_2020 <- read.csv("team_batting_L7_2020.csv")
team_batting_L7_2020$Date <- as.Date(team_batting_L7_2020$Date)
team_batting_L14_2020 <- read.csv("team_batting_L14_2020.csv")
team_batting_L14_2020$Date <- as.Date(team_batting_L14_2020$Date)
team_batting_L30_2020 <- read.csv("team_batting_L30_2020.csv")
team_batting_L30_2020$Date <- as.Date(team_batting_L30_2020$Date)
team_batting_s2d_2020 <- read.csv("team_batting_s2d_2020.csv")
team_batting_s2d_2020$Date <- as.Date(team_batting_s2d_2020$Date)

daily_team_batting_2020 <- full_join(team_batting_L7_2020, team_batting_L14_2020, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_batting_L30_2020, by = c("Team", "Date"))
colnames(daily_team_batting_2020)[57:83] <- paste0(colnames(daily_team_batting_2020)[57:83],'_L30')
daily_team_batting_2020 <- full_join(daily_team_batting_2020, team_batting_s2d_2020, by = c("Team", "Date"))

team_bullpen_L7_2020 <- read.csv("team_bullpen_L7_2020.csv")
team_bullpen_L7_2020$Date <- as.Date(team_bullpen_L7_2020$Date)
team_bullpen_L14_2020 <- read.csv("team_bullpen_L14_2020.csv")
team_bullpen_L14_2020$Date <- as.Date(team_bullpen_L14_2020$Date)
team_bullpen_L30_2020 <- read.csv("team_bullpen_L30_2020.csv")
team_bullpen_L30_2020$Date <- as.Date(team_bullpen_L30_2020$Date)
team_bullpen_s2d_2020 <- read.csv("team_bullpen_s2d_2020.csv")
team_bullpen_s2d_2020$Date <- as.Date(team_bullpen_s2d_2020$Date)

daily_team_bullpen_2020 <- full_join(team_bullpen_L7_2020, team_bullpen_L14_2020, by = c("Team", "Date"), suffix = c("_L7", "_L14")) %>%
  full_join(team_bullpen_L30_2020, by = c("Team", "Date"))
colnames(daily_team_bullpen_2020)[51:74] <- paste0(colnames(daily_team_bullpen_2020)[51:74],'_L30')
daily_team_bullpen_2020 <- full_join(daily_team_bullpen_2020, team_bullpen_s2d_2020, by = c("Team", "Date"))

daily_pitchers_2020 <- read.csv("pitchers_s2d_2020.csv")
daily_pitchers_2020$Date <- as.Date(daily_pitchers_2020$Date)

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
DC_batting_20 <- mutate(DC_batting_20, ï..Name = if_else(retro_name == "" | is.na(retro_name), ï..Name, retro_name))
DC_pitching_20 <- left_join(DC_pitching_20, player_id_map[,c(18,25,26)], by = c("playerid" = "fg_id"))
DC_pitching_20 <- mutate(DC_pitching_20, ï..Name = if_else(retro_name == "" | is.na(retro_name), ï..Name, retro_name))
PECOTA_batting_20 <- left_join(PECOTA_batting_20, player_id_map[,c(1,25,26)], by = c("mlbid" = "mlb_id"))
PECOTA_batting_20 <- mutate(PECOTA_batting_20, name = if_else(retro_name == "" | is.na(retro_name), name, retro_name))
PECOTA_pitching_20 <- left_join(PECOTA_pitching_20, player_id_map[,c(1,25,26)], by = c("mlbid" = "mlb_id"))
PECOTA_pitching_20 <- mutate(PECOTA_pitching_20, name = if_else(retro_name == "" | is.na(retro_name), name, retro_name))

DC_PECOTA_batting <- full_join(DC_batting_20[,c(1,24)], PECOTA_batting_20[,c(5,43)], by = c("ï..Name" = "name"))

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
  group_by(ï..Name) %>%
  summarize(pit_WAR200_proj = (sum(WAR) / sum(IP)) * 200)

PWARP_20 <- PECOTA_pitching_20 %>%
  group_by(name) %>%
  summarize(PWARP200_proj = (sum(warp) / sum(ip)) * 200)

proj_SP_20 <- full_join(pit_WAR_20, PWARP_20, by = c("ï..Name" = "name"))

gl2020 <- left_join(gl2020, proj_SP_20, by = c("VisitorStartingPitcherName" = "ï..Name"))
colnames(gl2020)[630:631] <- paste0("Visiting_", colnames(gl2020)[630:631])
gl2020 <- left_join(gl2020, proj_SP_20, by = c("HomeStartingPitcherName" = "ï..Name"))
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




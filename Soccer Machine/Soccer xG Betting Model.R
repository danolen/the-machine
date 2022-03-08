### Soccer betting model
overallStart <- Sys.time()
library(rvest)
library(xml2)
library(readr)
library(janitor)
library(lubridate)
library(plyr)
library(tidyverse)
library(caret)
library(caretEnsemble)
library(parallel)
library(doParallel)
library(beepr)
#library(worldfootballR) # bring in transfermarkt data?

intervalStart <- Sys.time()
urls <- c("https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/9/10728/schedule/2020-2021-Premier-League-Scores-and-Fixtures", 
          "https://fbref.com/en/comps/9/3232/schedule/2019-2020-Premier-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/9/1889/schedule/2018-2019-Premier-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/9/1631/schedule/2017-2018-Premier-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/12/schedule/La-Liga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/12/10731/schedule/2020-2021-La-Liga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/12/3239/schedule/2019-2020-La-Liga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/12/1886/schedule/2018-2019-La-Liga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/12/1652/schedule/2017-2018-La-Liga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/20/schedule/Bundesliga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/20/10737/schedule/2020-2021-Bundesliga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/20/3248/schedule/2019-2020-Bundesliga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/20/2109/schedule/2018-2019-Bundesliga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/20/1634/schedule/2017-2018-Bundesliga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/11/schedule/Serie-A-Scores-and-Fixtures",
          "https://fbref.com/en/comps/11/10730/schedule/2020-2021-Serie-A-Scores-and-Fixtures",
          "https://fbref.com/en/comps/11/3260/schedule/2019-2020-Serie-A-Scores-and-Fixtures",
          "https://fbref.com/en/comps/11/1896/schedule/2018-2019-Serie-A-Scores-and-Fixtures",
          "https://fbref.com/en/comps/11/1640/schedule/2017-2018-Serie-A-Scores-and-Fixtures",
          "https://fbref.com/en/comps/13/schedule/Ligue-1-Scores-and-Fixtures",
          "https://fbref.com/en/comps/13/10732/schedule/2020-2021-Ligue-1-Scores-and-Fixtures",
          "https://fbref.com/en/comps/13/3243/schedule/2019-2020-Ligue-1-Scores-and-Fixtures",
          "https://fbref.com/en/comps/13/2104/schedule/2018-2019-Ligue-1-Scores-and-Fixtures",
          "https://fbref.com/en/comps/13/1632/schedule/2017-2018-Ligue-1-Scores-and-Fixtures",
          "https://fbref.com/en/comps/22/11006/schedule/2021-Major-League-Soccer-Scores-and-Fixtures",
          "https://fbref.com/en/comps/22/10090/schedule/2020-Major-League-Soccer-Scores-and-Fixtures",
          "https://fbref.com/en/comps/22/2798/schedule/2019-Major-League-Soccer-Scores-and-Fixtures",
          "https://fbref.com/en/comps/22/1759/schedule/2018-Major-League-Soccer-Scores-and-Fixtures",
          "https://fbref.com/en/comps/8/schedule/Champions-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/8/10096/schedule/2020-2021-Champions-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/8/2900/schedule/2019-2020-Champions-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/8/2102/schedule/2018-2019-Champions-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/19/schedule/Europa-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/19/10097/schedule/2020-2021-Europa-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/19/2901/schedule/2019-2020-Europa-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/19/2103/schedule/2018-2019-Europa-League-Scores-and-Fixtures")

epl_21_22 <- urls[[1]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "EPL", Season = "2021-2022")

epl_20_21 <- urls[[2]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "EPL", Season = "2020-2021")

epl_19_20 <- urls[[3]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "EPL", Season = "2019-2020")

epl_18_19 <- urls[[4]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "EPL", Season = "2018-2019")

epl_17_18 <- urls[[5]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "EPL", Season = "2017-2018")

laliga_21_22 <- urls[[6]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "La Liga", Season = "2021-2022")

laliga_20_21 <- urls[[7]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "La Liga", Season = "2020-2021")

laliga_19_20 <- urls[[8]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "La Liga", Season = "2019-2020")

laliga_18_19 <- urls[[9]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "La Liga", Season = "2018-2019")

laliga_17_18 <- urls[[10]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "La Liga", Season = "2017-2018")

bundes_21_22 <- urls[[11]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Bundesliga", Season = "2021-2022")

bundes_20_21 <- urls[[12]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Bundesliga", Season = "2020-2021")

bundes_19_20 <- urls[[13]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Bundesliga", Season = "2019-2020")

bundes_18_19 <- urls[[14]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Bundesliga", Season = "2018-2019")

bundes_17_18 <- urls[[15]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Bundesliga", Season = "2017-2018")

seriea_21_22 <- urls[[16]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Serie A", Season = "2021-2022")

seriea_20_21 <- urls[[17]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Serie A", Season = "2020-2021")

seriea_19_20 <- urls[[18]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Serie A", Season = "2019-2020")

seriea_18_19 <- urls[[19]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Serie A", Season = "2018-2019")

seriea_17_18 <- urls[[20]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Serie A", Season = "2017-2018")

ligue1_21_22 <- urls[[21]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Ligue 1", Season = "2021-2022")

ligue1_20_21 <- urls[[22]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Ligue 1", Season = "2020-2021")

ligue1_19_20 <- urls[[23]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Ligue 1", Season = "2019-2020")

ligue1_18_19 <- urls[[24]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Ligue 1", Season = "2018-2019")

ligue1_17_18 <- urls[[25]] %>%
  read_html() %>% 
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Ligue 1", Season = "2017-2018")

mls_21 <- urls[[26]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "Day") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "MLS", Season = "2021")

mls_20 <- urls[[27]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "Day" & xG != "" & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  filter(Home_Score != "") %>%
  mutate(League = "MLS", Season = "2020")
  
mls_19 <- urls[[28]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "Day" & xG != "" & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "MLS", Season = "2019")

mls_18 <- urls[[29]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "Day" & xG != "" & is.na(xG) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  filter(Home_Score != "") %>%
  mutate(League = "MLS", Season = "2018")

UCL_21_22 <- urls[[30]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "UCL", Season = "2021-2022")

UCL_20_21 <- urls[[31]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Wk != "" & Wk != "Wk") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "UCL", Season = "2020-2021")

UCL_19_20 <- urls[[32]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Wk != "" & Wk != "Wk") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "UCL", Season = "2019-2020")

UCL_18_19 <- urls[[33]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Wk != "" & Wk != "Wk") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "UCL", Season = "2018-2019")

UEL_21_22 <- urls[[34]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Wk != "" & Wk != "Wk") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "UEL", Season = "2021-2022")

UEL_20_21 <- urls[[35]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "" & Day != "Day") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  filter(Home_Score != "") %>%
  mutate(League = "UEL", Season = "2020-2021")

UEL_19_20 <- urls[[36]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "" & Day != "Day") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  filter(Home_Score != "") %>%
  mutate(League = "UEL", Season = "2019-2020")

UEL_18_19 <- urls[[37]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "" & Day != "Day") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  filter(Home_Score != "") %>%
  mutate(League = "UEL", Season = "2018-2019")

intervalEnd <- Sys.time()
paste("Web scraping took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

fixtures <- rbind(epl_21_22, epl_20_21, epl_19_20, epl_18_19, epl_17_18,
                  laliga_21_22, laliga_20_21, laliga_19_20, laliga_18_19, laliga_17_18,
                  bundes_21_22, bundes_20_21, bundes_19_20, bundes_18_19, bundes_17_18,
                  seriea_21_22, seriea_20_21, seriea_19_20, seriea_18_19, seriea_17_18,
                  ligue1_21_22, ligue1_20_21, ligue1_19_20, ligue1_18_19, ligue1_17_18,
                  mls_21, mls_20, mls_19, mls_18,
                  UCL_21_22, UCL_20_21, UCL_19_20, UCL_18_19,
                  UEL_21_22, UEL_20_21, UEL_19_20, UEL_18_19)
fixtures$Date <- as.Date(fixtures$Date)
today <- Sys.Date()

upcoming <- fixtures %>% 
  filter(Date >= today) %>%
  arrange(Date, Time) %>%
  mutate(xG = 0, Home_Score = 0, Away_Score = 0, xG.1 = 0)

scores <- fixtures %>%
  filter(is.na(xG) == FALSE & xG != "" & is.na(Home_Score) == FALSE & Home_Score != "") %>%
  arrange(Date, Time)

scores$xG <- as.numeric(scores$xG)
scores$Home_Score <- as.numeric(scores$Home_Score) 
scores$Away_Score <- as.numeric(scores$Away_Score)
scores$xG.1 <- as.numeric(scores$xG.1)

scores <- bind_rows(scores, upcoming) 

home <- scores %>% 
  mutate(ID = gsub(" ", "", gsub("[[:punct:]]","",paste0(Home, Away, Date, Time)), fixed = TRUE)) %>% 
  select(ID, Date, Day, Time, League, Season, Home, Away, xG:xG.1) %>% 
  mutate(Home_or_Away = "Home") %>% 
  select(ID:Away, Home_or_Away, xG, xG.1, Home_Score, Away_Score) %>% 
  rename(Team = Home,
         Opponent = Away,
         xGA = xG.1,
         Goals = Home_Score,
         GoalsAllowed = Away_Score)

away <- scores %>% 
  mutate(ID = gsub(" ", "", gsub("[[:punct:]]","",paste0(Home, Away, Date, Time)), fixed = TRUE)) %>% 
  select(ID, Date, Day, Time, League, Season, Away, Home, xG:xG.1) %>% 
  mutate(Home_or_Away = "Away") %>% 
  select(ID:Home, Home_or_Away, xG.1, xG, Away_Score, Home_Score) %>% 
  rename(Team = Away,
         Opponent = Home,
         xG = xG.1,
         xGA = xG,
         Goals = Away_Score,
         GoalsAllowed = Home_Score)

metrics <- bind_rows(home, away) %>% 
  arrange(Date, Time, League, ID) %>% 
  mutate(Team = trimws(case_when(League %in% c('UCL', 'UEL') & Home_or_Away == "Home" ~ substr(Team, 1, nchar(Team)-3),
                                 League %in% c('UCL', 'UEL') & Home_or_Away == "Away" ~ substr(Team, 4, nchar(Team)),
                                 TRUE ~ Team), which = c("both"))) %>% 
  group_by(Team, League, Season, Home_or_Away) %>% 
  mutate(SplitxG = cumsum(xG) - xG,
         SplitxGA = cumsum(xGA) - xGA,
         SplitGoals = cumsum(Goals) - Goals,
         SplitGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SplitGP = row_number() - 1,
         SplitxG_roll4 = (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4,
         SplitxGA_roll4 = (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4,
         SplitGoals_roll4 = (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4,
         SplitGoalsAllowed_roll4 = (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4) %>% 
  group_by(Team, League, Season) %>% 
  mutate(SeasonxG = cumsum(xG) - xG,
         SeasonxGA = cumsum(xGA) - xGA,
         SeasonGoals = cumsum(Goals) - Goals,
         SeasonGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SeasonGP = row_number() - 1,
         SeasonxG_roll4 = (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4,
         SeasonxGA_roll4 = (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4,
         SeasonGoals_roll4 = (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4,
         SeasonGoalsAllowed_roll4 = (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4) %>% 
  ungroup() %>% 
  mutate(SplitxG = SplitxG / SplitGP,
         SplitxGA = SplitxGA / SplitGP,
         SplitGoals = SplitGoals / SplitGP,
         SplitGoalsAllowed = SplitGoalsAllowed / SplitGP,
         SeasonxG = SeasonxG / SeasonGP,
         SeasonxGA = SeasonxGA / SeasonGP,
         SeasonGoals = SeasonGoals / SeasonGP,
         SeasonGoalsAllowed = SeasonGoalsAllowed / SeasonGP) %>% 
  replace(is.na(.), 0)

train_df <- metrics %>% 
  left_join(metrics, by = c("ID" = "ID", "Opponent" = "Team"), suffix = c("", "_Opp")) %>% 
  select(-(Date_Opp:GoalsAllowed_Opp)) %>% 
  mutate(Outcome = as.factor(case_when(Goals > GoalsAllowed ~ 'Win',
                                       Goals == GoalsAllowed ~ 'Draw',
                                       TRUE ~ 'Lose')),
         Minus0.5 = as.factor(case_when(Goals - 0.5 > GoalsAllowed ~ "Win",
                                        TRUE ~ "Lose")),
         Minus1 = as.factor(case_when(Goals - 1 > GoalsAllowed ~ "Win",
                                      Goals - 1 == GoalsAllowed ~ "Push",
                                      TRUE ~ "Lose")),
         Minus1.5 = as.factor(case_when(Goals - 1.5 > GoalsAllowed ~ "Win",
                                        TRUE ~ "Lose")),
         Minus2 = as.factor(case_when(Goals - 2 > GoalsAllowed ~ "Win",
                                      Goals - 2 == GoalsAllowed ~ "Push",
                                      TRUE ~ "Lose")),
         Minus2.5 = as.factor(case_when(Goals - 2.5 > GoalsAllowed ~ "Win",
                                        TRUE ~ "Lose")),
         Minus3 = as.factor(case_when(Goals - 3 > GoalsAllowed ~ "Win",
                                      Goals - 3 == GoalsAllowed ~ "Push",
                                      TRUE ~ "Lose")),
         Minus3.5 = as.factor(case_when(Goals - 3.5 > GoalsAllowed ~ "Win",
                                        TRUE ~ "Lose")),
         Plus0.5 = as.factor(case_when(Goals + 0.5 > GoalsAllowed ~ "Win",
                                       TRUE ~ "Lose")),
         Plus1 = as.factor(case_when(Goals + 1 > GoalsAllowed ~ "Win",
                                     Goals + 1 == GoalsAllowed ~ "Push",
                                     TRUE ~ "Lose")),
         Plus1.5 = as.factor(case_when(Goals + 1.5 > GoalsAllowed ~ "Win",
                                       TRUE ~ "Lose")),
         Plus2 = as.factor(case_when(Goals + 2 > GoalsAllowed ~ "Win",
                                     Goals + 2 == GoalsAllowed ~ "Push",
                                     TRUE ~ "Lose")),
         Plus2.5 = as.factor(case_when(Goals + 2.5 > GoalsAllowed ~ "Win",
                                       TRUE ~ "Lose")),
         Plus3 = as.factor(case_when(Goals + 3 > GoalsAllowed ~ "Win",
                                     Goals + 3 == GoalsAllowed ~ "Push",
                                     TRUE ~ "Lose")),
         Plus3.5 = as.factor(case_when(Goals + 3.5 > GoalsAllowed ~ "Win",
                                       TRUE ~ "Lose")),
         Total1.5 = as.factor(case_when(Goals + GoalsAllowed > 1.5 ~ "Over",
                                        TRUE ~ "Under")),
         Total2 = as.factor(case_when(Goals + GoalsAllowed > 2 ~ "Over",
                                       Goals + GoalsAllowed == 2 ~ "Push",
                                       TRUE ~ "Under")),
         Total2.5 = as.factor(case_when(Goals + GoalsAllowed > 2.5 ~ "Over",
                                        TRUE ~ "Under")),
         Total3 = as.factor(case_when(Goals + GoalsAllowed > 3 ~ "Over",
                                       Goals + GoalsAllowed == 3 ~ "Push",
                                       TRUE ~ "Under")),
         Total3.5 = as.factor(case_when(Goals + GoalsAllowed > 3.5 ~ "Over",
                                        TRUE ~ "Under")),
         Total4 = as.factor(case_when(Goals + GoalsAllowed > 4 ~ "Over",
                                       Goals + GoalsAllowed == 4 ~ "Push",
                                       TRUE ~ "Under")),
         Total4.5 = as.factor(case_when(Goals + GoalsAllowed > 4.5 ~ "Over",
                                        TRUE ~ "Under")),
         BTTS = as.factor(case_when(Goals > 0 & GoalsAllowed > 0 ~ "Yes",
                          TRUE ~ "No")),
         TT0.5 = as.factor(case_when(Goals > 0.5 ~ "Over",
                                     TRUE ~ "Under")),
         TT1 = as.factor(case_when(Goals > 1 ~ "Over",
                                   Goals == 1 ~ "Push",
                                   TRUE ~ "Under")),
         TT1.5 = as.factor(case_when(Goals > 1.5 ~ "Over",
                                     TRUE ~ "Under")),
         TT2 = as.factor(case_when(Goals > 2 ~ "Over",
                                   Goals == 2 ~ "Push",
                                   TRUE ~ "Under")),
         TT2.5 = as.factor(case_when(Goals > 2.5 ~ "Over",
                                     TRUE ~ "Under")),
         TT3 = as.factor(case_when(Goals > 3 ~ "Over",
                                   Goals == 3 ~ "Push",
                                   TRUE ~ "Under")),
         TT3.5 = as.factor(case_when(Goals > 3.5 ~ "Over",
                                     TRUE ~ "Under")))

train <- train_df %>% 
  filter(SeasonGP > 3 & SeasonGP_Opp > 3 & Date < today)%>% 
  select(-ID,
         -Date,
         -Day,
         -Time, 
         -Team,
         -Opponent,
         -xG,
         -xGA,
         -GoalsAllowed,
         -(Outcome:TT3.5))

upcoming_games <- filter(train_df, Date >= today)

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
gbm_mod <- train(Goals ~ .,
                 data = train,
                 method = "gbm",
                 trControl = fitControl)
set.seed(1234)
cub_mod <- train(Goals ~ .,
                 data = train,
                 method = "cubist",
                 trControl = fitControl,
                 tuneGrid = expand.grid(.committees=20,
                                        .neighbors=9))
set.seed(1234)
rf_mod <- train(Goals ~ .,
                 data = train,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = expand.grid(.mtry = c(10,15,20),
                                        .splitrule = c("variance", "extratrees"),
                                        .min.node.size = c(5,10)))
set.seed(1234)
ctree_mod <- train(Goals ~ .,
                 data = train,
                 method = "ctree",
                 trControl = fitControl,
                 tuneLength = 10)
set.seed(1234)
pls_mod <- train(Goals ~ .,
                 data = train,
                 method = "pls",
                 trControl = fitControl,
                 tuneLength = 15,
                 preProc = c("center", "scale"))
set.seed(1234)
lm_mod <- train(Goals ~ .,
                 data = train,
                 method = "lm",
                 trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Goals regression model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(gbm_mod, "Soccer Machine/Models/train_gbm.rds")
saveRDS(cub_mod, "Soccer Machine/Models/train_cub.rds")
saveRDS(rf_mod, "Soccer Machine/Models/train_rf.rds")
saveRDS(ctree_mod, "Soccer Machine/Models/train_ctree.rds")
saveRDS(pls_mod, "Soccer Machine/Models/train_pls.rds")
saveRDS(lm_mod, "Soccer Machine/Models/train_lm.rds")

train_prob <- train_df %>% 
  filter(SeasonGP > 3 & SeasonGP_Opp > 3 & Date < today)%>% 
  select(-ID,
         -Date,
         -Day,
         -Time, 
         -Team,
         -Opponent,
         -xG,
         -xGA,
         -Goals,
         -GoalsAllowed)

upcoming_games_prob <- filter(train_df, Date >= today)

Outcome_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Minus0.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
outcome_gbm <- train(Outcome ~ .,
                 data = Outcome_df,
                 method = "gbm",
                 trControl = fitControl)
set.seed(1234)
outcome_pls <- train(Outcome ~ .,
                     data = Outcome_df,
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
outcome_xgb <- train(Outcome ~ .,
                      data = Outcome_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Outcome classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(outcome_gbm, "Soccer Machine/Models/outcome_gbm.rds")
saveRDS(outcome_pls, "Soccer Machine/Models/outcome_pls.rds")
saveRDS(outcome_xgb, "Soccer Machine/Models/outcome_xgb.rds")

Minus0.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -Outcome, -(Minus1:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
minus0.5_gbm <- train(Minus0.5 ~ .,
                     data = Minus0.5_df,
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
minus0.5_pls <- train(Minus0.5 ~ .,
                     data = Minus0.5_df,
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
minus0.5_xgb <- train(Minus0.5 ~ .,
                         data = Minus0.5_df,
                         method = "xgbTree",
                         trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Minus0.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(minus0.5_gbm, "Soccer Machine/Models/minus0.5_gbm.rds")
saveRDS(minus0.5_pls, "Soccer Machine/Models/minus0.5_pls.rds")
saveRDS(minus0.5_xgb, "Soccer Machine/Models/minus0.5_xgb.rds")

Minus1_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Minus0.5), -(Minus1.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
minus1_gbm <- train(Minus1 ~ .,
                      data = Minus1_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
minus1_pls <- train(Minus1 ~ .,
                      data = Minus1_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
minus1_xgb <- train(Minus1 ~ .,
                          data = Minus1_df,
                          method = "xgbTree",
                          trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Minus1 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(minus1_gbm, "Soccer Machine/Models/minus1_gbm.rds")
saveRDS(minus1_pls, "Soccer Machine/Models/minus1_pls.rds")
saveRDS(minus1_xgb, "Soccer Machine/Models/minus1_xgb.rds")

Minus1.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Minus1), -(Minus2:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
minus1.5_gbm <- train(Minus1.5 ~ .,
                    data = Minus1.5_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
minus1.5_pls <- train(Minus1.5 ~ .,
                    data = Minus1.5_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
minus1.5_xgb <- train(Minus1.5 ~ .,
                        data = Minus1.5_df,
                        method = "xgbTree",
                        trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Minus1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(minus1.5_gbm, "Soccer Machine/Models/minus1.5_gbm.rds")
saveRDS(minus1.5_pls, "Soccer Machine/Models/minus1.5_pls.rds")
saveRDS(minus1.5_xgb, "Soccer Machine/Models/minus1.5_xgb.rds")

Minus2_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Minus1.5), -(Minus2.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
minus2_gbm <- train(Minus2 ~ .,
                    data = Minus2_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
minus2_pls <- train(Minus2 ~ .,
                    data = Minus2_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
minus2_xgb <- train(Minus2 ~ .,
                    data = Minus2_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Minus2 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(minus2_gbm, "Soccer Machine/Models/minus2_gbm.rds")
saveRDS(minus2_pls, "Soccer Machine/Models/minus2_pls.rds")
saveRDS(minus2_xgb, "Soccer Machine/Models/minus2_xgb.rds")

Minus2.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Minus2), -(Minus3:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
minus2.5_gbm <- train(Minus2.5 ~ .,
                      data = Minus2.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
minus2.5_pls <- train(Minus2.5 ~ .,
                      data = Minus2.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
minus2.5_xgb <- train(Minus2.5 ~ .,
                      data = Minus2.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Minus2.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(minus2.5_gbm, "Soccer Machine/Models/minus2.5_gbm.rds")
saveRDS(minus2.5_pls, "Soccer Machine/Models/minus2.5_pls.rds")
saveRDS(minus2.5_xgb, "Soccer Machine/Models/minus2.5_xgb.rds")

Minus3_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Minus2.5), -(Minus3.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
minus3_gbm <- train(Minus3 ~ .,
                    data = Minus3_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
minus3_pls <- train(Minus3 ~ .,
                    data = Minus3_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
minus3_xgb <- train(Minus3 ~ .,
                    data = Minus3_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Minus3 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(minus3_gbm, "Soccer Machine/Models/minus3_gbm.rds")
saveRDS(minus3_pls, "Soccer Machine/Models/minus3_pls.rds")
saveRDS(minus3_xgb, "Soccer Machine/Models/minus3_xgb.rds")

Minus3.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Minus3), -(Plus0.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
minus3.5_gbm <- train(Minus3.5 ~ .,
                      data = Minus3.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
minus3.5_pls <- train(Minus3.5 ~ .,
                      data = Minus3.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
minus3.5_xgb <- train(Minus3.5 ~ .,
                      data = Minus3.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Minus3.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(minus3.5_gbm, "Soccer Machine/Models/minus3.5_gbm.rds")
saveRDS(minus3.5_pls, "Soccer Machine/Models/minus3.5_pls.rds")
saveRDS(minus3.5_xgb, "Soccer Machine/Models/minus3.5_xgb.rds")

Plus0.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Minus3.5), -(Plus1:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
plus0.5_gbm <- train(Plus0.5 ~ .,
                      data = Plus0.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
plus0.5_pls <- train(Plus0.5 ~ .,
                      data = Plus0.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
plus0.5_xgb <- train(Plus0.5 ~ .,
                      data = Plus0.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Plus0.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(plus0.5_gbm, "Soccer Machine/Models/plus0.5_gbm.rds")
saveRDS(plus0.5_pls, "Soccer Machine/Models/plus0.5_pls.rds")
saveRDS(plus0.5_xgb, "Soccer Machine/Models/plus0.5_xgb.rds")

Plus1_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Plus0.5), -(Plus1.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
plus1_gbm <- train(Plus1 ~ .,
                    data = Plus1_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
plus1_pls <- train(Plus1 ~ .,
                    data = Plus1_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
plus1_xgb <- train(Plus1 ~ .,
                    data = Plus1_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Plus1 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(plus1_gbm, "Soccer Machine/Models/plus1_gbm.rds")
saveRDS(plus1_pls, "Soccer Machine/Models/plus1_pls.rds")
saveRDS(plus1_xgb, "Soccer Machine/Models/plus1_xgb.rds")

Plus1.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Plus1), -(Plus2:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
plus1.5_gbm <- train(Plus1.5 ~ .,
                      data = Plus1.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
plus1.5_pls <- train(Plus1.5 ~ .,
                      data = Plus1.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
plus1.5_xgb <- train(Plus1.5 ~ .,
                      data = Plus1.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Plus1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(plus1.5_gbm, "Soccer Machine/Models/plus1.5_gbm.rds")
saveRDS(plus1.5_pls, "Soccer Machine/Models/plus1.5_pls.rds")
saveRDS(plus1.5_xgb, "Soccer Machine/Models/plus1.5_xgb.rds")

Plus2_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Plus1.5), -(Plus2.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
plus2_gbm <- train(Plus2 ~ .,
                    data = Plus2_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
plus2_pls <- train(Plus2 ~ .,
                    data = Plus2_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
plus2_xgb <- train(Plus2 ~ .,
                    data = Plus2_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Plus2 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(plus2_gbm, "Soccer Machine/Models/plus2_gbm.rds")
saveRDS(plus2_pls, "Soccer Machine/Models/plus2_pls.rds")
saveRDS(plus2_xgb, "Soccer Machine/Models/plus2_xgb.rds")

Plus2.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Plus2), -(Plus3:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
plus2.5_gbm <- train(Plus2.5 ~ .,
                      data = Plus2.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
plus2.5_pls <- train(Plus2.5 ~ .,
                      data = Plus2.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
plus2.5_xgb <- train(Plus2.5 ~ .,
                      data = Plus2.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Plus2.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(plus2.5_gbm, "Soccer Machine/Models/plus2.5_gbm.rds")
saveRDS(plus2.5_pls, "Soccer Machine/Models/plus2.5_pls.rds")
saveRDS(plus2.5_xgb, "Soccer Machine/Models/plus2.5_xgb.rds")

Plus3_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Plus2.5), -(Plus3.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
plus3_gbm <- train(Plus3 ~ .,
                    data = Plus3_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
plus3_pls <- train(Plus3 ~ .,
                    data = Plus3_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
plus3_xgb <- train(Plus3 ~ .,
                    data = Plus3_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Plus3 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(plus3_gbm, "Soccer Machine/Models/plus3_gbm.rds")
saveRDS(plus3_pls, "Soccer Machine/Models/plus3_pls.rds")
saveRDS(plus3_xgb, "Soccer Machine/Models/plus3_xgb.rds")

Plus3.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Plus3), -(Total1.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
plus3.5_gbm <- train(Plus3.5 ~ .,
                      data = Plus3.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
plus3.5_pls <- train(Plus3.5 ~ .,
                      data = Plus3.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
plus3.5_xgb <- train(Plus3.5 ~ .,
                      data = Plus3.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Plus3.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(plus3.5_gbm, "Soccer Machine/Models/plus3.5_gbm.rds")
saveRDS(plus3.5_pls, "Soccer Machine/Models/plus3.5_pls.rds")
saveRDS(plus3.5_xgb, "Soccer Machine/Models/plus3.5_xgb.rds")

Total1.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Plus3.5), -(Total2:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
total1.5_gbm <- train(Total1.5 ~ .,
                      data = Total1.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
total1.5_pls <- train(Total1.5 ~ .,
                      data = Total1.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
total1.5_xgb <- train(Total1.5 ~ .,
                      data = Total1.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Total1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(total1.5_gbm, "Soccer Machine/Models/total1.5_gbm.rds")
saveRDS(total1.5_pls, "Soccer Machine/Models/total1.5_pls.rds")
saveRDS(total1.5_xgb, "Soccer Machine/Models/total1.5_xgb.rds")

Total2_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Total1.5), -(Total2.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
total2_gbm <- train(Total2 ~ .,
                    data = Total2_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
total2_pls <- train(Total2 ~ .,
                    data = Total2_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
total2_xgb <- train(Total2 ~ .,
                    data = Total2_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Total2 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(total2_gbm, "Soccer Machine/Models/total2_gbm.rds")
saveRDS(total2_pls, "Soccer Machine/Models/total2_pls.rds")
saveRDS(total2_xgb, "Soccer Machine/Models/total2_xgb.rds")

Total2.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Total2), -(Total3:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
total2.5_gbm <- train(Total2.5 ~ .,
                      data = Total2.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
total2.5_pls <- train(Total2.5 ~ .,
                      data = Total2.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
total2.5_xgb <- train(Total2.5 ~ .,
                      data = Total2.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Total2.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(total2.5_gbm, "Soccer Machine/Models/total2.5_gbm.rds")
saveRDS(total2.5_pls, "Soccer Machine/Models/total2.5_pls.rds")
saveRDS(total2.5_xgb, "Soccer Machine/Models/total2.5_xgb.rds")

Total3_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Total2.5), -(Total3.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
total3_gbm <- train(Total3 ~ .,
                    data = Total3_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
total3_pls <- train(Total3 ~ .,
                    data = Total3_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
total3_xgb <- train(Total3 ~ .,
                    data = Total3_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Total3 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(total3_gbm, "Soccer Machine/Models/total3_gbm.rds")
saveRDS(total3_pls, "Soccer Machine/Models/total3_pls.rds")
saveRDS(total3_xgb, "Soccer Machine/Models/total3_xgb.rds")

Total3.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Total3), -(Total4:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
total3.5_gbm <- train(Total3.5 ~ .,
                      data = Total3.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
total3.5_pls <- train(Total3.5 ~ .,
                      data = Total3.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
total3.5_xgb <- train(Total3.5 ~ .,
                      data = Total3.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Total3.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(total3.5_gbm, "Soccer Machine/Models/total3.5_gbm.rds")
saveRDS(total3.5_pls, "Soccer Machine/Models/total3.5_pls.rds")
saveRDS(total3.5_xgb, "Soccer Machine/Models/total3.5_xgb.rds")

Total4_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Total3.5), -(Total4.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
total4_gbm <- train(Total4 ~ .,
                    data = Total4_df,
                    method = "gbm",
                    trControl = fitControl)
set.seed(1234)
total4_pls <- train(Total4 ~ .,
                    data = Total4_df,
                    method = "pls",
                    trControl = fitControl,
                    tuneLength = 15,
                    preProc = c("center", "scale"))
set.seed(1234)
total4_xgb <- train(Total4 ~ .,
                    data = Total4_df,
                    method = "xgbTree",
                    trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Total4 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(total4_gbm, "Soccer Machine/Models/total4_gbm.rds")
saveRDS(total4_pls, "Soccer Machine/Models/total4_pls.rds")
saveRDS(total4_xgb, "Soccer Machine/Models/total4_xgb.rds")

Total4.5_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Total4), -(BTTS:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
total4.5_gbm <- train(Total4.5 ~ .,
                      data = Total4.5_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
total4.5_pls <- train(Total4.5 ~ .,
                      data = Total4.5_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
total4.5_xgb <- train(Total4.5 ~ .,
                      data = Total4.5_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("Total4.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(total4.5_gbm, "Soccer Machine/Models/total4.5_gbm.rds")
saveRDS(total4.5_pls, "Soccer Machine/Models/total4.5_pls.rds")
saveRDS(total4.5_xgb, "Soccer Machine/Models/total4.5_xgb.rds")

BTTS_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(Outcome:Total4.5), -(TT0.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
BTTS_gbm <- train(BTTS ~ .,
                      data = BTTS_df,
                      method = "gbm",
                      trControl = fitControl)
set.seed(1234)
BTTS_pls <- train(BTTS ~ .,
                      data = BTTS_df,
                      method = "pls",
                      trControl = fitControl,
                      tuneLength = 15,
                      preProc = c("center", "scale"))
set.seed(1234)
BTTS_xgb <- train(BTTS ~ .,
                      data = BTTS_df,
                      method = "xgbTree",
                      trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("BTTS classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(BTTS_gbm, "Soccer Machine/Models/BTTS_gbm.rds")
saveRDS(BTTS_pls, "Soccer Machine/Models/BTTS_pls.rds")
saveRDS(BTTS_xgb, "Soccer Machine/Models/BTTS_xgb.rds")

TT0.5_df <- train_prob %>%
  select(-Home_or_Away, -(Outcome:BTTS), -(TT1:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
tt0.5_gbm <- train(TT0.5 ~ .,
                     data = TT0.5_df,
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
tt0.5_pls <- train(TT0.5 ~ .,
                     data = TT0.5_df,
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
tt0.5_xgb <- train(TT0.5 ~ .,
                     data = TT0.5_df,
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("TT0.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(tt0.5_gbm, "Soccer Machine/Models/tt0.5_gbm.rds")
saveRDS(tt0.5_pls, "Soccer Machine/Models/tt0.5_pls.rds")
saveRDS(tt0.5_xgb, "Soccer Machine/Models/tt0.5_xgb.rds")

TT1_df <- train_prob %>%
  select(-Home_or_Away, -(Outcome:TT0.5), -(TT1.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
tt1_gbm <- train(TT1 ~ .,
                   data = TT1_df,
                   method = "gbm",
                   trControl = fitControl)
set.seed(1234)
tt1_pls <- train(TT1 ~ .,
                   data = TT1_df,
                   method = "pls",
                   trControl = fitControl,
                   tuneLength = 15,
                   preProc = c("center", "scale"))
set.seed(1234)
tt1_xgb <- train(TT1 ~ .,
                   data = TT1_df,
                   method = "xgbTree",
                   trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("TT1 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(tt1_gbm, "Soccer Machine/Models/tt1_gbm.rds")
saveRDS(tt1_pls, "Soccer Machine/Models/tt1_pls.rds")
saveRDS(tt1_xgb, "Soccer Machine/Models/tt1_xgb.rds")

TT1.5_df <- train_prob %>%
  select(-Home_or_Away, -(Outcome:TT1), -(TT2:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
tt1.5_gbm <- train(TT1.5 ~ .,
                     data = TT1.5_df,
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
tt1.5_pls <- train(TT1.5 ~ .,
                     data = TT1.5_df,
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
tt1.5_xgb <- train(TT1.5 ~ .,
                     data = TT1.5_df,
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("TT1.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(tt1.5_gbm, "Soccer Machine/Models/tt1.5_gbm.rds")
saveRDS(tt1.5_pls, "Soccer Machine/Models/tt1.5_pls.rds")
saveRDS(tt1.5_xgb, "Soccer Machine/Models/tt1.5_xgb.rds")

TT2_df <- train_prob %>%
  select(-Home_or_Away, -(Outcome:TT1.5), -(TT2.5:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
tt2_gbm <- train(TT2 ~ .,
                   data = TT2_df,
                   method = "gbm",
                   trControl = fitControl)
set.seed(1234)
tt2_pls <- train(TT2 ~ .,
                   data = TT2_df,
                   method = "pls",
                   trControl = fitControl,
                   tuneLength = 15,
                   preProc = c("center", "scale"))
set.seed(1234)
tt2_xgb <- train(TT2 ~ .,
                   data = TT2_df,
                   method = "xgbTree",
                   trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("TT2 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(tt2_gbm, "Soccer Machine/Models/tt2_gbm.rds")
saveRDS(tt2_pls, "Soccer Machine/Models/tt2_pls.rds")
saveRDS(tt2_xgb, "Soccer Machine/Models/tt2_xgb.rds")

TT2.5_df <- train_prob %>%
  select(-Home_or_Away, -(Outcome:TT2), -(TT3:TT3.5))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
tt2.5_gbm <- train(TT2.5 ~ .,
                     data = TT2.5_df,
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
tt2.5_pls <- train(TT2.5 ~ .,
                     data = TT2.5_df,
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
tt2.5_xgb <- train(TT2.5 ~ .,
                     data = TT2.5_df,
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("TT2.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(tt2.5_gbm, "Soccer Machine/Models/tt2.5_gbm.rds")
saveRDS(tt2.5_pls, "Soccer Machine/Models/tt2.5_pls.rds")
saveRDS(tt2.5_xgb, "Soccer Machine/Models/tt2.5_xgb.rds")

TT3_df <- train_prob %>%
  select(-Home_or_Away, -(Outcome:TT2.5), -TT3.5)

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
tt3_gbm <- train(TT3 ~ .,
                   data = TT3_df,
                   method = "gbm",
                   trControl = fitControl)
set.seed(1234)
tt3_pls <- train(TT3 ~ .,
                   data = TT3_df,
                   method = "pls",
                   trControl = fitControl,
                   tuneLength = 15,
                   preProc = c("center", "scale"))
set.seed(1234)
tt3_xgb <- train(TT3 ~ .,
                   data = TT3_df,
                   method = "xgbTree",
                   trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("TT3 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))


saveRDS(tt3_gbm, "Soccer Machine/Models/tt3_gbm.rds")
saveRDS(tt3_pls, "Soccer Machine/Models/tt3_pls.rds")
saveRDS(tt3_xgb, "Soccer Machine/Models/tt3_xgb.rds")

TT3.5_df <- train_prob %>%
  select(-Home_or_Away, -(Outcome:TT3))

intervalStart <- Sys.time()
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           allowParallel = TRUE)

set.seed(1234)
tt3.5_gbm <- train(TT3.5 ~ .,
                     data = TT3.5_df,
                     method = "gbm",
                     trControl = fitControl)
set.seed(1234)
tt3.5_pls <- train(TT3.5 ~ .,
                     data = TT3.5_df,
                     method = "pls",
                     trControl = fitControl,
                     tuneLength = 15,
                     preProc = c("center", "scale"))
set.seed(1234)
tt3.5_xgb <- train(TT3.5 ~ .,
                     data = TT3.5_df,
                     method = "xgbTree",
                     trControl = fitControl)

stopCluster(cluster)
intervalEnd <- Sys.time()
paste("TT3.5 classification model training took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

saveRDS(tt3.5_gbm, "Soccer Machine/Models/tt3.5_gbm.rds")
saveRDS(tt3.5_pls, "Soccer Machine/Models/tt3.5_pls.rds")
saveRDS(tt3.5_xgb, "Soccer Machine/Models/tt3.5_xgb.rds")

overallEnd <- Sys.time()
paste("Entire script took",overallEnd - overallStart,attr(overallEnd - overallStart,"units"))

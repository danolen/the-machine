### Soccer betting model

library("pacman")
p_load("rvest", "xml2", "readr", "janitor", "lubridate", "plyr", "tidyverse", "caret")

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
         # Win = as.factor(case_when(Goals > GoalsAllowed ~ 1,
         #                 TRUE ~ 0)),
         # Draw = as.factor(case_when(Goals == GoalsAllowed ~ 1,
         #                  TRUE ~ 0)),
         WinMinus1.5 = as.factor(case_when(Goals - 1.5 > GoalsAllowed ~ 1,
                                 TRUE ~ 0)),
         WinMinus2.5 = as.factor(case_when(Goals - 2.5 > GoalsAllowed ~ 1,
                                 TRUE ~ 0)),
         WinMinus3.5 = as.factor(case_when(Goals - 3.5 > GoalsAllowed ~ 1,
                                 TRUE ~ 0)),
         WinPlus0.5 = as.factor(case_when(Goals - 0.5 > GoalsAllowed ~ 1,
                                TRUE ~ 0)),
         WinPlus1.5 = as.factor(case_when(Goals - 1.5 > GoalsAllowed ~ 1,
                                TRUE ~ 0)),
         WinPlus2.5 = as.factor(case_when(Goals - 2.5 > GoalsAllowed ~ 1,
                                TRUE ~ 0)),
         WinPlus3.5 = as.factor(case_when(Goals - 3.5 > GoalsAllowed ~ 1,
                                TRUE ~ 0)),
         TotalOver1.5 = as.factor(case_when(Goals + GoalsAllowed > 1.5 ~ 1,
                                  TRUE ~ 0)),
         TotalOver2.5 = as.factor(case_when(Goals + GoalsAllowed > 2.5 ~ 1,
                                  TRUE ~ 0)),
         TotalOver3.5 = as.factor(case_when(Goals + GoalsAllowed > 3.5 ~ 1,
                                  TRUE ~ 0)),
         TotalOver4.5 = as.factor(case_when(Goals + GoalsAllowed > 4.5 ~ 1,
                                  TRUE ~ 0)),
         BTTS = as.factor(case_when(Goals > 0 & GoalsAllowed > 0 ~ 1,
                          TRUE ~ 0)),
         TTOver0.5 = as.factor(case_when(Goals > 0 ~ 1,
                               TRUE ~ 0)),
         TTOver1.5 = as.factor(case_when(Goals > 1 ~ 1,
                               TRUE ~ 0)),
         TTOver2.5 = as.factor(case_when(Goals > 2 ~ 1,
                               TRUE ~ 0)),
         TTOver3.5 = as.factor(case_when(Goals > 3 ~ 1,
                               TRUE ~ 0)))

train <- train_df %>% 
  filter(SeasonGP > 3 & SeasonGP_Opp > 3 & !(Season %in% c('2021-2022', '2021')))%>% 
  select(-ID,
         -Date,
         -Day,
         -Time, 
         -Team,
         -Opponent,
         -xG,
         -xGA,
         -GoalsAllowed,
         -(Win:TTOver3.5))
test <- train_df %>%
  filter(SeasonGP > 3 & SeasonGP_Opp > 3 & (Season %in% c('2021-2022', '2021')) & Date < today) %>% 
  select(-ID,
         -Date,
         -Day,
         -Time, 
         -Team,
         -Opponent,
         -xG,
         -xGA,
         -GoalsAllowed,
         -(Win:TTOver3.5))
set.seed(1234)
picked <- sample(seq_len(nrow(test)), 1500)
add <- test[-picked,]
test <- test[picked,]
train <- bind_rows(train, add)
upcoming_games <- filter(train_df, Date >= today)

set.seed(1234)
gbm_mod <- train(Goals ~ .,
                 data = train,
                 method = "gbm",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10))
set.seed(1234)
cub_mod <- train(Goals ~ .,
                 data = train,
                 method = "cubist",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10),
                 tuneGrid = expand.grid(.committees=20,
                                        .neighbors=9))
set.seed(1234)
rf_mod <- train(Goals ~ .,
                 data = train,
                 method = "ranger",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10),
                 tuneGrid = expand.grid(.mtry = c(10,15,20),
                                        .splitrule = c("variance", "extratrees"),
                                        .min.node.size = c(5,10)))
# set.seed(1234)
# svm_mod <- train(Goals ~ .,
#                 data = train,
#                 method = "svmRadial",
#                 trControl = trainControl(method = "repeatedcv",
#                                          number = 10),
#                 tuneLength = 10,
#                 preProc = c("center", "scale"))
set.seed(1234)
ctree_mod <- train(Goals ~ .,
                 data = train,
                 method = "ctree",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10),
                 tuneLength = 10)
set.seed(1234)
pls_mod <- train(Goals ~ .,
                 data = train,
                 method = "pls",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10),
                 tuneLength = 15,
                 preProc = c("center", "scale"))
set.seed(1234)
lm_mod <- train(Goals ~ .,
                 data = train,
                 method = "lm",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10))
set.seed(1234)
allResamples <- resamples(list("GBM" = gbm_mod,
                               "Cubist" = cub_mod,
                               "RF" = rf_mod,
                               #"SVM" = svm_mod,
                               "CTree" = ctree_mod,
                               "PLS" = pls_mod,
                               "LM" = lm_mod))

parallelplot(allResamples, metric = "RMSE")
parallelplot(allResamples)
parallelplot(allResamples, metric = "Rsquared")

saveRDS(gbm_mod, "train_gbm.rds")
saveRDS(cub_mod, "train_cub.rds")
saveRDS(rf_mod, "train_rf.rds")
#saveRDS(svm_mod, "train_svm.rds")
saveRDS(ctree_mod, "train_ctree.rds")
saveRDS(pls_mod, "train_pls.rds")
saveRDS(lm_mod, "train_lm.rds")

performance <- train

performance$gbm_G <- predict(gbm_mod, performance)
performance$cub_G <- predict(cub_mod, performance)
performance$rf_G <- predict(rf_mod, performance)
performance$ctree_G <- predict(ctree_mod, performance)
performance$pls_G <- predict(pls_mod, performance)
performance$lm_G <- predict(lm_mod, performance)
performance <- performance %>% 
  mutate(equal_weight = (gbm_G + cub_G + rf_G + ctree_G + pls_G + lm_G) / 6)
ens_lm <- lm(Goals ~ gbm_G + cub_G + rf_G + ctree_G + pls_G + lm_G, performance)
summary(ens_lm)
performance$linear_weight <- predict(ens_lm, performance)

summary(performance %>% select(Goals, gbm_G:linear_weight))

RMSE(performance$gbm_G, performance$Goals)
RMSE(performance$cub_G, performance$Goals)
RMSE(performance$rf_G, performance$Goals)
RMSE(performance$ctree_G, performance$Goals)
RMSE(performance$pls_G, performance$Goals)
RMSE(performance$lm_G, performance$Goals)
RMSE(performance$equal_weight, performance$Goals)
RMSE(performance$linear_weight, performance$Goals)

test_performance <- test

test_performance$gbm_G <- predict(gbm_mod, test_performance)
test_performance$cub_G <- predict(cub_mod, test_performance)
test_performance$rf_G <- predict(rf_mod, test_performance)
test_performance$ctree_G <- predict(ctree_mod, test_performance)
test_performance$pls_G <- predict(pls_mod, test_performance)
test_performance$lm_G <- predict(lm_mod, test_performance)
test_performance <- test_performance %>% 
  mutate(equal_weight = (gbm_G + cub_G + rf_G + ctree_G + pls_G + lm_G) / 6)
test_performance$linear_weight <- predict(ens_lm, test_performance)

summary(test_performance %>% select(Goals, gbm_G:linear_weight))

RMSE(test_performance$gbm_G, test_performance$Goals)
RMSE(test_performance$cub_G, test_performance$Goals)
RMSE(test_performance$rf_G, test_performance$Goals)
RMSE(test_performance$ctree_G, test_performance$Goals)
RMSE(test_performance$pls_G, test_performance$Goals)
RMSE(test_performance$lm_G, test_performance$Goals)
RMSE(test_performance$equal_weight, test_performance$Goals)
RMSE(test_performance$linear_weight, test_performance$Goals)

train_prob <- train_df %>% 
  filter(SeasonGP > 3 & SeasonGP_Opp > 3 & !(Season %in% c('2021-2022', '2021')))%>% 
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
test_prob <- train_df %>%
  filter(SeasonGP > 3 & SeasonGP_Opp > 3 & (Season %in% c('2021-2022', '2021')) & Date < today) %>% 
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
set.seed(1234)
picked_prob <- sample(seq_len(nrow(test_prob)), 1500)
add_prob <- test_prob[-picked_prob,]
test_prob <- test_prob[picked_prob,]
train_prob <- bind_rows(train_prob, add_prob)
upcoming_games_prob <- filter(train_df, Date >= today)

Outcome_df <- train_prob %>% 
  filter(Home_or_Away == "Home") %>% 
  select(-Home_or_Away, -(WinMinus1.5:TTOver3.5))

set.seed(1234)
win_glm <- train(Outcome ~ .,
                 data = Outcome_df,
                 method = "glm",
                 family = "binomial",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10))
set.seed(1234)
win_gbm <- train(Outcome ~ .,
                 data = Outcome_df,
                 method = "gbm",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 10))
set.seed(1234)
win_rf <- train(Outcome ~ .,
                data = Outcome_df,
                method = "ranger",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10),
                tuneGrid = expand.grid(.mtry = c(10,15,20),
                                       .splitrule = c("gini", "extratrees"),
                                       .min.node.size = c(5,10)))

Outcome_df$preds <- predict(win_gbm, Outcome_df, type = "prob")


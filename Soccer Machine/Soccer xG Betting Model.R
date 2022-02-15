### Soccer betting model

library("pacman")
p_load("tidyverse", "rvest", "xml2", "readr", "janitor", "lubridate", "plyr", "h2o")

# Get xGoals data from FBRef.com

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
          "https://fbref.com/en/comps/22/schedule/Major-League-Soccer-Scores-and-Fixtures",
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

metrics <- scores %>%
  group_by(Home, League, Season) %>%
  dplyr::mutate(HomexGHome = cumsum(xG) - xG,
                HomeGHome = cumsum(Home_Score) - Home_Score,
                HomexGAHome = cumsum(xG.1) - xG.1,
                HomeGAHome = cumsum(Away_Score) - Away_Score,
                HomeGPHome = row_number() - 1)

averageif<-function(df,id,year,tiime,valuestoaverage, new_column_name){
  df<- df[order(df[id],df[tiime]),]
  z<-dim(df)[2]+1
  df[,z]<-0;colnames(df)[z]<-new_column_name
  n<-dim(df)[1]
  for(i in 1:n){
    df[i,z]<-mean(df[df[,id]==df[i,id]&df[,year]==df[i,year]&df[,tiime]<df[i,tiime],valuestoaverage])
  }
  return(df)
}

scores <- averageif(scores, 'Home', 'Season', 'Date', 'xG', 'HomexGHome') %>%
  averageif('Home', 'Season', 'Date', 'xG.1', 'HomexGAHome') %>%
  averageif('Away', 'Season', 'Date', 'xG.1', 'AwayxGAway') %>%
  averageif('Away', 'Season', 'Date', 'xG', 'AwayxGAAway')
  
scores$League <- as.factor(scores$League)

train <- filter(scores, Date < today)
upcoming_games <- filter(scores, Date >= today)

x_home <- c('HomexGHome', 'AwayxGAAway', 'League')
x_away <- c('AwayxGAway', 'HomexGAHome', 'League')
y_home <- 'Home_Score'
y_away <- 'Away_Score'

h2o.init()

train <- as.h2o(train)

home_model <- h2o.automl(x = x_home, y = y_home,
                         training_frame = train,
                         nfolds = 10)
home_lb <- home_model@leaderboard
home_leader <- home_model@leader
saved_path_home <- h2o.saveModel(object = home_leader, path = getwd(), force = TRUE)
print(saved_path_home) # Save this somewhere
# C:\\Users\\danie\\Desktop\\Sports Stuff\\Soccer Betting\\StackedEnsemble_AllModels_AutoML_20210713_170146
# RMSE (10/14): 1.255557
# RMSE (11/15): 1.253940

away_model <- h2o.automl(x = x_away, y = y_away,
                         training_frame = train,
                         nfolds = 10)
away_lb <- away_model@leaderboard
away_leader <- away_model@leader
saved_path_away <- h2o.saveModel(object = away_leader, path = getwd(), force = TRUE)
print(saved_path_away) # Save this somewhere
# C:\\Users\\danie\\Desktop\\Sports Stuff\\Soccer Betting\\StackedEnsemble_AllModels_AutoML_20210713_173827
# RMSE (10/14): 1.132495
# RMSE (11/15): 1.133055

h2o.shutdown(prompt = FALSE)




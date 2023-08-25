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
library(worldfootballR) # bring in transfermarkt data?

intervalStart <- Sys.time()

mls <- fb_match_results(country = "USA",
                          gender = "M",
                          season_end_year = c(2018, 2019, 2020, 2021, 2022, 2023), tier = "1st") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>% 
  filter(Day != "") %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         League = "MLS",
         Season = as.character(Season))

Big5 <- fb_match_results(country = c("ENG","ESP","ITA","GER","FRA"),
                           gender = "M",
                           season_end_year = c(2018,2019,2020,2021,2022,2023), tier = "1st") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         League = case_when(League == "Premier League" ~ "EPL",
                            League == "Fußball-Bundesliga" ~ "Bundesliga",
                            TRUE ~ League),
         Season = paste0(Season-1,"-",Season))

uefa <- fb_match_results(country = c("NED","POR"),
                         gender = "M",
                         season_end_year = c(2019,2020,2021,2022,2023), tier = "1st") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         Season = paste0(Season-1,"-",Season))

Champ <- fb_match_results(country = "ENG",
                              gender = "M",
                              season_end_year = c(2019,2020,2021,2022,2023), tier = "2nd") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         Season = paste0(Season-1,"-",Season))

MX <- fb_match_results(country = "MEX",
                          gender = "M",
                          season_end_year = c(2019,2020,2021,2022,2023), tier = "1st") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         Season = paste0(Season-1,"-",Season))

Brazil <- fb_match_results(country = "BRA",
                       gender = "M",
                       season_end_year = c(2019,2020,2021,2022,2023), tier = "1st") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         League = "Brasileiro Serie A",
         Season = paste0(Season-1,"-",Season))

ucl <- fb_match_results(country = "",
                        gender = "M",
                        season_end_year = c(2018,2019,2020,2021,2022,2023),
                        tier = "",
                        non_dom_league_url = "https://fbref.com/en/comps/8/history/Champions-League-Seasons") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         Season = paste0(Season-1,"-",Season)) %>% 
  mutate(Home = trimws(substr(Home, 1, nchar(Home)-3)),
         Away = trimws(substr(Away, 4, nchar(Away))))

uel <- fb_match_results(country = "",
                        gender = "M",
                        season_end_year = c(2018,2019,2020,2021,2022,2023),
                        tier = "",
                        non_dom_league_url = "https://fbref.com/en/comps/19/history/Europa-League-Seasons") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
  rename(xG = Home_xG,
         Home_Score = HomeGoals,
         Away_Score = AwayGoals,
         xG.1 = Away_xG,
         League = Competition_Name,
         Season = Season_End_Year) %>%
  mutate(xG = as.numeric(xG),
         Home_Score = as.numeric(Home_Score),
         Away_Score = as.numeric(Away_Score),
         xG.1 = as.numeric(xG.1),
         Season = paste0(Season-1,"-",Season)) %>% 
  mutate(Home = trimws(substr(Home, 1, nchar(Home)-3)),
         Away = trimws(substr(Away, 4, nchar(Away))))

uel <- uel_save %>% 
  mutate(Home = trimws(substr(Home, 1, nchar(Home)-3)),
         Away = trimws(substr(Away, 4, nchar(Away))))


intervalEnd <- Sys.time()
paste("Web scraping took",intervalEnd - intervalStart,attr(intervalEnd - intervalStart,"units"))

fixtures <- bind_rows(Big5, mls, Champ, MX, Brazil, uefa, ucl, uel)
fixtures$Date <- as.Date(fixtures$Date)
today <- Sys.Date()

upcoming <- fixtures %>% 
  filter(Date >= today) %>%
  arrange(Date, Time) %>%
  mutate(xG = 0, Home_Score = 0, Away_Score = 0, xG.1 = 0)

scores <- fixtures %>%
  filter(is.na(xG) == FALSE & xG != "" & is.na(Home_Score) == FALSE & Home_Score != "") %>%
  arrange(Date, Time)

scores <- bind_rows(scores, upcoming)

home <- scores %>% 
  mutate(ID = gsub(" ", "", gsub("[[:punct:]]","",paste0(Home, Away, Date, Time)), fixed = TRUE)) %>% 
  select(ID, Date, Day, Time, League, Season, Home, Away, xG:xG.1) %>% 
  mutate(Home_or_Away = "Home") %>% 
  select(ID:Away, Home_or_Away, xG, xG.1, Home_Score, Away_Score) %>% 
  dplyr::rename(Team = Home,
         Opponent = Away,
         xGA = xG.1,
         Goals = Home_Score,
         GoalsAllowed = Away_Score)

away <- scores %>% 
  mutate(ID = gsub(" ", "", gsub("[[:punct:]]","",paste0(Home, Away, Date, Time)), fixed = TRUE)) %>% 
  select(ID, Date, Day, Time, League, Season, Away, Home, xG:xG.1) %>% 
  mutate(Home_or_Away = "Away") %>% 
  select(ID:Home, Home_or_Away, xG.1, xG, Away_Score, Home_Score) %>% 
  dplyr::rename(Team = Away,
         Opponent = Home,
         xG = xG.1,
         xGA = xG,
         Goals = Away_Score,
         GoalsAllowed = Home_Score)

metrics <- bind_rows(home, away) %>% 
  arrange(Date, Time, League, ID) %>%
  filter(!League %in% c('UEFA Champions League', 'UEFA Europa League',
                        'Eredivisie', 'Primeira Liga')) %>%
  group_by(Team, League, Season, Home_or_Away) %>% 
  dplyr::mutate(SplitxG = cumsum(xG) - xG,
         SplitxGA = cumsum(xGA) - xGA,
         SplitGoals = cumsum(Goals) - Goals,
         SplitGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SplitGP = row_number() - 1,
         SplitxG_roll4 = case_when(SplitGP == 1 ~ lag(xG,1),
                                   SplitGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                   SplitGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/3,
                                   TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
         SplitxGA_roll4 = case_when(SplitGP == 1 ~ lag(xGA,1),
                                   SplitGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                   SplitGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/3,
                                   TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
         SplitGoals_roll4 = case_when(SplitGP == 1 ~ lag(Goals,1),
                                   SplitGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                   SplitGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3))/3,
                                   TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals)+lag(Goals,4))/4),
         SplitGoalsAllowed_roll4 = case_when(SplitGP == 1 ~ lag(GoalsAllowed,1),
                                      SplitGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                      SplitGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3))/3,
                                      TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>% 
  group_by(Team, League, Season) %>% 
  dplyr::mutate(SeasonxG = cumsum(xG) - xG,
         SeasonxGA = cumsum(xGA) - xGA,
         SeasonGoals = cumsum(Goals) - Goals,
         SeasonGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SeasonGP = row_number() - 1,
         SeasonxG_roll4 = case_when(SeasonGP == 1 ~ lag(xG,1),
                                    SeasonGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                    SeasonGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/4,
                                    TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
         SeasonxGA_roll4 = case_when(SeasonGP == 1 ~ lag(xGA,1),
                                     SeasonGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                     SeasonGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/4,
                                     TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
         SeasonGoals_roll4 = case_when(SeasonGP == 1 ~ lag(Goals,1),
                                       SeasonGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                       SeasonGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals))/4,
                                       TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4),
         SeasonGoalsAllowed_roll4 = case_when(SeasonGP == 1 ~ lag(GoalsAllowed,1),
                                              SeasonGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                              SeasonGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed))/4,
                                              TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>% 
  ungroup() %>% 
  dplyr::mutate(SplitxG = SplitxG / SplitGP,
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
  filter(SplitGP > 3 & SplitGP_Opp > 3 & Date < today)%>% 
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

saveRDS(gbm_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_gbm.rds")
saveRDS(cub_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_cub.rds")
saveRDS(rf_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_rf.rds")
saveRDS(ctree_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_ctree.rds")
saveRDS(pls_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_pls.rds")
saveRDS(lm_mod, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_lm.rds")

train_prob <- train_df %>% 
  filter(SplitGP > 3 & SplitGP_Opp > 3 & Date < today)%>% 
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

saveRDS(outcome_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_gbm.rds")
saveRDS(outcome_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_pls.rds")
saveRDS(outcome_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_xgb.rds")

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

saveRDS(minus1_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_gbm.rds")
saveRDS(minus1_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_pls.rds")
saveRDS(minus1_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_xgb.rds")

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

saveRDS(minus1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_gbm.rds")
saveRDS(minus1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_pls.rds")
saveRDS(minus1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_xgb.rds")

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

saveRDS(minus2_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_gbm.rds")
saveRDS(minus2_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_pls.rds")
saveRDS(minus2_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_xgb.rds")

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

saveRDS(minus2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_gbm.rds")
saveRDS(minus2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_pls.rds")
saveRDS(minus2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_xgb.rds")

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

saveRDS(minus3_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_gbm.rds")
saveRDS(minus3_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_pls.rds")
saveRDS(minus3_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_xgb.rds")

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

saveRDS(minus3.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_gbm.rds")
saveRDS(minus3.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_pls.rds")
saveRDS(minus3.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_xgb.rds")

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

saveRDS(plus1_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_gbm.rds")
saveRDS(plus1_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_pls.rds")
saveRDS(plus1_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_xgb.rds")

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

saveRDS(plus1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_gbm.rds")
saveRDS(plus1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_pls.rds")
saveRDS(plus1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_xgb.rds")

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

saveRDS(plus2_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_gbm.rds")
saveRDS(plus2_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_pls.rds")
saveRDS(plus2_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_xgb.rds")

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

saveRDS(plus2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_gbm.rds")
saveRDS(plus2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_pls.rds")
saveRDS(plus2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_xgb.rds")

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

saveRDS(plus3_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_gbm.rds")
saveRDS(plus3_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_pls.rds")
saveRDS(plus3_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_xgb.rds")

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

saveRDS(plus3.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_gbm.rds")
saveRDS(plus3.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_pls.rds")
saveRDS(plus3.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_xgb.rds")

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

saveRDS(total1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_gbm.rds")
saveRDS(total1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_pls.rds")
saveRDS(total1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_xgb.rds")

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

saveRDS(total2_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_gbm.rds")
saveRDS(total2_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_pls.rds")
saveRDS(total2_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_xgb.rds")

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

saveRDS(total2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_gbm.rds")
saveRDS(total2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_pls.rds")
saveRDS(total2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_xgb.rds")

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

saveRDS(total3_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_gbm.rds")
saveRDS(total3_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_pls.rds")
saveRDS(total3_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_xgb.rds")

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

saveRDS(total3.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_gbm.rds")
saveRDS(total3.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_pls.rds")
saveRDS(total3.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_xgb.rds")

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

saveRDS(total4_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_gbm.rds")
saveRDS(total4_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_pls.rds")
saveRDS(total4_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_xgb.rds")

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

saveRDS(total4.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_gbm.rds")
saveRDS(total4.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_pls.rds")
saveRDS(total4.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_xgb.rds")

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

saveRDS(BTTS_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_gbm.rds")
saveRDS(BTTS_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_pls.rds")
saveRDS(BTTS_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_xgb.rds")

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

saveRDS(tt0.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_gbm.rds")
saveRDS(tt0.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_pls.rds")
saveRDS(tt0.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_xgb.rds")

TT1_df <- train_prob %>%
  select(-(Outcome:TT0.5), -(TT1.5:TT3.5))

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

saveRDS(tt1_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_gbm.rds")
saveRDS(tt1_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_pls.rds")
saveRDS(tt1_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_xgb.rds")

TT1.5_df <- train_prob %>%
  select(-(Outcome:TT1), -(TT2:TT3.5))

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

saveRDS(tt1.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_gbm.rds")
saveRDS(tt1.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_pls.rds")
saveRDS(tt1.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_xgb.rds")

TT2_df <- train_prob %>%
  select(-(Outcome:TT1.5), -(TT2.5:TT3.5))

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

saveRDS(tt2_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_gbm.rds")
saveRDS(tt2_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_pls.rds")
saveRDS(tt2_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_xgb.rds")

TT2.5_df <- train_prob %>%
  select(-(Outcome:TT2), -(TT3:TT3.5))

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

saveRDS(tt2.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_gbm.rds")
saveRDS(tt2.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_pls.rds")
saveRDS(tt2.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_xgb.rds")

TT3_df <- train_prob %>%
  select(-(Outcome:TT2.5), -TT3.5)

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


saveRDS(tt3_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_gbm.rds")
saveRDS(tt3_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_pls.rds")
saveRDS(tt3_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_xgb.rds")

TT3.5_df <- train_prob %>%
  select(-(Outcome:TT3))

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

saveRDS(tt3.5_gbm, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_gbm.rds")
saveRDS(tt3.5_pls, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_pls.rds")
saveRDS(tt3.5_xgb, "C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_xgb.rds")

cup_metrics <- bind_rows(home, away) %>% 
  arrange(Date, Time, League, ID) %>%
  filter(League %in% c('UEFA Champions League', 'UEFA Europa League')) %>%
  group_by(Team, League, Season, Home_or_Away) %>% 
  dplyr::mutate(SplitxG = cumsum(xG) - xG,
                SplitxGA = cumsum(xGA) - xGA,
                SplitGoals = cumsum(Goals) - Goals,
                SplitGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
                SplitGP = row_number() - 1,
                SplitxG_roll4 = case_when(SplitGP == 1 ~ lag(xG,1),
                                          SplitGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                          SplitGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/3,
                                          TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
                SplitxGA_roll4 = case_when(SplitGP == 1 ~ lag(xGA,1),
                                           SplitGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                           SplitGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/3,
                                           TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
                SplitGoals_roll4 = case_when(SplitGP == 1 ~ lag(Goals,1),
                                             SplitGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                             SplitGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3))/3,
                                             TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals)+lag(Goals,4))/4),
                SplitGoalsAllowed_roll4 = case_when(SplitGP == 1 ~ lag(GoalsAllowed,1),
                                                    SplitGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                                    SplitGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3))/3,
                                                    TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>% 
  group_by(Team, League, Season) %>% 
  dplyr::mutate(SeasonxG = cumsum(xG) - xG,
                SeasonxGA = cumsum(xGA) - xGA,
                SeasonGoals = cumsum(Goals) - Goals,
                SeasonGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
                SeasonGP = row_number() - 1,
                SeasonxG_roll4 = case_when(SeasonGP == 1 ~ lag(xG,1),
                                           SeasonGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                           SeasonGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/4,
                                           TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
                SeasonxGA_roll4 = case_when(SeasonGP == 1 ~ lag(xGA,1),
                                            SeasonGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                            SeasonGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/4,
                                            TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
                SeasonGoals_roll4 = case_when(SeasonGP == 1 ~ lag(Goals,1),
                                              SeasonGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                              SeasonGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals))/4,
                                              TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4),
                SeasonGoalsAllowed_roll4 = case_when(SeasonGP == 1 ~ lag(GoalsAllowed,1),
                                                     SeasonGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                                     SeasonGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed))/4,
                                                     TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>% 
  ungroup() %>% 
  dplyr::mutate(SplitxG = SplitxG / SplitGP,
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
  filter(SplitGP > 3 & SplitGP_Opp > 3 & Date < today)%>% 
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

overallEnd <- Sys.time()
paste("Entire script took",overallEnd - overallStart,attr(overallEnd - overallStart,"units"))

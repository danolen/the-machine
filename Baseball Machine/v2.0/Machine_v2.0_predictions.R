### The Machine v2.0
### Predictions

library("pacman")
p_load("tidyverse", "h2o", "bit64", "zoo", "reshape")

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

## Bring in odds for 2020
odds <- read.csv("mlb odds 2020.csv")
colnames(odds)[1] <- "Date"
odds$Date <- as.Date(odds$Date)
odds$Open_V <- as.numeric(odds$Open_V)
odds$OpenOU_O <- as.numeric(odds$OpenOU_O)
odds$OpenOUOdds_O <- as.numeric(odds$OpenOUOdds_O)
odds$Open_H <- as.numeric(odds$Open_H)
odds$OpenOU_U <- as.numeric(odds$OpenOU_U)
odds$OpenOUOdds_U <- as.numeric(odds$OpenOUOdds_U)

gl2019_2020 <- left_join(gl2019_2020, odds, by = c("Date" = "Date", "VisitingTeam" = "Team_V", "HomeTeam" = "Team_H"))

## train models, save to disk, and predict
h2o.init(nthreads = -1)

train <- filter(gl2019_2020,
                Date > as.Date("2020-01-01") &
                is.na(VisitingBatters_PA_L7) == FALSE &
                is.na(HomeBatters_PA_L7) == FALSE &
                LengthInOuts > 50 &
                LengthInOuts < 55) %>% as.h2o()

model_VisitorRunsScored <- h2o.automl(x = x_VisitorRunsScored,
                                      y = y_VisitorRunsScored,
                                      training_frame = train,
                                      nfolds = 10)
VisitorRunsScored_lb <- model_VisitorRunsScored@leaderboard
VisitorRunsScored_leader <- model_VisitorRunsScored@leader
saved_path_VRS <- h2o.saveModel(object = VisitorRunsScored_leader, path = getwd(), force = TRUE)
print(saved_path_VRS) # Save this somewhere
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_BestOfFamily_AutoML_20210218_175719"
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_AllModels_AutoML_20210306_123626"
loaded_model_VRS <- h2o.loadModel(saved_path_VRS)

model_HomeRunsScore <- h2o.automl(x = x_HomeRunsScore,
                                  y = y_HomeRunsScore,
                                  training_frame = train,
                                  nfolds = 10)
HomeRunsScore_lb <- model_HomeRunsScore@leaderboard
HomeRunsScore_leader <- model_HomeRunsScore@leader
saved_path_HRS <- h2o.saveModel(object = HomeRunsScore_leader, path = getwd(), force = TRUE)
print(saved_path_HRS) # Save this somewhere
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\StackedEnsemble_BestOfFamily_AutoML_20210218_183920"
# "C:\\Users\\danie\\Desktop\\Baseball Stuff\\MLB Team Projections\\GLM_1_AutoML_20210306_131838"
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

test <- filter(gl2019_2020, 
               Date > as.Date("2020-01-01") &
                 is.na(VisitingBatters_PA_L7) == FALSE &
                 is.na(HomeBatters_PA_L7) == FALSE &
                 LengthInOuts > 50 &
                 LengthInOuts < 55) %>% as.h2o()

Visitor_pred <- predict(loaded_model_VRS, test) %>% as.data.frame()
colnames(Visitor_pred) <- "Visitor_pred"

Home_pred <- predict(loaded_model_HRS, test) %>% as.data.frame()
colnames(Home_pred) <- "Home_pred"

## Combine predictions with test set
test <- filter(gl2019_2020, 
               Date > as.Date("2020-01-01") &
                 is.na(VisitingBatters_PA_L7) == FALSE &
                 is.na(HomeBatters_PA_L7) == FALSE &
                 LengthInOuts > 50 &
                 LengthInOuts < 55) %>% cbind(Visitor_pred) %>% cbind(Home_pred)

## Functions to simulate ML, RL, and totals probabilities
simulate_game <- function(awayScorePred, homeScorePred, homeORaway = c("home", "away"), max_score = 20) {
  score_matrix = dpois(0:max_score, awayScorePred) %o% dpois(0:max_score, homeScorePred)
  if_else(homeORaway == "home", 
          sum(score_matrix[upper.tri(score_matrix)]) + (sum(diag(score_matrix))/2), 
          sum(score_matrix[lower.tri(score_matrix)]) + (sum(diag(score_matrix))/2))
}

simulate_RL <- function(awayScorePred, homeScorePred, homeORaway = c("home", "away"), rl_spread = c(-1.5, 1.5), max_score = 20) {
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

## Add in implied odds, projected odds, edge, kelly criteria
test <- mutate(test,
               V_ImpliedOdds = if_else(Open_V > 0, 100 / (Open_V + 100), abs(Open_V) / (abs(Open_V) + 100)),
               H_ImpliedOdds = if_else(Open_H > 0, 100 / (Open_H + 100), abs(Open_H) / (abs(Open_H) + 100)),
               V_ImpliedOdds_RL = if_else(RLOdds_V > 0, 100 / (RLOdds_V + 100), abs(RLOdds_V) / (abs(RLOdds_V) + 100)),
               H_ImpliedOdds_RL = if_else(RLOdds_H > 0, 100 / (RLOdds_H + 100), abs(RLOdds_H) / (abs(RLOdds_H) + 100)),
               O_ImpliedOdds = if_else(OpenOUOdds_O > 0, 100 / (OpenOUOdds_O + 100), abs(OpenOUOdds_O) / (abs(OpenOUOdds_O) + 100)),
               U_ImpliedOdds = if_else(OpenOUOdds_U > 0, 100 / (OpenOUOdds_U + 100), abs(OpenOUOdds_U) / (abs(OpenOUOdds_U) + 100)))

test <- test %>% 
  rowwise %>%
  mutate(V_ProjOdds = simulate_game(Visitor_pred, Home_pred, "away"),
         H_ProjOdds = simulate_game(Visitor_pred, Home_pred, "home"),
         V_ProjOdds_RL = simulate_RL(Visitor_pred, Home_pred, "away", rl_spread = RunLine_V),
         H_ProjOdds_RL = simulate_RL(Visitor_pred, Home_pred, "home", rl_spread = RunLine_H),
         O_ProjOdds = simulate_total(Visitor_pred, Home_pred, "over", ou_total = OpenOU_O),
         U_ProjOdds = simulate_total(Visitor_pred, Home_pred, "under", ou_total = OpenOU_U))

test <- mutate(test,
               V_MLDiff = V_ProjOdds - V_ImpliedOdds,
               H_MLDiff = H_ProjOdds - H_ImpliedOdds,
               MLPick = if_else(V_MLDiff > H_MLDiff, VisitingTeam, HomeTeam),
               Pick_MLOdds = if_else(V_MLDiff > H_MLDiff, Open_V, Open_H),
               Pick_MLWinProb = if_else(V_MLDiff > H_MLDiff, V_ProjOdds, H_ProjOdds),
               MLPick_Edge = if_else(V_MLDiff > H_MLDiff, V_MLDiff, H_MLDiff),
               Fract_Odds_ML = (110 / abs(Pick_MLOdds)) ^ if_else(Pick_MLOdds < 0, 1, -1),
               Kelly_Criteria_ML = (Pick_MLWinProb * (Fract_Odds_ML + 1) - 1) / Fract_Odds_ML,
               MLWinner = if_else(VisitorRunsScored > HomeRunsScore, VisitingTeam, HomeTeam),
               MLCorrect = if_else(MLPick == MLWinner, 1, 0),
               V_RLDiff = V_ProjOdds_RL - V_ImpliedOdds_RL,
               H_RLDiff = H_ProjOdds_RL - H_ImpliedOdds_RL,
               RLPick = if_else(V_RLDiff > H_RLDiff, VisitingTeam, HomeTeam),
               Pick_RLOdds = if_else(V_RLDiff > H_RLDiff, RLOdds_V, RLOdds_H),
               Pick_RLWinProb = if_else(V_RLDiff > H_RLDiff, V_ProjOdds_RL, H_ProjOdds_RL),
               MLPick_Edge = if_else(V_MLDiff > H_MLDiff, V_MLDiff, H_MLDiff),
               Fract_Odds_RL = (110 / abs(Pick_RLOdds)) ^ if_else(Pick_RLOdds < 0, 1, -1),
               Kelly_Criteria_RL = (Pick_RLWinProb * (Fract_Odds_RL + 1) - 1) / Fract_Odds_RL,
               RLWinner = if_else(VisitorRunsScored + RunLine_V > HomeRunsScore, VisitingTeam, HomeTeam),
               RLCorrect = if_else(RLPick == RLWinner, 1, 0),
               O_Diff = O_ProjOdds - O_ImpliedOdds,
               U_Diff = U_ProjOdds - U_ImpliedOdds,
               OUPick = if_else(O_Diff > U_Diff, "over", "under"),
               Pick_OUOdds = if_else(O_Diff > U_Diff, OpenOUOdds_O, OpenOUOdds_U),
               Pick_OUWinProb = if_else(O_Diff > U_Diff, O_ProjOdds, U_ProjOdds),
               OUPick_Edge = if_else(O_Diff > U_Diff, O_Diff, U_Diff),
               Fract_Odds_OU = (110 / abs(Pick_OUOdds)) ^ if_else(Pick_OUOdds < 0, 1, -1),
               Kelly_Criteria_OU = (Pick_OUWinProb * (Fract_Odds_OU + 1) - 1) / Fract_Odds_OU,
               OUWinner = if_else((VisitorRunsScored + HomeRunsScore) > OpenOU_O, "over", "under"),
               OUCorrect = if_else(OUPick == OUWinner, 1, 0))
  
## Test dataframes for ML, RL, and totals picks
test_MLpicks <- filter(test, Kelly_Criteria_ML > 0 & Kelly_Criteria_ML < 0.55 & Pick_MLOdds > -200) %>%
  mutate(Units = if_else(MLCorrect == 1, 
                         if_else(Pick_MLOdds > 0, Pick_MLOdds / 100, 1),
                         if_else(Pick_MLOdds > 0, -1, Pick_MLOdds / 100)),
         Kelly_Bet_ML = if_else(Kelly_Criteria_ML < 0.1, 10, Kelly_Criteria_ML * 100),
         Kelly_Profit_ML = Units * Kelly_Bet_ML)

mean(test_MLpicks$MLCorrect)
sum(test_MLpicks$Units)*10
sum(test_MLpicks$Kelly_Profit_ML)
sum(test_MLpicks$Units)*10/nrow(test_MLpicks)

test_RLpicks <- filter(test, Kelly_Criteria_RL >= 0.35 & Kelly_Criteria_RL < 0.5 & Pick_RLOdds > -200) %>%
  mutate(Units = if_else(RLCorrect == 1, 
                         if_else(Pick_RLOdds > 0, Pick_RLOdds / 100, 1),
                         if_else(Pick_RLOdds > 0, -1, Pick_RLOdds / 100)),
         Kelly_Bet_RL = if_else(Kelly_Criteria_RL < 0.1, 10, Kelly_Criteria_RL * 100),
         Kelly_Profit_RL = Units * Kelly_Bet_RL)

mean(test_RLpicks$RLCorrect)
sum(test_RLpicks$Units)*10
sum(test_RLpicks$Kelly_Profit_RL)
sum(test_RLpicks$Units)*10/nrow(test_RLpicks)

test_OUpicks <- filter(test, Kelly_Criteria_OU > 0 & Kelly_Criteria_OU <= 0.2 & OUPick == "under") %>%
  mutate(Units = if_else(OUCorrect == 1, 
                         if_else(Pick_OUOdds > 0, Pick_OUOdds / 100, 1),
                         if_else(Pick_OUOdds > 0, -1, Pick_OUOdds / 100)),
         Kelly_Bet_OU = if_else(Kelly_Criteria_OU < 0.1, 10, Kelly_Criteria_OU * 100),
         Kelly_Profit_OU = Units * Kelly_Bet_OU)

mean(test_OUpicks$OUCorrect)
sum(test_OUpicks$Units)*10
sum(test_OUpicks$Kelly_Profit_OU)
sum(test_OUpicks$Units)*10/nrow(test_OUpicks)

h2o.shutdown()





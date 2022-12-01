# Machine Performance

library("jsonlite")
library("rlist")
library("stringr")
library("tidyjson")
library("reshape2")
library("readxl")
library("rvest")
library("DataCombine")
library("plyr")
library("RDCOMClient")
library("xtable")
library("data.table")
library("lubridate")
library("esquisse")
library("blastula")
library(worldfootballR)
library("tidyverse")

setwd("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine")

# club_names <- read_excel("Soccer Machine/Club Names.xlsx")
# 
# mls_22 <- fb_match_results(country = "USA", gender = "M", season_end_year = 2022, tier = "1st") %>% 
#   select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away, Competition_Name, Season_End_Year) %>% 
#   rename(xG = Home_xG,
#          Home_Score = HomeGoals,
#          Away_Score = AwayGoals,
#          xG.1 = Away_xG,
#          League = Competition_Name,
#          Season = Season_End_Year) %>% 
#   filter(Day != "") %>% 
#   mutate(xG = as.numeric(xG),
#          Home_Score = as.numeric(Home_Score),
#          Away_Score = as.numeric(Away_Score),
#          xG.1 = as.numeric(xG.1),
#          League = "MLS",
#          Season = as.character(Season))
# 
# Big5 <- fb_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = c(2022, 2023), tier = "1st") %>% 
#   select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away, Competition_Name, Season_End_Year) %>% 
#   rename(xG = Home_xG,
#          Home_Score = HomeGoals,
#          Away_Score = AwayGoals,
#          xG.1 = Away_xG,
#          League = Competition_Name,
#          Season = Season_End_Year) %>%
#   mutate(xG = as.numeric(xG),
#          Home_Score = as.numeric(Home_Score),
#          Away_Score = as.numeric(Away_Score),
#          xG.1 = as.numeric(xG.1),
#          League = case_when(League == "Premier League" ~ "EPL",
#                             League == "Fußball-Bundesliga" ~ "Bundesliga",
#                             TRUE ~ League),
#          Season = paste0(Season-1,"-",Season))
# 
# fixtures <- rbind(Big5, mls_22)
# 
# # fixtures$Date <- as.Date(fixtures$Date)
# today <- Sys.Date()
# 
# # fixtures$xG <- as.numeric(fixtures$xG)
# # fixtures$Home_Score <- as.numeric(fixtures$Home_Score) 
# # fixtures$Away_Score <- as.numeric(fixtures$Away_Score)
# # fixtures$xG.1 <- as.numeric(fixtures$xG.1)
# 
# fixtures <- FindReplace(fixtures, Var = "Home", replaceData = club_names,
#                         from = "FBRef", to = "Name")
# fixtures <- FindReplace(fixtures, Var = "Away", replaceData = club_names,
#                         from = "FBRef", to = "Name")
# 
# scores <- filter(fixtures, !is.na(Away_Score))
# 
# history <- readRDS("Soccer Machine/PicksHistory.rds")
# 
# history <- inner_join(history, scores, by = c("gamedate" = "Date", "HomeTeam" = "Home",
#                                               "AwayTeam" = "Away", "Day" = "Day",
#                                               "Time" = "Time", "League" = "League")) %>%
#   mutate(Total_Score = Home_Score + Away_Score)
# 
# history2 <- history %>%
#   rowwise() %>%
#   mutate(Winner = case_when(bet_type == "ML" ~ if_else(Away_Score > Home_Score,
#                                                        AwayTeam,
#                                                        if_else(Away_Score == Home_Score,
#                                                                "Draw",
#                                                                HomeTeam)),
#                             bet_type == "Draw No Bet" ~ if_else(Away_Score > Home_Score,
#                                                                 AwayTeam,
#                                                                 if_else(Away_Score == Home_Score,
#                                                                         "Push",
#                                                                         HomeTeam)),
#                             grepl("Spread", bet_type) ~ if_else(Pick == paste0(AwayTeam),
#                                                                 if_else(Away_Score + Pick_SpreadTotal > Home_Score,
#                                                                         AwayTeam,
#                                                                         if_else(Away_Score + Pick_SpreadTotal == Home_Score,
#                                                                                 "Push",
#                                                                                 HomeTeam)),
#                                                                 if_else(Home_Score + Pick_SpreadTotal > Away_Score,
#                                                                         HomeTeam,
#                                                                         if_else(Home_Score + Pick_SpreadTotal == Away_Score,
#                                                                                 "Push",
#                                                                                 AwayTeam))),
#                             grepl("Total", bet_type) ~ if_else(Total_Score > Pick_SpreadTotal,
#                                                                "Over",
#                                                                if_else(Total_Score == Pick_SpreadTotal,
#                                                                        "Push",
#                                                                        "Under")),
#                             bet_type == "BTTS" ~ if_else(Away_Score > 0 & Home_Score > 0,
#                                                          "Yes", "No"),
#                             grepl("TT", bet_type) ~ if_else(grepl(paste0(AwayTeam), bet_type_full),
#                                                             if_else(Away_Score > Pick_SpreadTotal,
#                                                                     "Over",
#                                                                     if_else(Away_Score == Pick_SpreadTotal,
#                                                                             "Push",
#                                                                             "Under")),
#                                                             if_else(grepl(paste0(HomeTeam), bet_type_full),
#                                                                     if_else(Home_Score > Pick_SpreadTotal,
#                                                                             "Over",
#                                                                             if_else(Home_Score == Pick_SpreadTotal,
#                                                                                     "Push",
#                                                                                     "Under")),
#                                                                     "NA"))),
#          Pick_Correct = if_else(Winner == Pick, 1, 0),
#          Units = if_else(Pick_Correct == 1, Fract_Odds, -1),
#          # Units = if_else(Pick_Correct == 1,
#          #                 if_else(Pick_Odds > 0, Pick_Odds / 100, 1),
#          #                 if_else(Pick_Odds > 0, -1, Pick_Odds / 100)),
#          Kelly_Bet = Kelly_Criteria * 100,
#          # Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
#          Half_Kelly_Bet = Kelly_Criteria * 50,
#          # Half_Kelly_Bet = if_else(Kelly_Criteria < 0.1, 5, Kelly_Criteria * 50),
#          Kelly_Profit = Units * Kelly_Bet,
#          Half_Kelly_Profit = Units * Half_Kelly_Bet) %>%
#   arrange(desc(run_timestamp)) %>%
#   group_by(ID, bet_type_full) %>%
#   mutate(partition = row_number())
# 
# types <- filter(history2,
#                 Winner != "Push" &
#                   Kelly_Criteria > 0 &
#                   Pick_Odds >= -250 &
#                   Pick_WinProb >= 0.3 &
#                   bet_type_full != 'Alternate Total - 1.5' &
#                   partition == 2) %>%
#   mutate(SGP_eligible = case_when(bet_type %in% c('Spread', 'Alt Spread') ~ case_when(HOY.SpreadTotal %in% c(0, 0.5, -0.5) ~ 'Y',
#                                                                                       TRUE ~ 'N'),
#                                   TRUE ~ 'Y')) %>% 
#   select(gamedate, League, bet_type, Pick_Odds, Pick_WinProb, Pick_LoseProb, Fract_Odds,
#          Kelly_Criteria, EV, KC_tier, Pick_Correct, Units, SGP_eligible) %>%
#   mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
#          Kelly_Profit = Units * Kelly_Bet,
#          WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
#          Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
#          EV_tier = round_any(EV, 1, floor),
#          bets = as.integer(1),
#          Total = "Total",
#          Side_or_Total = case_when(bet_type %in% c('Alt Spread', 'Draw No Bet', 'ML', 'Spread') ~ "Side",
#                                    TRUE ~ "Total"))

types <- readRDS("Soccer Machine/PicksHistory_Outcomes.rds") %>%
  filter(Winner != "Push" &
           Kelly_Criteria > 0 &
           Pick_Odds >= -250 &
           Pick_WinProb >= 0.3 &
           bet_type_full != 'Alternate Total - 1.5' &
           partition == 2) %>%
  mutate(SGP_eligible = case_when(bet_type %in% c('Spread', 'Alt Spread') ~ case_when(HOY.SpreadTotal %in% c(0, 0.5, -0.5) ~ 'Y',
                                                                                      TRUE ~ 'N'),
                                  TRUE ~ 'Y'),
         bet_type = case_when(bet_type_full == 'Alternate Spread - Home: 0' |
                                (bet_type_full == 'Goal Spread' & Pick_SpreadTotal == 0) ~ 'Draw No Bet',
                              TRUE ~ bet_type)) %>% 
  select(gamedate, League, bet_type, Pick_Odds, Pick_WinProb, Pick_LoseProb, Fract_Odds,
         Kelly_Criteria, EV, KC_tier, Pick_Correct, Units, SGP_eligible) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
         Odds_tier = round_any(Pick_Odds, 10, floor),
         EV_tier = round_any(EV, 1, floor),
         bets = as.integer(1),
         Total = "Total",
         Side_or_Total = case_when(bet_type %in% c('Alt Spread', 'Draw No Bet', 'ML', 'Spread') ~ "Side",
                                   TRUE ~ "Total")) %>% 
  mutate(`Bet Grade` = case_when(Side_or_Total == 'Side' ~ case_when(KC_tier %in% c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65,
                                                             0.7, 0.75, 0.8, 0.85, 0.9, 0.95) ~ 'A+',
                                                             KC_tier %in% c(0.3, 0.35) ~ 'A',
                                                             KC_tier %in% c(0.2, 0.25) ~ case_when(as.integer(EV_tier) >= 2 ~ 'B',
                                                                                                   TRUE ~ 'C'),
                                                             as.integer(EV_tier) >= 2 & KC_tier %in% c(0.15) ~ 'C',
                                                             KC_tier %in% c(0.15) ~ 'D',
                                                             TRUE ~ 'F'),
                                 TRUE ~ case_when(KC_tier %in% c(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6,
                                                                 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95) ~ 'A+',
                                                  KC_tier %in% c(0.2, 0.25) ~ 'A',
                                                  KC_tier %in% c(0.1, 0.15) ~ 'B',
                                                  KC_tier %in% c(0.05) ~ 'C',
                                                  KC_tier %in% c(0) ~ 'D',
                                                  TRUE ~ 'F')),
         `Graded Risk` = case_when(`Bet Grade` == 'A+' ~ 2,
                                   `Bet Grade` == 'A' ~ 1.5,
                                   `Bet Grade` == 'B' ~ 1,
                                   `Bet Grade` == 'C' ~ 0.5,
                                   TRUE ~ 0),
         `Graded Profit` = Units*`Graded Risk`,
         `Bet Grade` = factor(`Bet Grade`, levels = c('A+', 'A', 'B', 'C', 'D', 'F')))

#### Tiered Grades ####

grades <- types %>% 
  # filter(Pick_Odds > -200) %>%
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>%
  group_by(ID) %>%
  mutate(KC_Rank = row_number()) %>%
  arrange(gamedate, ID, desc(EV)) %>%
  group_by(ID) %>%
  mutate(EV_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>%
  arrange(gamedate, ID, Rank) %>%
  mutate(Final_Rank = row_number()) %>%
  filter(Final_Rank == 1) 

grades %>%
  # filter(gamedate >= as.Date('2022-08-01')) %>% 
  group_by(`Bet Grade`,
           # Side_or_Total,
           # Odds = case_when(Pick_Odds > 0 ~ "Positive",
           #                  TRUE ~ "Negative"),
           `Suggested Wager` = case_when(`Bet Grade` == 'A+' ~ '2 units',
                                         `Bet Grade` == 'A' ~ '1.5 unit',
                                         `Bet Grade` == 'B' ~ '1 unit',
                                         `Bet Grade` == 'C' ~ '0.5 units',
                                         TRUE ~ 'No bet')) %>% 
  dplyr::summarise(`Hit Rate` = mean(Pick_Correct),
                   Bets = sum(bets),
                   `Profit: 1 Unit Wagers` = sum(Units),
                   `Profit: Suggested Wagers` = sum(`Graded Profit`)) %>%
  mutate(ROI = case_when(`Bet Grade` %in% c('D+', 'F+') ~ 0,
                         TRUE ~ `Profit: 1 Unit Wagers` / Bets)) %>% 
  mutate(`Hit Rate` = paste0(round_any(`Hit Rate`*100, 1), '%'),
         Bets = formatC(Bets, format="d", big.mark=","),
         `Profit: 1 Unit Wagers` = paste0(round_any(`Profit: 1 Unit Wagers`, 0.1), ' units'),
         `Profit: Suggested Wagers` = paste0(round_any(`Profit: Suggested Wagers`, 0.1), ' units'),
         ROI = paste0(round_any(ROI*100, 0.01), '%')) %>% 
  print(n=40)

grades %>%
  filter(`Bet Grade` %in% c('D')) %>% 
  # group_by(League) %>%
  group_by(Odds_tier = round_any(Pick_Odds, 20, floor)) %>%
  dplyr::summarise(`Hit Rate` = mean(Pick_Correct),
                   Bets = sum(bets),
                   `Profit: 1 Unit Wagers` = sum(Units),
                   `Profit: Suggested Wagers` = sum(`Graded Profit`)) %>%
  mutate(ROI = `Profit: 1 Unit Wagers` / Bets) %>% 
  mutate(`Hit Rate` = paste0(round_any(`Hit Rate`*100, 1), '%'),
         Bets = formatC(Bets, format="d", big.mark=","),
         `Profit: 1 Unit Wagers` = paste0(round_any(`Profit: 1 Unit Wagers`, 0.1), ' units'),
         `Profit: Suggested Wagers` = paste0(round_any(`Profit: Suggested Wagers`, 0.1), ' units'),
         ROI = paste0(round_any(ROI*100, 0.01), '%')) %>% 
  print(n=40)

grades %>% 
  filter((str_detect(bet_type, 'Total') | str_detect(bet_type, 'TT') | bet_type == 'BTTS')) %>% 
  #group_by(bet_type) %>%
  group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  # group_by(EV_tier = as.integer(EV_tier)) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

plot_data <- grades %>% 
  filter(`Bet Grade` %in% c('A+', 'A', 'B')) %>% 
  select(gamedate, Units, `Graded Profit`) %>% 
  rename(`Profit: 1 Unit Wagers` = Units,
         `Profit: Suggested Wagers` = `Graded Profit`) %>% 
  melt("gamedate", c("Profit: 1 Unit Wagers", "Profit: Suggested Wagers")) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))
  

ggplot(plot_data) +
  aes(x = gamedate, y = cumulative_value, group = variable) +
  geom_line() +
  theme_minimal()

#### Current Email tables ####

email_table_1 <- types %>%
  filter(Kelly_Criteria >= 0) %>%
  #group_by(Total) %>%
  group_by(bet_type) %>%
  #group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_1 <- print(xtable(email_table_1), type = "html", print.results = FALSE)

email_table_2 <- types %>%
  filter(Kelly_Criteria >= 0) %>%
  group_by(League) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_2 <- print(xtable(email_table_2), type = "html", print.results = FALSE)

email_table_3a <- types %>%
  filter(Kelly_Criteria >= 0.2 & EV >= 2) %>%
  #group_by(Total) %>%
  group_by(Strategy = 'Bet Everything') %>% 
  #group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

email_table_3b <- types %>% 
  filter(Kelly_Criteria >= 0.2 & EV >= 2) %>%
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID) %>% 
  mutate(EV_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, ID, Rank) %>% 
  mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1) %>%
  #group_by(Total) %>%
  group_by(Strategy = 'One Bet Per Game') %>%
  #group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

email_table_3c <- types %>% 
  filter(Kelly_Criteria >= 0.2 & EV >= 2 & Pick_Odds > 0) %>%
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID) %>% 
  mutate(EV_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, ID, Rank) %>% 
  mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1) %>%
  #group_by(Total) %>%
  group_by(Strategy = 'One Bet Per Game at Even Odds or Better') %>%
  #group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

email_table_3d <- types %>%
  filter(Kelly_Criteria >= 0.2 & EV >= 2 & Pick_Odds > 0) %>%
  #group_by(Total) %>%
  group_by(Strategy = 'Bet Everything at Even Odds or Better') %>% 
  #group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

SGPs <- types %>%
  filter((Pick_Odds < 0 | (Side_or_Total == 'Side' & Pick_Odds <= 140)) &
           Kelly_Criteria >= 0.2 & EV >= 2 &
           SGP_eligible == 'Y'
  ) %>%
  mutate(Pushable = case_when(Pick_WinProb + Pick_LoseProb < 0.999 ~ 'Y',
                              TRUE ~ 'N')) %>% 
  filter(Pushable == 'N') %>% 
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID, Side_or_Total) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID, Side_or_Total) %>% 
  mutate(EV_Rank = row_number()) %>%
  arrange(gamedate, ID, desc(Pick_WinProb)) %>% 
  group_by(ID, Side_or_Total) %>% 
  mutate(WinProb_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>%
  arrange(gamedate, ID, Side_or_Total, Rank) %>% 
  mutate(Final_Rank = row_number()) %>%  
  filter(Final_Rank == 1) %>% 
  group_by(ID) %>% 
  mutate(legs = sum(bets),
         hits = sum(Pick_Correct)) %>% 
  filter(legs > 1) %>%
  mutate(Parlay_Odds = floor((prod(Fract_Odds+1)-1)*100),
         winner = if_else(legs==hits,1,0),
         Parlay_Units = if_else(winner==1,Parlay_Odds/100,-1),
         Parlay_WinProb = prod(Pick_WinProb),
         Parlay_Fract_Odds = (100 / abs(Parlay_Odds))^if_else(Parlay_Odds < 0, 1, -1),
         Parlay_KC = (Parlay_WinProb * (Parlay_Fract_Odds + 1) - 1) / Parlay_Fract_Odds,
         Parlay_Kelly_Profit = (Parlay_KC*100*Parlay_Units)/2) %>% 
  filter(Parlay_Odds >= 100)

SGP_performance <- SGPs %>% 
  distinct(ID, gamedate, Parlay_Odds, winner, Parlay_Units, Parlay_Kelly_Profit) %>% 
  mutate(bets = 1)

email_SGP_table <- SGP_performance %>%
  #group_by(Total) %>%
  group_by(Strategy = 'Same-Game Parlays') %>% 
  #group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(winner),
                   bets = sum(bets),
                   Flat_Profit = sum(Parlay_Units),
                   Kelly_Profit = sum(Parlay_Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

Parlays <- types %>%
  filter(Pick_Odds < 0 & Kelly_Criteria >= 0.2 & EV >= 2) %>%
  mutate(Pushable = case_when(Pick_WinProb + Pick_LoseProb < 0.999 ~ 'Y',
                              TRUE ~ 'N')) %>% 
  filter(Pushable == 'N') %>% 
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID) %>% 
  mutate(EV_Rank = row_number()) %>%
  arrange(gamedate, ID, desc(Pick_WinProb)) %>% 
  group_by(ID) %>% 
  mutate(WinProb_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>%
  arrange(gamedate, ID, Rank) %>% 
  mutate(Final_Rank = row_number()) %>%  
  filter(Final_Rank == 1) %>%
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(gamedate) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(gamedate) %>% 
  mutate(EV_Rank = row_number()) %>%
  arrange(gamedate, ID, desc(Pick_WinProb)) %>% 
  group_by(gamedate) %>% 
  mutate(WinProb_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>%
  arrange(gamedate, ID, Side_or_Total, Rank) %>% 
  mutate(Final_Rank = row_number()) %>%  
  filter(Final_Rank <= 4) %>%
  group_by(gamedate) %>% 
  mutate(legs = sum(bets),
         hits = sum(Pick_Correct)) %>% 
  filter(legs > 1) %>%
  mutate(Parlay_Odds = floor((prod(Fract_Odds+1)-1)*100),
         winner = if_else(legs==hits,1,0),
         Parlay_Units = if_else(winner==1,Parlay_Odds/100,-1),
         Parlay_WinProb = prod(Pick_WinProb),
         Parlay_Fract_Odds = (100 / abs(Parlay_Odds))^if_else(Parlay_Odds < 0, 1, -1),
         Parlay_KC = (Parlay_WinProb * (Parlay_Fract_Odds + 1) - 1) / Parlay_Fract_Odds,
         Parlay_Kelly_Profit = (Parlay_KC*100*Parlay_Units)/2) %>% 
  filter(Parlay_Odds >= 100)

Parlay_performance <- Parlays %>% 
  group_by(gamedate) %>% 
  mutate(row_num = row_number()) %>% 
  filter(row_num == 1) %>% 
  select(ID, gamedate, Parlay_Odds, winner, Parlay_Units, Parlay_Kelly_Profit, legs) %>% 
  mutate(bets = 1)

email_Parlay_table <- Parlay_performance %>%
  #group_by(Total) %>%
  group_by(Strategy = 'Multi-Game Parlays (2-4 Legs/Day)') %>% 
  #group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(winner),
                   bets = sum(bets),
                   Flat_Profit = sum(Parlay_Units),
                   Kelly_Profit = sum(Parlay_Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

email_table_3 <- bind_rows(email_table_3a, email_table_3d, email_table_3b, email_table_3c, email_SGP_table, email_Parlay_table) %>% 
  mutate(bets = as.integer(bets))

df_html_3 <- print(xtable(email_table_3), type = "html", print.results = FALSE)

email_table_4 <- types %>% 
  #group_by(bet_type) %>%
  group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_4 <- print(xtable(email_table_4), type = "html", print.results = FALSE)

email_table_5 <- types %>%
  #group_by(Total) %>%
  #group_by(bet_type) %>%
  #group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(Odds_tier) %>%
  group_by(EV_tier = as.integer(EV_tier)) %>% 
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_5 <- print(xtable(email_table_5), type = "html", print.results = FALSE)

graph_data1 <- types %>%
  ungroup() %>%
  filter(Kelly_Criteria >= 0.2 & EV >= 2) %>%
  select(gamedate, Units, Kelly_Profit) %>%
  rename(`Flat Profit: Bet Everything` = Units,
         `Bet Everything` = Kelly_Profit) %>% 
  melt("gamedate", c("Flat Profit: Bet Everything", "Bet Everything")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

graph_data2 <- types %>%
  ungroup() %>%
  filter(Pick_Odds >= 100 & Kelly_Criteria >= 0.2 & EV >= 2) %>% 
  select(gamedate, Units, Kelly_Profit) %>%
  rename(`Flat Profit: Bet Everything Positive` = Units,
         `Bet Everything Positive` = Kelly_Profit) %>% 
  melt("gamedate", c("Flat Profit: Bet Everything Positive", "Bet Everything Positive")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

graph_data3 <- types %>%
  filter(Kelly_Criteria >= 0.2 & EV >= 2) %>%
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID) %>% 
  mutate(EV_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, ID, Rank) %>% 
  mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1) %>%
  ungroup() %>% 
  select(gamedate, Units, Kelly_Profit) %>%
  rename(`Flat Profit: One per Game` = Units,
         `One per Game` = Kelly_Profit) %>% 
  melt("gamedate", c("Flat Profit: One per Game", "One per Game")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

graph_data4 <- types %>%
  filter(Pick_Odds >= 100 & Kelly_Criteria >= 0.2 & EV >= 2) %>%
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID) %>% 
  mutate(EV_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, ID, Rank) %>% 
  mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1) %>%
  ungroup() %>% 
  select(gamedate, Units, Kelly_Profit) %>%
  rename(`Flat Profit: One per Game Positive` = Units,
         `One per Game Positive` = Kelly_Profit) %>% 
  melt("gamedate", c("Flat Profit: One per Game Positive", "One per Game Positive")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

graph_data5 <- SGP_performance %>%
  select(gamedate, Parlay_Units, Parlay_Kelly_Profit) %>%
  rename(`Flat Profit: Same-Game Parlays` = Parlay_Units,
         `Same-Game Parlays` = Parlay_Kelly_Profit) %>% 
  melt("gamedate", c("Flat Profit: Same-Game Parlays", "Same-Game Parlays")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

graph_data6 <- Parlay_performance %>%
  select(gamedate, Parlay_Units, Parlay_Kelly_Profit) %>%
  rename(`Flat Profit: Multi-Game Parlays` = Parlay_Units,
         `Multi-Game Parlays` = Parlay_Kelly_Profit) %>% 
  melt("gamedate", c("Flat Profit: Multi-Game Parlays", "Multi-Game Parlays")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

graph_data <- bind_rows(graph_data1, graph_data2, graph_data3, graph_data4, graph_data5, graph_data6) %>% 
  filter(!str_detect(variable, 'Flat'))

plot <- ggplot(graph_data) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Game Date",
    y = "Cumulative Profit",
    color = ""
  ) +
  theme_minimal()

plot_html <- add_ggplot(plot_object = plot)
  
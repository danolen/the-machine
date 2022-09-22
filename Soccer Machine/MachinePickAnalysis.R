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

club_names <- read_excel("Soccer Machine/Club Names.xlsx")

mls_22 <- load_match_results(country = "USA", gender = "M", season_end_year = 2022, tier = "1st") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away, Competition_Name, Season_End_Year) %>% 
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

Big5 <- load_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = c(2022, 2023), tier = "1st") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away, Competition_Name, Season_End_Year) %>% 
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

fixtures <- rbind(Big5, mls_22)

# fixtures$Date <- as.Date(fixtures$Date)
today <- Sys.Date()

# fixtures$xG <- as.numeric(fixtures$xG)
# fixtures$Home_Score <- as.numeric(fixtures$Home_Score) 
# fixtures$Away_Score <- as.numeric(fixtures$Away_Score)
# fixtures$xG.1 <- as.numeric(fixtures$xG.1)

fixtures <- FindReplace(fixtures, Var = "Home", replaceData = club_names,
                        from = "FBRef", to = "Name")
fixtures <- FindReplace(fixtures, Var = "Away", replaceData = club_names,
                        from = "FBRef", to = "Name")

scores <- filter(fixtures, !is.na(Away_Score))

history <- readRDS("Soccer Machine/PicksHistory.rds")

history <- inner_join(history, scores, by = c("gamedate" = "Date", "HomeTeam" = "Home",
                                              "AwayTeam" = "Away", "Day" = "Day",
                                              "Time" = "Time", "League" = "League")) %>%
  mutate(Total_Score = Home_Score + Away_Score)

history2 <- history %>%
  rowwise() %>%
  mutate(Winner = case_when(bet_type == "ML" ~ if_else(Away_Score > Home_Score,
                                                       AwayTeam,
                                                       if_else(Away_Score == Home_Score,
                                                               "Draw",
                                                               HomeTeam)),
                            bet_type == "Draw No Bet" ~ if_else(Away_Score > Home_Score,
                                                                AwayTeam,
                                                                if_else(Away_Score == Home_Score,
                                                                        "Push",
                                                                        HomeTeam)),
                            grepl("Spread", bet_type) ~ if_else(Pick == paste0(AwayTeam),
                                                                if_else(Away_Score + Pick_SpreadTotal > Home_Score,
                                                                        AwayTeam,
                                                                        if_else(Away_Score + Pick_SpreadTotal == Home_Score,
                                                                                "Push",
                                                                                HomeTeam)),
                                                                if_else(Home_Score + Pick_SpreadTotal > Away_Score,
                                                                        HomeTeam,
                                                                        if_else(Home_Score + Pick_SpreadTotal == Away_Score,
                                                                                "Push",
                                                                                AwayTeam))),
                            grepl("Total", bet_type) ~ if_else(Total_Score > Pick_SpreadTotal,
                                                               "Over",
                                                               if_else(Total_Score == Pick_SpreadTotal,
                                                                       "Push",
                                                                       "Under")),
                            bet_type == "BTTS" ~ if_else(Away_Score > 0 & Home_Score > 0,
                                                         "Yes", "No"),
                            grepl("TT", bet_type) ~ if_else(grepl(paste0(AwayTeam), bet_type_full),
                                                            if_else(Away_Score > Pick_SpreadTotal,
                                                                    "Over",
                                                                    if_else(Away_Score == Pick_SpreadTotal,
                                                                            "Push",
                                                                            "Under")),
                                                            if_else(grepl(paste0(HomeTeam), bet_type_full),
                                                                    if_else(Home_Score > Pick_SpreadTotal,
                                                                            "Over",
                                                                            if_else(Home_Score == Pick_SpreadTotal,
                                                                                    "Push",
                                                                                    "Under")),
                                                                    "NA"))),
         Pick_Correct = if_else(Winner == Pick, 1, 0),
         Units = if_else(Pick_Correct == 1, Fract_Odds, -1),
         # Units = if_else(Pick_Correct == 1,
         #                 if_else(Pick_Odds > 0, Pick_Odds / 100, 1),
         #                 if_else(Pick_Odds > 0, -1, Pick_Odds / 100)),
         Kelly_Bet = Kelly_Criteria * 100,
         # Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Half_Kelly_Bet = Kelly_Criteria * 50,
         # Half_Kelly_Bet = if_else(Kelly_Criteria < 0.1, 5, Kelly_Criteria * 50),
         Kelly_Profit = Units * Kelly_Bet,
         Half_Kelly_Profit = Units * Half_Kelly_Bet) %>%
  arrange(desc(run_timestamp)) %>%
  group_by(ID, bet_type_full) %>%
  mutate(partition = row_number())

types <- filter(history2,
                Winner != "Push" &
                  Kelly_Criteria > 0 &
                  Pick_Odds >= -250 &
                  Pick_WinProb >= 0.3 &
                  bet_type_full != 'Alternate Total - 1.5' &
                  partition == 2) %>%
  mutate(SGP_eligible = case_when(bet_type %in% c('Spread', 'Alt Spread') ~ case_when(HOY.SpreadTotal %in% c(0, 0.5, -0.5) ~ 'Y',
                                                                                      TRUE ~ 'N'),
                                  TRUE ~ 'Y')) %>% 
  select(gamedate, League, bet_type, Pick_Odds, Pick_WinProb, Pick_LoseProb, Fract_Odds,
         Kelly_Criteria, EV, KC_tier, Pick_Correct, Units, SGP_eligible) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         EV_tier = round_any(EV, 1, floor),
         bets = as.integer(1),
         Total = "Total",
         Side_or_Total = case_when(bet_type %in% c('Alt Spread', 'Draw No Bet', 'ML', 'Spread') ~ "Side",
                                   TRUE ~ "Total"))
types %>%
  filter(Kelly_Criteria >= 0 &
          # Kelly_Criteria >= 0.15 & 
          # Kelly_Criteria < 0.4 & 
          # EV >= 3 & 
          # EV < 7 &
          !(League %in% c("UCL", "UEL"))) %>%
  #group_by(Total) %>%
  group_by(bet_type) %>%
  #group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(Odds_tier = as.numeric(as.character(Odds_tier))) %>%
  #group_by(WinProb_tier = as.numeric(as.character(WinProb_tier))) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=100)

types %>%
  filter(Kelly_Criteria >= 0) %>%
  group_by(League) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=100)

types %>% 
  filter(Kelly_Criteria >= 0.15 & 
          Kelly_Criteria < 0.4 & 
          EV >= 3 & 
          EV < 7 & 
          Pick_Odds > 0 &
          !(League %in% c("UCL", "UEL"))) %>%
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
  group_by(Total) %>%
  #group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter(!(League %in% c("UCL", "UEL"))) %>%
  #filter(League == 'MLS' &) %>% 
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

types %>%
  filter(!(League %in% c("UCL", "UEL"))) %>%
  #filter(League == 'Serie A') %>% 
  #group_by(Total) %>%
  #group_by(bet_type) %>%
  #group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(Odds_tier) %>%
  group_by(EV_tier) %>% 
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter((bet_type == "BTTS"
          # | bet_type == "Alt Total"
          # | bet_type == "Draw No Bet"
          )
         #& League.x == "EPL"
         ) %>%
  group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

SGPs <- types %>%
  # filter(League == 'MLS') %>% 
  filter(Pick_Odds < 0 &
           Kelly_Criteria >= 0.15 &
           Kelly_Criteria < 0.4 &
           EV >= 2 &
           EV < 7 &
           !(League %in% c("UCL", "UEL"))) %>%
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

graph_data1 <- types %>%
  ungroup() %>%
  filter(Kelly_Criteria >= 0.15 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 7 & !(League %in% c("UCL", "UEL"))) %>%
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
  filter(Pick_Odds >= 100 & Kelly_Criteria >= 0.15 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 7 & !(League %in% c("UCL", "UEL"))) %>% 
  select(gamedate, Units, Kelly_Profit) %>%
  rename(`Flat Profit: Bet Everything Positive` = Units,
         `Bet Everything Positive` = Kelly_Profit) %>% 
  melt("gamedate", c("Flat Profit: Bet Everything Positive", "Bet Everything Positive")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

graph_data3 <- types %>%
  filter(Kelly_Criteria >= 0.15 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 7 & !(League %in% c("UCL", "UEL"))) %>%
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
  filter(Pick_Odds >= 100 & Kelly_Criteria >= 0.15 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 7 & !(League %in% c("UCL", "UEL"))) %>%
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

bets_table <- read.csv("Soccer Machine/upcoming_bets.csv") %>% 
  mutate(gamedate = as.Date(gamedate)) %>%
  filter(Kelly_Criteria >= 0.15 & 
           Kelly_Criteria < 0.4 & 
           EV >= 3 & 
           EV < 7 & 
           !(League %in% c("UCL", "UEL")) &
           Pick_Odds > 0 &
           Pick_WinProb >= 0.3 &
           bet_type_full != 'Alternate Total - 1.5' &
           gamedate <= Sys.Date() + 7) %>%
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
  mutate(bet_size = case_when(KC_tier %in% c(0.1, 0.15) ~ '$5',
                              KC_tier %in% c(0.2, 0.25) ~ '$10',
                              KC_tier %in% c(0.3, 0.35) ~ '$15',
                              TRUE ~ 'No bet - something went wrong')) %>%
  arrange(gamedate, desc(Kelly_Criteria)) %>% 
  ungroup() %>% 
  mutate(Pick = case_when(is.na(Pick_SpreadTotal) | Pick_SpreadTotal == 0 ~ paste0(Pick),
                          str_detect(bet_type_full, "Spread") & Pick_SpreadTotal > 0 ~ paste0(Pick, " +", Pick_SpreadTotal),
                          TRUE ~ paste0(Pick, " ", Pick_SpreadTotal))) %>% 
  select(gamedate, League, HomeTeam, AwayTeam, bet_type_full, Pick, Pick_Odds, Machine_Odds, bet_size) %>% 
  mutate(Machine_Odds =  as.integer(pmax(Machine_Odds, 100)),
         gamedate = as.character(gamedate)) %>% 
  rename(`Game Date` = gamedate,
         `Home Team` = HomeTeam,
         `Away Team` = AwayTeam,
         `Bet` = bet_type_full,
         `Current Pick Odds` = Pick_Odds,
         `Don't Bet if Odds Worse Than` = Machine_Odds,
         `Wager Amount` = bet_size)

bets_SGP <- read.csv("Soccer Machine/upcoming_bets.csv") %>%
  # history %>% 
  # filter(gamedate > as.Date("2022-06-17")) %>%
  # arrange(desc(run_timestamp)) %>%
  # group_by(ID, bet_type_full) %>%
  # mutate(partition = row_number()) %>% 
  # filter(partition == 2) %>% 
  mutate(Side_or_Total = case_when(bet_type %in% c('Alt Spread', 'Draw No Bet', 'ML', 'Spread') ~ "Side",
                                   TRUE ~ "Total")) %>% 
  mutate(gamedate = as.Date(gamedate)) %>%
  filter(Pick_Odds < 0 &
           Kelly_Criteria >= 0.15 &
           Kelly_Criteria < 0.4 &
           EV >= 2 &
           EV < 7 &
           !(League %in% c("UCL", "UEL")) &
           bet_type_full != 'Alternate Total - 1.5') %>% 
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
  mutate(bets = 1,
         legs = sum(bets)) %>% 
  filter(legs > 1)

bets_SGP2 <- bets_SGP %>%
  mutate(Pick = case_when(is.na(Pick_SpreadTotal) | Pick_SpreadTotal == 0 ~ paste0(Pick),
                          str_detect(bet_type_full, "Spread") & Pick_SpreadTotal > 0 ~ paste0(Pick, " +", Pick_SpreadTotal),
                          TRUE ~ paste0(Pick, " ", Pick_SpreadTotal))) %>% 
  select(ID, gamedate, League, HomeTeam, AwayTeam, bet_type_full, Pick, Pick_Odds, Machine_Odds, Fract_Odds, Pick_WinProb) %>% 
  group_by(ID) %>% 
  mutate(Parlay_Odds = floor((prod(Fract_Odds+1)-1)*100),
         Parlay_WinProb = prod(Pick_WinProb),
         Parlay_Fract_Odds = (100 / abs(Parlay_Odds))^if_else(Parlay_Odds < 0, 1, -1),
         Parlay_KC = (Parlay_WinProb * (Parlay_Fract_Odds + 1) - 1) / Parlay_Fract_Odds,
         Bet = paste0(unique(bet_type_full), collapse = " & "),
         Pick = paste0(unique(Pick), collapse = " & "),
         Parlay_Machine_Odds = as.integer(pmax(if_else(Parlay_WinProb < 0.5,
                                                       (100 / Parlay_WinProb) - 100,
                                                       -1 * (100 * Parlay_WinProb) / (1 - Parlay_WinProb)),
                                               100)),
         Parlay_KC_tier = as.factor(round_any(Parlay_KC, 0.05, floor)),
         bet_size = case_when(Parlay_KC_tier %in% c(0.1, 0.15) ~ '$5',
                              Parlay_KC_tier %in% c(0.2, 0.25) ~ '$10',
                              Parlay_KC_tier %in% c(0.3, 0.35) ~ '$15',
                              TRUE ~ 'No bet - something went wrong')) %>% 
  filter(Parlay_Odds >= 100) %>% 
  ungroup() %>% 
  select(gamedate, League, HomeTeam, AwayTeam, Bet, Pick, Parlay_Odds, Parlay_Machine_Odds, bet_size) %>% 
  distinct() %>% 
  rename(`Game Date` = gamedate,
         `Home Team` = HomeTeam,
         `Away Team` = AwayTeam,
         `Current Pick Odds` = Parlay_Odds,
         `Don't Bet if Odds Worse Than` = Parlay_Machine_Odds,
         `Wager Amount` = bet_size)

bets_table2 <- bind_rows(bets_table %>% mutate(`Game Date` = as.Date(`Game Date`)), bets_SGP2)

Parlays <- types %>%
  #filter(League == 'MLS') %>% 
  filter(Pick_Odds < 0 & Kelly_Criteria >= 0.15 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 7 & !(League %in% c("UCL", "UEL"))) %>%
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

Parlay_table <- Parlay_performance %>%
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
  
bets_Parlay <- read.csv("Soccer Machine/upcoming_bets.csv") %>%
  # history %>%
  # filter(gamedate > as.Date("2022-06-17")) %>%
  # arrange(desc(run_timestamp)) %>%
  # group_by(ID, bet_type_full) %>%
  # mutate(partition = row_number()) %>%
  # filter(partition == 2) %>%
  mutate(Side_or_Total = case_when(bet_type %in% c('Alt Spread', 'Draw No Bet', 'ML', 'Spread') ~ "Side",
                                   TRUE ~ "Total")) %>% 
  mutate(gamedate = as.Date(gamedate)) %>%
  filter(Pick_Odds < 0 &
           Kelly_Criteria >= 0.15 &
           Kelly_Criteria < 0.4 &
           EV >= 2 &
           EV < 7 &
           !(League %in% c("UCL", "UEL")) &
           bet_type_full != 'Alternate Total - 1.5') %>% 
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
  mutate(bets = 1,
         legs = sum(bets)) %>% 
  filter(legs > 1)

bets_Parlay2 <- bets_Parlay %>%
  mutate(Pick = case_when(is.na(Pick_SpreadTotal) | Pick_SpreadTotal == 0 ~ paste0(Pick),
                          str_detect(bet_type_full, "Spread") & Pick_SpreadTotal > 0 ~ paste0(Pick, " +", Pick_SpreadTotal),
                          TRUE ~ paste0(Pick, " ", Pick_SpreadTotal))) %>% 
  select(ID, gamedate, League, HomeTeam, AwayTeam, bet_type_full, Pick, Pick_Odds, Machine_Odds, Fract_Odds, Pick_WinProb) %>% 
  group_by(gamedate) %>% 
  mutate(Parlay_Odds = floor((prod(Fract_Odds+1)-1)*100),
         Parlay_WinProb = prod(Pick_WinProb),
         Parlay_Fract_Odds = (100 / abs(Parlay_Odds))^if_else(Parlay_Odds < 0, 1, -1),
         Parlay_KC = (Parlay_WinProb * (Parlay_Fract_Odds + 1) - 1) / Parlay_Fract_Odds,
         Bet = paste0(unique(bet_type_full), collapse = " & "),
         Pick = paste0(unique(Pick), collapse = " & "),
         Parlay_Machine_Odds = as.integer(pmax(if_else(Parlay_WinProb < 0.5,
                                                       (100 / Parlay_WinProb) - 100,
                                                       -1 * (100 * Parlay_WinProb) / (1 - Parlay_WinProb)),
                                               100)),
         Parlay_KC_tier = as.factor(round_any(Parlay_KC, 0.05, floor)),
         bet_size = case_when(Parlay_KC_tier %in% c(0.0, 0.05, 0.1, 0.15) ~ '$5',
                              Parlay_KC_tier %in% c(0.2, 0.25) ~ '$10',
                              Parlay_KC_tier %in% c(0.3, 0.35) ~ '$15',
                              TRUE ~ 'No bet - something went wrong')) %>% 
  filter(Parlay_Odds >= 100) %>% 
  ungroup() %>% 
  mutate(HomeTeam = "--",
         AwayTeam = "--") %>% 
  select(gamedate, League, HomeTeam, AwayTeam, Bet, Pick, Parlay_Odds, Parlay_Machine_Odds, bet_size) %>% 
  distinct() %>% 
  rename(`Game Date` = gamedate,
         `Home Team` = HomeTeam,
         `Away Team` = AwayTeam,
         `Current Pick Odds` = Parlay_Odds,
         `Don't Bet if Odds Worse Than` = Parlay_Machine_Odds,
         `Wager Amount` = bet_size)
  
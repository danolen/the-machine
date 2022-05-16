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
library("tidyverse")

setwd("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine")

club_names <- read_excel("Soccer Machine/Club Names.xlsx")

urls <- c("https://fbref.com/en/comps/22/schedule/Major-League-Soccer-Scores-and-Fixtures",
          "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/13/schedule/Ligue-1-Scores-and-Fixtures",
          "https://fbref.com/en/comps/20/schedule/Bundesliga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/11/schedule/Serie-A-Scores-and-Fixtures",
          "https://fbref.com/en/comps/12/schedule/La-Liga-Scores-and-Fixtures",
          "https://fbref.com/en/comps/8/schedule/Champions-League-Scores-and-Fixtures",
          "https://fbref.com/en/comps/19/schedule/Europa-League-Scores-and-Fixtures")

mls_22 <- urls[[1]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "Day") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "MLS", Season = "2022")

epl_21_22 <- urls[[2]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "EPL", Season = "2021-2022")

ligue1_21_22 <- urls[[3]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Ligue 1", Season = "2021-2022")

bundes_21_22 <- urls[[4]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Bundesliga", Season = "2021-2022")

seriea_21_22 <- urls[[5]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "Serie A", Season = "2021-2022")

laliga_21_22 <- urls[[6]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(is.na(Wk) == FALSE) %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "La Liga", Season = "2021-2022")

ucl_21_22 <- urls[[7]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "UCL", Season = "2021-2022")

ucl_21_22 <- mutate(ucl_21_22,
                    Home = if_else(endsWith(Home, "tr"), "Besiktas tr", Home),
                    Away = if_else(startsWith(Away, "tr"), "tr Besiktas", Away))

uel_21_22 <- urls[[8]] %>%
  read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  filter(Day != "" & Day != "Day") %>%
  select(Day:Away) %>%
  separate(Score, c("Home_Score", "Away_Score")) %>%
  mutate(League = "UEL", Season = "2021-2022")

fixtures <- rbind(epl_21_22, laliga_21_22, bundes_21_22, seriea_21_22, 
                  ligue1_21_22, mls_22, ucl_21_22, uel_21_22) 

fixtures$Date <- as.Date(fixtures$Date)
today <- Sys.Date()

fixtures$xG <- as.numeric(fixtures$xG)
fixtures$Home_Score <- as.numeric(fixtures$Home_Score) 
fixtures$Away_Score <- as.numeric(fixtures$Away_Score)
fixtures$xG.1 <- as.numeric(fixtures$xG.1)

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
  select(gamedate, League, bet_type, Pick_Odds, Pick_WinProb, Fract_Odds, Kelly_Criteria, EV, KC_tier, Pick_Correct, Units) %>% 
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         EV_tier = round_any(EV, 1, floor),
         bets = as.integer(1),
         Total = "Total",
         Side_or_Total = case_when(bet_type %in% c('Alt Spread', 'Draw No Bet', 'ML', 'Spread') ~ "Side",
                                   TRUE ~ Total))

types %>%
  filter(Kelly_Criteria >= 0 & !(League %in% c("UCL", "UEL"))) %>%
  #group_by(Total) %>%
  group_by(bet_type) %>%
  #group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(Odds_tier = as.numeric(as.character(Odds_tier))) %>%
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
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID, Side_or_Total) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID, Side_or_Total) %>% 
  mutate(EV_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, ID, Rank, desc(Kelly_Criteria)) %>% 
  mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1) %>% 
  filter(Kelly_Criteria >= 0.1 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 6 & !(League %in% c("UCL", "UEL"))) %>%
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
  #group_by(Total) %>%
  #group_by(bet_type) %>%
  #group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  #group_by(Odds_tier) %>%
  group_by(EV_tier) %>% 
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter(#(bet_type == "TT"
          #| bet_type == "Alt Total"
          #| bet_type == "Draw No Bet"
          bet_type_full == 'Alternate Total - 1.5'
          #)
         #& League.x == "EPL"
         ) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

graph_data <- types %>%
  ungroup() %>% 
  select(gamedate, Units, Kelly_Profit) %>%
  rename(Flat_Profit = Units) %>% 
  melt("gamedate", c("Flat_Profit", "Kelly_Profit")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

ggplot(graph_data) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Game Date",
    y = "Cumulative Profit",
    color = ""
  ) +
  theme_minimal()

graph_data2 <- types %>%
  ungroup() %>%
  filter(Pick_Odds >= 100) %>% 
  select(gamedate, Units, Kelly_Profit) %>%
  rename(Flat_Profit = Units) %>% 
  melt("gamedate", c("Flat_Profit", "Kelly_Profit")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

ggplot(graph_data2) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Game Date",
    y = "Cumulative Profit",
    color = ""
  ) +
  theme_minimal()



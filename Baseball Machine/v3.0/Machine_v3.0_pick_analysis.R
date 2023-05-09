library(tidyverse)
library(plyr)
library(reshape2)

bets3 <- read.csv("Baseball Machine/v3.0/upcoming_bets.csv")
history2 <- readRDS("Baseball Machine/v3.0/PicksHistory_Outcomes.rds")

types <- history2 %>%
  ungroup() %>% 
  filter(Kelly_Criteria > 0 &
           Pick_Odds >= -180 &
           Pick_WinProb >= 0.3 &
           partition == 1 &
           OddsType == 'Day Of' &
           !str_detect(bet_type, "RFI") &
           bet_type != "F5 Alt TT") %>% 
  mutate(Units = if_else(Pick_Correct == 1,
                    if_else(Pick_Odds > 0, Pick_Odds / 100, 1),
                    if_else(Pick_Odds > 0, -1, Pick_Odds / 100))) %>% 
  select(gamedate, AwayTeam, HomeTeam, bet_type, Pick_Odds, Pick_WinProb, Pick_LoseProb, Fract_Odds,
         Kelly_Criteria, EV, KC_tier, Winner, Pick_Correct, Units, Kelly_Bet, Kelly_Profit, run_timestamp) %>% 
  dplyr::mutate(WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
                Odds_tier = round_any(Pick_Odds, 10, floor),
                EV_tier = round_any(EV, 1, floor),
                bets = as.integer(1),
                EV_Grade = case_when(EV_tier >= 3 ~ 3,
                                     EV_tier == 2 ~ 2,
                                     EV_tier == 1 ~ 1,
                                     EV_tier < 1 ~ 0),
                KC_Grade = case_when(as.numeric(as.character(KC_tier)) >= 0.35 ~ 4,
                                     as.numeric(as.character(KC_tier)) >= 0.3 ~ 3,
                                     as.numeric(as.character(KC_tier)) >= 0.15 ~ 2,
                                     as.numeric(as.character(KC_tier)) == 0.1 ~ 1,
                                     as.numeric(as.character(KC_tier)) < 0.1 ~ 0),
                New_Grade = (KC_Grade + EV_Grade) / 2)

grades <- types %>%
  arrange(gamedate, AwayTeam, HomeTeam, desc(EV)) %>%
  group_by(gamedate, AwayTeam, HomeTeam) %>%
  dplyr::mutate(EV_Rank = row_number()) %>%
  arrange(gamedate, AwayTeam, HomeTeam, desc(Kelly_Criteria)) %>%
  group_by(gamedate, AwayTeam, HomeTeam) %>%
  dplyr::mutate(KC_Rank = row_number(),
                Rank = (KC_Rank + EV_Rank) / 2) %>%
  arrange(gamedate, gamedate, AwayTeam, HomeTeam, Rank) %>%
  dplyr::mutate(Final_Rank = row_number()) %>%
  filter(Final_Rank == 1) %>%
  ungroup() %>% 
  dplyr::mutate(`Bet Grade` = case_when(New_Grade > 3 ~ 'A+',
                                        New_Grade >= 2.5 ~ 'A',
                                        New_Grade >= 1.5 ~ 'B',
                                        New_Grade >= 1 ~ 'C',
                                        New_Grade < 1 ~ 'D'),
         `Graded Risk` = case_when(`Bet Grade` == 'A+' ~ 2,
                                   `Bet Grade` == 'A' ~ 1.5,
                                   `Bet Grade` == 'B' ~ 1,
                                   `Bet Grade` == 'C' ~ 0.5,
                                   `Bet Grade` == 'D' ~ 0),
         `Graded Profit` = Units*`Graded Risk`,
         `Bet Grade` = factor(`Bet Grade`, levels = c('A+', 'A', 'B', 'C', 'D'))) %>% 
  filter(Winner != "Push")

grades %>%
  # filter(`Bet Grade` == "A") %>% 
  # filter(as.numeric(as.character(KC_tier)) <= 0.05 &
  #          as.numeric(as.character(EV_tier)) == 0) %>%
  # group_by(KC_tier = as.numeric(as.character(KC_tier))) %>%
  # group_by(EV_tier = as.numeric(as.character(EV_tier))) %>%
  group_by(`Bet Grade`,
           `Suggested Wager` = case_when(`Bet Grade` == 'A+' ~ '2 units',
                                         `Bet Grade` == 'A' ~ '1.5 unit',
                                         `Bet Grade` == 'B' ~ '1 unit',
                                         `Bet Grade` == 'C' ~ '0.5 unit',
                                         `Bet Grade` == 'D' ~ 'No bet')) %>%
  dplyr::summarise(`Hit Rate` = mean(Pick_Correct),
                   `Average Implied Odds` = mean(if_else(Pick_Odds > 0, 100 / (Pick_Odds + 100), abs(Pick_Odds) / (abs(Pick_Odds) + 100))),
                   `Average Odds` = if_else(`Average Implied Odds` < 0.5,
                                            (100 / `Average Implied Odds`) - 100,
                                            -1 * (100 * `Average Implied Odds`) / (1 - `Average Implied Odds`)),
                   Bets = sum(bets),
                   `Profit: 1 Unit Wagers` = sum(Units),
                   `Profit: Suggested Wagers` = sum(`Graded Profit`)) %>% 
  dplyr::mutate(ROI = `Profit: 1 Unit Wagers` / Bets) %>%
  dplyr::mutate(`Hit Rate` = paste0(round_any(`Hit Rate`*100, 1), '%'),
                `Average Odds` = as.integer(round_any(`Average Odds`,1)),
                `Average Implied Odds` = paste0(round_any(`Average Implied Odds`*100, 1), '%'),
                Bets = formatC(Bets, format="d", big.mark=","),
                `Profit: 1 Unit Wagers` = paste0(round_any(`Profit: 1 Unit Wagers`, 0.1), ' units'),
                `Profit: Suggested Wagers` = paste0(round_any(`Profit: Suggested Wagers`, 0.1), ' units'),
                ROI = paste0(round_any(ROI*100, 0.01), '%')) %>%
  print()

ass <- grades %>% 
  # dplyr::mutate(New_Grade = (KC_Grade + EV_Grade) / 2,
  #               `Bet Grade` = case_when(New_Grade > 3 ~ 'A+',
  #                                       New_Grade >= 2.5 ~ 'A',
  #                                       New_Grade >= 1.5 ~ 'B',
  #                                       New_Grade >= 1 ~ 'C',
  #                                       New_Grade < 1 ~ 'D')) %>% 
  group_by(#`Bet Grade`,
    # KC_Grade,
    # KC_tier = as.numeric(as.character(KC_tier)),
    # EV_Grade,
    # EV_tier = as.numeric(as.character(EV_tier)),
    New_Grade
    # `Bet Grade` = factor(`Bet Grade`, levels = c('A+', 'A', 'B', 'C', 'D'))
    ) %>%
  dplyr::summarise(total_bets = sum(bets),
                   ROI = sum(Units) / sum(bets))

plot_data <- grades %>% 
  filter(`Bet Grade` == 'D') %>% 
  select(gamedate, Units, `Graded Profit`) %>% 
  dplyr::rename(`Profit: 1 Unit Wagers` = Units,
                `Profit: Suggested Wagers` = `Graded Profit`) %>% 
  melt("gamedate", c("Profit: 1 Unit Wagers", "Profit: Suggested Wagers")) %>% 
  group_by(gamedate, variable) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  dplyr::mutate(cumulative_value = cumsum(value))

ggplot(plot_data) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Game Date",
    y = "Cumulative Profit (Units)",
    color = ""
  ) +
  theme(legend.position = "bottom")

bets_table <- bets3 %>% 
  filter(Kelly_Criteria > 0 &
           Pick_Odds >= -180 &
           Pick_WinProb > 0.3 &
           !str_detect(bet_type, "RFI") &
           bet_type != "F5 Alt TT" &
           gamedate == Sys.Date()) %>% 
  arrange(gamedate, AwayTeam, HomeTeam, desc(EV)) %>% 
  group_by(gamedate, AwayTeam, HomeTeam) %>% 
  dplyr::mutate(EV_Rank = row_number()) %>% 
  arrange(gamedate, AwayTeam, HomeTeam, desc(Kelly_Criteria)) %>% 
  group_by(gamedate, AwayTeam, HomeTeam) %>% 
  dplyr::mutate(KC_Rank = row_number(),
                Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, gamedate, AwayTeam, HomeTeam, Rank) %>% 
  dplyr::mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1) %>%
  arrange(gamedate, desc(Kelly_Criteria)) %>% 
  ungroup() %>% 
  dplyr::mutate(Pick = case_when(is.na(Pick_SpreadTotal) | Pick_SpreadTotal == 0 ~ paste0(Pick),
                                 str_detect(bet_type, "RL") & Pick_SpreadTotal > 0 ~ paste0(Pick, " +", Pick_SpreadTotal),
                                 str_detect(bet_type_full, HomeTeam) ~ paste0(HomeTeam, " ", Pick, " ", Pick_SpreadTotal),
                                 str_detect(bet_type_full, AwayTeam) ~ paste0(AwayTeam, " ", Pick, " ", Pick_SpreadTotal),
                                 TRUE ~ paste0(Pick, " ", Pick_SpreadTotal))) %>% 
  select(gamedate, AwayTeam, HomeTeam, bet_type_full, Pick, Pick_Odds, Machine_Odds, KC_tier, EV_tier) %>% 
  dplyr::mutate(gamedate = as.character(gamedate)) %>% 
  dplyr::rename(`Game Date` = gamedate,
                `Home Team` = HomeTeam,
                `Away Team` = AwayTeam,
                `Bet Type` = bet_type_full,
                `Current Pick Odds` = Pick_Odds,
                `Odds Should Be` = Machine_Odds,
                KC = KC_tier,
                EV = EV_tier) %>% 
  dplyr::mutate(`Current Pick Odds` = as.integer(`Current Pick Odds`),
                `Odds Should Be` = as.integer(`Odds Should Be`),
                EV = as.integer(as.character(EV)),
                `Bet Grade` = case_when(as.numeric(as.character(KC)) < 0.1 &
                                          as.numeric(as.character(EV)) < 1 ~ "C",
                                        as.numeric(as.character(KC)) < 0.2 |
                                          as.numeric(as.character(EV)) < 2 ~ "B",
                                        as.numeric(as.character(KC)) < 0.35 ~ "A",
                                        as.numeric(as.character(KC)) >= 0.35 ~ "A+",
                                        TRUE ~ "No Grade"))

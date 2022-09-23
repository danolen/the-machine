library(shiny)

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

ui <- navbarPage(
  title = "Shiny Application",
  theme = bslib::bs_theme(4),
  # tabPanel(
  #   title = "Bets",
  #   value = "tab_vf8gukmk2z",
  #   textOutput(
  #     outputId = "Suggested Bets"
  #   ),
  #   dataTableOutput(
  #     outputId = "Suggested Bets"
  #   )
  # ),
  tabPanel(
    title = "Performance",
    value = "tab_1b7co4lyda",
    tableOutput(
      outputId = "BetTypes"
    )
  )
)

# ui <- dashboardPage(
#   dashboardHeader(title = "Pony Express Betting"),
#   skin = c("red"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Model Inputs", tabName = "DepthCharts", icon = icon("edit"))
#     )
#   ),

server <- server <- function(input, output) {
  
  output$BetTypes <- renderTable({
    
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
      mutate(HitRate = paste0(round_any(HitRate*100, 1), '%'),
             bets = formatC(bets, format="d", big.mark=","),
             Flat_Profit = paste0(round_any(Flat_Profit, 0.1), ' units'),
             Kelly_Profit = case_when(Kelly_Profit >= 0 ~ paste0('$', formatC(Kelly_Profit, format = "d", big.mark=",")),
                                      TRUE ~ paste0('-$', formatC(abs(Kelly_Profit), format = "d", big.mark=","))),
             Units_per_bet = paste0(round_any(Units_per_bet, 0.01), ' units')) %>% 
      rename(`Bet Type` = bet_type,
             `Hit Rate` = HitRate,
             `Bets` = bets,
             `Flat Profit` = Flat_Profit,
             `Kelly Profit` = Kelly_Profit,
             `Units per Bet` = Units_per_bet) %>% 
      print(n=100)
    
  })
  
}

shinyApp(ui = ui, server = server)

## Get odds from Bovada API
get_bovada_odds <- function(sport, league) {
  library(tidyverse)
  library(tidyjson)
  library(jsonlite)
  library(reshape2)
  url <- paste0("https://www.bovada.lv/services/sports/event/v2/events/A/description/",sport,"/",league)
  
  bovada_odds <- fromJSON(url) %>%
    .[[2]] %>%
    .[[1]] %>%
    select(description, link, displayGroups) %>%
    mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
    separate(description, c("AwayTeam", "HomeTeam"), " @ ") %>%
    select(gamedate, AwayTeam, HomeTeam, displayGroups) %>%
    unnest(displayGroups) %>%
    select(gamedate, AwayTeam, HomeTeam, markets) %>%
    unnest(markets) %>%
    filter(period$live == FALSE) %>%
    mutate(bet_type = paste0(description, " - ", period$description)) %>%
    select(gamedate, AwayTeam, HomeTeam, bet_type, outcomes) %>%
    unnest(outcomes) %>%
    mutate(Odds = as.numeric(price$american),
           SpreadTotal = price$handicap,
           type = case_when(str_detect(bet_type, "To Score - 1st Inning") |
                              bet_type == "Will there be a run scored in the 1st inning - Game" ~
                                if_else(description %in% c("Yes", "Yes - 1I"), "AOY", "HUN"),
                            TRUE ~ if_else(type == "A" | type == "O", "AOY", "HUN"))) %>%
    mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
    select(gamedate, AwayTeam, HomeTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
    filter(bet_type %in% c("Moneyline - Game", "Runline - Game", "Total - Game",
                           "Will there be a run scored in the 1st inning - Game",
                           "Moneyline - 5 Inning Line", "Runline - 5 Inning Line",
                           "Total - 5 Inning Line", "Spread - Game", "Spread - 5 Inning Line", 
                           "Total Runs O/U - Game", "Total Runs O/U - 5 Inning Line",
                           paste0("Total Runs O/U - ", AwayTeam, " - Game"), 
                           paste0("Total Runs O/U - ", HomeTeam, " - Game"),
                           paste0("Total Runs O/U - ", AwayTeam, " - 5 Inning Line"), 
                           paste0("Total Runs O/U - ", HomeTeam, " - 5 Inning Line"),
                           paste0(AwayTeam, " To Score - 1st Inning"), 
                           paste0(HomeTeam, " To Score - 1st Inning"))) %>%
    mutate(bet_type = case_when(bet_type == "Spread - Game" ~ paste0("Alternate Runline - Game - ", abs(as.numeric(SpreadTotal))),
                                bet_type == "Spread - 5 Inning Line" ~ paste0("Alternate Runline - 5 Inning Line - ", abs(as.numeric(SpreadTotal))),
                                bet_type == "Total Runs O/U - Game" ~ paste0("Alternate Total - Game - ", abs(as.numeric(SpreadTotal))),
                                bet_type == "Total Runs O/U - 5 Inning Line" ~ paste0("Alternate Total - 5 Inning Line - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", AwayTeam, " - Game") ~
                                  paste0("Team Total - ", AwayTeam, " - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", HomeTeam, " - Game") ~
                                  paste0("Team Total - ", HomeTeam, " - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", AwayTeam, " - 5 Inning Line") ~
                                  paste0("Team Total - ", AwayTeam, " - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", HomeTeam, " - 5 Inning Line") ~
                                  paste0("Team Total - ", HomeTeam, " - ", abs(as.numeric(SpreadTotal))),
                                TRUE ~ bet_type)) %>%
    filter(bet_type %in% c("Moneyline - Game", "Runline - Game", "Total - Game",
                           "Will there be a run scored in the 1st inning - Game",
                           "Moneyline - 5 Inning Line", "Runline - 5 Inning Line", 
                           "Total - 5 Inning Line", "Alternate Runline - Game - 1.5",
                           "Alternate Runline - Game - 2.5", 
                           "Alternate Runline - 5 Inning Line - 0.5",
                           "Alternate Runline - 5 Inning Line - 1.5",
                           paste0("Alternate Total - Game - ", abs(as.numeric(SpreadTotal))),
                           paste0("Alternate Total - 5 Inning Line - ", abs(as.numeric(SpreadTotal))),
                           paste0("Total Runs O/U - ", AwayTeam, " - Game"), 
                           paste0("Total Runs O/U - ", HomeTeam, " - Game"),
                           paste0(AwayTeam, " To Score - 1st Inning"), 
                           paste0(HomeTeam, " To Score - 1st Inning"))) %>%
    distinct(gamedate, AwayTeam, HomeTeam, bet_type, type, .keep_all = TRUE) %>% 
    melt(id.vars = c("gamedate", "AwayTeam", "HomeTeam", "bet_type", "type")) %>%
    mutate(name = paste0(type, ".", variable)) %>%
    select(-type, -variable) %>%
    mutate(value = as.numeric(value)) %>%
    dcast(gamedate + AwayTeam + HomeTeam + bet_type ~ name, fun.aggregate = mean) %>%
    mutate(AOY.Odds = round(AOY.Odds, 0),
           HUN.Odds = round(HUN.Odds, 0)) %>%
    filter((AOY.Odds >= 100 | AOY.Odds <= -100) & (HUN.Odds >= 100 | HUN.Odds <= -100))
}

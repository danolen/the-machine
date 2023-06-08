## Get odds from Bovada API
get_bovada_odds <- function(sport) {
  library(tidyverse)
  library(tidyjson)
  library(jsonlite)
  library(reshape2)
  url <- paste0("https://www.bovada.lv/services/sports/event/v2/events/A/description/",sport)
  
  bovada_odds <- fromJSON(url) %>%
    .[[2]] %>%
    .[[1]] %>%
    select(description, link, competitors, displayGroups) %>%
    mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
    separate(description, c("AwayTeam", "HomeTeam"), " @ ") %>%
    unnest(competitors) %>% 
    group_by(link) %>% 
    mutate(AwayStartingPitcher = if_else(name == AwayTeam, pitcher$name, NA_character_),
           HomeStartingPitcher = if_else(name == HomeTeam, pitcher$name, NA_character_)) %>% 
    ungroup() %>% 
    fill(AwayStartingPitcher, .direction = "up") %>% 
    fill(HomeStartingPitcher, .direction = "down") %>% 
    mutate(AwayStartingPitcher = if_else(is.na(pitcher$name), NA_character_, AwayStartingPitcher),
           HomeStartingPitcher = if_else(is.na(pitcher$name), NA_character_, HomeStartingPitcher)) %>% 
    mutate(AwayStartingPitcher = iconv(substr(AwayStartingPitcher, 1, nchar(AwayStartingPitcher) - 4), to = "ASCII//TRANSLIT"),
           HomeStartingPitcher = iconv(substr(HomeStartingPitcher, 1, nchar(HomeStartingPitcher) - 4), to = "ASCII//TRANSLIT")) %>% 
    select(gamedate, AwayTeam, HomeTeam, AwayStartingPitcher, HomeStartingPitcher, displayGroups) %>%
    unnest(displayGroups) %>%
    select(gamedate, AwayTeam, HomeTeam, AwayStartingPitcher, HomeStartingPitcher, markets) %>%
    unnest(markets) %>%
    filter(period$live == FALSE) %>%
    mutate(bet_type = paste0(description, " - ", period$description)) %>%
    select(gamedate, AwayTeam, HomeTeam, AwayStartingPitcher, HomeStartingPitcher, bet_type, outcomes) %>%
    filter(bet_type %in% c("Moneyline - Game", "Runline - Game", "Total - Game",
                           "Will there be a run scored in the 1st inning - Game",
                           "Moneyline - First 5 Innings", "Runline - First 5 Innings",
                           "Total - First 5 Innings", "Spread - Game", "Spread - First 5 Innings", 
                           "Total Runs O/U - Game", "Total Runs O/U - First 5 Innings",
                           paste0("Total Runs O/U - ", AwayTeam, " - Game"), 
                           paste0("Total Runs O/U - ", HomeTeam, " - Game"),
                           paste0("Total Runs O/U - ", AwayTeam, " - First 5 Innings"), 
                           paste0("Total Runs O/U - ", HomeTeam, " - First 5 Innings"),
                           paste0(AwayTeam, " To Score - 1st Inning"), 
                           paste0(HomeTeam, " To Score - 1st Inning"))) %>%
    unnest(outcomes) %>%
    mutate(Odds = as.numeric(price$american),
           SpreadTotal = price$handicap,
           type = case_when(str_detect(bet_type, "To Score - 1st Inning") |
                              bet_type == "Will there be a run scored in the 1st inning - Game" ~
                                if_else(description %in% c("Yes", "Yes - 1I"), "AOY", "HUN"),
                            TRUE ~ if_else(type == "A" | type == "O", "AOY", "HUN"))) %>%
    mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
    select(gamedate, AwayTeam, HomeTeam, AwayStartingPitcher, HomeStartingPitcher, bet_type, type, Odds, any_of("SpreadTotal")) %>%
    mutate(bet_type = case_when(bet_type == "Spread - Game" ~ paste0("Alternate Runline - Game - ", abs(as.numeric(SpreadTotal))),
                                bet_type == "Spread - First 5 Innings" ~ paste0("Alternate Runline - First 5 Innings - ", abs(as.numeric(SpreadTotal))),
                                bet_type == "Total Runs O/U - Game" ~ paste0("Alternate Total - Game - ", abs(as.numeric(SpreadTotal))),
                                bet_type == "Total Runs O/U - First 5 Innings" ~ paste0("Alternate Total - First 5 Innings - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", AwayTeam, " - Game") ~
                                  paste0("Team Total - ", AwayTeam, " - Game - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", HomeTeam, " - Game") ~
                                  paste0("Team Total - ", HomeTeam, " - Game - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", AwayTeam, " - First 5 Innings") ~
                                  paste0("Team Total - ", AwayTeam, " - First 5 Innings - ", abs(as.numeric(SpreadTotal))),
                                bet_type == paste0("Total Runs O/U - ", HomeTeam, " - First 5 Innings") ~
                                  paste0("Team Total - ", HomeTeam, " - First 5 Innings - ", abs(as.numeric(SpreadTotal))),
                                TRUE ~ bet_type)) %>%
    filter(bet_type %in% c("Moneyline - Game", "Runline - Game", "Total - Game",
                           "Will there be a run scored in the 1st inning - Game",
                           "Moneyline - First 5 Innings", "Runline - First 5 Innings", 
                           "Total - First 5 Innings", "Alternate Runline - Game - 1.5",
                           "Alternate Runline - Game - 2.5", 
                           "Alternate Runline - First 5 Innings - 0.5",
                           "Alternate Runline - First 5 Innings - 1.5",
                           paste0("Alternate Total - Game - ", abs(as.numeric(SpreadTotal))),
                           paste0("Alternate Total - First 5 Innings - ", abs(as.numeric(SpreadTotal))),
                           paste0(AwayTeam, " To Score - 1st Inning"), 
                           paste0(HomeTeam, " To Score - 1st Inning")) |
             str_detect(bet_type, paste0("Team Total - ", AwayTeam, " - Game")) |
             str_detect(bet_type, paste0("Team Total - ", HomeTeam, " - Game")) |
             str_detect(bet_type, paste0("Team Total - ", AwayTeam, " - First 5 Innings")) |
             str_detect(bet_type, paste0("Team Total - ", HomeTeam, " - First 5 Innings"))) %>%
    distinct(gamedate, AwayTeam, HomeTeam, bet_type, type, .keep_all = TRUE) %>% 
    melt(id.vars = c("gamedate", "AwayTeam", "HomeTeam", "AwayStartingPitcher", "HomeStartingPitcher", "bet_type", "type")) %>%
    mutate(name = paste0(type, ".", variable)) %>%
    select(-type, -variable) %>%
    mutate(value = as.numeric(value)) %>%
    dcast(gamedate + AwayTeam + HomeTeam + AwayStartingPitcher + HomeStartingPitcher + bet_type ~ name, fun.aggregate = mean) %>%
    mutate(AOY.Odds = round(AOY.Odds, 0),
           HUN.Odds = round(HUN.Odds, 0)) %>%
    filter((AOY.Odds >= 100 | AOY.Odds <= -100) & (HUN.Odds >= 100 | HUN.Odds <= -100))
}

### Soccer betting model
## Picks
overallStart <- Sys.time()
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

mls_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/major-league-soccer"
epl_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/england-premier-league"
esp_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/spain-la-liga"
ger_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/germany-bundesliga"
fra_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/france-ligue-1"
ita_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/italy-serie-a"
ucl_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/uefa-champions-league"
uel_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/uefa-europa-league"

mls_odds <- fromJSON(mls_url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
  select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
  select(gamedate, HomeTeam, AwayTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE & period$description == "Regulation Time") %>%
  mutate(bet_type = description) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
  mutate(type = if_else(type == "X",
                        if_else(description == "Yes", "Y","N"),
                        type)) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
                        if_else(type == "D", "D", "AUN"))) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
                              bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
                              bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - Orlando City", bet_type) ~ paste0("Total Goals O/U - Orlando City SC", " - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - LA Galaxy", bet_type) ~ paste0("Total Goals O/U - Los Angeles Galaxy", " - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

epl_odds <- fromJSON(epl_url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
  select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
  select(gamedate, HomeTeam, AwayTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE & period$description == "Regulation Time") %>%
  mutate(bet_type = description) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
  mutate(type = if_else(type == "X",
                        if_else(description == "Yes", "Y","N"),
                        type)) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
                        if_else(type == "D", "D", "AUN"))) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
                              bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
                              bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

esp_odds <- fromJSON(esp_url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
  select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
  select(gamedate, HomeTeam, AwayTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE & period$description == "Regulation Time") %>%
  mutate(bet_type = description) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
  mutate(type = if_else(type == "X",
                        if_else(description == "Yes", "Y","N"),
                        type)) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
                        if_else(type == "D", "D", "AUN"))) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
                              bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
                              bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

# ger_odds <- fromJSON(ger_url) %>%
#   .[[2]] %>%
#   .[[1]] %>%
#   select(description, link, displayGroups) %>%
#   mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
#   separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
#   select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
#   unnest(displayGroups) %>%
#   filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
#   select(gamedate, HomeTeam, AwayTeam, markets) %>%
#   unnest(markets) %>%
#   filter(period$live == FALSE & period$description == "Regulation Time") %>%
#   mutate(bet_type = description) %>%
#   select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
#   unnest(outcomes) %>%
#   filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
#   mutate(type = if_else(type == "X",
#                         if_else(description == "Yes", "Y","N"),
#                         type)) %>%
#   mutate(Odds = as.numeric(price$american),
#          SpreadTotal = price$handicap,
#          type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
#                         if_else(type == "D", "D", "AUN"))) %>%
#   mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
#   select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
#   mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
#          AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
#          bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
#          bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
#                               bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
#                               bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
#                               grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
#                               TRUE ~ bet_type)) %>%
#   reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
#   mutate(name = paste0(type, ".", variable)) %>%
#   select(-type, -variable) %>%
#   mutate(value = as.numeric(value)) %>%
#   reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
#   mutate(AUN.Odds = round(AUN.Odds, 0),
#          HOY.Odds = round(HOY.Odds, 0),
#          D.Odds = round(D.Odds, 0)) %>%
#   filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
#   select(-D.SpreadTotal)

fra_odds <- fromJSON(fra_url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
  select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
  select(gamedate, HomeTeam, AwayTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE & period$description == "Regulation Time") %>%
  mutate(bet_type = description) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
  mutate(type = if_else(type == "X",
                        if_else(description == "Yes", "Y","N"),
                        type)) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
                        if_else(type == "D", "D", "AUN"))) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
                              bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
                              bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

ita_odds <- fromJSON(ita_url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
  select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
  select(gamedate, HomeTeam, AwayTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE & period$description == "Regulation Time") %>%
  mutate(bet_type = description) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
  mutate(type = if_else(type == "X",
                        if_else(description == "Yes", "Y","N"),
                        type)) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
                        if_else(type == "D", "D", "AUN"))) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
                              bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
                              bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - Inter Milan", bet_type) ~ paste0("Total Goals O/U - Inter Milano - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

ucl_odds <- fromJSON(ucl_url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
  select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
  select(gamedate, HomeTeam, AwayTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE & period$description == "Regulation Time") %>%
  mutate(bet_type = description) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
  mutate(type = if_else(type == "X",
                        if_else(description == "Yes", "Y","N"),
                        type)) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
                        if_else(type == "D", "D", "AUN"))) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
                              bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
                              bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

uel_odds <- fromJSON(uel_url) %>%
  .[[2]] %>%
  .[[1]] %>%
  select(description, link, displayGroups) %>%
  mutate(gamedate = as.Date(str_sub(link, -12, -5), format = "%Y%m%d")) %>%
  separate(description, c("HomeTeam", "AwayTeam"), " vs ") %>%
  select(gamedate, HomeTeam, AwayTeam, displayGroups) %>%
  unnest(displayGroups) %>%
  filter(description %in% c("Game Lines", "Alternate Lines", "Both Teams to Score")) %>%
  select(gamedate, HomeTeam, AwayTeam, markets) %>%
  unnest(markets) %>%
  filter(period$live == FALSE & period$description == "Regulation Time") %>%
  mutate(bet_type = description) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, outcomes) %>%
  unnest(outcomes) %>%
  filter(is.na(price$handicap) == TRUE | (is.na(price$handicap) == FALSE & is.na(price$handicap2) == TRUE)) %>%
  mutate(type = if_else(type == "X",
                        if_else(description == "Yes", "Y","N"),
                        type)) %>%
  mutate(Odds = as.numeric(price$american),
         SpreadTotal = price$handicap,
         type = if_else(type == "H" | type == "O" | type == "Y", "HOY",
                        if_else(type == "D", "D", "AUN"))) %>%
  mutate(Odds = if_else(is.na(Odds), 100, Odds)) %>%
  select(gamedate, HomeTeam, AwayTeam, bet_type, type, Odds, any_of("SpreadTotal")) %>%
  mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = iconv(bet_type, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         bet_type = case_when(bet_type == "Spread" & type == "HOY" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)),
                              bet_type == "Spread" & type == "AUN" ~ paste0("Alternate Spread - Home: ", as.numeric(SpreadTotal)*-1),
                              bet_type == "Total Goals O/U" ~ paste0("Alternate Total - ", abs(as.numeric(SpreadTotal))),
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  reshape2::melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  reshape2::dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

bovada_odds <- bind_rows(epl_odds, 
                         esp_odds, 
                         #ger_odds, 
                         ita_odds,
                         fra_odds, 
                         mls_odds, 
                         ucl_odds, 
                         uel_odds
                         )

club_names <- read_excel("Soccer Machine/Club Names.xlsx")

bovada_odds <- FindReplace(bovada_odds, Var = "HomeTeam", replaceData = club_names,
                           from = "Bovada", to = "Name")
bovada_odds <- FindReplace(bovada_odds, Var = "AwayTeam", replaceData = club_names,
                           from = "Bovada", to = "Name")

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

home <- fixtures %>% 
  mutate(ID = gsub(" ", "", gsub("[[:punct:]]","",paste0(Home, Away, Date, Time)), fixed = TRUE)) %>% 
  select(ID, Date, Day, Time, League, Season, Home, Away, xG:xG.1) %>% 
  mutate(Home_or_Away = "Home") %>% 
  select(ID:Away, Home_or_Away, xG, xG.1, Home_Score, Away_Score) %>% 
  rename(Team = Home,
         Opponent = Away,
         xGA = xG.1,
         Goals = Home_Score,
         GoalsAllowed = Away_Score)

away <- fixtures %>% 
  mutate(ID = gsub(" ", "", gsub("[[:punct:]]","",paste0(Home, Away, Date, Time)), fixed = TRUE)) %>% 
  select(ID, Date, Day, Time, League, Season, Away, Home, xG:xG.1) %>% 
  mutate(Home_or_Away = "Away") %>% 
  select(ID:Home, Home_or_Away, xG.1, xG, Away_Score, Home_Score) %>% 
  rename(Team = Away,
         Opponent = Home,
         xG = xG.1,
         xGA = xG,
         Goals = Away_Score,
         GoalsAllowed = Home_Score)

metrics <- bind_rows(home, away) %>%
  filter(!League %in% c('UCL', 'UEL')) %>% 
  filter(!is.na(Date) & (!is.na(xG) | Date >= Sys.Date())) %>% 
  replace(is.na(.), 0) %>% 
  arrange(Date, Time, League, ID) %>% 
  # mutate(Team = trimws(case_when(League %in% c('UCL', 'UEL') & Home_or_Away == "Home" ~ substr(Team, 1, nchar(Team)-3),
  #                                League %in% c('UCL', 'UEL') & Home_or_Away == "Away" ~ substr(Team, 4, nchar(Team)),
  #                                TRUE ~ Team), which = c("both")),
  #        Opponent = trimws(case_when(League %in% c('UCL', 'UEL') & Home_or_Away == "Home" ~ substr(Opponent, 1, nchar(Opponent)-3),
  #                                League %in% c('UCL', 'UEL') & Home_or_Away == "Away" ~ substr(Opponent, 4, nchar(Opponent)),
  #                                TRUE ~ Opponent), which = c("both"))) %>% 
  group_by(Team, League, Season, Home_or_Away) %>% 
  mutate(SplitxG = cumsum(xG) - xG,
         SplitxGA = cumsum(xGA) - xGA,
         SplitGoals = cumsum(Goals) - Goals,
         SplitGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SplitGP = cumsum(case_when(Date < today ~ 1, TRUE ~ 0)),
         SplitxG_roll4 = case_when(SplitGP < 5 ~ SplitxG / (SplitGP-1),
                                   TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
         SplitxGA_roll4 = case_when(SplitGP < 5 ~ SplitxG / (SplitGP-1),
                                    TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
         SplitGoals_roll4 = case_when(SplitGP < 5 ~ SplitGoals / (SplitGP-1),
                                      TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals)+lag(Goals,4))/4),
         SplitGoalsAllowed_roll4 = case_when(SplitGP < 5 ~ SplitGoalsAllowed / (SplitGP-1),
                                             TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed)+lag(GoalsAllowed,4))/4)) %>%
  
  group_by(Team, League, Season) %>% 
  mutate(SeasonxG = cumsum(xG) - xG,
         SeasonxGA = cumsum(xGA) - xGA,
         SeasonGoals = cumsum(Goals) - Goals,
         SeasonGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SeasonGP = cumsum(case_when(Date < today ~ 1, TRUE ~ 0)),
         SeasonxG_roll4 = (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4,
         SeasonxGA_roll4 = (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4,
         SeasonGoals_roll4 = (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4,
         SeasonGoalsAllowed_roll4 = (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4) %>% 
  ungroup() %>% 
  mutate(SplitGP = case_when(Date < today ~ SplitGP - 1, TRUE ~ SplitGP),
         SeasonGP = case_when(Date < today ~ SeasonGP - 1, TRUE ~ SeasonGP)) %>%
  mutate(SplitxG = SplitxG / SplitGP,
         SplitxGA = SplitxGA / SplitGP,
         SplitGoals = SplitGoals / SplitGP,
         SplitGoalsAllowed = SplitGoalsAllowed / SplitGP,
         SeasonxG = SeasonxG / SeasonGP,
         SeasonxGA = SeasonxGA / SeasonGP,
         SeasonGoals = SeasonGoals / SeasonGP,
         SeasonGoalsAllowed = SeasonGoalsAllowed / SeasonGP) %>% 
  group_by(Team, League, Season, Home_or_Away) %>% 
  mutate(SplitxG_roll4 = case_when(SplitGP == lag(SplitGP,1) ~ lag(SplitxG_roll4,1),
                                   TRUE ~ SplitxG_roll4),
         SplitxGA_roll4 = case_when(SplitGP == lag(SplitGP,1) ~ lag(SplitxGA_roll4,1),
                                    TRUE ~ SplitxGA_roll4),
         SplitGoals_roll4 = case_when(SplitGP == lag(SplitGP,1) ~ lag(SplitGoals_roll4,1),
                                      TRUE ~ SplitGoals_roll4),
         SplitGoalsAllowed_roll4 = case_when(SplitGP == lag(SplitGP,1) ~ lag(SplitGoalsAllowed_roll4,1),
                                             TRUE ~ SplitGoalsAllowed_roll4)) %>% 
  group_by(Team, League, Season) %>% 
  mutate(SeasonxG_roll4 = case_when(SeasonGP == lag(SeasonGP,1) ~ lag(SeasonxG_roll4,1),
                                    TRUE ~ SeasonxG_roll4),
         SeasonxGA_roll4 = case_when(SeasonGP == lag(SeasonGP,1) ~ lag(SeasonxGA_roll4,1),
                                     TRUE ~ SeasonxGA_roll4),
         SeasonGoals_roll4 = case_when(SeasonGP == lag(SeasonGP,1) ~ lag(SeasonGoals_roll4,1),
                                       TRUE ~ SeasonGoals_roll4),
         SeasonGoalsAllowed_roll4 = case_when(SeasonGP == lag(SeasonGP,1) ~ lag(SeasonGoalsAllowed_roll4,1),
                                              TRUE ~ SeasonGoalsAllowed_roll4)) %>% 
  ungroup() %>% 
  replace(is.na(.), 0)

metrics_df <- metrics %>% 
  left_join(metrics, by = c("ID" = "ID", "Date" = "Date", "Day" = "Day", "Time" = "Time",
                            "League" = "League", "Season" = "Season", "Opponent" = "Team"),
            suffix = c("", "_Opp")) %>% 
  filter(Home_or_Away == "Home" & Date >= today & SplitGP > 1 & SplitGP_Opp > 1) %>%
  select(-(Home_or_Away:GoalsAllowed), -(Opponent_Opp:GoalsAllowed_Opp))

metrics_tt <- metrics %>% 
  left_join(metrics, by = c("ID" = "ID", "Date" = "Date", "Day" = "Day", "Time" = "Time",
                            "League" = "League", "Season" = "Season", "Opponent" = "Team"),
            suffix = c("", "_Opp")) %>%
  filter(Date >= today & SplitGP > 1 & SplitGP_Opp > 1) %>% 
  select(-(xG:GoalsAllowed), -(Opponent_Opp:GoalsAllowed_Opp))

gbm_reg <- readRDS("Soccer Machine/Models/train_gbm.rds")
cub_reg <- readRDS("Soccer Machine/Models/train_cub.rds")
rf_reg <- readRDS("Soccer Machine/Models/train_rf.rds")
ctree_reg <- readRDS("Soccer Machine/Models/train_ctree.rds")
pls_reg <- readRDS("Soccer Machine/Models/train_pls.rds")
lm_reg <- readRDS("Soccer Machine/Models/train_lm.rds")
outcome_gbm <- readRDS("Soccer Machine/Models/outcome_gbm.rds")
outcome_pls <- readRDS("Soccer Machine/Models/outcome_pls.rds")
outcome_xgb <- readRDS("Soccer Machine/Models/outcome_xgb.rds")
minus1_gbm <- readRDS("Soccer Machine/Models/minus1_gbm.rds")
minus1_pls <- readRDS("Soccer Machine/Models/minus1_pls.rds")
minus1_xgb <- readRDS("Soccer Machine/Models/minus1_xgb.rds")
minus1.5_gbm <- readRDS("Soccer Machine/Models/minus1.5_gbm.rds")
minus1.5_pls <- readRDS("Soccer Machine/Models/minus1.5_pls.rds")
minus1.5_xgb <- readRDS("Soccer Machine/Models/minus1.5_xgb.rds")
minus2_gbm <- readRDS("Soccer Machine/Models/minus2_gbm.rds")
minus2_pls <- readRDS("Soccer Machine/Models/minus2_pls.rds")
minus2_xgb <- readRDS("Soccer Machine/Models/minus2_xgb.rds")
minus2.5_gbm <- readRDS("Soccer Machine/Models/minus2.5_gbm.rds")
minus2.5_pls <- readRDS("Soccer Machine/Models/minus2.5_pls.rds")
minus2.5_xgb <- readRDS("Soccer Machine/Models/minus2.5_xgb.rds")
minus3_gbm <- readRDS("Soccer Machine/Models/minus3_gbm.rds")
minus3_pls <- readRDS("Soccer Machine/Models/minus3_pls.rds")
minus3_xgb <- readRDS("Soccer Machine/Models/minus3_xgb.rds")
minus3.5_gbm <- readRDS("Soccer Machine/Models/minus3.5_gbm.rds")
minus3.5_pls <- readRDS("Soccer Machine/Models/minus3.5_pls.rds")
minus3.5_xgb <- readRDS("Soccer Machine/Models/minus3.5_xgb.rds")
plus1_gbm <- readRDS("Soccer Machine/Models/plus1_gbm.rds")
plus1_pls <- readRDS("Soccer Machine/Models/plus1_pls.rds")
plus1_xgb <- readRDS("Soccer Machine/Models/plus1_xgb.rds")
plus1.5_gbm <- readRDS("Soccer Machine/Models/plus1.5_gbm.rds")
plus1.5_pls <- readRDS("Soccer Machine/Models/plus1.5_pls.rds")
plus1.5_xgb <- readRDS("Soccer Machine/Models/plus1.5_xgb.rds")
plus2_gbm <- readRDS("Soccer Machine/Models/plus2_gbm.rds")
plus2_pls <- readRDS("Soccer Machine/Models/plus2_pls.rds")
plus2_xgb <- readRDS("Soccer Machine/Models/plus2_xgb.rds")
plus2.5_gbm <- readRDS("Soccer Machine/Models/plus2.5_gbm.rds")
plus2.5_pls <- readRDS("Soccer Machine/Models/plus2.5_pls.rds")
plus2.5_xgb <- readRDS("Soccer Machine/Models/plus2.5_xgb.rds")
plus3_gbm <- readRDS("Soccer Machine/Models/plus3_gbm.rds")
plus3_pls <- readRDS("Soccer Machine/Models/plus3_pls.rds")
plus3_xgb <- readRDS("Soccer Machine/Models/plus3_xgb.rds")
plus3.5_gbm <- readRDS("Soccer Machine/Models/plus3.5_gbm.rds")
plus3.5_pls <- readRDS("Soccer Machine/Models/plus3.5_pls.rds")
plus3.5_xgb <- readRDS("Soccer Machine/Models/plus3.5_xgb.rds")
total1.5_gbm <- readRDS("Soccer Machine/Models/total1.5_gbm.rds")
total1.5_pls <- readRDS("Soccer Machine/Models/total1.5_pls.rds")
total1.5_xgb <- readRDS("Soccer Machine/Models/total1.5_xgb.rds")
total2_gbm <- readRDS("Soccer Machine/Models/total2_gbm.rds")
total2_pls <- readRDS("Soccer Machine/Models/total2_pls.rds")
total2_xgb <- readRDS("Soccer Machine/Models/total2_xgb.rds")
total2.5_gbm <- readRDS("Soccer Machine/Models/total2.5_gbm.rds")
total2.5_pls <- readRDS("Soccer Machine/Models/total2.5_pls.rds")
total2.5_xgb <- readRDS("Soccer Machine/Models/total2.5_xgb.rds")
total3_gbm <- readRDS("Soccer Machine/Models/total3_gbm.rds")
total3_pls <- readRDS("Soccer Machine/Models/total3_pls.rds")
total3_xgb <- readRDS("Soccer Machine/Models/total3_xgb.rds")
total3.5_gbm <- readRDS("Soccer Machine/Models/total3.5_gbm.rds")
total3.5_pls <- readRDS("Soccer Machine/Models/total3.5_pls.rds")
total3.5_xgb <- readRDS("Soccer Machine/Models/total3.5_xgb.rds")
total4_gbm <- readRDS("Soccer Machine/Models/total4_gbm.rds")
total4_pls <- readRDS("Soccer Machine/Models/total4_pls.rds")
total4_xgb <- readRDS("Soccer Machine/Models/total4_xgb.rds")
total4.5_gbm <- readRDS("Soccer Machine/Models/total4.5_gbm.rds")
total4.5_pls <- readRDS("Soccer Machine/Models/total4.5_pls.rds")
total4.5_xgb <- readRDS("Soccer Machine/Models/total4.5_xgb.rds")
BTTS_gbm <- readRDS("Soccer Machine/Models/BTTS_gbm.rds")
BTTS_pls <- readRDS("Soccer Machine/Models/BTTS_pls.rds")
BTTS_xgb <- readRDS("Soccer Machine/Models/BTTS_xgb.rds")
tt0.5_gbm <- readRDS("Soccer Machine/Models/tt0.5_gbm.rds")
tt0.5_pls <- readRDS("Soccer Machine/Models/tt0.5_pls.rds")
tt0.5_xgb <- readRDS("Soccer Machine/Models/tt0.5_xgb.rds")
tt1_gbm <- readRDS("Soccer Machine/Models/tt1_gbm.rds")
tt1_pls <- readRDS("Soccer Machine/Models/tt1_pls.rds")
tt1_xgb <- readRDS("Soccer Machine/Models/tt1_xgb.rds")
tt1.5_gbm <- readRDS("Soccer Machine/Models/tt1.5_gbm.rds")
tt1.5_pls <- readRDS("Soccer Machine/Models/tt1.5_pls.rds")
tt1.5_xgb <- readRDS("Soccer Machine/Models/tt1.5_xgb.rds")
tt2_gbm <- readRDS("Soccer Machine/Models/tt2_gbm.rds")
tt2_pls <- readRDS("Soccer Machine/Models/tt2_pls.rds")
tt2_xgb <- readRDS("Soccer Machine/Models/tt2_xgb.rds")
tt2.5_gbm <- readRDS("Soccer Machine/Models/tt2.5_gbm.rds")
tt2.5_pls <- readRDS("Soccer Machine/Models/tt2.5_pls.rds")
tt2.5_xgb <- readRDS("Soccer Machine/Models/tt2.5_xgb.rds")
tt3_gbm <- readRDS("Soccer Machine/Models/tt3_gbm.rds")
tt3_pls <- readRDS("Soccer Machine/Models/tt3_pls.rds")
tt3_xgb <- readRDS("Soccer Machine/Models/tt3_xgb.rds")
tt3.5_gbm <- readRDS("Soccer Machine/Models/tt3.5_gbm.rds")
tt3.5_pls <- readRDS("Soccer Machine/Models/tt3.5_pls.rds")
tt3.5_xgb <- readRDS("Soccer Machine/Models/tt3.5_xgb.rds")

singles <- metrics_tt
singles$pG_gbm <- predict(gbm_reg, metrics_tt)
singles$pG_cub <- predict(cub_reg, metrics_tt)
singles$pG_rf <- predict(rf_reg, metrics_tt)
singles$pG_ctree <- predict(ctree_reg, metrics_tt)
singles$pG_pls <- predict(pls_reg, metrics_tt)
singles$pG_lm <- predict(lm_reg, metrics_tt)
singles$tt0.5_gbm <- predict(tt0.5_gbm, metrics_tt, type = "prob")
singles$tt0.5_pls <- predict(tt0.5_pls, metrics_tt, type = "prob")
singles$tt0.5_xgb <- predict(tt0.5_xgb, metrics_tt, type = "prob")
singles$tt1_gbm <- predict(tt1_gbm, metrics_tt, type = "prob")
singles$tt1_pls <- predict(tt1_pls, metrics_tt, type = "prob")
singles$tt1_xgb <- predict(tt1_xgb, metrics_tt, type = "prob")
singles$tt1.5_gbm <- predict(tt1.5_gbm, metrics_tt, type = "prob")
singles$tt1.5_pls <- predict(tt1.5_pls, metrics_tt, type = "prob")
singles$tt1.5_xgb <- predict(tt1.5_xgb, metrics_tt, type = "prob")
singles$tt2_gbm <- predict(tt2_gbm, metrics_tt, type = "prob")
singles$tt2_pls <- predict(tt2_pls, metrics_tt, type = "prob")
singles$tt2_xgb <- predict(tt2_xgb, metrics_tt, type = "prob")
singles$tt2.5_gbm <- predict(tt2.5_gbm, metrics_tt, type = "prob")
singles$tt2.5_pls <- predict(tt2.5_pls, metrics_tt, type = "prob")
singles$tt2.5_xgb <- predict(tt2.5_xgb, metrics_tt, type = "prob")
singles$tt3_gbm <- predict(tt3_gbm, metrics_tt, type = "prob")
singles$tt3_pls <- predict(tt3_pls, metrics_tt, type = "prob")
singles$tt3_xgb <- predict(tt3_xgb, metrics_tt, type = "prob")
singles$tt3.5_gbm <- predict(tt3.5_gbm, metrics_tt, type = "prob")
singles$tt3.5_pls <- predict(tt3.5_pls, metrics_tt, type = "prob")
singles$tt3.5_xgb <- predict(tt3.5_xgb, metrics_tt, type = "prob")
singles <- singles %>% 
  mutate(pG_ens = (pG_gbm + pG_cub + pG_rf + pG_ctree + pG_pls + pG_lm) / 6,
         pG_ensrf = (pG_ens + pG_rf) / 2,
         ptt0.5 = (tt0.5_gbm + tt0.5_pls + tt0.5_xgb) / 3,
         ptt1 = (tt1_gbm + tt1_pls + tt1_xgb) / 3,
         ptt1.5 = (tt1.5_gbm + tt1.5_pls + tt1.5_xgb) / 3,
         ptt2 = (tt2_gbm + tt2_pls + tt2_xgb) / 3,
         ptt2.5 = (tt2.5_gbm + tt2.5_pls + tt2.5_xgb) / 3,
         ptt3 = (tt3_gbm + tt3_pls + tt3_xgb) / 3,
         ptt3.5 = (tt3.5_gbm + tt3.5_pls + tt3.5_xgb) / 3)

singles2 <- singles %>% 
  select(ID:Home_or_Away, pG_ensrf:ptt3.5) %>%
  left_join(singles %>% 
              select(ID:Home_or_Away, pG_ensrf:ptt3.5),
            by = c("ID", "Date", "Day", "Time", "League", "Season", "Team" = "Opponent", "Opponent" = "Team"),
            suffix = c("_Home", "_Away")) %>% 
  select(ID:pG_ensrf_Home, pG_ensrf_Away, ptt0.5_Home:ptt3.5_Away, -Home_or_Away_Away) %>% 
  rename(Home = Team,
         Away = Opponent) %>% 
  filter(Home_or_Away_Home == "Home") %>% 
  select(-Home_or_Away_Home)

doubles <- metrics_df
doubles$outcome_gbm <- predict(outcome_gbm, metrics_df, type = "prob")
doubles$outcome_pls <- predict(outcome_pls, metrics_df, type = "prob")
doubles$outcome_xgb <- predict(outcome_xgb, metrics_df, type = "prob")
doubles$minus1_gbm <- predict(minus1_gbm, metrics_df, type = "prob")
doubles$minus1_pls <- predict(minus1_pls, metrics_df, type = "prob")
doubles$minus1_xgb <- predict(minus1_xgb, metrics_df, type = "prob")
doubles$minus1.5_gbm <- predict(minus2.5_gbm, metrics_df, type = "prob")
doubles$minus1.5_pls <- predict(minus2.5_pls, metrics_df, type = "prob")
doubles$minus1.5_xgb <- predict(minus2.5_xgb, metrics_df, type = "prob")
doubles$minus2_gbm <- predict(minus2_gbm, metrics_df, type = "prob")
doubles$minus2_pls <- predict(minus2_pls, metrics_df, type = "prob")
doubles$minus2_xgb <- predict(minus2_xgb, metrics_df, type = "prob")
doubles$minus2.5_gbm <- predict(minus2.5_gbm, metrics_df, type = "prob")
doubles$minus2.5_pls <- predict(minus2.5_pls, metrics_df, type = "prob")
doubles$minus2.5_xgb <- predict(minus2.5_xgb, metrics_df, type = "prob")
doubles$minus3_gbm <- predict(minus3_gbm, metrics_df, type = "prob")
doubles$minus3_pls <- predict(minus3_pls, metrics_df, type = "prob")
doubles$minus3_xgb <- predict(minus3_xgb, metrics_df, type = "prob")
doubles$minus3.5_gbm <- predict(minus3.5_gbm, metrics_df, type = "prob")
doubles$minus3.5_pls <- predict(minus3.5_pls, metrics_df, type = "prob")
doubles$minus3.5_xgb <- predict(minus3.5_xgb, metrics_df, type = "prob")
doubles$plus1_gbm <- predict(plus1_gbm, metrics_df, type = "prob")
doubles$plus1_pls <- predict(plus1_pls, metrics_df, type = "prob")
doubles$plus1_xgb <- predict(plus1_xgb, metrics_df, type = "prob")
doubles$plus1.5_gbm <- predict(plus2.5_gbm, metrics_df, type = "prob")
doubles$plus1.5_pls <- predict(plus2.5_pls, metrics_df, type = "prob")
doubles$plus1.5_xgb <- predict(plus2.5_xgb, metrics_df, type = "prob")
doubles$plus2_gbm <- predict(plus2_gbm, metrics_df, type = "prob")
doubles$plus2_pls <- predict(plus2_pls, metrics_df, type = "prob")
doubles$plus2_xgb <- predict(plus2_xgb, metrics_df, type = "prob")
doubles$plus2.5_gbm <- predict(plus2.5_gbm, metrics_df, type = "prob")
doubles$plus2.5_pls <- predict(plus2.5_pls, metrics_df, type = "prob")
doubles$plus2.5_xgb <- predict(plus2.5_xgb, metrics_df, type = "prob")
doubles$plus3_gbm <- predict(plus3_gbm, metrics_df, type = "prob")
doubles$plus3_pls <- predict(plus3_pls, metrics_df, type = "prob")
doubles$plus3_xgb <- predict(plus3_xgb, metrics_df, type = "prob")
doubles$plus3.5_gbm <- predict(plus3.5_gbm, metrics_df, type = "prob")
doubles$plus3.5_pls <- predict(plus3.5_pls, metrics_df, type = "prob")
doubles$plus3.5_xgb <- predict(plus3.5_xgb, metrics_df, type = "prob")
doubles$total1.5_gbm <- predict(total2.5_gbm, metrics_df, type = "prob")
doubles$total1.5_pls <- predict(total2.5_pls, metrics_df, type = "prob")
doubles$total1.5_xgb <- predict(total2.5_xgb, metrics_df, type = "prob")
doubles$total2_gbm <- predict(total2_gbm, metrics_df, type = "prob")
doubles$total2_pls <- predict(total2_pls, metrics_df, type = "prob")
doubles$total2_xgb <- predict(total2_xgb, metrics_df, type = "prob")
doubles$total2.5_gbm <- predict(total2.5_gbm, metrics_df, type = "prob")
doubles$total2.5_pls <- predict(total2.5_pls, metrics_df, type = "prob")
doubles$total2.5_xgb <- predict(total2.5_xgb, metrics_df, type = "prob")
doubles$total3_gbm <- predict(total3_gbm, metrics_df, type = "prob")
doubles$total3_pls <- predict(total3_pls, metrics_df, type = "prob")
doubles$total3_xgb <- predict(total3_xgb, metrics_df, type = "prob")
doubles$total3.5_gbm <- predict(total3.5_gbm, metrics_df, type = "prob")
doubles$total3.5_pls <- predict(total3.5_pls, metrics_df, type = "prob")
doubles$total3.5_xgb <- predict(total3.5_xgb, metrics_df, type = "prob")
doubles$total4_gbm <- predict(total4_gbm, metrics_df, type = "prob")
doubles$total4_pls <- predict(total4_pls, metrics_df, type = "prob")
doubles$total4_xgb <- predict(total4_xgb, metrics_df, type = "prob")
doubles$total4.5_gbm <- predict(total4.5_gbm, metrics_df, type = "prob")
doubles$total4.5_pls <- predict(total4.5_pls, metrics_df, type = "prob")
doubles$total4.5_xgb <- predict(total4.5_xgb, metrics_df, type = "prob")
doubles$BTTS_gbm <- predict(BTTS_gbm, metrics_df, type = "prob")
doubles$BTTS_pls <- predict(BTTS_pls, metrics_df, type = "prob")
doubles$BTTS_xgb <- predict(BTTS_xgb, metrics_df, type = "prob")
doubles <- doubles %>% 
  mutate(poutcome = (outcome_gbm + outcome_pls + outcome_xgb) / 3,
         pminus1 = (minus1_gbm + minus1_pls + minus1_xgb) / 3,
         pminus1.5 = (minus1.5_gbm + minus1.5_pls + minus1.5_xgb) / 3,
         pminus2 = (minus2_gbm + minus2_pls + minus2_xgb) / 3,
         pminus2.5 = (minus2.5_gbm + minus2.5_pls + minus2.5_xgb) / 3,
         pminus3 = (minus3_gbm + minus3_pls + minus3_xgb) / 3,
         pminus3.5 = (minus3.5_gbm + minus3.5_pls + minus3.5_xgb) / 3,
         pplus1 = (plus1_gbm + plus1_pls + plus1_xgb) / 3,
         pplus1.5 = (plus1.5_gbm + plus1.5_pls + plus1.5_xgb) / 3,
         pplus2 = (plus2_gbm + plus2_pls + plus2_xgb) / 3,
         pplus2.5 = (plus2.5_gbm + plus2.5_pls + plus2.5_xgb) / 3,
         pplus3 = (plus3_gbm + plus3_pls + plus3_xgb) / 3,
         pplus3.5 = (plus3.5_gbm + plus3.5_pls + plus3.5_xgb) / 3,
         ptotal1.5 = (total1.5_gbm + total1.5_pls + total1.5_xgb) / 3,
         ptotal2 = (total2_gbm + total2_pls + total2_xgb) / 3,
         ptotal2.5 = (total2.5_gbm + total2.5_pls + total2.5_xgb) / 3,
         ptotal3 = (total3_gbm + total3_pls + total3_xgb) / 3,
         ptotal3.5 = (total3.5_gbm + total3.5_pls + total3.5_xgb) / 3,
         ptotal4 = (total4_gbm + total4_pls + total4_xgb) / 3,
         ptotal4.5 = (total4.5_gbm + total4.5_pls + total4.5_xgb) / 3,
         pBTTS = (BTTS_gbm + BTTS_pls + BTTS_xgb) / 3)

doubles2 <- doubles %>% 
  select(ID:Opponent, poutcome:pBTTS) %>% 
  rename(Home = Team,
         Away = Opponent)

predsDF <- doubles2 %>% 
  left_join(singles2)

upcoming <- left_join(bovada_odds, predsDF,
                      by = c("gamedate" = "Date", "HomeTeam" = "Home", "AwayTeam" = "Away")) %>%
  select(ID, gamedate, Day, Time, League, HomeTeam:bet_type, HOY.Odds, HOY.SpreadTotal,
         D.Odds, AUN.Odds, AUN.SpreadTotal, poutcome:ptt3.5_Away)

simulate_game <- function(homeScorePred, awayScorePred, homeORaway = c("home", "away"), max_score = 10) {
  score_matrix = dpois(0:max_score, homeScorePred) %o% dpois(0:max_score, awayScorePred)
  if_else(homeORaway == "home", 
          sum(score_matrix[lower.tri(score_matrix)]), 
          sum(score_matrix[upper.tri(score_matrix)]))
}

simulate_spread <- function(homeScorePred, awayScorePred, homeORaway = c("home", "away"), spread, max_score = 10) {
  score_matrix = dpois(0:max_score, homeScorePred) %o% dpois(0:max_score, awayScorePred)
  colnames(score_matrix) = 0:max_score
  rownames(score_matrix) = 0:max_score
  score_matrix = rownames_to_column(as.data.frame(score_matrix)) %>% reshape2::melt(id = "rowname")
  colnames(score_matrix) = c("home_score", "away_score", "prob")
  score_matrix = mutate(score_matrix,
                        home_score = as.numeric(home_score),
                        away_score = as.numeric(away_score) - 1,
                        home_margin = away_score - home_score,
                        away_margin = home_score - away_score)
  if_else(homeORaway == "home",
          sum(subset(score_matrix, home_margin < spread)$prob),
          sum(subset(score_matrix, away_margin < spread)$prob))
}

simulate_total <- function(homeScorePred, awayScorePred, overORunder = c("over", "under"), ou_total, max_score = 10) {
  score_matrix = dpois(0:max_score, homeScorePred) %o% dpois(0:max_score, awayScorePred)
  colnames(score_matrix) = 0:max_score
  rownames(score_matrix) = 0:max_score
  score_matrix = rownames_to_column(as.data.frame(score_matrix)) %>% reshape2::melt(id = "rowname")
  colnames(score_matrix) = c("home_score", "away_score", "prob")
  score_matrix = mutate(score_matrix,
                        home_score = as.numeric(home_score),
                        away_score = as.numeric(away_score) - 1,
                        total_score = home_score + away_score)
  if_else(overORunder == "over",
          sum(subset(score_matrix, total_score > ou_total)$prob),
          sum(subset(score_matrix, total_score < ou_total)$prob))
}

simulate_team_total <- function(homeScorePred, awayScorePred, homeORaway = c("home", "away"), 
                                overORunder = c("over", "under"), team_total, max_score = 10) {
  score_prob = dpois(0:max_score, if_else(homeORaway == "away", awayScorePred, homeScorePred))
  score = 0:max_score
  score_matrix = cbind(score, score_prob) %>% as.data.frame()
  if_else(overORunder == "over",
          sum(subset(score_matrix, score > team_total)$score_prob),
          sum(subset(score_matrix, score < team_total)$score_prob))
  
}

bets <- mutate(upcoming,
               HOY_ImpliedOdds = if_else(HOY.Odds > 0, 100 / (HOY.Odds + 100), abs(HOY.Odds) / (abs(HOY.Odds) + 100)),
               AUN_ImpliedOdds = if_else(AUN.Odds > 0, 100 / (AUN.Odds + 100), abs(AUN.Odds) / (abs(AUN.Odds) + 100)),
               D_ImpliedOdds = if_else(D.Odds > 0, 100 / (D.Odds + 100), abs(D.Odds) / (abs(D.Odds) + 100))) %>% 
  rename(Home_pred = pG_ensrf_Home,
         Away_pred = pG_ensrf_Away)

bets2 <- bets %>%
  rowwise() %>%
  dplyr::mutate(HOY_ProjOdds1 = case_when(bet_type == "3-Way Moneyline" | bet_type == "Draw No Bet" ~ simulate_game(Home_pred, Away_pred, "home"),
                                  grepl("Spread", bet_type) ~ simulate_spread(Home_pred, Away_pred, "home", spread = HOY.SpreadTotal),
                                  bet_type == "Total" | grepl("Alternate Total", bet_type) ~ simulate_total(Home_pred, Away_pred, "over", ou_total = HOY.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "home", "over", team_total = HOY.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "away", "over", team_total = HOY.SpreadTotal),
                                  bet_type == "Both Teams To Score" ~ simulate_team_total(Home_pred, Away_pred, "home", "over", team_total = 0.5) * simulate_team_total(Home_pred, Away_pred, "away", "over", team_total = 0.5),
                                  TRUE ~ 0),
         AUN_ProjOdds1 = case_when(bet_type == "3-Way Moneyline" | bet_type == "Draw No Bet" ~ simulate_game(Home_pred, Away_pred, "away"),
                                  grepl("Spread", bet_type) ~ simulate_spread(Home_pred, Away_pred, "away", spread = AUN.SpreadTotal),
                                  bet_type == "Total" | grepl("Alternate Total", bet_type) ~ simulate_total(Home_pred, Away_pred, "under", ou_total = AUN.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "home", "under", team_total = AUN.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "away", "under", team_total = AUN.SpreadTotal),
                                  bet_type == "Both Teams To Score" ~ 1 - HOY_ProjOdds1,
                                  TRUE ~ 0)) %>%
  dplyr::mutate(D_ProjOdds1 = case_when(bet_type == "3-Way Moneyline" ~ 1 - HOY_ProjOdds1 - AUN_ProjOdds1))

bets3 <- bets2 %>% 
  mutate(HOY_ProjOdds2 = case_when(bet_type == "3-Way Moneyline" | 
                                     bet_type == "Draw No Bet" | 
                                     (grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 0.0) ~ poutcome$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -0.5 ~ poutcome$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -1 ~ pminus1$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -1.5 ~ pminus1.5$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -2 ~ pminus2$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -2.5 ~ pminus2.5$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -3 ~ pminus3$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -3.5 ~ pminus3.5$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 0.5 ~ poutcome$Win + poutcome$Draw,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 1 ~ pplus1$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 1.5 ~ pplus1.5$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 2 ~ pplus2$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 2.5 ~ pplus2.5$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 3 ~ pplus3$Win,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 3.5 ~ pplus3.5$Win,
                                   (bet_type == "Total" | 
                                     grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 1.5 ~ ptotal1.5$Over,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 2 ~ ptotal2$Over,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 2.5 ~ ptotal2.5$Over,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 3 ~ ptotal3$Over,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 3.5 ~ ptotal3.5$Over,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 4 ~ ptotal4$Over,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 4.5 ~ ptotal4.5$Over,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 0.5 ~ ptt0.5_Home$Over,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 1 ~ ptt1_Home$Over,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 1.5 ~ ptt1.5_Home$Over,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 2 ~ ptt2_Home$Over,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 2.5 ~ ptt2.5_Home$Over,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 3 ~ ptt3_Home$Over,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 3.5 ~ ptt3.5_Home$Over,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 0.5 ~ ptt0.5_Away$Over,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 1 ~ ptt1_Away$Over,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 1.5 ~ ptt1.5_Away$Over,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 2 ~ ptt2_Away$Over,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 2.5 ~ ptt2.5_Away$Over,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 3 ~ ptt3_Away$Over,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 3.5 ~ ptt3.5_Away$Over,
                                   bet_type == "Both Teams To Score" ~ pBTTS$Yes),
         AUN_ProjOdds2 = case_when(bet_type == "3-Way Moneyline" | 
                                     bet_type == "Draw No Bet" | 
                                     (grepl("Spread", bet_type) &
                                        HOY.SpreadTotal == 0.0) ~ poutcome$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -0.5 ~ poutcome$Lose + poutcome$Draw,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -1 ~ pminus1$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -1.5 ~ pminus1.5$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -2 ~ pminus2$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -2.5 ~ pminus2.5$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -3 ~ pminus3$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == -3.5 ~ pminus3.5$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 0.5 ~ poutcome$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 1 ~ pplus1$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 1.5 ~ pplus1.5$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 2 ~ pplus2$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 2.5 ~ pplus2.5$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 3 ~ pplus3$Lose,
                                   grepl("Spread", bet_type) &
                                     HOY.SpreadTotal == 3.5 ~ pplus3.5$Lose,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 1.5 ~ ptotal1.5$Under,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 2 ~ ptotal2$Under,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 2.5 ~ ptotal2.5$Under,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 3 ~ ptotal3$Under,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 3.5 ~ ptotal3.5$Under,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 4 ~ ptotal4$Under,
                                   (bet_type == "Total" | 
                                      grepl("Alternate Total", bet_type)) &
                                     HOY.SpreadTotal == 4.5 ~ ptotal4.5$Under,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 0.5 ~ ptt0.5_Home$Under,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 1 ~ ptt1_Home$Under,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 1.5 ~ ptt1.5_Home$Under,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 2 ~ ptt2_Home$Under,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 2.5 ~ ptt2.5_Home$Under,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 3 ~ ptt3_Home$Under,
                                   grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) &
                                     HOY.SpreadTotal == 3.5 ~ ptt3.5_Home$Under,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 0.5 ~ ptt0.5_Away$Under,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 1 ~ ptt1_Away$Under,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 1.5 ~ ptt1.5_Away$Under,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 2 ~ ptt2_Away$Under,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 2.5 ~ ptt2.5_Away$Under,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 3 ~ ptt3_Away$Under,
                                   grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) &
                                     HOY.SpreadTotal == 3.5 ~ ptt3.5_Away$Under,
                                   bet_type == "Both Teams To Score" ~ pBTTS$No)) %>% 
  mutate(D_ProjOdds2 = case_when(bet_type == "3-Way Moneyline" ~ 1 - HOY_ProjOdds2 - AUN_ProjOdds2))

bets4 <- bets3 %>% 
  select(-(poutcome:pBTTS), -(ptt0.5_Home:ptt3.5_Away)) %>% 
  mutate(HOY_ProjOdds = (HOY_ProjOdds1 + HOY_ProjOdds2) / 2,
         AUN_ProjOdds = (AUN_ProjOdds1 + AUN_ProjOdds2) / 2,
         D_ProjOdds = (D_ProjOdds1 + D_ProjOdds2) / 2,
         HOY.Odds_Diff = HOY_ProjOdds - HOY_ImpliedOdds,
         AUN.Odds_Diff = AUN_ProjOdds - AUN_ImpliedOdds,
         D.Odds_Diff = D_ProjOdds - D_ImpliedOdds,
         Pick = if_else(bet_type == "3-Way Moneyline",
                        case_when(AUN.Odds_Diff > HOY.Odds_Diff & AUN.Odds_Diff > D.Odds_Diff ~ AwayTeam,
                                  HOY.Odds_Diff > AUN.Odds_Diff & HOY.Odds_Diff > D.Odds_Diff ~ HomeTeam,
                                  D.Odds_Diff > AUN.Odds_Diff & D.Odds_Diff > HOY.Odds_Diff ~ "Draw"),
                        if_else(AUN.Odds_Diff > HOY.Odds_Diff, AwayTeam, HomeTeam))) %>%
  mutate(Pick_Odds = case_when(Pick == AwayTeam ~ AUN.Odds,
                               Pick == HomeTeam ~ HOY.Odds,
                               Pick == "Draw" ~ D.Odds),
         Pick_SpreadTotal = case_when(Pick == AwayTeam ~ AUN.SpreadTotal,
                                      Pick == HomeTeam ~ HOY.SpreadTotal),
         Pick_WinProb = case_when(Pick == AwayTeam ~ AUN_ProjOdds,
                                  Pick == HomeTeam ~ HOY_ProjOdds,
                                  Pick == "Draw" ~ D_ProjOdds),
         Pick_LoseProb = case_when(bet_type == "3-Way Moneyline" ~
                                     case_when(Pick == AwayTeam ~ HOY_ProjOdds + D_ProjOdds,
                                               Pick == HomeTeam ~ AUN_ProjOdds + D_ProjOdds,
                                               Pick == "Draw" ~ AUN_ProjOdds + HOY_ProjOdds),
                                   TRUE ~ case_when(Pick == AwayTeam ~ HOY_ProjOdds,
                                                    TRUE ~ AUN_ProjOdds)),
         Pick_Edge = case_when(Pick == AwayTeam ~ AUN.Odds_Diff,
                               Pick == HomeTeam ~ HOY.Odds_Diff,
                               Pick == "Draw" ~ D.Odds_Diff),
         Fract_Odds = (100 / abs(Pick_Odds))^if_else(Pick_Odds < 0, 1, -1),
         Kelly_Criteria = (Pick_WinProb * (Fract_Odds + 1) - 1) / Fract_Odds,
         EV = case_when(Pick_Odds < 0 ~ (10*Pick_WinProb) - ((abs(Pick_Odds)/10)*Pick_LoseProb),
                        TRUE ~ ((Pick_Odds/10*Pick_WinProb) - (10*Pick_LoseProb)))) %>%
  mutate(Pick = case_when(grepl("Total", bet_type) ~ if_else(Pick == HomeTeam, "Over", "Under"),
                          bet_type == "Both Teams To Score" ~ if_else(Pick == HomeTeam, "Yes", "No"),
                          TRUE ~ Pick),
         bet_type_full = bet_type,
         bet_type = case_when(bet_type == "3-Way Moneyline" ~ "ML",
                              bet_type == "Goal Spread" ~ "Spread",
                              grepl("Alternate Spread", bet_type) ~ "Alt Spread",
                              bet_type == "Total" ~ "Total",
                              grepl("Alternate Total", bet_type) ~ "Alt Total",
                              bet_type == "Both Teams To Score" ~ "BTTS",
                              grepl("Total Goals O/U", bet_type) & (Pick_Odds > -250) & (Pick_Odds < 210) ~ "TT",
                              grepl("Total Goals O/U", bet_type) & ((Pick_Odds <= -250) | (Pick_Odds >= 210)) ~ "Alt TT",
                              bet_type == "Draw No Bet" ~ "Draw No Bet",
                              TRUE ~ "Other"),
         Machine_Odds = round(if_else(Pick_WinProb < 0.5, (100 / Pick_WinProb) - 100, -1 * (100 * Pick_WinProb) / (1 - Pick_WinProb)),0),
         KC_tier = as.factor(round_any(Kelly_Criteria, 0.05, floor)),
         run_timestamp = Sys.time()) %>% 
  filter(!is.na(Pick))

write.csv(bets4, "Soccer Machine/upcoming_bets.csv", row.names = FALSE, na = "")

## Analyze performance

history <- readRDS("Soccer Machine/PicksHistory.rds") %>% 
  bind_rows(bets4) %>% 
  distinct()

saveRDS(history, "Soccer Machine/PicksHistory.rds")

history <- inner_join(history, scores, by = c("gamedate" = "Date", "HomeTeam" = "Home",
                                              "AwayTeam" = "Away", "Day" = "Day",
                                              "Time" = "Time", "League" = "League")) %>%
  mutate(Total_Score = Home_Score + Away_Score)

#history$Pick_SpreadTotal <- as.numeric(history$Pick_SpreadTotal)

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
         bets = 1,
         Total = "Total")

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
  print(n=40)

types %>%
  filter(Kelly_Criteria >= 0) %>%
  group_by(League) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter(Kelly_Criteria >= 0.1 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 5 & !(League %in% c("UCL", "UEL"))) %>%
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
  filter(Kelly_Criteria >= 0.1 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 6 & !(League %in% c("UCL", "UEL"))) %>%
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

# types %>%
#   filter((bet_type == "TT"
#           #| bet_type == "Alt Total"
#           #| bet_type == "Draw No Bet"
#           )
#          & League.x == "EPL"
#          ) %>%
#   group_by(KC_tier) %>%
#   #group_by(WinProb_tier) %>%
#   #group_by(Odds_tier) %>%
#   dplyr::summarise(HitRate = mean(Pick_Correct),
#                    bets = sum(bets),
#                    Flat_Profit = sum(Units),
#                    Kelly_Profit = sum(Kelly_Profit)) %>%
#   mutate(Units_per_bet = Flat_Profit / bets) %>%
#   print(n=40)
# 
# ## Create email tables

email_table_1 <- types %>%
  filter(Kelly_Criteria >= 0 & !(League %in% c("UCL", "UEL"))) %>%
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

email_table_3 <- types %>%
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

df_html_3 <- print(xtable(email_table_3), type = "html", print.results = FALSE)

email_table_3b <- types %>% 
  filter(Kelly_Criteria >= 0.1 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 6 & !(League %in% c("UCL", "UEL"))) %>%
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

df_html_3b <- print(xtable(email_table_3b), type = "html", print.results = FALSE)

email_table_4 <- types %>%
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

df_html_4 <- print(xtable(email_table_4), type = "html", print.results = FALSE)

email_table_5 <- types %>%
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

df_html_5 <- print(xtable(email_table_5), type = "html", print.results = FALSE)

graph_data <- types %>%
  ungroup() %>% 
  select(gamedate, Units, Kelly_Profit) %>%
  rename(Flat_Profit = Units) %>% 
  melt("gamedate", c("Flat_Profit", "Kelly_Profit")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

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

plot2 <- ggplot(graph_data2) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Game Date",
    y = "Cumulative Profit",
    color = ""
  ) +
  theme_minimal()

plot_html2 <- add_ggplot(plot_object = plot2)

graph_data3 <- types %>%
  filter(Pick_Odds >= 100 & Kelly_Criteria >= 0.1 & Kelly_Criteria < 0.4 & EV >= 2 & EV < 6 & !(League %in% c("UCL", "UEL"))) %>%
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
  rename(Flat_Profit = Units) %>% 
  melt("gamedate", c("Flat_Profit", "Kelly_Profit")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

plot3 <- ggplot(graph_data3) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Game Date",
    y = "Cumulative Profit",
    color = ""
  ) +
  theme_minimal()

plot_html3 <- add_ggplot(plot_object = plot3)

## Send an email

Outlook <- COMCreate("Outlook.Application")

Email = Outlook$CreateItem(0)
Email[["to"]] = paste("dnolen@smu.edu", "jorler@smu.edu", "asnolen@crimson.ua.edu", "jamestodd425@gmail.com", sep = ";", collapse = NULL)
#Email[["to"]] = "dnolen@smu.edu"
Email[["subject"]] = paste0("Soccer Machine Picks: ", Sys.Date())
Email[["HTMLbody"]] = sprintf("
The Machine's picks for upcoming soccer matches are in! The Machine currently offers picks for the Big 5 European Leagues plus MLS. Previously, for the analysis below I was using the odds for each bet that were closest to the date of the game. I've switched it to use the second closest timestamp, which improved the results. In general, you will get worse odds as you get closer to kickoff. It might be helpful to make these bets a day or two ahead of the game.
</p><br></p>
Another change that I've made - for bet sizing on bets with negaitve odds, I used to use the amount needed to win one unit as the wager. For example, if the odds are -200, I would use 2 units as the wager to profit 1 unit. I have changed that so that now each wager is 1 unit no matter what. So a 1 unit bet with odds of -200 would profit 0.5 units. For the Kelly bets, I still multiply the units by 100. So a 1 unit wager would turn into a $10 wager if the Kelly Criteria is 0.1. This change lowers our risk of losing big when betting on favorites (but also lowers the amount of profit we can make on those bets).
</p><br></p>
The attached document contains all of the pertinent betting information for the upcoming matches. Good luck!
</p><br></p>
Below are the results for each bet type:
</p><br></p>
%s
</p><br></p>
Below are the results for each league:
</p><br></p>
%s
</p><br></p>
Bets with a KC of at least 0.1 and less than 0.4 and EV of at least 2 and less than 5 have had the best performance so far. This is expected. If there is not a big difference between the Machine projection and the odds then there is not much of an edge. On the other end, if the Machine projections are way off from the odds, then it's more likely that the oddsmakers know something that the Machine doesn't, rather than the other way around. Here are those results:
</p><br></p>
%s
</p><br></p>
Here are the same results as above, but filtered to only include the top bet for each game (which is much more realistic than betting everything):
</p><br></p>
%s
</p><br></p>
Results grouped by KC rounded to the nearest 0.05:
</p><br></p>
%s
</p><br></p>
Results grouped by EV rounded to the nearest 1:
</p><br></p>
%s
</p><br></p>
Here is the flat betting vs. Kelly betting results over time:
</p><br></p>
%s
</p><br></p>
Here are the same results if you only place bets where the odds are even or better. It might make sense to only bet on plus money or even odds. Maybe I will start to only place negative odds bets if I parlay two or three of them.
</p><br></p>
%s
</p><br></p>
Here are the same results again only using the top bet for each game:
</p><br></p>
%s
</p><br></p>
NOTE: Consider this a BETA version. If you feel like reviewing this, please let me know if anything looks off.
</p><br></p>
ANOTHER NOTE: I filtered out bets where the odds are less than -250 from this analysis. I suggest never betting juice higher than -250. I also filtered out bets that have less than a 30%% win probability. I don't believe it is worth it to bet on these huge underdogs. I also removed any bets on Alternate Totals set at 1.5 goals. The Machine almost always suggests an under bet on those. The performance on those bets was not very good, and that is also a very lame bet. Nobody likes cheering for a game to be that low scoring.
", df_html_1, df_html_2, df_html_3, df_html_3b, df_html_4, df_html_5, plot_html, plot_html2, plot_html3)
Email[["attachments"]]$Add("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine/Soccer Machine/upcoming_bets.csv")

Email$Send()

overallEnd <- Sys.time()
paste("Entire script took",overallEnd - overallStart,attr(overallEnd - overallStart,"units"))





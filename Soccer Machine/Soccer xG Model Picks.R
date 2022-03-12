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
library("tidyverse")

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
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
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
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
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
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

ger_odds <- fromJSON(ger_url) %>%
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
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

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
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
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
                              grepl("Total Goals O/U - ", bet_type) ~ paste0(bet_type, " - ", abs(as.numeric(SpreadTotal))),
                              TRUE ~ bet_type)) %>%
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
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
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
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
  melt(id.vars = c("gamedate", "HomeTeam", "AwayTeam", "bet_type", "type")) %>%
  mutate(name = paste0(type, ".", variable)) %>%
  select(-type, -variable) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(gamedate + HomeTeam + AwayTeam + bet_type ~ name, fun.aggregate = mean) %>%
  mutate(AUN.Odds = round(AUN.Odds, 0),
         HOY.Odds = round(HOY.Odds, 0),
         D.Odds = round(D.Odds, 0)) %>%
  filter((AUN.Odds >= 100 | AUN.Odds <= -100) & (HOY.Odds >= 100 | HOY.Odds <= -100)) %>%
  select(-D.SpreadTotal)

bovada_odds <- bind_rows(epl_odds, esp_odds, ger_odds, ita_odds, 
                     fra_odds, mls_odds, ucl_odds, uel_odds)

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
  filter(is.na(Wk) == FALSE) %>%
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
  filter(Wk != "" & Wk != "Wk") %>%
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
  filter(!is.na(Date) & (!is.na(xG) | Date >= Sys.Date())) %>% 
  replace(is.na(.), 0) %>% 
  arrange(Date, Time, League, ID) %>% 
  # mutate(Team = trimws(case_when(League %in% c('UCL', 'UEL') & Home_or_Away == "Home" ~ substr(Team, 1, nchar(Team)-3),
  #                                League %in% c('UCL', 'UEL') & Home_or_Away == "Away" ~ substr(Team, 4, nchar(Team)),
  #                                TRUE ~ Team), which = c("both"))) %>% 
  group_by(Team, League, Season, Home_or_Away) %>% 
  mutate(SplitxG = cumsum(xG) - xG,
         SplitxGA = cumsum(xGA) - xGA,
         SplitGoals = cumsum(Goals) - Goals,
         SplitGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SplitGP = cumsum(case_when(Date < today ~ 1, TRUE ~ 0)),
         SplitxG_roll4 = (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4,
         SplitxGA_roll4 = (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4,
         SplitGoals_roll4 = (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4,
         SplitGoalsAllowed_roll4 = (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4) %>% 
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
  replace(is.na(.), 0)

metrics_df <- metrics %>% 
  left_join(metrics, by = c("ID" = "ID", "Date" = "Date", "Day" = "Day", "Time" = "Time",
                            "League" = "League", "Season" = "Season", "Opponent" = "Team"),
            suffix = c("", "_Opp")) %>% 
  filter(Home_or_Away == "Home") %>%
  select(-(Home_or_Away:GoalsAllowed), -(Opponent_Opp:GoalsAllowed_Opp))

## Join Fixture data with odds data

upcoming <- left_join(bovada_odds, metrics,
                      by = c("gamedate" = "Date", "HomeTeam" = "Home", "AwayTeam" = "Away")) %>%
  select(gamedate, Day, Time, League, HomeTeam:bet_type, HOY.Odds, HOY.SpreadTotal,
         D.Odds, AUN.Odds, AUN.SpreadTotal, HomexGHome:AwayxGAAway)

## Make Predictions

h2o.init(nthreads = -1)

Home_pred <- predict(h2o.loadModel("C:\\Users\\danie\\Desktop\\Sports Stuff\\Soccer Betting\\StackedEnsemble_AllModels_AutoML_20211115_234023"),
                     as.h2o(upcoming)) %>% as.data.frame()
colnames(Home_pred) <- "Home_pred"

Away_pred <- predict(h2o.loadModel("C:\\Users\\danie\\Desktop\\Sports Stuff\\Soccer Betting\\StackedEnsemble_AllModels_AutoML_20211116_001854"),
                     as.h2o(upcoming)) %>% as.data.frame()
colnames(Away_pred) <- "Away_pred"

## Join predictions to odds data

upcoming <- upcoming %>% cbind(Home_pred, Away_pred)

## Functions to simulate outcome probabilities

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
  score_matrix = rownames_to_column(as.data.frame(score_matrix)) %>% melt(id = "rowname")
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
  score_matrix = rownames_to_column(as.data.frame(score_matrix)) %>% melt(id = "rowname")
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

## Add in implied odds, projected odds, edge, kelly criteria
bets <- mutate(upcoming,
               HOY_ImpliedOdds = if_else(HOY.Odds > 0, 100 / (HOY.Odds + 100), abs(HOY.Odds) / (abs(HOY.Odds) + 100)),
               AUN_ImpliedOdds = if_else(AUN.Odds > 0, 100 / (AUN.Odds + 100), abs(AUN.Odds) / (abs(AUN.Odds) + 100)),
               D_ImpliedOdds = if_else(D.Odds > 0, 100 / (D.Odds + 100), abs(D.Odds) / (abs(D.Odds) + 100)))

bets <- bets %>%
  rowwise() %>%
  dplyr::mutate(HOY_ProjOdds = case_when(bet_type == "3-Way Moneyline" | bet_type == "Draw No Bet" ~ simulate_game(Home_pred, Away_pred, "home"),
                                  grepl("Spread", bet_type) ~ simulate_spread(Home_pred, Away_pred, "home", spread = HOY.SpreadTotal),
                                  bet_type == "Total" | grepl("Alternate Total", bet_type) ~ simulate_total(Home_pred, Away_pred, "over", ou_total = HOY.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "home", "over", team_total = HOY.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "away", "over", team_total = HOY.SpreadTotal),
                                  bet_type == "Both Teams To Score" ~ simulate_team_total(Home_pred, Away_pred, "home", "over", team_total = 0.5) * simulate_team_total(Home_pred, Away_pred, "away", "over", team_total = 0.5),
                                  TRUE ~ 0),
         AUN_ProjOdds = case_when(bet_type == "3-Way Moneyline" | bet_type == "Draw No Bet" ~ simulate_game(Home_pred, Away_pred, "away"),
                                  grepl("Spread", bet_type) ~ simulate_spread(Home_pred, Away_pred, "away", spread = AUN.SpreadTotal),
                                  bet_type == "Total" | grepl("Alternate Total", bet_type) ~ simulate_total(Home_pred, Away_pred, "under", ou_total = AUN.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", HomeTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "home", "under", team_total = AUN.SpreadTotal),
                                  grepl(paste0("Total Goals O/U - ", AwayTeam), bet_type) ~ simulate_team_total(Home_pred, Away_pred, "away", "under", team_total = AUN.SpreadTotal),
                                  bet_type == "Both Teams To Score" ~ 1 - HOY_ProjOdds,
                                  TRUE ~ 0)) %>%
  dplyr::mutate(D_ProjOdds = case_when(bet_type == "3-Way Moneyline" ~ 1 - HOY_ProjOdds - AUN_ProjOdds))

bets <- mutate(bets,
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
         Pick_Edge = case_when(Pick == AwayTeam ~ AUN.Odds_Diff,
                               Pick == HomeTeam ~ HOY.Odds_Diff,
                               Pick == "Draw" ~ D.Odds_Diff),
         Fract_Odds = (100 / abs(Pick_Odds))^if_else(Pick_Odds < 0, 1, -1),
         Kelly_Criteria = (Pick_WinProb * (Fract_Odds + 1) - 1) / Fract_Odds) %>%
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
         KC_tier = as.factor(round_any(Kelly_Criteria, 0.05, floor)))

write.csv(bets, "upcoming_bets.csv", row.names = FALSE)

## Analyze performance

history <- read_excel("Machine Picks History - Soccer.xlsx")

history <- inner_join(history, scores, by = c("gamedate" = "Date", "HomeTeam" = "Home", "AwayTeam" = "Away")) %>%
  mutate(Total_Score = Home_Score + Away_Score)

history$Pick_SpreadTotal <- as.numeric(history$Pick_SpreadTotal)

history <- history %>%
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
         Units = if_else(Pick_Correct == 1, 
                         if_else(Pick_Odds > 0, Pick_Odds / 100, 1),
                         if_else(Pick_Odds > 0, -1, Pick_Odds / 100)),
         Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Half_Kelly_Bet = if_else(Kelly_Criteria < 0.1, 5, Kelly_Criteria * 50),
         Kelly_Profit = Units * Kelly_Bet,
         Half_Kelly_Profit = Units * Half_Kelly_Bet)

write.csv(history, "machine_pa_soccer.csv", row.names = FALSE)

types <- filter(history, Winner != "Push" & Kelly_Criteria > 0 & Pick_Odds >= -250 & Pick_WinProb >= 0.3) %>%
  select(gamedate, League.x, bet_type, Pick_Odds, Pick_WinProb, Fract_Odds, Kelly_Criteria, KC_tier, Pick_Correct, Units)

types %>%
  filter(Kelly_Criteria >= 0 & !(League.x %in% c("UCL", "UEL"))) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1,
         Total = "Total") %>%
  #group_by(Total) %>%
  group_by(bet_type) %>%
  #group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter(Kelly_Criteria >= 0) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1,
         Total = "Total") %>%
  group_by(League.x) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter(Kelly_Criteria >= 0.3 & Kelly_Criteria < 1 & !(League.x %in% c("UCL", "UEL"))) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1,
         Total = "Total") %>%
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
  filter(!(League.x %in% c("UCL", "UEL"))) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  #group_by(bet_type) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

types %>%
  filter((bet_type == "TT"
          #| bet_type == "Alt Total"
          #| bet_type == "Draw No Bet"
          )
         & League.x == "EPL"
         ) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = round_any(Pick_WinProb, 0.05, floor),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  group_by(KC_tier) %>%
  #group_by(WinProb_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

## Create email tables

email_table_1 <- types %>%
  filter(Kelly_Criteria >= 0 & !(League.x %in% c("UCL", "UEL"))) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1,
         Total = "Total") %>%
  #group_by(Total) %>%
  group_by(bet_type) %>%
  #group_by(KC_tier) %>%
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
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1,
         Total = "Total") %>%
  group_by(League.x) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_2 <- print(xtable(email_table_2), type = "html", print.results = FALSE)

email_table_3 <- types %>%
  filter(Kelly_Criteria >= 0.3 & Kelly_Criteria < 1 & !(League.x %in% c("UCL", "UEL"))) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1,
         Total = "Total") %>%
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

email_table_4 <- types %>%
  filter(!(League.x %in% c("UCL", "UEL"))) %>%
  mutate(Kelly_Bet = if_else(Kelly_Criteria < 0.05, 5, Kelly_Criteria * 100),
         Kelly_Profit = Units * Kelly_Bet,
         WinProb_tier = as.factor(round_any(Pick_WinProb, 0.05, floor)),
         Odds_tier = as.factor(round_any(Pick_Odds, 10, floor)),
         bets = 1) %>%
  #group_by(bet_type) %>%
  group_by(KC_tier) %>%
  #group_by(Odds_tier) %>%
  dplyr::summarise(HitRate = mean(Pick_Correct),
                   bets = sum(bets),
                   Flat_Profit = sum(Units),
                   Kelly_Profit = sum(Kelly_Profit)) %>%
  mutate(Units_per_bet = Flat_Profit / bets) %>%
  print(n=40)

df_html_4 <- print(xtable(email_table_4), type = "html", print.results = FALSE)

## Send an email

Outlook <- COMCreate("Outlook.Application")

Email = Outlook$CreateItem(0)
Email[["to"]] = paste("dnolen@smu.edu", "jorler@smu.edu", sep = ";", collapse = NULL)
#Email[["to"]] = "dnolen@smu.edu"
Email[["subject"]] = paste0("Soccer Machine Picks: ", Sys.Date())
Email[["HTMLbody"]] = sprintf("
The Machine's picks for upcoming soccer matches are in! The Machine now has picks for the UEFA Champons League and Europa League. These leagues have much less data to go off of, especially for clubs not from the Big 5 European leagues. Take these picks with a grain of salt. The attached document contains all of the pertinent betting information for the upcoming matches. Good luck!
</p><br></p>
NOTE: Mapping team names correctly is a bitch, especially since Bovada isn't always consistent with how they name teams. If anything looks off, let me know. It probably has something to do with team name mapping.
</p><br></p>
Below are the results for each bet type (excluding UCL & UEL):
</p><br></p>
%s
</p><br></p>
Below are the results for each league:
</p><br></p>
%s
</p><br></p>
Bets with a KC of at least 0.3 (excluding UCL & UEL) have had the best performance so far. Here are their results:
</p><br></p>
%s
</p><br></p>
Below are the results so far, grouped by KC tier (excluding UCL & UEL):
</p><br></p>
%s
</p><br></p>
ANOTHER NOTE: I filtered out bets where the odds are less than -250 from this analysis. I suggest never betting juice higher than -250. I also filtered out bets that have less than a 30%% win probability. I don't believe it is worth it to bet on these huge underdogs.
", df_html_1, df_html_2, df_html_3, df_html_4)
Email[["attachments"]]$Add("C:/Users/danie/Desktop/Sports Stuff/Soccer Betting/upcoming_bets.csv")

Email$Send()

h2o.shutdown()

overallEnd <- Sys.time()
paste("Entire script took",overallEnd - overallStart,attr(overallEnd - overallStart,"units"))





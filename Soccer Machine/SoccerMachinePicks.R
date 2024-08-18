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
library(worldfootballR)
library("tidyverse")
library(zoo)

setwd("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine")

mls_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/north-america/united-states/mls"
epl_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/europe/england/premier-league"
esp_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/europe/spain/la-liga"
ger_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/europe/germany/1-bundesliga"
fra_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/europe/france/ligue-1"
ita_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/europe/italy/serie-a"
champ_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/europe/england/championship"
mex_cl_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/north-america/mexico/liga-mx-clausura"
mex_ap_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/north-america/mexico/liga-mx-apertura"
bra_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/south-america/brazil/brasileirao-serie-a"
ucl_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/international-club/uefa-champions-league"
uel_url <- "https://www.bovada.lv/services/sports/event/v2/events/A/description/soccer/international-club/uefa-europa-league"

loadOdds <- function(url) {
  out <- tryCatch(
    {
      odds <- fromJSON(url) %>%
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
    },
    error=function(e) {
      odds <- data.frame()
    }
  )    
  return(out)
}

epl_odds <- loadOdds(epl_url)
esp_odds <- loadOdds(esp_url)
ger_odds <- loadOdds(ger_url)
ita_odds <- loadOdds(ita_url)
fra_odds <- loadOdds(fra_url)
mls_odds <- loadOdds(mls_url)
champ_odds <- loadOdds(champ_url)
mex_cl_odds <- loadOdds(mex_cl_url)
mex_ap_odds <- loadOdds(mex_ap_url)
bra_odds <- loadOdds(bra_url)
ucl_odds <- loadOdds(ucl_url)
uel_odds <- loadOdds(uel_url)

bovada_odds <- bind_rows(epl_odds, 
                         esp_odds,
                         ger_odds,
                         ita_odds,
                         fra_odds,
                         mls_odds,
                         champ_odds,
                         mex_cl_odds,
                         mex_ap_odds,
                         bra_odds,
                         ucl_odds,
                         uel_odds
                         )

club_names <- read_excel("Soccer Machine/Club Names.xlsx")
# %>%
#   mutate(FBRef = iconv(FBRef, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
#          Bovada = iconv(Bovada, from = 'UTF-8', to = 'ASCII//TRANSLIT'))

bovada_odds <- FindReplace(bovada_odds, Var = "HomeTeam", replaceData = club_names,
                           from = "Bovada", to = "Name")
bovada_odds <- FindReplace(bovada_odds, Var = "AwayTeam", replaceData = club_names,
                           from = "Bovada", to = "Name")

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

mls_23 <- fb_match_results(country = "USA", gender = "M", season_end_year = 2023, tier = "1st") %>% 
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

mls_24 <- fb_match_results(country = "USA", gender = "M", season_end_year = 2024, tier = "1st") %>% 
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

Big5_2223 <- load_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = c(2022,2023), tier = "1st") %>% 
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

Big5_24 <- fb_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"), gender = "M", season_end_year = 2024, tier = "1st") %>% 
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

Champ_23 <- load_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "2nd") %>% 
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
         Season = paste0(Season-1,"-",Season))

Champ_24 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2024, tier = "2nd") %>% 
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
         Season = paste0(Season-1,"-",Season))

Mex_23 <- load_match_results(country = "MEX", gender = "M", season_end_year = 2023, tier = "1st") %>% 
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
         Season = paste0(Season-1,"-",Season))

Mex_24 <- fb_match_results(country = "MEX", gender = "M", season_end_year = 2024, tier = "1st") %>% 
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
         Season = paste0(Season-1,"-",Season))

bra_23 <- fb_match_results(country = "BRA", gender = "M", season_end_year = 2023, tier = "1st") %>% 
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
         League = "Brasileiro Serie A",
         Season = as.character(Season))

bra_24 <- fb_match_results(country = "BRA", gender = "M", season_end_year = 2024, tier = "1st") %>% 
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
         League = "Brasileiro Serie A",
         Season = as.character(Season))

uefa_24 <- fb_match_results(country = c("NED","POR","BEL"), gender = "M", season_end_year = 2024, tier = "1st") %>% 
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
         Season = paste0(Season-1,"-",Season))

ucl_24 <- fb_match_results(country = "",
                        gender = "M",
                        season_end_year = 2024,
                        tier = "",
                        non_dom_league_url = "https://fbref.com/en/comps/8/history/Champions-League-Seasons") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
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
         Season = paste0(Season-1,"-",Season)) %>% 
  mutate(Home = trimws(substr(Home, 1, nchar(Home)-3)),
         Away = trimws(substr(Away, 4, nchar(Away))))

uel_24 <- fb_match_results(country = "",
                        gender = "M",
                        season_end_year = 2024,
                        tier = "",
                        non_dom_league_url = "https://fbref.com/en/comps/19/history/Europa-League-Seasons") %>% 
  select(Day, Date, Time, Home, Home_xG, HomeGoals, AwayGoals, Away_xG, Away,
         Competition_Name, Season_End_Year) %>% 
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
         Season = paste0(Season-1,"-",Season)) %>% 
  mutate(Home = trimws(substr(Home, 1, nchar(Home)-3)),
         Away = trimws(substr(Away, 4, nchar(Away))))

fixtures <- rbind(Big5_2223
                  , Big5_24
                  , mls_22
                  , mls_23
                  , mls_24
                  , Champ_23
                  , Champ_24
                  , Mex_23
                  , Mex_24
                  , bra_23
                  , bra_24
                  , uefa_24
                  , ucl_24
                  , uel_24
                  ) 
# %>% 
#   mutate(Home = iconv(Home, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
#          Away = iconv(Away, from = 'UTF-8', to = 'ASCII//TRANSLIT'))

  
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
  filter(!League %in% c('UEFA Champions League', 'UEFA Europa League',
                        'Eredivisie', 'Primeira Liga', 'Belgian Pro League')) %>%
  group_by(Team, League, Season, Home_or_Away) %>% 
  mutate(SplitxG = cumsum(xG) - xG,
         SplitxGA = cumsum(xGA) - xGA,
         SplitGoals = cumsum(Goals) - Goals,
         SplitGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SplitGP = cumsum(case_when(Date < today ~ 1, TRUE ~ 0)),
         SplitGP = case_when(Date < today ~ SplitGP - 1, TRUE ~ SplitGP),
         SplitxG_roll4 = case_when(SplitGP == 1 ~ lag(xG,1),
                                   SplitGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                   SplitGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/3,
                                   TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
         SplitxGA_roll4 = case_when(SplitGP == 1 ~ lag(xGA,1),
                                    SplitGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                    SplitGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/3,
                                    TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
         SplitGoals_roll4 = case_when(SplitGP == 1 ~ lag(Goals,1),
                                      SplitGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                      SplitGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3))/3,
                                      TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals)+lag(Goals,4))/4),
         SplitGoalsAllowed_roll4 = case_when(SplitGP == 1 ~ lag(GoalsAllowed,1),
                                             SplitGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                             SplitGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3))/3,
                                             TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>%
  group_by(Team, League, Season) %>% 
  mutate(SeasonxG = cumsum(xG) - xG,
         SeasonxGA = cumsum(xGA) - xGA,
         SeasonGoals = cumsum(Goals) - Goals,
         SeasonGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
         SeasonGP = cumsum(case_when(Date < today ~ 1, TRUE ~ 0)),
         SeasonGP = case_when(Date < today ~ SeasonGP - 1, TRUE ~ SeasonGP),
         SeasonxG_roll4 = case_when(SeasonGP == 1 ~ lag(xG,1),
                                    SeasonGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                    SeasonGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/4,
                                    TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
         SeasonxGA_roll4 = case_when(SeasonGP == 1 ~ lag(xGA,1),
                                     SeasonGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                     SeasonGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/4,
                                     TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
         SeasonGoals_roll4 = case_when(SeasonGP == 1 ~ lag(Goals,1),
                                       SeasonGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                       SeasonGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals))/4,
                                       TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4),
         SeasonGoalsAllowed_roll4 = case_when(SeasonGP == 1 ~ lag(GoalsAllowed,1),
                                              SeasonGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                              SeasonGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed))/4,
                                              TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>% 
  ungroup() %>% 
  # mutate(SplitGP = case_when(Date < today ~ SplitGP - 1, TRUE ~ SplitGP),
  #        SeasonGP = case_when(Date < today ~ SeasonGP - 1, TRUE ~ SeasonGP)) %>%
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
  filter(Home_or_Away == "Home" & Date >= today & SplitGP > 0 & SplitGP_Opp > 0) %>%
  select(-(Home_or_Away:GoalsAllowed), -(Opponent_Opp:GoalsAllowed_Opp))

metrics_tt <- metrics %>% 
  left_join(metrics, by = c("ID" = "ID", "Date" = "Date", "Day" = "Day", "Time" = "Time",
                            "League" = "League", "Season" = "Season", "Opponent" = "Team"),
            suffix = c("", "_Opp")) %>%
  filter(Date >= today & SplitGP > 0 & SplitGP_Opp > 0) %>% 
  select(-(xG:GoalsAllowed), -(Opponent_Opp:GoalsAllowed_Opp))

cup_fixtures <- bind_rows(home, away) %>% 
  arrange(Date, Time, League, ID) %>%
  filter(League %in% c('UEFA Champions League', 'UEFA Europa League'))

domestic_leagues <- bind_rows(home, away) %>% 
  filter(!is.na(Date) & (!is.na(xG) | Date >= Sys.Date())) %>% 
  replace(is.na(.), 0) %>% 
  arrange(Date, Time, League, ID) %>%
  filter(League %in% c('EPL', 'La Liga', 'Bundesliga', 'Ligue 1', 'Serie A',
                       'Eredivisie', 'Primeira Liga', 'Belgian Pro League'
                       )) %>%
  group_by(Team, League, Season, Home_or_Away) %>% 
  dplyr::mutate(SplitxG = cumsum(xG) - xG,
                SplitxGA = cumsum(xGA) - xGA,
                SplitGoals = cumsum(Goals) - Goals,
                SplitGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
                SplitGP = row_number() - 1,
                SplitxG_roll4 = case_when(SplitGP == 1 ~ lag(xG,1),
                                          SplitGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                          SplitGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/3,
                                          TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
                SplitxGA_roll4 = case_when(SplitGP == 1 ~ lag(xGA,1),
                                           SplitGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                           SplitGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/3,
                                           TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
                SplitGoals_roll4 = case_when(SplitGP == 1 ~ lag(Goals,1),
                                             SplitGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                             SplitGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3))/3,
                                             TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals)+lag(Goals,4))/4),
                SplitGoalsAllowed_roll4 = case_when(SplitGP == 1 ~ lag(GoalsAllowed,1),
                                                    SplitGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                                    SplitGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3))/3,
                                                    TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>% 
  group_by(Team, League, Season) %>% 
  dplyr::mutate(SeasonxG = cumsum(xG) - xG,
                SeasonxGA = cumsum(xGA) - xGA,
                SeasonGoals = cumsum(Goals) - Goals,
                SeasonGoalsAllowed = cumsum(GoalsAllowed) - GoalsAllowed,
                SeasonGP = row_number() - 1,
                SeasonxG_roll4 = case_when(SeasonGP == 1 ~ lag(xG,1),
                                           SeasonGP == 2 ~ (lag(xG,1)+lag(xG,2))/2,
                                           SeasonGP == 3 ~ (lag(xG,1)+lag(xG,2)+lag(xG,3))/4,
                                           TRUE ~ (lag(xG,1)+lag(xG,2)+lag(xG,3)+lag(xG,4))/4),
                SeasonxGA_roll4 = case_when(SeasonGP == 1 ~ lag(xGA,1),
                                            SeasonGP == 2 ~ (lag(xGA,1)+lag(xGA,2))/2,
                                            SeasonGP == 3 ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3))/4,
                                            TRUE ~ (lag(xGA,1)+lag(xGA,2)+lag(xGA,3)+lag(xGA,4))/4),
                SeasonGoals_roll4 = case_when(SeasonGP == 1 ~ lag(Goals,1),
                                              SeasonGP == 2 ~ (lag(Goals,1)+lag(Goals,2))/2,
                                              SeasonGP == 3 ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals))/4,
                                              TRUE ~ (lag(Goals,1)+lag(Goals,2)+lag(Goals,3)+lag(Goals,4))/4),
                SeasonGoalsAllowed_roll4 = case_when(SeasonGP == 1 ~ lag(GoalsAllowed,1),
                                                     SeasonGP == 2 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2))/2,
                                                     SeasonGP == 3 ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed))/4,
                                                     TRUE ~ (lag(GoalsAllowed,1)+lag(GoalsAllowed,2)+lag(GoalsAllowed,3)+lag(GoalsAllowed,4))/4)) %>% 
  ungroup() %>% 
  dplyr::mutate(SplitxG = SplitxG / SplitGP,
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

team_leagues <- domestic_leagues %>% 
  distinct(Team, League) %>% 
  rename(DomesticLeague = League)

uefa_metrics <- domestic_leagues %>% 
  bind_rows(cup_fixtures) %>% 
  arrange(Date, Time, League, ID) %>% 
  group_by(Team, Home_or_Away) %>% 
  mutate(SplitxG = ifelse(is.na(SplitxG), na.locf(SplitxG, fromLast = TRUE), SplitxG),
         SplitxGA = ifelse(is.na(SplitxGA), na.locf(SplitxGA, fromLast = TRUE), SplitxGA),
         SplitGoals = ifelse(is.na(SplitGoals), na.locf(SplitGoals, fromLast = TRUE), SplitGoals),
         SplitGoalsAllowed = ifelse(is.na(SplitGoalsAllowed), na.locf(SplitGoalsAllowed, fromLast = TRUE), SplitGoalsAllowed),
         SplitGP = ifelse(is.na(SplitGP), na.locf(SplitGP, fromLast = TRUE), SplitGP),
         SplitxG_roll4 = ifelse(is.na(SplitxG_roll4), na.locf(SplitxG_roll4, fromLast = TRUE), SplitxG_roll4),
         SplitxGA_roll4 = ifelse(is.na(SplitxGA_roll4), na.locf(SplitxGA_roll4, fromLast = TRUE), SplitxGA_roll4),
         SplitGoals_roll4 = ifelse(is.na(SplitGoals_roll4), na.locf(SplitGoals_roll4, fromLast = TRUE), SplitGoals_roll4),
         SplitGoalsAllowed_roll4 = ifelse(is.na(SplitGoalsAllowed_roll4), na.locf(SplitGoalsAllowed_roll4, fromLast = TRUE), SplitGoalsAllowed_roll4),
         SeasonxG = ifelse(is.na(SeasonxG), na.locf(SeasonxG, fromLast = TRUE), SeasonxG),
         SeasonxGA = ifelse(is.na(SeasonxGA), na.locf(SeasonxGA, fromLast = TRUE), SeasonxGA),
         SeasonGoals = ifelse(is.na(SeasonGoals), na.locf(SeasonGoals, fromLast = TRUE), SeasonGoals),
         SeasonGoalsAllowed = ifelse(is.na(SeasonGoalsAllowed), na.locf(SeasonGoalsAllowed, fromLast = TRUE), SeasonGoalsAllowed),
         SeasonGP = ifelse(is.na(SeasonGP), na.locf(SeasonGP, fromLast = TRUE), SeasonGP),
         SeasonxG_roll4 = ifelse(is.na(SeasonxG_roll4), na.locf(SeasonxG_roll4, fromLast = TRUE), SeasonxG_roll4),
         SeasonxGA_roll4 = ifelse(is.na(SeasonxGA_roll4), na.locf(SeasonxGA_roll4, fromLast = TRUE), SeasonxGA_roll4),
         SeasonGoals_roll4 = ifelse(is.na(SeasonGoals_roll4), na.locf(SeasonGoals_roll4, fromLast = TRUE), SeasonGoals_roll4),
         SeasonGoalsAllowed_roll4 = ifelse(is.na(SeasonGoalsAllowed_roll4), na.locf(SeasonGoalsAllowed_roll4, fromLast = TRUE), SeasonGoalsAllowed_roll4)) %>%
  ungroup() %>% 
  left_join(team_leagues) %>% 
  filter(League %in% c('UEFA Champions League', 'UEFA Europa League')) %>% 
  mutate(Season = case_when(Season == '2023-2024' ~ '2022-2023',
                            TRUE ~ Season))

cup_metrics_df <- uefa_metrics %>% 
  left_join(uefa_metrics, by = c("ID" = "ID", "Date" = "Date", "Day" = "Day", "Time" = "Time",
                            "League" = "League", "Season" = "Season", "Opponent" = "Team"),
            suffix = c("", "_Opp")) %>% 
  filter(Home_or_Away == "Home" & Date >= today & SplitGP > 0 & SplitGP_Opp > 0) %>%
  select(-(Home_or_Away:GoalsAllowed), -(Opponent_Opp:GoalsAllowed_Opp))

cup_metrics_tt <- uefa_metrics %>% 
  left_join(uefa_metrics, by = c("ID" = "ID", "Date" = "Date", "Day" = "Day", "Time" = "Time",
                            "League" = "League", "Season" = "Season", "Opponent" = "Team"),
            suffix = c("", "_Opp")) %>%
  filter(Date >= today & SplitGP > 0 & SplitGP_Opp > 0) %>% 
  select(-(xG:GoalsAllowed), -(Opponent_Opp:GoalsAllowed_Opp))

gbm_reg <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_gbm.rds")
cub_reg <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_cub.rds")
rf_reg <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_rf.rds")
ctree_reg <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_ctree.rds")
pls_reg <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_pls.rds")
lm_reg <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_lm.rds")
outcome_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_gbm.rds")
outcome_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_pls.rds")
outcome_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_xgb.rds")
minus1_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_gbm.rds")
minus1_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_pls.rds")
minus1_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_xgb.rds")
minus1.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_gbm.rds")
minus1.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_pls.rds")
minus1.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_xgb.rds")
minus2_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_gbm.rds")
minus2_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_pls.rds")
minus2_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_xgb.rds")
minus2.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_gbm.rds")
minus2.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_pls.rds")
minus2.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_xgb.rds")
minus3_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_gbm.rds")
minus3_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_pls.rds")
minus3_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_xgb.rds")
minus3.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_gbm.rds")
minus3.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_pls.rds")
minus3.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_xgb.rds")
plus1_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_gbm.rds")
plus1_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_pls.rds")
plus1_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_xgb.rds")
plus1.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_gbm.rds")
plus1.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_pls.rds")
plus1.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_xgb.rds")
plus2_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_gbm.rds")
plus2_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_pls.rds")
plus2_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_xgb.rds")
plus2.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_gbm.rds")
plus2.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_pls.rds")
plus2.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_xgb.rds")
plus3_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_gbm.rds")
plus3_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_pls.rds")
plus3_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_xgb.rds")
plus3.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_gbm.rds")
plus3.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_pls.rds")
plus3.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_xgb.rds")
total1.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_gbm.rds")
total1.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_pls.rds")
total1.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_xgb.rds")
total2_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_gbm.rds")
total2_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_pls.rds")
total2_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_xgb.rds")
total2.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_gbm.rds")
total2.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_pls.rds")
total2.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_xgb.rds")
total3_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_gbm.rds")
total3_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_pls.rds")
total3_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_xgb.rds")
total3.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_gbm.rds")
total3.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_pls.rds")
total3.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_xgb.rds")
total4_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_gbm.rds")
total4_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_pls.rds")
total4_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_xgb.rds")
total4.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_gbm.rds")
total4.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_pls.rds")
total4.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_xgb.rds")
BTTS_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_gbm.rds")
BTTS_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_pls.rds")
BTTS_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_xgb.rds")
tt0.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_gbm.rds")
tt0.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_pls.rds")
tt0.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_xgb.rds")
tt1_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_gbm.rds")
tt1_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_pls.rds")
tt1_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_xgb.rds")
tt1.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_gbm.rds")
tt1.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_pls.rds")
tt1.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_xgb.rds")
tt2_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_gbm.rds")
tt2_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_pls.rds")
tt2_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_xgb.rds")
tt2.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_gbm.rds")
tt2.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_pls.rds")
tt2.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_xgb.rds")
tt3_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_gbm.rds")
tt3_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_pls.rds")
tt3_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_xgb.rds")
tt3.5_gbm <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_gbm.rds")
tt3.5_pls <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_pls.rds")
tt3.5_xgb <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_xgb.rds")

gbm_reg_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_gbm_cups.rds")
cub_reg_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_cub_cups.rds")
rf_reg_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_rf_cups.rds")
ctree_reg_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_ctree_cups.rds")
pls_reg_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_pls_cups.rds")
lm_reg_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/train_lm_cups.rds")
outcome_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_gbm_cups.rds")
outcome_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_pls_cups.rds")
outcome_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/outcome_xgb_cups.rds")
minus1_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_gbm_cups.rds")
minus1_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_pls_cups.rds")
minus1_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1_xgb_cups.rds")
minus1.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_gbm_cups.rds")
minus1.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_pls_cups.rds")
minus1.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus1.5_xgb_cups.rds")
minus2_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_gbm_cups.rds")
minus2_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_pls_cups.rds")
minus2_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2_xgb_cups.rds")
minus2.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_gbm_cups.rds")
minus2.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_pls_cups.rds")
minus2.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus2.5_xgb_cups.rds")
minus3_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_gbm_cups.rds")
minus3_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_pls_cups.rds")
minus3_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3_xgb_cups.rds")
minus3.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_gbm_cups.rds")
minus3.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_pls_cups.rds")
minus3.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/minus3.5_xgb_cups.rds")
plus1_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_gbm_cups.rds")
plus1_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_pls_cups.rds")
plus1_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1_xgb_cups.rds")
plus1.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_gbm_cups.rds")
plus1.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_pls_cups.rds")
plus1.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus1.5_xgb_cups.rds")
plus2_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_gbm_cups.rds")
plus2_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_pls_cups.rds")
plus2_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2_xgb_cups.rds")
plus2.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_gbm_cups.rds")
plus2.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_pls_cups.rds")
plus2.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus2.5_xgb_cups.rds")
plus3_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_gbm_cups.rds")
plus3_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_pls_cups.rds")
plus3_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3_xgb_cups.rds")
plus3.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_gbm_cups.rds")
plus3.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_pls_cups.rds")
plus3.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/plus3.5_xgb_cups.rds")
total1.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_gbm_cups.rds")
total1.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_pls_cups.rds")
total1.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total1.5_xgb_cups.rds")
total2_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_gbm_cups.rds")
total2_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_pls_cups.rds")
total2_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2_xgb_cups.rds")
total2.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_gbm_cups.rds")
total2.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_pls_cups.rds")
total2.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total2.5_xgb_cups.rds")
total3_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_gbm_cups.rds")
total3_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_pls_cups.rds")
total3_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3_xgb_cups.rds")
total3.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_gbm_cups.rds")
total3.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_pls_cups.rds")
total3.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total3.5_xgb_cups.rds")
total4_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_gbm_cups.rds")
total4_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_pls_cups.rds")
total4_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4_xgb_cups.rds")
total4.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_gbm_cups.rds")
total4.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_pls_cups.rds")
total4.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/total4.5_xgb_cups.rds")
BTTS_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_gbm_cups.rds")
BTTS_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_pls_cups.rds")
BTTS_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/BTTS_xgb_cups.rds")
tt0.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_gbm_cups.rds")
tt0.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_pls_cups.rds")
tt0.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt0.5_xgb_cups.rds")
tt1_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_gbm_cups.rds")
tt1_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_pls_cups.rds")
tt1_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1_xgb_cups.rds")
tt1.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_gbm_cups.rds")
tt1.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_pls_cups.rds")
tt1.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt1.5_xgb_cups.rds")
tt2_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_gbm_cups.rds")
tt2_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_pls_cups.rds")
tt2_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2_xgb_cups.rds")
tt2.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_gbm_cups.rds")
tt2.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_pls_cups.rds")
tt2.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt2.5_xgb_cups.rds")
tt3_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_gbm_cups.rds")
tt3_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_pls_cups.rds")
tt3_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3_xgb_cups.rds")
tt3.5_gbm_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_gbm_cups.rds")
tt3.5_pls_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_pls_cups.rds")
tt3.5_xgb_cups <- readRDS("C:/Users/danie/Desktop/SportsStuff/TheMachine/SoccerModels/tt3.5_xgb_cups.rds")

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

# cup_singles <- cup_metrics_tt
# cup_singles$pG_gbm <- predict(gbm_reg_cups, cup_metrics_tt)
# cup_singles$pG_cub <- predict(cub_reg_cups, cup_metrics_tt)
# cup_singles$pG_rf <- predict(rf_reg_cups, cup_metrics_tt)
# cup_singles$pG_ctree <- predict(ctree_reg_cups, cup_metrics_tt)
# cup_singles$pG_pls <- predict(pls_reg_cups, cup_metrics_tt)
# cup_singles$pG_lm <- predict(lm_reg_cups, cup_metrics_tt)
# cup_singles$tt0.5_gbm <- predict(tt0.5_gbm_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt0.5_pls <- predict(tt0.5_pls_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt0.5_xgb <- predict(tt0.5_xgb_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt1_gbm <- predict(tt1_gbm_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt1_pls <- predict(tt1_pls_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt1_xgb <- predict(tt1_xgb_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt1.5_gbm <- predict(tt1.5_gbm_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt1.5_pls <- predict(tt1.5_pls_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt1.5_xgb <- predict(tt1.5_xgb_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt2_gbm <- predict(tt2_gbm_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt2_pls <- predict(tt2_pls_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt2_xgb <- predict(tt2_xgb_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt2.5_gbm <- predict(tt2.5_gbm_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt2.5_pls <- predict(tt2.5_pls_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt2.5_xgb <- predict(tt2.5_xgb_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt3_gbm <- predict(tt3_gbm_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt3_pls <- predict(tt3_pls_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt3_xgb <- predict(tt3_xgb_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt3.5_gbm <- predict(tt3.5_gbm_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt3.5_pls <- predict(tt3.5_pls_cups, cup_metrics_tt, type = "prob")
# cup_singles$tt3.5_xgb <- predict(tt3.5_xgb_cups, cup_metrics_tt, type = "prob")
# cup_singles <- cup_singles %>% 
#   mutate(pG_ens = (pG_gbm + pG_cub + pG_rf + pG_ctree + pG_pls + pG_lm) / 6,
#          pG_ensrf = (pG_ens + pG_rf) / 2,
#          ptt0.5 = (tt0.5_gbm + tt0.5_pls + tt0.5_xgb) / 3,
#          ptt1 = (tt1_gbm + tt1_pls + tt1_xgb) / 3,
#          ptt1.5 = (tt1.5_gbm + tt1.5_pls + tt1.5_xgb) / 3,
#          ptt2 = (tt2_gbm + tt2_pls + tt2_xgb) / 3,
#          ptt2.5 = (tt2.5_gbm + tt2.5_pls + tt2.5_xgb) / 3,
#          ptt3 = (tt3_gbm + tt3_pls + tt3_xgb) / 3,
#          ptt3.5 = (tt3.5_gbm + tt3.5_pls + tt3.5_xgb) / 3)
# 
# cup_singles2 <- cup_singles %>% 
#   select(ID:Home_or_Away, pG_ensrf:ptt3.5) %>%
#   left_join(cup_singles %>% 
#               select(ID:Home_or_Away, pG_ensrf:ptt3.5),
#             by = c("ID", "Date", "Day", "Time", "League", "Season", "Team" = "Opponent", "Opponent" = "Team"),
#             suffix = c("_Home", "_Away")) %>% 
#   select(ID:pG_ensrf_Home, pG_ensrf_Away, ptt0.5_Home:ptt3.5_Away, -Home_or_Away_Away) %>% 
#   rename(Home = Team,
#          Away = Opponent) %>% 
#   filter(Home_or_Away_Home == "Home") %>% 
#   select(-Home_or_Away_Home)
# 
# cup_doubles <- cup_metrics_df
# cup_doubles$outcome_gbm <- predict(outcome_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$outcome_pls <- predict(outcome_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$outcome_xgb <- predict(outcome_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus1_gbm <- predict(minus1_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus1_pls <- predict(minus1_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus1_xgb <- predict(minus1_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus1.5_gbm <- predict(minus2.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus1.5_pls <- predict(minus2.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus1.5_xgb <- predict(minus2.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus2_gbm <- predict(minus2_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus2_pls <- predict(minus2_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus2_xgb <- predict(minus2_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus2.5_gbm <- predict(minus2.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus2.5_pls <- predict(minus2.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus2.5_xgb <- predict(minus2.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus3_gbm <- predict(minus3_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus3_pls <- predict(minus3_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus3_xgb <- predict(minus3_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus3.5_gbm <- predict(minus3.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus3.5_pls <- predict(minus3.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$minus3.5_xgb <- predict(minus3.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus1_gbm <- predict(plus1_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus1_pls <- predict(plus1_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus1_xgb <- predict(plus1_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus1.5_gbm <- predict(plus2.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus1.5_pls <- predict(plus2.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus1.5_xgb <- predict(plus2.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus2_gbm <- predict(plus2_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus2_pls <- predict(plus2_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus2_xgb <- predict(plus2_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus2.5_gbm <- predict(plus2.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus2.5_pls <- predict(plus2.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus2.5_xgb <- predict(plus2.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus3_gbm <- predict(plus3_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus3_pls <- predict(plus3_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus3_xgb <- predict(plus3_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus3.5_gbm <- predict(plus3.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus3.5_pls <- predict(plus3.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$plus3.5_xgb <- predict(plus3.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$total1.5_gbm <- predict(total2.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$total1.5_pls <- predict(total2.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$total1.5_xgb <- predict(total2.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$total2_gbm <- predict(total2_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$total2_pls <- predict(total2_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$total2_xgb <- predict(total2_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$total2.5_gbm <- predict(total2.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$total2.5_pls <- predict(total2.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$total2.5_xgb <- predict(total2.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$total3_gbm <- predict(total3_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$total3_pls <- predict(total3_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$total3_xgb <- predict(total3_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$total3.5_gbm <- predict(total3.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$total3.5_pls <- predict(total3.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$total3.5_xgb <- predict(total3.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$total4_gbm <- predict(total4_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$total4_pls <- predict(total4_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$total4_xgb <- predict(total4_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$total4.5_gbm <- predict(total4.5_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$total4.5_pls <- predict(total4.5_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$total4.5_xgb <- predict(total4.5_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles$BTTS_gbm <- predict(BTTS_gbm_cups, cup_metrics_df, type = "prob")
# cup_doubles$BTTS_pls <- predict(BTTS_pls_cups, cup_metrics_df, type = "prob")
# cup_doubles$BTTS_xgb <- predict(BTTS_xgb_cups, cup_metrics_df, type = "prob")
# cup_doubles <- cup_doubles %>% 
#   mutate(poutcome = (outcome_gbm + outcome_pls + outcome_xgb) / 3,
#          pminus1 = (minus1_gbm + minus1_pls + minus1_xgb) / 3,
#          pminus1.5 = (minus1.5_gbm + minus1.5_pls + minus1.5_xgb) / 3,
#          pminus2 = (minus2_gbm + minus2_pls + minus2_xgb) / 3,
#          pminus2.5 = (minus2.5_gbm + minus2.5_pls + minus2.5_xgb) / 3,
#          pminus3 = (minus3_gbm + minus3_pls + minus3_xgb) / 3,
#          pminus3.5 = (minus3.5_gbm + minus3.5_pls + minus3.5_xgb) / 3,
#          pplus1 = (plus1_gbm + plus1_pls + plus1_xgb) / 3,
#          pplus1.5 = (plus1.5_gbm + plus1.5_pls + plus1.5_xgb) / 3,
#          pplus2 = (plus2_gbm + plus2_pls + plus2_xgb) / 3,
#          pplus2.5 = (plus2.5_gbm + plus2.5_pls + plus2.5_xgb) / 3,
#          pplus3 = (plus3_gbm + plus3_pls + plus3_xgb) / 3,
#          pplus3.5 = (plus3.5_gbm + plus3.5_pls + plus3.5_xgb) / 3,
#          ptotal1.5 = (total1.5_gbm + total1.5_pls + total1.5_xgb) / 3,
#          ptotal2 = (total2_gbm + total2_pls + total2_xgb) / 3,
#          ptotal2.5 = (total2.5_gbm + total2.5_pls + total2.5_xgb) / 3,
#          ptotal3 = (total3_gbm + total3_pls + total3_xgb) / 3,
#          ptotal3.5 = (total3.5_gbm + total3.5_pls + total3.5_xgb) / 3,
#          ptotal4 = (total4_gbm + total4_pls + total4_xgb) / 3,
#          ptotal4.5 = (total4.5_gbm + total4.5_pls + total4.5_xgb) / 3,
#          pBTTS = (BTTS_gbm + BTTS_pls + BTTS_xgb) / 3)
# 
# cup_doubles2 <- cup_doubles %>% 
#   select(ID:Opponent, poutcome:pBTTS) %>% 
#   rename(Home = Team,
#          Away = Opponent)

predsDF <- doubles2 %>% 
  left_join(singles2) #%>% 
  # bind_rows(cup_doubles2 %>% 
  #             left_join(cup_singles2))

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
         Pushable = case_when(Pick_WinProb + Pick_LoseProb < 0.999 ~ 'Y',
                              TRUE ~ 'N'),
         Kelly_Criteria = if_else(Pushable == 'Y',
                                  ((Pick_WinProb / (Pick_WinProb + Pick_LoseProb)) * (Fract_Odds + 1) - 1) / Fract_Odds,
                                  (Pick_WinProb * (Fract_Odds + 1) - 1) / Fract_Odds),
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
         Machine_Odds = round(if_else(Pushable == 'Y',
                                      if_else((Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2)) < 0.5,
                                              (100 / (Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2))) - 100,
                                              -1 * (100 * (Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2))) / 
                                                (1 - (Pick_WinProb + ((1-Pick_WinProb-Pick_LoseProb)/2)))),
                                      if_else(Pick_WinProb < 0.5,
                                              (100 / Pick_WinProb) - 100,
                                              -1 * (100 * Pick_WinProb) / (1 - Pick_WinProb))),
                              0),
         KC_tier = as.factor(round_any(Kelly_Criteria, 0.05, floor)),
         run_timestamp = Sys.time()) %>% 
  filter(!is.na(Pick)) %>% 
  select(-Pushable)

write.csv(bets4, "Soccer Machine/upcoming_bets.csv", row.names = FALSE, na = "")

## Analyze performance

history <- readRDS("Soccer Machine/PicksHistory_current_season.rds") %>% 
  # mutate(HomeTeam = iconv(HomeTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
  #        AwayTeam = iconv(AwayTeam, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  bind_rows(bets4) %>% 
  distinct()

saveRDS(history, "Soccer Machine/PicksHistory_current_season.rds")

history <- inner_join(history, scores, by = c("gamedate" = "Date", "HomeTeam" = "Home",
                                              "AwayTeam" = "Away", "Day" = "Day",
                                              "Time" = "Time", "League" = "League")) %>%
  mutate(Total_Score = Home_Score + Away_Score)

#history$Pick_SpreadTotal <- as.numeric(history$Pick_SpreadTotal)

history2 <- history %>%
  # rowwise() %>%
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

saveRDS(history2, "Soccer Machine/PicksHistory_Outcomes_current_season.rds")

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
         Kelly_Criteria, EV, KC_tier, Pick_Correct, Units, SGP_eligible, run_timestamp) %>%
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
                                   `Bet Grade` == 'B' ~ case_when(run_timestamp > as.Date('2023-04-06') ~ 0,
                                                                  TRUE ~ 1),
                                   # `Bet Grade` == 'C' ~ 0.5,
                                   TRUE ~ 0),
         `Graded Profit` = Units*`Graded Risk`,
         `Bet Grade` = factor(`Bet Grade`, levels = c('A+', 'A', 'B', 'C', 'D', 'F')))

season_starts <- fixtures %>% 
  group_by(League, Season) %>% 
  summarise(start_date = min(Date)) %>% 
  group_by(League) %>% 
  arrange(desc(start_date)) %>% 
  mutate(row_num = row_number())%>% 
  ungroup() %>% 
  filter(row_num == 1) %>% 
  select(League, start_date)

grades <- types %>% 
  filter(Pick_Odds > -180) %>%
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
  left_join(season_starts)

## Create email tables (New)

email_table_1 <- grades %>%
  filter(gamedate >= start_date) %>%
  group_by(`Bet Grade`,
           # Side_or_Total,
           # Odds = case_when(Pick_Odds > 0 ~ "Positive",
           #                  TRUE ~ "Negative"),
           `Suggested Wager` = case_when(`Bet Grade` == 'A+' ~ '2 units',
                                         `Bet Grade` == 'A' ~ '1.5 unit',
                                         # `Bet Grade` == 'B' ~ '1 unit',
                                         # `Bet Grade` == 'C' ~ '0.5 units',
                                         TRUE ~ 'No bet')) %>% 
  dplyr::summarise(`Hit Rate` = mean(Pick_Correct),
                   `Average Implied Odds` = mean(if_else(Pick_Odds > 0, 100 / (Pick_Odds + 100), abs(Pick_Odds) / (abs(Pick_Odds) + 100))),
                   `Average Odds` = if_else(`Average Implied Odds` < 0.5,
                                            (100 / `Average Implied Odds`) - 100,
                                            -1 * (100 * `Average Implied Odds`) / (1 - `Average Implied Odds`)),
                   Bets = sum(bets),
                   `Profit: 1 Unit Wagers` = sum(Units),
                   `Profit: Suggested Wagers` = sum(`Graded Profit`)) %>%
  # mutate(ROI = `Profit: 1 Unit Wagers` / Bets) %>%
  mutate(ROI = case_when(`Bet Grade` %in% c('B', 'C', 'D', 'F') ~ 0,
                         TRUE ~ `Profit: 1 Unit Wagers` / Bets)) %>%
  mutate(`Hit Rate` = paste0(round_any(`Hit Rate`*100, 1), '%'),
         `Average Odds` = as.integer(round_any(`Average Odds`,1)),
         `Average Implied Odds` = paste0(round_any(`Average Implied Odds`*100, 1), '%'),
         Bets = formatC(Bets, format="d", big.mark=","),
         `Profit: 1 Unit Wagers` = paste0(round_any(`Profit: 1 Unit Wagers`, 0.1), ' units'),
         `Profit: Suggested Wagers` = paste0(round_any(`Profit: Suggested Wagers`, 0.1), ' units'),
         ROI = paste0(round_any(ROI*100, 0.01), '%')) %>% 
  select(`Bet Grade`, `Suggested Wager`, `Hit Rate`, `Average Odds`, `Average Implied Odds`, Bets, `Profit: 1 Unit Wagers`, `Profit: Suggested Wagers`, ROI) %>% 
  print(n=150)

df_html_1 <- print(xtable(email_table_1), type = "html", print.results = FALSE)

email_table_1b <- grades %>%
  filter(gamedate >= start_date &
           `Bet Grade` %in% c('A+', 'A')) %>%
  group_by(League) %>% 
  dplyr::summarise(`Hit Rate` = mean(Pick_Correct),
                   `Average Implied Odds` = mean(if_else(Pick_Odds > 0, 100 / (Pick_Odds + 100), abs(Pick_Odds) / (abs(Pick_Odds) + 100))),
                   `Average Odds` = if_else(`Average Implied Odds` < 0.5,
                                            (100 / `Average Implied Odds`) - 100,
                                            -1 * (100 * `Average Implied Odds`) / (1 - `Average Implied Odds`)),
                   Bets = sum(bets),
                   `Profit: 1 Unit Wagers` = sum(Units),
                   `Profit: Suggested Wagers` = sum(`Graded Profit`)) %>%
  mutate(ROI = `Profit: 1 Unit Wagers` / Bets) %>%
  mutate(`Hit Rate` = paste0(round_any(`Hit Rate`*100, 1), '%'),
         `Average Odds` = as.integer(round_any(`Average Odds`,1)),
         `Average Implied Odds` = paste0(round_any(`Average Implied Odds`*100, 1), '%'),
         Bets = formatC(Bets, format="d", big.mark=","),
         `Profit: 1 Unit Wagers` = paste0(round_any(`Profit: 1 Unit Wagers`, 0.1), ' units'),
         `Profit: Suggested Wagers` = paste0(round_any(`Profit: Suggested Wagers`, 0.1), ' units'),
         ROI = paste0(round_any(ROI*100, 0.01), '%')) %>% 
  select(League, `Hit Rate`, `Average Odds`, `Average Implied Odds`, Bets, `Profit: 1 Unit Wagers`, `Profit: Suggested Wagers`, ROI) %>% 
  print(n=40)

df_html_1b <- print(xtable(email_table_1b), type = "html", print.results = FALSE)

email_table_2 <- scores %>% 
  group_by(League) %>% 
  summarise(`Most Recent Game Data` = as.character(max(Date)))%>% 
  print()

df_html_2 <- print(xtable(email_table_2), type = "html", print.results = FALSE)

plot_data <- grades %>% 
  filter(gamedate >= start_date & `Bet Grade` %in% c('A+', 'A')) %>% 
  select(gamedate, Units, `Graded Profit`) %>% 
  rename(`Profit: 1 Unit Wagers` = Units,
         `Profit: Suggested Wagers` = `Graded Profit`) %>% 
  melt("gamedate", c("Profit: 1 Unit Wagers", "Profit: Suggested Wagers")) %>% 
  group_by(gamedate, variable) %>% 
  summarise(value = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(cumulative_value = cumsum(value))

plot <- ggplot(plot_data) +
  aes(x = gamedate, y = cumulative_value, colour = variable) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Game Date",
    y = "Cumulative Profit (Units)",
    color = ""
  ) +
  theme(legend.position = "bottom")

plot_html <- add_ggplot(plot_object = plot)

bets_table <- #read.csv("Soccer Machine/upcoming_bets.csv") %>%
  bets4 %>%
  mutate(gamedate = as.Date(gamedate)) %>%
  mutate(EV_tier = round_any(EV, 1, floor),
         Side_or_Total = case_when(bet_type %in% c('Alt Spread', 'Draw No Bet', 'ML', 'Spread') ~ "Side",
                                   TRUE ~ "Total"),
         `Bet Grade` = case_when(Side_or_Total == 'Side' ~ case_when(KC_tier %in% c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65,
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
         `Bet Grade` = factor(`Bet Grade`, levels = c('A+', 'A', 'B', 'C', 'D', 'F'))) %>% 
  filter(Kelly_Criteria > 0 &
           Pick_Odds > -180 &
           Pick_WinProb >= 0.3 &
           bet_type_full != 'Alternate Total - 1.5' &
           gamedate <= Sys.Date() + 4) %>%
  arrange(gamedate, ID, desc(Kelly_Criteria)) %>% 
  group_by(ID) %>% 
  mutate(KC_Rank = row_number()) %>% 
  arrange(gamedate, ID, desc(EV)) %>% 
  group_by(ID) %>% 
  mutate(EV_Rank = row_number(),
         Rank = (KC_Rank + EV_Rank) / 2) %>% 
  arrange(gamedate, ID, Rank) %>% 
  mutate(Final_Rank = row_number()) %>% 
  filter(Final_Rank == 1 & `Bet Grade` %in% c('A+', 'A')) %>%
  arrange(gamedate, desc(Kelly_Criteria)) %>% 
  ungroup() %>% 
  mutate(Pick = case_when(is.na(Pick_SpreadTotal) | Pick_SpreadTotal == 0 ~ paste0(Pick),
                          str_detect(bet_type_full, "Spread") & Pick_SpreadTotal > 0 ~ paste0(Pick, " +", Pick_SpreadTotal),
                          str_detect(bet_type_full, HomeTeam) ~ paste0(HomeTeam, " ", Pick, " ", Pick_SpreadTotal),
                          str_detect(bet_type_full, AwayTeam) ~ paste0(AwayTeam, " ", Pick, " ", Pick_SpreadTotal),
                          TRUE ~ paste0(Pick, " ", Pick_SpreadTotal))) %>% 
  select(gamedate, League, HomeTeam, AwayTeam, bet_type_full, Pick, Pick_Odds, Machine_Odds, `Bet Grade`) %>% 
  mutate(Machine_Odds =  Machine_Odds,
         gamedate = as.character(gamedate)) %>% 
  rename(`Game Date` = gamedate,
         `Home Team` = HomeTeam,
         `Away Team` = AwayTeam,
         `Bet Type` = bet_type_full,
         `Current Pick Odds` = Pick_Odds,
         `Odds Should Be` = Machine_Odds)

bets_table2 <- bind_rows(bets_table
                         ) %>% 
  arrange(`Game Date`, case_when(League == 'EPL' ~ 1,
                                 League == 'La Liga' ~ 2,
                                 League == 'Bundesliga' ~ 3,
                                 League == 'Ligue 1' ~ 4,
                                 League == 'Serie A' ~ 5,
                                 League == 'MLS' ~ 6,
                                 League == 'EFL Championship' ~ 7,
                                 League == 'Liga MX' ~ 8,
                                 League == 'Brasileiro Serie A' ~ 9,
                                 League == '--' ~ 10)) %>% 
  mutate(`Current Pick Odds` = as.integer(`Current Pick Odds`),
         `Odds Should Be` = as.integer(`Odds Should Be`))

df_html_bets <- if_else(nrow(bets_table2)==0,
                        "<b>At the odds currently available, no bets are recommended</b>",
                        print(xtable(bets_table2), type = "html", print.results = FALSE))

## Send an email

Outlook <- COMCreate("Outlook.Application")

Email = Outlook$CreateItem(0)
Email[["to"]] = "dnolen@smu.edu"
Email[["bcc"]] = paste("jamesorler@gmail.com",
                       "asnolen@crimson.ua.edu",
                       "jamestodd425@gmail.com",
                       "jordanreticker@gmail.com",
                       "brentcaminiti@gmail.com",
                       "dougmyers4987@gmail.com",
                       "ralphmstudley@gmail.com",
                       "johnpavese@gmail.com",
                       "amishra1293@gmail.com",
                       "rfinstra@gmail.com",
                       "james_bueck@yahoo.com",
                       "jouanscott@gmail.com",
                       "dnassar15@gmail.com",
                       "mshin0630@gmail.com",
                       "jasonarata@yahoo.com",
                       sep = ";", collapse = NULL)
Email[["subject"]] = paste0("Soccer Machine Picks: ", Sys.Date())
Email[["HTMLbody"]] = sprintf("
The Machine's picks for upcoming soccer matches are in! The Machine currently offers picks for the Big 5 European Leagues plus MLS, the EFL Championship, Liga MX, and the Brazilian Serie A. The attached documents contains all of the pertinent betting information for the upcoming matches, as well as a glossary. Good luck!
</p><br></p>
UPDATES: The Machine has started using a new model for the Champions League and Europa League. For these competitions it is important to consider how important the match is to each team. If a team is already elimitated or secured a spot in the next round they may rotate the squad instead of playing their starters. It is recommended to avoid any suggested bets where one team may be significantly more motivated to win than the other. The historical results below will now only include the current season for each league. If you are interested in the historical results for previous season feel free to reach out to me. I have also included below a breakdown of The Machine's performance by league. The Machine may perform better or worse with certain leagues so this will help to identify those trends.
</p><br></p>
%s
</p><br></p>
%s
</p><br></p>
These are the bets that The Machine recommends that you should make for games coming up in the next few days.
</p><br></p>
%s
</p><br></p>
Below are the historical results for both a flat betting strategy (1 unit wagers), and the suggested wager sizes from above:
</p><br></p>
%s
</p><br></p>
Most recent date with complete game data for each league:
</p><br></p>
%s
</p><br></p>
", df_html_1, df_html_1b, df_html_bets, plot_html, df_html_2)
Email[["attachments"]]$Add("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine/Soccer Machine/upcoming_bets.csv")
Email[["attachments"]]$Add("C:/Users/danie/Desktop/SportsStuff/TheMachine/the-machine/Soccer Machine/Glossary.pdf")

Email$Send()

overallEnd <- Sys.time()
paste("Entire script took",overallEnd - overallStart,attr(overallEnd - overallStart,"units"))





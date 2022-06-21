library(tidyverse)
library(baseballr)

try(mlb_game_linescore(game_pk = 566001))

yesterday <- baseballr::get_game_pks_mlb("2022-04-08")
today <- get_game_pks_mlb(Sys.Date())

gm1 <- mlb_game_linescore(today$game_pk[1])

i <- 1
date <- as.Date("2022-04-07")
gamedates <- list()
while (date < as.Date(Sys.Date())) {
  gamedate = date
  gamedates[[i]] = gamedate
  i <- i+1
  date <- date + 1
}

tbl <- data.frame(game_pk = NA)
j <- 1
for (j in seq_along(gamedates)) {
  pks = get_game_pks_mlb(gamedates[[j]])
  tbl = tbl %>% 
    bind_rows(pks)
  j = j+1
}

game_pks <- tbl %>% 
  filter(status.detailedState != 'Postponed' &
           !is.na(game_pk)) %>% 
  distinct(game_pk, officialDate, gameNumber, doubleHeader)

game_tbl <- data.frame(game_pk = NA)
k <- 1
for (k in 1:nrow(game_pks)) {
  gm = mlb_game_linescore(game_pks$game_pk[k])
  game_tbl = game_tbl %>% 
    bind_rows(gm)
  k = k+1
}

scores9 <- game_tbl %>% 
  filter(!is.na(game_pk)) %>% 
  group_by(game_pk, away_team_name, home_team_name) %>% 
  summarise(VisitorRunsScored = sum(away_runs, na.rm = T),
            HomeRunsScored = sum(home_runs, na.rm = T))

scores5 <- game_tbl %>% 
  filter(!is.na(game_pk) & num <= 5) %>% 
  group_by(game_pk, away_team_name, home_team_name) %>% 
  summarise(F5_VisitorRunsScored = sum(away_runs, na.rm = T),
            F5_HomeRunsScored = sum(home_runs, na.rm = T))

scores_2022 <- game_pks %>% 
  left_join(scores9) %>% 
  left_join(scores5)

write.csv(scores_2022, "Baseball Machine/scores_2022.csv", row.names = F, na = "")




library(tidyverse)
library(baseballr)

start19 <- as.Date('2019-03-20')
end19 <- as.Date('2019-09-29')
start20 <- as.Date('2020-07-23')
end20 <- as.Date('2020-09-27')
start21 <- as.Date('2021-04-01')
end21 <- as.Date('2021-10-03')
start22 <- as.Date('2022-04-07')
end22 <- as.Date('2022-10-05')

i <- 1
date <- start22
gamedates <- list()
while (date < Sys.Date()+2) {
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

write.csv(tbl %>%
            filter(!is.na(game_pk)) %>% 
            select(-dates), "Baseball Machine/Daily Files/2019/game_pks_2019.csv", row.names = F)

game_pks <- tbl %>% 
  filter(status.detailedState != 'Postponed' &
           seriesDescription == 'Regular Season' &
           !is.na(game_pk)) %>% 
  distinct(game_pk, officialDate, gameNumber, doubleHeader, dayNight, scheduledInnings)

game_tbl <- data.frame(game_pk = NA)
k <- 1
for (k in 1:nrow(game_pks)) {
  gm = mlb_game_linescore(game_pks$game_pk[k])
  game_tbl = game_tbl %>% 
    bind_rows(gm)
  k = k+1
}

write.csv(game_tbl, "Baseball Machine/Daily Files/2021/games_scores_2021.csv", row.names = F)

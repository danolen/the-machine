## Get MLB game scores for a given date range
get_mlb_daily_scores <- function(start_date, end_date, file_type = c("pks", "scores")) {
  library(tidyverse)
  library(baseballr)
  startDate = as.Date(start_date)
  endDate = as.Date(end_date)

  i = 1
  date = startDate
  gamedates = list()
  while (date <= endDate) {
    gamedate = date
    gamedates[[i]] = gamedate
    i = i+1
    date = date + 1
  }
  
  tbl = data.frame()
  j = 1
  for (j in seq_along(gamedates)) {
    pks = get_game_pks_mlb(gamedates[[j]])
    tbl = tbl %>% 
      bind_rows(pks)
    j = j+1
  }
  
  
  if (file_type == "pks") {
    return(tbl)
  }
  
  if (file_type == "scores") {
    game_pks <- tbl %>% 
      filter(status.detailedState != 'Postponed' &
               seriesDescription == 'Regular Season' &
               !is.na(game_pk)) %>% 
      distinct(game_pk, officialDate, gameNumber, doubleHeader, dayNight, scheduledInnings)
    
    game_tbl <- data.frame()
    k <- 1
    for (k in 1:nrow(game_pks)) {
      gm = mlb_game_linescore(game_pks$game_pk[k])
      game_tbl = game_tbl %>% 
        bind_rows(gm)
      k = k+1
    }
    
    return(game_tbl)
  }
  
}

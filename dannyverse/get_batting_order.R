## Get Probable Pitchers for a given date range
get_batting_order <- function(start_date, end_date) {
  library(tidyverse)
  
  date_range <- format(seq(as.Date(start_date), as.Date(end_date), by="day"), "%Y-%m-%d")
  
  df = data.frame()
  for (i in seq_along(date_range)) {
    pks = baseballr::get_game_pks_mlb(date_range[[i]]) %>% 
      distinct(game_pk, officialDate)
    df = df %>% 
      bind_rows(pks)
  }
  
  pk_list = df$game_pk
  date_list = df$officialDate
  
  lineups = data.frame()
  j = 1
  for (j in seq_along(pk_list)) {
    order = baseballr::mlb_batting_orders(pk_list[[j]]) %>% 
      mutate(date = date_list[[j]])
    lineups = lineups %>% 
      bind_rows(order)
    print(j)
  }
  
  return(lineups)
  
}

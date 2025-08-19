## Get Probable Pitchers for a given date range
get_probable_pitchers <- function(start_date, end_date) {
  library(tidyverse)
  
  date_range <- format(seq(as.Date(start_date), as.Date(end_date), by="day"), "%Y-%m-%d")
  
  df = data.frame()
  for (i in seq_along(date_range)) {
    pks = baseballr::get_game_pks_mlb(date_range[[i]]) 
    df = df %>% 
      bind_rows(pks) %>% 
      distinct(game_pk)
  }
  
  pk_list = df$game_pk
  
  pp = data.frame()
  j = 1
  for (j in seq_along(pk_list)) {
    sps = baseballr::mlb_probables(pk_list[[j]])
    pp = pp %>% 
      bind_rows(sps)
  }
  
  return(pp)
  
}

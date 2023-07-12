## Get pitcher game logs from FanGraphs
get_pitcher_game_logs <- function(dataframe) {
  library(tidyverse)
  
  pitchers <- dataframe %>% 
    select(key_fangraphs, year)
  
  df = data.frame()
  for (i in 1:nrow(pitchers)) {
    gls = baseballr::fg_pitcher_game_logs(dataframe[i,"key_fangraphs"], dataframe[i,"year"])
    df = df %>% 
      bind_rows(gls)
    print(i)
  }
  
  return(df)
  
}

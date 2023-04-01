## Get Daily Rosters for a given date range
get_daily_rosters <- function(start_date, end_date) {
  library(tidyverse)
  
  team_names <- read.csv("Baseball Machine/team_names.csv") %>% 
    left_join(baseballr::teams_lu_table %>%
                filter(sport.name == "Major League Baseball") %>%
                distinct(id, name),
              by = c("Full.Name" = "name"))
  
  team_ids <- team_names$id
  date_range <- format(seq(as.Date(start_date), as.Date(end_date), by="day"), "%Y-%m-%d")
  team_dates <- merge(team_ids, date_range)
  
  df <- data.frame()
  for (i in 1:nrow(team_dates)) {
    roster = baseballr::mlb_rosters(team_id = team_dates[i,1], date = team_dates[i,2], roster_type = 'active') %>%
      select(person_id, person_full_name, position_type, position_abbreviation, team_id, date)
    df = df %>%
      bind_rows(roster)
  }
  
  return(df)
  
}

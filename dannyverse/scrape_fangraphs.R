## Scrape fangraphs.com for hitting or pitching stats for a given date range
scrape_fangraphs <- function(position = c("team batting", "starting pitchers", "team bullpen"), season, start_date, end_date, time_frame = c("L7", "L14", "L30", "s2d")) {
  library("tidyverse")
  library("rvest")
  library("xml2")
  library("readr")
  library("janitor")
  library("lubridate")
  library("plyr")
  library("baseballr")
  season_info = baseballr::mlb_seasons_all()
  
  delta <- case_when(time_frame == "L7" ~ 7,
                     time_frame == "L14" ~ 14,
                     time_frame == "L30" ~ 30)
  
  if (position == "team batting") {
    i <- 1
    startdate <- case_when(time_frame == "s2d" ~ as.Date(season_info[season_info$season_id==as.character(season),"regular_season_start_date"]$regular_season_start_date),
                           TRUE ~ as.Date(start_date)-delta)
    enddate <- as.Date(start_date)-1
    urls <- list()
    while (enddate < as.Date(end_date)) {
      url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=',season,'&month=1000&season1=',season,'&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
      urls[[i]] = url
      i <- i + 1
      startdate <- startdate + case_when(time_frame == "s2d" ~ 0,
                                         TRUE ~ 1)
      enddate <- enddate + 1
    }
    
    tbl <- list()
    dates <- as.Date(start_date)
    j = 1
    for (j in seq_along(urls)) {
      tbl[[j]] = urls[[j]] %>%
        read_html() %>%
        html_nodes("table") %>%
        .[9] %>%
        html_table(fill = TRUE) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        .[-c(1,3),] %>%
        .[,-1] %>% row_to_names(row_number = 1)
      tbl[[j]]$Date = dates
      tbl[[j]]$url = urls[[j]]
      print(j)
      j = j+1
      dates = dates+1
    }
    
    df <- ldply(tbl, data.frame)
    clms <- c(7,8,21:28)
    for (i in 1:length(clms)) {
      df[,clms[i]] <- substr(df[,clms[i]], 1, nchar(df[,clms[i]])-1)
    }
    
    df[,2:28] <- lapply(df[,2:28], as.numeric)
    df[,c(7,8,21:28)] <- df[,c(7,8,21:28)] / 100
    
  }
  
  if (position == "starting pitchers") {
    i <- 1
    startdate <- case_when(time_frame == "s2d" ~ as.Date(season_info[season_info$season_id==as.character(season),"regular_season_start_date"]$regular_season_start_date),
                           TRUE ~ as.Date(start_date)-delta)
    enddate <- as.Date(start_date)-1
    urls <- list()
    while (enddate < as.Date(end_date)) {
      url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c%2C7%2C8%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C55%2C57%2C105%2C112%2C113%2C111%2C325%2C328&season=',season,'&month=1000&season1=',season,'&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate,'&page=1_1000')
      urls[[i]] = url
      i <- i + 1
      startdate <- startdate + case_when(time_frame == "s2d" ~ 0,
                                         TRUE ~ 1)
      enddate <- enddate + 1
    }
    
    tbl <- list()
    dates <- as.Date(start_date)
    j = 1
    for (j in seq_along(urls)) {
      tbl[[j]] = urls[[j]] %>%
        read_html() %>%
        html_nodes("table") %>%
        .[9] %>%
        html_table(fill = TRUE) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        .[-c(1,3),] %>%
        .[,-1] %>% row_to_names(row_number = 1)
      tbl[[j]]$Date = dates
      tbl[[j]]$url = urls[[j]]
      print(j)
      j = j+1
      dates = dates+1
    }
    
    df <- ldply(tbl, data.frame)
    clms <- c(6:7,10:14,24:29)
    for (i in 1:length(clms)) {
      df[,clms[i]] <- substr(df[,clms[i]], 1, nchar(df[,clms[i]])-1)
    }
    
    df[,3:29] <- lapply(df[,3:29], as.numeric)
    df[,c(6:7,10:14,24:29)] <- df[,c(6:7,10:14,24:29)] / 100
  }
  
  if (position == "team bullpen") {
    i <- 1
    startdate <- case_when(time_frame == "s2d" ~ as.Date(season_info[season_info$season_id==as.character(season),"regular_season_start_date"]$regular_season_start_date),
                           TRUE ~ as.Date(start_date)-delta)
    enddate <- as.Date(start_date)-1
    urls <- list()
    while (enddate < as.Date(end_date)) {
      url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=',season,'&month=1000&season1=',season,'&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
      urls[[i]] = url
      i <- i + 1
      startdate <- startdate + case_when(time_frame == "s2d" ~ 0,
                                         TRUE ~ 1)
      enddate <- enddate + 1
    }
    
    tbl <- list()
    dates <- as.Date(start_date)
    j = 1
    for (j in seq_along(urls)) {
      tbl[[j]] = urls[[j]] %>%
        read_html() %>%
        html_nodes("table") %>%
        .[9] %>%
        html_table(fill = TRUE) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        .[-c(1,3),] %>%
        .[,-1] %>% row_to_names(row_number = 1)
      tbl[[j]]$Date = dates
      tbl[[j]]$url = urls[[j]]
      print(j)
      j = j+1
      dates = dates+1
    }
    
    df <- ldply(tbl, data.frame)
    clms <- c(4,5,8:12,20:25)
    for (i in 1:length(clms)) {
      df[,clms[i]] <- substr(df[,clms[i]], 1, nchar(df[,clms[i]])-1)
    }
    
    df[,2:25] <- lapply(df[,2:25], as.numeric)
    df[,c(4,5,8:12,20:25)] <- df[,c(4,5,8:12,20:25)] / 100
  }
  
  return(df)
  
}

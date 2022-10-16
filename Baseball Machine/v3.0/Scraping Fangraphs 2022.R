## Scraping FanGraphs

library("pacman")
p_load("tidyverse", "rvest", "xml2", "readr", "janitor", "lubridate", "plyr")

# Example: L7 stats, 7 days after Opening Day 2022

url <- "https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2022&month=1000&season1=2022&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=2022-04-02&enddate=2022-04-08"

L7_team_batting <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  .[17] %>% 
  html_table(trim = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  .[-c(1,3),] %>%
  .[,-1] %>% row_to_names(row_number = 1)

###########################################################

# Last 7 days team batting stats (2022)

L7_batting_file <- read.csv("Baseball Machine/Daily Files/2022/team_batting_L7_2022.csv")

i <- 1
startdate <- as.Date(max(L7_batting_file$Date))-6
enddate <- as.Date(max(L7_batting_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2022&month=1000&season1=2022&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(L7_batting_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_L7 <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L7[,clms[i]] <- substr(team_batting_L7[,clms[i]], 1, nchar(team_batting_L7[,clms[i]])-1)
}

team_batting_L7[,2:28] <- lapply(team_batting_L7[,2:28], as.numeric)
team_batting_L7[,c(7,8,21:28)] <- team_batting_L7[,c(7,8,21:28)] / 100

team_batting_L7 <- L7_batting_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_batting_L7) %>% 
  distinct()

write.csv(team_batting_L7, "Baseball Machine/Daily Files/2022/team_batting_L7_2022.csv", row.names = FALSE)

# Last 14 days team batting stats (2022)

L14_batting_file <- read.csv("Baseball Machine/Daily Files/2022/team_batting_L14_2022.csv")

i <- 1
startdate <- as.Date(max(L14_batting_file$Date))-6
enddate <- as.Date(max(L14_batting_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2022&month=1000&season1=2022&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(L14_batting_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_L14 <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L14[,clms[i]] <- substr(team_batting_L14[,clms[i]], 1, nchar(team_batting_L14[,clms[i]])-1)
}

team_batting_L14[,2:28] <- lapply(team_batting_L14[,2:28], as.numeric)
team_batting_L14[,c(7,8,21:28)] <- team_batting_L14[,c(7,8,21:28)] / 100

team_batting_L14 <- L14_batting_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_batting_L14) %>% 
  distinct()

write.csv(team_batting_L14, "Baseball Machine/Daily Files/2022/team_batting_L14_2022.csv", row.names = FALSE)

# Last 30 days team batting stats (2022)

L30_batting_file <- read.csv("Baseball Machine/Daily Files/2022/team_batting_L30_2022.csv")

i <- 1
startdate <- as.Date(max(L30_batting_file$Date))-6
enddate <- as.Date(max(L30_batting_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2022&month=1000&season1=2022&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(L30_batting_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_L30 <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L30[,clms[i]] <- substr(team_batting_L30[,clms[i]], 1, nchar(team_batting_L30[,clms[i]])-1)
}

team_batting_L30[,2:28] <- lapply(team_batting_L30[,2:28], as.numeric)
team_batting_L30[,c(7,8,21:28)] <- team_batting_L30[,c(7,8,21:28)] / 100

team_batting_L30 <- L30_batting_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_batting_L30) %>% 
  distinct()

write.csv(team_batting_L30, "Baseball Machine/Daily Files/2022/team_batting_L30_2022.csv", row.names = FALSE)

# Season-to-date team batting stats (2022)

s2d_batting_file <- read.csv("Baseball Machine/Daily Files/2022/team_batting_s2d_2022.csv")

i <- 1
startdate <- as.Date(max(s2d_batting_file$Date))-6
enddate <- as.Date(max(s2d_batting_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2022&month=1000&season1=2022&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(s2d_batting_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_batting_s2d <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_s2d[,clms[i]] <- substr(team_batting_s2d[,clms[i]], 1, nchar(team_batting_s2d[,clms[i]])-1)
}

team_batting_s2d[,2:28] <- lapply(team_batting_s2d[,2:28], as.numeric)
team_batting_s2d[,c(7,8,21:28)] <- team_batting_s2d[,c(7,8,21:28)] / 100

team_batting_s2d <- s2d_batting_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_batting_s2d) %>% 
  distinct()

write.csv(team_batting_s2d, "Baseball Machine/Daily Files/2022/team_batting_s2d_2022.csv", row.names = FALSE)

# Season-to-date starting pitcher stats (2022)

s2d_SP_file <- read.csv("Baseball Machine/Daily Files/2022/pitchers_s2d_2022.csv")

i <- 1
startdate <- as.Date(max(s2d_SP_file$Date))-6
enddate <- as.Date(max(s2d_SP_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c%2C7%2C8%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C55%2C57%2C105%2C112%2C113%2C111%2C325%2C328&season=2022&month=1000&season1=2022&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate,'&page=1_1000')
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(s2d_SP_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

pitchers_s2d <- ldply(tbl, data.frame)
clms <- c(6:7,10:14,24:29)
for (i in 1:length(clms)) {
  pitchers_s2d[,clms[i]] <- substr(pitchers_s2d[,clms[i]], 1, nchar(pitchers_s2d[,clms[i]])-1)
}

pitchers_s2d[,3:29] <- lapply(pitchers_s2d[,3:29], as.numeric)
pitchers_s2d[,c(6:7,10:14,24:29)] <- pitchers_s2d[,c(6:7,10:14,24:29)] / 100

pitchers_s2d <- s2d_SP_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(pitchers_s2d) %>% 
  distinct()

write.csv(pitchers_s2d, "Baseball Machine/Daily Files/2022/pitchers_s2d_2022.csv", row.names = FALSE)

# Last 7 days team bullpen stats (2022)

L7_bullpen_file <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_L7_2022.csv")

i <- 1
startdate <- as.Date(max(L7_bullpen_file$Date))-6
enddate <- as.Date(max(L7_bullpen_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2022&month=1000&season1=2022&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(L7_bullpen_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L7 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L7[,clms[i]] <- substr(team_bullpen_L7[,clms[i]], 1, nchar(team_bullpen_L7[,clms[i]])-1)
}

team_bullpen_L7[,2:25] <- lapply(team_bullpen_L7[,2:25], as.numeric)
team_bullpen_L7[,c(4,5,8:12,20:25)] <- team_bullpen_L7[,c(4,5,8:12,20:25)] / 100

team_bullpen_L7 <- L7_bullpen_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_bullpen_L7) %>% 
  distinct()

write.csv(team_bullpen_L7, "Baseball Machine/Daily Files/2022/team_bullpen_L7_2022.csv", row.names = FALSE)

# Last 14 days team bullpen stats (2022)

L14_bullpen_file <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_L14_2022.csv")

i <- 1
startdate <- as.Date(max(L14_bullpen_file$Date))-6
enddate <- as.Date(max(L14_bullpen_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2022&month=1000&season1=2022&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(L14_bullpen_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L14 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L14[,clms[i]] <- substr(team_bullpen_L14[,clms[i]], 1, nchar(team_bullpen_L14[,clms[i]])-1)
}

team_bullpen_L14[,2:25] <- lapply(team_bullpen_L14[,2:25], as.numeric)
team_bullpen_L14[,c(4,5,8:12,20:25)] <- team_bullpen_L14[,c(4,5,8:12,20:25)] / 100

team_bullpen_L14 <- L14_bullpen_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_bullpen_L14) %>% 
  distinct()

write.csv(team_bullpen_L14, "Baseball Machine/Daily Files/2022/team_bullpen_L14_2022.csv", row.names = FALSE)

# Last 30 days team bullpen stats (2022)

L30_bullpen_file <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_L30_2022.csv")

i <- 1
startdate <- as.Date(max(L30_bullpen_file$Date))-6
enddate <- as.Date(max(L30_bullpen_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2022&month=1000&season1=2022&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(L30_bullpen_file$Date))
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L30 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L30[,clms[i]] <- substr(team_bullpen_L30[,clms[i]], 1, nchar(team_bullpen_L30[,clms[i]])-1)
}

team_bullpen_L30[,2:25] <- lapply(team_bullpen_L30[,2:25], as.numeric)
team_bullpen_L30[,c(4,5,8:12,20:25)] <- team_bullpen_L30[,c(4,5,8:12,20:25)] / 100

team_bullpen_L30 <- L30_bullpen_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_bullpen_L30) %>% 
  distinct()

write.csv(team_bullpen_L30, "Baseball Machine/Daily Files/2022/team_bullpen_L30_2022.csv", row.names = FALSE)

# Season-to-date team bullpen stats (2022)

s2d_bullpen_file <- read.csv("Baseball Machine/Daily Files/2022/team_bullpen_s2d_2022.csv")

i <- 1
startdate <- as.Date(max(s2d_bullpen_file$Date))-6
enddate <- as.Date(max(s2d_bullpen_file$Date))
urls <- list()
while (enddate < as.Date('2022-10-05')) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2022&month=1000&season1=2022&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date(max(s2d_bullpen_file$Date))+1
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_s2d <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_s2d[,clms[i]] <- substr(team_bullpen_s2d[,clms[i]], 1, nchar(team_bullpen_s2d[,clms[i]])-1)
}

team_bullpen_s2d[,2:25] <- lapply(team_bullpen_s2d[,2:25], as.numeric)
team_bullpen_s2d[,c(4,5,8:12,20:25)] <- team_bullpen_s2d[,c(4,5,8:12,20:25)] / 100

team_bullpen_s2d <- s2d_bullpen_file %>% 
  mutate(Date = as.Date(Date)) %>% 
  bind_rows(team_bullpen_s2d) %>% 
  distinct()

write.csv(team_bullpen_s2d, "Baseball Machine/Daily Files/2022/team_bullpen_s2d_2022.csv", row.names = FALSE)

###########################################################

# Last 7 days team batting stats (2019)

i <- 1
startdate <- as.Date("2019-03-22")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2019&month=1000&season1=2019&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_batting_L7 <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L7[,clms[i]] <- substr(team_batting_L7[,clms[i]], 1, nchar(team_batting_L7[,clms[i]])-1)
}

team_batting_L7[,2:28] <- lapply(team_batting_L7[,2:28], as.numeric)
team_batting_L7[,c(7,8,21:28)] <- team_batting_L7[,c(7,8,21:28)] / 100

write.csv(team_batting_L7, "team_batting_L7_2019.csv", row.names = FALSE)

# Last 14 days team batting stats (2019)

i <- 1
startdate <- as.Date("2019-03-15")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2019&month=1000&season1=2019&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_batting_L14 <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L14[,clms[i]] <- substr(team_batting_L14[,clms[i]], 1, nchar(team_batting_L14[,clms[i]])-1)
}

team_batting_L14[,2:28] <- lapply(team_batting_L14[,2:28], as.numeric)
team_batting_L14[,c(7,8,21:28)] <- team_batting_L14[,c(7,8,21:28)] / 100

write.csv(team_batting_L14, "team_batting_L14_2019.csv", row.names = FALSE)

# Last 30 days team batting stats (2019)

i <- 1
startdate <- as.Date("2019-02-27")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2019&month=1000&season1=2019&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_batting_L30 <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_L30[,clms[i]] <- substr(team_batting_L30[,clms[i]], 1, nchar(team_batting_L30[,clms[i]])-1)
}

team_batting_L30[,2:28] <- lapply(team_batting_L30[,2:28], as.numeric)
team_batting_L30[,c(7,8,21:28)] <- team_batting_L30[,c(7,8,21:28)] / 100

write.csv(team_batting_L30, "team_batting_L30_2019.csv", row.names = FALSE)

# Season-to-date team batting stats (2019)

i <- 1
startdate <- as.Date("2019-03-27")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,11,12,13,21,34,35,40,41,23,37,38,50,61,305,111,203,199,58,43,44,45,47,102,110,308,311&season=2019&month=1000&season1=2019&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_batting_s2d <- ldply(tbl, data.frame)
clms <- c(7,8,21:28)
for (i in 1:length(clms)) {
  team_batting_s2d[,clms[i]] <- substr(team_batting_s2d[,clms[i]], 1, nchar(team_batting_s2d[,clms[i]])-1)
}

team_batting_s2d[,2:28] <- lapply(team_batting_s2d[,2:28], as.numeric)
team_batting_s2d[,c(7,8,21:28)] <- team_batting_s2d[,c(7,8,21:28)] / 100

write.csv(team_batting_s2d, "team_batting_s2d_2019.csv", row.names = FALSE)

# Season-to-date starting pitcher stats (2019)

i <- 1
startdate <- as.Date("2019-03-27")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c%2C7%2C8%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C55%2C57%2C105%2C112%2C113%2C111%2C325%2C328&season=2019&month=1000&season1=2019&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate,'&page=1_1000')
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
pitchers_s2d <- ldply(tbl, data.frame)
clms <- c(6:7,10:14,24:29)
for (i in 1:length(clms)) {
  pitchers_s2d[,clms[i]] <- substr(pitchers_s2d[,clms[i]], 1, nchar(pitchers_s2d[,clms[i]])-1)
}

pitchers_s2d[,3:29] <- lapply(pitchers_s2d[,3:29], as.numeric)
pitchers_s2d[,c(6:7,10:14,24:29)] <- pitchers_s2d[,c(6:7,10:14,24:29)] / 100

write.csv(pitchers_s2d, "pitchers_s2d_2019.csv", row.names = FALSE)

# Last 7 days team bullpen stats (2019)

i <- 1
startdate <- as.Date("2019-03-22")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2019&month=1000&season1=2019&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_bullpen_L7 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L7[,clms[i]] <- substr(team_bullpen_L7[,clms[i]], 1, nchar(team_bullpen_L7[,clms[i]])-1)
}

team_bullpen_L7[,2:25] <- lapply(team_bullpen_L7[,2:25], as.numeric)
team_bullpen_L7[,c(4,5,8:12,20:25)] <- team_bullpen_L7[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_L7, "team_bullpen_L7_2019.csv", row.names = FALSE)

# Last 14 days team bullpen stats (2019)

i <- 1
startdate <- as.Date("2019-03-15")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2019&month=1000&season1=2019&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_bullpen_L14 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L14[,clms[i]] <- substr(team_bullpen_L14[,clms[i]], 1, nchar(team_bullpen_L14[,clms[i]])-1)
}

team_bullpen_L14[,2:25] <- lapply(team_bullpen_L14[,2:25], as.numeric)
team_bullpen_L14[,c(4,5,8:12,20:25)] <- team_bullpen_L14[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_L14, "team_bullpen_L14_2019.csv", row.names = FALSE)

# Last 30 days team bullpen stats (2019)

i <- 1
startdate <- as.Date("2019-02-27")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2019&month=1000&season1=2019&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_bullpen_L30 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L30[,clms[i]] <- substr(team_bullpen_L30[,clms[i]], 1, nchar(team_bullpen_L30[,clms[i]])-1)
}

team_bullpen_L30[,2:25] <- lapply(team_bullpen_L30[,2:25], as.numeric)
team_bullpen_L30[,c(4,5,8:12,20:25)] <- team_bullpen_L30[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_L30, "team_bullpen_L30_2019.csv", row.names = FALSE)

# Season-to-date team bullpen stats (2019)

i <- 1
startdate <- as.Date("2019-03-27")
enddate <- as.Date("2019-03-28")
urls <- list()
while (enddate < as.Date("2019-09-30")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2019&month=1000&season1=2019&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

firstthird <- urls[1:62]
secondthird <- urls[63:124]
lastthird <- urls[125:186]

tbl1 <- list()
dates <- as.Date("2019-03-29")
j = 1
for (j in seq_along(firstthird)) {
  tbl1[[j]] = firstthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl1[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl2 <- list()
dates <- as.Date("2019-05-30")
j = 1
for (j in seq_along(secondthird)) {
  tbl2[[j]] = secondthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl2[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl3 <- list()
dates <- as.Date("2019-07-31")
j = 1
for (j in seq_along(lastthird)) {
  tbl3[[j]] = lastthird[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl3[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

tbl <- c(tbl1, tbl2, tbl3)
team_bullpen_s2d <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_s2d[,clms[i]] <- substr(team_bullpen_s2d[,clms[i]], 1, nchar(team_bullpen_s2d[,clms[i]])-1)
}

team_bullpen_s2d[,2:25] <- lapply(team_bullpen_s2d[,2:25], as.numeric)
team_bullpen_s2d[,c(4,5,8:12,20:25)] <- team_bullpen_s2d[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_s2d, "team_bullpen_s2d_2019.csv", row.names = FALSE)

###########################################################

## Add in xERA
# Season-to-date starting pitcher stats (2020)

i <- 1
startdate <- as.Date("2020-07-23")
enddate <- as.Date("2020-07-24")
urls <- list()
while (enddate < as.Date("2020-09-28")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c%2C332&season=2020&month=1000&season1=2020&ind=0&team=0&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate,'&page=1_1000')
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date("2020-07-25")
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

pitchers_s2d <- ldply(tbl, data.frame)
clms <- c(6:7,10:14,24:29)
for (i in 1:length(clms)) {
  pitchers_s2d[,clms[i]] <- substr(pitchers_s2d[,clms[i]], 1, nchar(pitchers_s2d[,clms[i]])-1)
}

pitchers_s2d[,3:29] <- lapply(pitchers_s2d[,3:29], as.numeric)
pitchers_s2d[,c(6:7,10:14,24:29)] <- pitchers_s2d[,c(6:7,10:14,24:29)] / 100

write.csv(pitchers_s2d, "pitchers_s2d_2020.csv", row.names = FALSE)

# Last 7 days team bullpen stats (2020)

i <- 1
startdate <- as.Date("2020-07-18")
enddate <- as.Date("2020-07-24")
urls <- list()
while (enddate < as.Date("2020-09-28")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date("2020-07-25")
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L7 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L7[,clms[i]] <- substr(team_bullpen_L7[,clms[i]], 1, nchar(team_bullpen_L7[,clms[i]])-1)
}

team_bullpen_L7[,2:25] <- lapply(team_bullpen_L7[,2:25], as.numeric)
team_bullpen_L7[,c(4,5,8:12,20:25)] <- team_bullpen_L7[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_L7, "team_bullpen_L7_2020.csv", row.names = FALSE)

# Last 14 days team bullpen stats (2020)

i <- 1
startdate <- as.Date("2020-07-11")
enddate <- as.Date("2020-07-24")
urls <- list()
while (enddate < as.Date("2020-09-28")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date("2020-07-25")
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L14 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L14[,clms[i]] <- substr(team_bullpen_L14[,clms[i]], 1, nchar(team_bullpen_L14[,clms[i]])-1)
}

team_bullpen_L14[,2:25] <- lapply(team_bullpen_L14[,2:25], as.numeric)
team_bullpen_L14[,c(4,5,8:12,20:25)] <- team_bullpen_L14[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_L14, "team_bullpen_L14_2020.csv", row.names = FALSE)

# Last 30 days team bullpen stats (2020)

i <- 1
startdate <- as.Date("2020-06-25")
enddate <- as.Date("2020-07-24")
urls <- list()
while (enddate < as.Date("2020-09-28")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  startdate <- startdate + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date("2020-07-25")
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_L30 <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_L30[,clms[i]] <- substr(team_bullpen_L30[,clms[i]], 1, nchar(team_bullpen_L30[,clms[i]])-1)
}

team_bullpen_L30[,2:25] <- lapply(team_bullpen_L30[,2:25], as.numeric)
team_bullpen_L30[,c(4,5,8:12,20:25)] <- team_bullpen_L30[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_L30, "team_bullpen_L30_2020.csv", row.names = FALSE)

# Season-to-date team bullpen stats (2020)

i <- 1
startdate <- as.Date("2020-07-23")
enddate <- as.Date("2020-07-24")
urls <- list()
while (enddate < as.Date("2020-09-28")) {
  url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c%2C7%2C13%2C120%2C121%2C40%2C43%2C44%2C48%2C47%2C49%2C51%2C322%2C42%2C6%2C45%2C62%2C122%2C59%2C105%2C112%2C113%2C111%2C325%2C328&season=2020&month=1000&season1=2020&ind=0&team=0%2Cts&rost=0&age=0&filter=&players=0&startdate=',startdate,'&enddate=',enddate)
  urls[[i]] = url
  i <- i + 1
  enddate <- enddate + 1
}

tbl <- list()
dates <- as.Date("2020-07-25")
j = 1
for (j in seq_along(urls)) {
  tbl[[j]] = urls[[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    .[17] %>%
    html_table(fill = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    .[-c(1,3),] %>%
    .[,-1] %>% row_to_names(row_number = 1)
  tbl[[j]]$Date = dates
  j = j+1
  dates = dates+1
}

team_bullpen_s2d <- ldply(tbl, data.frame)
clms <- c(4,5,8:12,20:25)
for (i in 1:length(clms)) {
  team_bullpen_s2d[,clms[i]] <- substr(team_bullpen_s2d[,clms[i]], 1, nchar(team_bullpen_s2d[,clms[i]])-1)
}

team_bullpen_s2d[,2:25] <- lapply(team_bullpen_s2d[,2:25], as.numeric)
team_bullpen_s2d[,c(4,5,8:12,20:25)] <- team_bullpen_s2d[,c(4,5,8:12,20:25)] / 100

write.csv(team_bullpen_s2d, "team_bullpen_s2d_2020.csv", row.names = FALSE)



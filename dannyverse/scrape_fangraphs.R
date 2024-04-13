## Scrape fangraphs.com for hitting or pitching stats for a given date range
scrape_fangraphs <- function(position = c("team batting", "starting pitchers", "team bullpen"), season, start_date, end_date, time_frame = c("L7", "L14", "L30", "s2d")) {
  library("plyr")
  library("tidyverse")
  library("rvest")
  library("xml2")
  library("readr")
  library("janitor")
  library("lubridate")
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
    df <- data.frame()
    while (enddate < as.Date(end_date)) {
      tbl <- fg_team_batter(qual = 0, startseason = season, endseason = season, startdate = startdate, enddate = enddate, month = "1000")
      df <- df %>%
        bind_rows(tbl %>% 
                    select(team_name,
                             PA,
                             HR,
                             R,
                             RBI,
                             SB,
                             BB_pct,
                             K_pct,
                             ISO,
                             BABIP,
                             AVG,
                             OBP,
                             SLG,
                             wOBA,
                             wRC_plus,
                             EV,
                             BaseRunning,
                             Offense,
                             Defense,
                             WAR,
                             LD_pct,
                             GB_pct,
                             FB_pct,
                             HR_FB,
                             `O-Swing_pct`,
                             SwStr_pct,
                             Barrel_pct,
                             HardHit_pct) %>% 
                    dplyr::rename(Team = team_name,
                           BB. = BB_pct,
                           K. = K_pct,
                           wRC. = wRC_plus,
                           BsR = BaseRunning,
                           Off = Offense,
                           Def = Defense,
                           LD. = LD_pct,
                           GB. = GB_pct,
                           FB. = FB_pct,
                           HR.FB = HR_FB,
                           O.Swing. = `O-Swing_pct`,
                           SwStr. = SwStr_pct,
                           Barrel. = Barrel_pct,
                           HardHit. = HardHit_pct) %>% 
                    mutate(Date = enddate+1)
                  )

      startdate <- startdate + case_when(time_frame == "s2d" ~ 0,
                                         TRUE ~ 1)
      enddate <- enddate + 1
    }
    
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
    df <- data.frame()
    while (enddate < as.Date(end_date)) {
      tbl <- fg_pitcher_leaders_dn(startseason = season, endseason = season, startdate = startdate, enddate = enddate, month = "1000")
      df <- df %>%
        bind_rows(tbl %>% 
                    select(PlayerNameRoute,
                           team_name,
                           G,
                           GS,
                           IP,
                           K_pct,
                           BB_pct,
                           HR_9,
                           BABIP,
                           LOB_pct,
                           GB_pct,
                           LD_pct,
                           FB_pct,
                           HR_FB,
                           EV,
                           WHIP,
                           ERA,
                           FIP,
                           xFIP,
                           SIERA,
                           WAR,
                           Start_IP,
                           Relief_IP,
                           `O-Swing_pct`,
                           `F-Strike_pct`,
                           SwStr_pct,
                           Zone_pct,
                           Barrel_pct,
                           HardHit_pct) %>% 
                    dplyr::rename(Name = PlayerNameRoute,
                           Team = team_name,
                           BB. = BB_pct,
                           K. = K_pct,
                           HR.9 = HR_9,
                           LOB. = LOB_pct,
                           LD. = LD_pct,
                           GB. = GB_pct,
                           FB. = FB_pct,
                           HR.FB = HR_FB,
                           Start.IP = Start_IP,
                           Relief.IP = Relief_IP,
                           O.Swing. = `O-Swing_pct`,
                           F.Strike. = `F-Strike_pct`,
                           SwStr. = SwStr_pct,
                           Zone. = Zone_pct,
                           Barrel. = Barrel_pct,
                           HardHit. = HardHit_pct) %>% 
                    mutate(Date = enddate+1)
        )
      startdate <- startdate + case_when(time_frame == "s2d" ~ 0,
                                         TRUE ~ 1)
      enddate <- enddate + 1
    }
    
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
    df <- data.frame()
    while (enddate < as.Date(end_date)) {
      tbl <- fg_team_relievers(startseason = season, endseason = season, startdate = startdate, enddate = enddate, month = "1000", qual = "0")
      df <- df %>%
        bind_rows(tbl %>% 
                    select(team_name,
                           G,
                           IP,
                           K_pct,
                           BB_pct,
                           HR_9,
                           BABIP,
                           LOB_pct,
                           LD_pct,
                           GB_pct,
                           FB_pct,
                           HR_FB,
                           EV,
                           WHIP,
                           ERA,
                           FIP,
                           xFIP,
                           SIERA,
                           WAR,
                           `O-Swing_pct`,
                           `F-Strike_pct`,
                           SwStr_pct,
                           Zone_pct,
                           Barrel_pct,
                           HardHit_pct) %>% 
                    dplyr::rename(Team = team_name,
                           BB. = BB_pct,
                           K. = K_pct,
                           HR.9 = HR_9,
                           LOB. = LOB_pct,
                           LD. = LD_pct,
                           GB. = GB_pct,
                           FB. = FB_pct,
                           HR.FB = HR_FB,
                           O.Swing. = `O-Swing_pct`,
                           F.Strike. = `F-Strike_pct`,
                           SwStr. = SwStr_pct,
                           Zone. = Zone_pct,
                           Barrel. = Barrel_pct,
                           HardHit. = HardHit_pct) %>% 
                    mutate(Date = enddate+1)
        )
      
      startdate <- startdate + case_when(time_frame == "s2d" ~ 0,
                                         TRUE ~ 1)
      enddate <- enddate + 1
    }
    
    clms <- c(4,5,8:12,20:25)
    for (i in 1:length(clms)) {
      df[,clms[i]] <- substr(df[,clms[i]], 1, nchar(df[,clms[i]])-1)
    }
    
    df[,2:25] <- lapply(df[,2:25], as.numeric)
    df[,c(4,5,8:12,20:25)] <- df[,c(4,5,8:12,20:25)] / 100
  }
  
  return(df)
  
}

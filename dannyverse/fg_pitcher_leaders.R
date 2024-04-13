fg_pitcher_leaders_dn <- function (age = "", pos = "all", stats = "pit", lg = "all", 
          qual = "0", startseason = "2023", endseason = "2023", startdate = "", 
          enddate = "", month = "0", hand = "", team = "0", pageitems = "10000", 
          pagenum = "1", ind = "0", rost = "0", players = "", type = "8", 
          postseason = "", sortdir = "default", sortstat = "WAR") 
{
  params <- list(age = age, pos = pos, stats = stats, lg = lg, 
                 qual = qual, season = startseason, season1 = endseason, 
                 startdate = startdate, enddate = enddate, month = month, 
                 hand = hand, team = team, pageitems = pageitems, pagenum = pagenum, 
                 ind = ind, rost = rost, players = players, type = type, 
                 postseason = postseason, sortdir = sortdir, sortstat = sortstat)
  url <- "https://www.fangraphs.com/api/leaders/major-league/data"
  fg_endpoint <- httr::modify_url(url, query = params)
  tryCatch(expr = {
    resp <- fg_endpoint %>% mlb_api_call_dn()
    fg_df <- resp$data %>% jsonlite::toJSON() %>% jsonlite::fromJSON(flatten = TRUE)
    c <- colnames(fg_df)
    c <- gsub("%", "_pct", c, fixed = TRUE)
    c <- gsub("/", "_", c, fixed = TRUE)
    c <- ifelse(substr(c, nchar(c) - 1 + 1, nchar(c)) == 
                  ".", gsub("\\.", "_pct", c), c)
    c <- gsub(" ", "_", c, fixed = TRUE)
    colnames(fg_df) <- c
    leaders <- fg_df %>% dplyr::rename_with(~gsub("pi", 
                                                  "pi_", .x), starts_with("pi")) %>% dplyr::rename_with(~gsub("pfx", 
                                                                                                              "pfx_", .x), starts_with("pfx")) %>% dplyr::rename(Start_IP = "Start-IP", 
                                                                                                                                                                 Relief_IP = "Relief-IP", WPA_minus = "-WPA", WPA_plus = "+WPA", 
                                                                                                                                                                 AgeRng = "AgeR", team_name = "TeamName", team_name_abb = "TeamNameAbb") %>% 
      dplyr::select(-dplyr::any_of(c("Name", "Team"))) %>% 
      dplyr::select("season", "team_name", "Throws", "xMLBAMID", 
                    "PlayerNameRoute", "PlayerName", "playerid", 
                    "Age", "AgeRng", tidyr::everything()) %>% make_baseballr_data_dn("MLB Player Pitching Leaders data from FanGraphs.com", 
                                                                                  Sys.time())
  }, error = function(e) {
    message(glue::glue("{Sys.time()}: Invalid arguments or no player pitching leaders data available!"))
  }, finally = {
  })
  return(leaders)
}

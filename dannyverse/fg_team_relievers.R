fg_team_relievers <- function (age = "", pos = "all", stats = "rel", lg = "all", 
          qual = "0", startseason = "2024", endseason = "2024", startdate = "2024-03-20", 
          enddate = "2024-03-28", month = "1000", hand = "", team = "0,ts", pageitems = "1000", 
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
                                                                                                              "pfx_", .x), starts_with("pfx")) %>% dplyr::rename(#Start_IP = "Start-IP", 
                                                                                                                                                                 Relief_IP = "Relief-IP", WPA_minus = "-WPA", WPA_plus = "+WPA", 
                                                                                                                                                                 FBall_pct = "FB_pct1", AgeRng = "AgeR", team_name = "TeamName", 
                                                                                                                                                                 team_name_abb = "TeamNameAbb") %>% dplyr::select(-dplyr::any_of(c("Throws", 
                                                                                                                                                                                                                                   "xMLBAMID", "Name", "Team", "PlayerNameRoute", "PlayerName", 
                                                                                                                                                                                                                                   "playerid", "Age", "AgeRng"))) %>% dplyr::select("season", 
                                                                                                                                                                                                                                                                                    "team_name", tidyr::everything()) %>% make_baseballr_data_dn("MLB Team Pitching data from FanGraphs.com", 
                                                                                                                                                                                                                                                                                                                                              Sys.time())
  }, error = function(e) {
    message(glue::glue("{Sys.time()}: Possible invalid arguments or no team pitching data available!"))
  }, finally = {
  })
  return(leaders)
}

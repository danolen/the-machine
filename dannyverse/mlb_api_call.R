mlb_api_call_dn <- function (url) 
{
  res <- httr::RETRY("GET", url)
  json <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)
  return(json)
}

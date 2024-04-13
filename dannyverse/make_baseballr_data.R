make_baseballr_data_dn <- function (df, type, timestamp) 
{
  out <- df %>% tidyr::as_tibble()
  class(out) <- c("baseballr_data", "tbl_df", "tbl", "data.table", 
                  "data.frame")
  attr(out, "baseballr_timestamp") <- timestamp
  attr(out, "baseballr_type") <- type
  return(out)
}
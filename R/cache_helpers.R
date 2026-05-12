download_remote_cache <- function(url) {
  tf <- tempfile(fileext = ".rds")
  
  resp <- request(url) |>
    req_perform()
  
  writeBin(resp_body_raw(resp), tf)
  x <- readRDS(tf)
  
  if (!all(c("forecast_df", "sfog_tables", "last_refresh") %in% names(x))) {
    stop("Remote cache is missing one or more required objects: forecast_df, sfog_tables, last_refresh")
  }
  
  x
}
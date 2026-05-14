download_sfog_display_cache <- function(url) {
  
  tf <- tempfile(fileext = ".rds")
  
  on.exit(unlink(tf), add = TRUE)
  
  utils::download.file(
    url = url,
    destfile = tf,
    mode = "wb",
    quiet = TRUE
  )
  
  obj <- readRDS(tf)
  
  required_names <- c(
    "overlay_info",
    "valid_times",
    "last_refresh"
  )
  
  missing_objects <- setdiff(required_objects, names(obj))
  
  if (length(missing_objects) > 0) {
    stop(
      paste(
        "Missing objects in superfog display cache:",
        paste(missing_objects, collapse = ", ")
      )
    )
  }
  
  obj
}

get_sfog_valid_times <- function(sfog_cache_obj) {
  
  valid_times <- sfog_cache_obj$valid_times
  
  base::as.POSIXct(
    valid_times,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

format_sfog_valid_time <- function(x) {
  
  format(
    lubridate::with_tz(x, "America/New_York"),
    "%b %d, %Y %I:%M %p %Z"
  )
}
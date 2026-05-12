download_sfog_cache <- function(url) {
  
  tf <- tempfile(fileext = ".rds")
  
  on.exit(unlink(tf), add = TRUE)
  
  download.file(
    url = url,
    destfile = tf,
    mode = "wb",
    quiet = TRUE
  )
  
  obj <- readRDS(tf)
  
  required_objects <- c(
    "sfog_ll",
    "valid_times",
    "last_refresh"
  )
  
  missing_objects <- setdiff(required_objects, names(obj))
  
  if (length(missing_objects) > 0) {
    stop(
      paste(
        "Missing objects in superfog cache:",
        paste(missing_objects, collapse = ", ")
      )
    )
  }
  
  if (inherits(obj$sfog_ll, "PackedSpatRaster")) {
    obj$sfog_ll <- terra::unwrap(obj$sfog_ll)
  }
  
  obj
}

get_sfog_valid_times <- function(sfog_cache_obj) {
  
  valid_times <- sfog_cache_obj$valid_times
  
  as.POSIXct(
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
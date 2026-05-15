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
  
  required_objects <- c(
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

download_sfog_extract_cache <- function(url) {
  
  tf <- tempfile(fileext = ".rds")
  
  on.exit(unlink(tf), add = TRUE)
  
  utils::download.file(
    url = url,
    destfile = tf,
    mode = "wb",
    quiet = TRUE
  )
  
  obj <- readRDS(tf)
  
  required_objects <- c(
    "sfog_extract_df",
    "valid_times",
    "last_refresh"
  )
  
  missing_objects <- setdiff(required_objects, names(obj))
  
  if (length(missing_objects) > 0) {
    stop(
      paste(
        "Missing objects in superfog extraction cache:",
        paste(missing_objects, collapse = ", ")
      )
    )
  }
  
  obj
}

lookup_point_tz <- function(lat, lon, default_tz = "America/New_York") {
  
  if (is.na(lat) || is.na(lon)) {
    return(default_tz)
  }
  
  tz <- tryCatch(
    lutz::tz_lookup_coords(
      lat = lat,
      lon = lon,
      method = "fast",
      warn = FALSE
    ),
    error = function(e) default_tz
  )
  
  if (is.na(tz) || is.null(tz)) {
    default_tz
  } else {
    tz
  }
}

format_time_local <- function(time, tz, fmt = "%b %d, %Y %I:%M %p %Z") {
  format(
    lubridate::with_tz(time, tz),
    fmt
  )
}


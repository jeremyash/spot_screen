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
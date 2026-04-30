library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(sf)

cache_file <- "cache/superfog_cache.rds"

# confirm R is actually in the git repo
getwd()
system2("git", "status")

git_log <- system2(
  "git",
  args = c(
    "log",
    "--pretty=format:%H%x09%ct",
    "--",
    cache_file
  ),
  stdout = TRUE
)

commits <- tibble(raw = git_log) %>%
  separate(raw, into = c("commit", "commit_unix"), sep = "\t") %>%
  mutate(
    commit_unix = as.numeric(commit_unix),
    commit_time = as.POSIXct(commit_unix, origin = "1970-01-01", tz = "UTC"),
    commit_date = as.Date(commit_time)
  )

daily_commits <- commits %>%
  group_by(commit_date) %>%
  slice_max(commit_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(commit_date)

read_forecast_from_commit <- function(commit_hash, commit_date, commit_time) {
  message("Reading ", commit_date, " | ", substr(commit_hash, 1, 7))
  
  system2(
    "git",
    args = c(
      "restore",
      paste0("--source=", commit_hash),
      cache_file
    )
  )
  
  x <- readRDS(cache_file)
  
  if (!is.list(x) || !"forecast_df" %in% names(x)) {
    stop("forecast_df not found in RDS for commit: ", commit_hash)
  }
  
  x$forecast_df %>%
    mutate(
      cache_commit = commit_hash,
      cache_commit_date = commit_date,
      cache_commit_time = commit_time,
      .before = 1
    )
}

forecast_history <- pmap_dfr(
  daily_commits,
  function(commit, commit_unix, commit_time, commit_date) {
    read_forecast_from_commit(commit, commit_date, commit_time)
  }
)

# restore latest checked-out branch version
system2("git", args = c("restore", cache_file))

saveRDS(forecast_history, "~/Downloads/forecast_df_daily_history.rds")
write_csv(forecast_history, "~/Downloads/forecast_df_daily_history.csv")


# forecast clean
forecast_clean <- forecast_history %>% 
  select(project_name, lat, lon, issuanceTime, productText, spot_id) %>% 
  distinct()

# intersect to get forest name
forecast_sf <- forecast_clean %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

r8_forests_4326 <- st_read("r8_forests") %>%
  st_transform(4326)

forecast_with_forest <- forecast_sf %>%
  st_join(
    r8_forests_4326 %>% select(forest_name = forest),
    join = st_intersects,
    left = TRUE
  ) %>%
  st_drop_geometry()





library(dplyr)
library(stringr)
library(fs)

out_dir <- "~/Downloads/superfog_product_text_by_forest"

safe_name <- function(x, allow_decimal = FALSE) {
  pattern <- if (allow_decimal) "[^A-Za-z0-9.]+" else "[^A-Za-z0-9]+"
  
  x %>%
    stringr::str_replace_all(pattern, "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_remove("^_") %>%
    stringr::str_remove("_$")
}

forecast_with_forest %>%
  mutate(
    forest_name = if_else(is.na(forest_name), "Unknown_Forest", forest_name),
    file_date = format(as.Date(issuanceTime), "%Y%m%d"),
    
    forest_dir = file.path(out_dir, safe_name(forest_name)),
    
    spot_safe = safe_name(spot_id, allow_decimal = TRUE),
    project_safe = safe_name(project_name),
    
    file_name = paste0(
      file_date,
      "__spot_", spot_safe,
      "__project_", project_safe,
      ".txt"
    ),
    
    file_path = file.path(forest_dir, file_name)
  ) %>%
  rowwise() %>%
  mutate(
    wrote_file = {
      fs::dir_create(forest_dir)
      writeLines(productText, file_path, useBytes = TRUE)
      TRUE
    }
  ) %>%
  ungroup()



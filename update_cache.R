# update_cache.R

# -------------------------------
# LIBRARIES
# -------------------------------

library(tidyverse)
library(lubridate)
library(lutz)
library(httr2)
library(jsonlite)
library(sf)

# -------------------------------
# LOG MESSAGING
# -------------------------------

Sys.setenv(TZ = "America/New_York")

log_msg <- function(...) {
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    " - ",
    paste(..., collapse = " "),
    "\n"
  )
}

log_msg("Starting cache update")

# -------------------------------
# GITHUB ACTIONS RUN MODE
# -------------------------------

force_run <- identical(Sys.getenv("FORCE_RUN"), "true")

if (force_run) {
  log_msg("FORCE_RUN detected.")
} else {
  log_msg("Scheduled run detected.")
}

# -------------------------------
# ENSURE CACHE DIRECTORY EXISTS
# -------------------------------

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

log_msg(paste("Working directory:", getwd()))

# -------------------------------
# STATIC DATA
# -------------------------------

wfo <- read_csv("r8_wfo.csv", show_col_types = FALSE)

r8_forests <- st_read("r8_forests", quiet = TRUE) |>
  st_transform(4326)

r8 <- st_read("region_8", quiet = TRUE) |>
  st_transform(4326)

r8_forests$forest_id <- seq_len(nrow(r8_forests))

fws_nws_base_url <- "https://api.weather.gov/products/types/FWS"
stq_nws_base_url <- "https://api.weather.gov/products/types/STQ"

cache_path <- "cache/superfog_cache.rds"

# -------------------------------
# EMPTY CACHE HELPER
# -------------------------------

empty_cache <- function() {
  list(
    forecast_df = tibble(),
    sfog_tables = list(),
    last_refresh = Sys.time()
  )
}

# -------------------------------
# API HELPERS
# -------------------------------

spot_df_fun <- function(SPOT_URL) {
  spot_api_ls <- request(SPOT_URL) %>%
    req_perform() %>%
    resp_body_json()
  
  as.data.frame(spot_api_ls[2:9]) %>%
    mutate(
      spot_id = str_extract(productText, "(?<=\\.TAG\\s)[0-9.]+"),
      usda = str_detect(productText, "usda\\.gov"),
      issuanceTime = ymd_hms(issuanceTime, tz = "UTC"),
      date_issued = as.Date(with_tz(issuanceTime, "America/New_York"))
    ) %>%
    filter(usda == TRUE)
}

stq_df_fun <- function(STQ_URL) {
  stq_api_ls <- request(STQ_URL) %>%
    req_perform() %>%
    resp_body_json()
  
  as.data.frame(stq_api_ls[2:9]) %>%
    mutate(
      spot_id = str_match(productText, "OFILE:\\s*([0-9.]+)")[, 2],
      project_name = str_match(productText, "PROJECT NAME:\\s*([^\\n]+)")[, 2],
      lat = as.numeric(str_match(productText, "DLAT:\\s*(-?[0-9.]+)")[, 2]),
      lon = -abs(as.numeric(str_match(productText, "DLON:\\s*(-?[0-9.]+)")[, 2])),
      date = str_match(productText, "DATE:\\s*(\\d{2}/\\d{2}/\\d{2})")[, 2],
      time = str_match(productText, "TIME:\\s*(\\d{4})")[, 2]
    ) %>%
    filter(!is.na(spot_id))
}

# -------------------------------
# TIME HELPER
# -------------------------------

convert_to_24hr <- function(time_str) {
  x <- toupper(trimws(time_str))
  x <- gsub("MID", "12AM", x, ignore.case = TRUE)
  x <- gsub("NOON", "12PM", x, ignore.case = TRUE)
  x <- gsub("([0-9])A$", "\\1AM", x)
  x <- gsub("([0-9])P$", "\\1PM", x)
  
  hour <- as.integer(sub("([0-9]+)(AM|PM)", "\\1", x))
  period <- sub("([0-9]+)(AM|PM)", "\\2", x)
  
  if (period == "AM") {
    if (hour == 12) "00" else sprintf("%02d", hour)
  } else {
    if (hour == 12) sprintf("%02d", hour) else sprintf("%02d", hour + 12)
  }
}

# -------------------------------
# SUPERFOG PARSING
# -------------------------------

parse_spot_forecasts <- function(df) {
  if (nrow(df) == 0) return(list())
  
  map(seq_len(nrow(df)), function(i) {
    tryCatch({
      spot_txt <- df$productText[i]
      spot_info <- df[i, ]
      
      spot_chunks <- strsplit(spot_txt, "\n\n")[[1]]
      time_chunks <- spot_chunks[grepl("^(TIME|[^\\n]*TIME)", spot_chunks)]
      
      if (length(time_chunks) == 0) {
        return(NULL)
      }
      
      time_dataframes <- lapply(seq_along(time_chunks), function(chunk_no) {
        chunk <- time_chunks[[chunk_no]]
        lines <- strsplit(chunk, "\n")[[1]]
        
        time_line <- lines[grepl("^TIME", lines)]
        time_line <- gsub("\\(.*?\\)", "  ", time_line)
        time_line <- gsub("(\\d)\\s+(AM|PM)", "\\1\\2", time_line)
        time_line <- strsplit(trimws(time_line), "\\s{1,}")[[1]]
        time_line <- gsub("MID", "12AM", time_line, ignore.case = TRUE)
        time_line <- gsub("NOON", "12PM", time_line, ignore.case = TRUE)
        time_line <- gsub("([0-9])A$", "\\1AM", time_line)
        time_line <- gsub("([0-9])P$", "\\1PM", time_line)
        time_line <- sub("^.*?(\\d{1,2}(AM|PM)).*$", "\\1", time_line, ignore.case = TRUE)
        time_line <- list(time_line)
        
        data_lines <- lines[!grepl("^TIME", trimws(lines))]
        data_lines <- data_lines[
          grepl("^(RH|Temp|Sky\\s*\\(\\s*%\\s*\\))", data_lines, ignore.case = TRUE) |
            grepl("^Surface\\s*(wnd|wind)\\s*spd", data_lines, ignore.case = TRUE) |
            (grepl("^20\\s*ft\\s*wind(\\s*mph)?", data_lines, ignore.case = TRUE) &
               !grepl("^20\\s*ft\\s*wind\\s*(dir|gust)", data_lines, ignore.case = TRUE))
        ]
        
        data_lines <- sub("^RH\\.*", "RH ", data_lines, ignore.case = TRUE)
        data_lines <- sub(
          "^(20\\s*ft\\s*wind(\\s*(spd|speed|mph))?|Surface\\s*(wnd|wind)\\s*spd)(?!\\s*(gust|dir))",
          "WIND ",
          data_lines,
          ignore.case = TRUE,
          perl = TRUE
        )
        data_lines <- sub("^Temp\\.*", "TEMP ", data_lines, ignore.case = TRUE)
        data_lines <- sub("^Sky\\s*\\(\\s*%\\s*\\)", "SKY ", data_lines, ignore.case = TRUE)
        data_lines <- str_remove_all(data_lines, "G\\d+")
        data_lines <- str_remove_all(data_lines, "\\b[NESW]{1,3}\\b\\s*")
        data_lines <- gsub("[.()%]", " ", data_lines)
        data_lines <- gsub("\\s+", " ", data_lines)
        data_lines <- trimws(data_lines)
        data_lines_split <- strsplit(data_lines, " ")
        data_lines_split <- append(data_lines_split, time_line, after = 0)
        
        headers <- paste0("ch_", chunk_no, "_", seq_along(time_line[[1]]))
        out <- do.call(rbind, lapply(data_lines_split, function(x) {
          chunk_df <- data.frame(t(x), stringsAsFactors = FALSE)
          colnames(chunk_df) <- headers
          chunk_df
        }))
        
        out
      })
      
      all_times_df <- do.call(cbind, time_dataframes)
      all_times_df <- all_times_df %>%
        select(-which(apply(all_times_df, 2, function(col) any(grepl("TIME", col))))[-1])
      
      t_all_times_df <- all_times_df[-1] %>%
        t() %>%
        as.data.frame() %>%
        setNames(all_times_df[, 1])
      
      t_all_times_df <- t_all_times_df %>%
        mutate(
          TIME_24 = sapply(TIME, convert_to_24hr),
          hour_num = as.integer(TIME_24)
        )
      
      base_date <- as.Date(spot_info$date, format = "%m/%d/%y")
      
      forecast_dates <- rep(base_date, nrow(t_all_times_df))
      if (nrow(t_all_times_df) > 1) {
        for (j in 2:nrow(t_all_times_df)) {
          forecast_dates[j] <- forecast_dates[j - 1]
          if (t_all_times_df$hour_num[j] < t_all_times_df$hour_num[j - 1]) {
            forecast_dates[j] <- forecast_dates[j] + 1
          }
        }
      }
      
      filter_start <- as.POSIXct(
        paste(base_date, "18:00:00"),
        tz = spot_info$tz
      )
      
      filter_end <- as.POSIXct(
        paste(base_date + 1, "08:00:00"),
        tz = spot_info$tz
      )
      
      time_all_times_df <- t_all_times_df %>%
        select(-TIME) %>%
        mutate(across(where(is.character), as.numeric)) %>%
        mutate(
          forecast_date = forecast_dates,
          DATETIME = as.POSIXct(
            paste(forecast_date, paste0(TIME_24, ":00:00")),
            tz = spot_info$tz
          )
        ) %>%
        filter(DATETIME >= filter_start, DATETIME <= filter_end) %>%
        arrange(DATETIME) %>%
        select(DATETIME, TEMP, RH, WIND, SKY)
      
      rownames(time_all_times_df) <- NULL
      
      sfog_df <- time_all_times_df %>%
        mutate(
          sky_screen = ifelse(SKY <= 40, "critical", ifelse(SKY > 60, "minimal", "watch_out")),
          temp_screen = ifelse(TEMP <= 55, "critical", ifelse(TEMP > 70, "minimal", "watch_out")),
          rh_screen = ifelse(RH >= 90, "critical", ifelse(RH < 70, "minimal", "watch_out")),
          wind_screen = ifelse(WIND <= 4, "critical", ifelse(WIND > 7, "minimal", "watch_out"))
        )
      
      sfog_df
    }, error = function(e) {
      log_msg("parse_spot_forecasts failed for row", i, ":", e$message)
      NULL
    })
  })
}

# -------------------------------
# BUILD CACHE
# -------------------------------

build_cache <- function() {
  today_val <- today(tzone = "America/New_York")
  
  fws_ls <- request(fws_nws_base_url) %>%
    req_perform() %>%
    resp_body_json()
  
  stq_ls <- request(stq_nws_base_url) %>%
    req_perform() %>%
    resp_body_json()
  
  fws_df <- bind_rows(fws_ls$`@graph`) %>%
    mutate(
      issuingOffice = str_sub(issuingOffice, 2),
      date_issued = as.Date(with_tz(ymd_hms(issuanceTime, tz = "UTC"), "America/New_York"))
    ) %>%
    filter(issuingOffice %in% wfo$WFO) %>%
    filter(productName == "Suppression Forecast") %>%
    rename(api_url = "@id") %>%
    filter(date_issued %in% c(today_val, today_val - 1))
  
  stq_df <- bind_rows(stq_ls$`@graph`) %>%
    mutate(
      issuingOffice = str_sub(issuingOffice, 2),
      date_issued = as.Date(with_tz(ymd_hms(issuanceTime, tz = "UTC"), "America/New_York"))
    ) %>%
    filter(issuingOffice %in% wfo$WFO) %>%
    filter(productName == "Spot Forecast Request") %>%
    rename(api_url = "@id") %>%
    filter(date_issued %in% c(today_val, today_val - 1, today_val - 2))
  
  log_msg("FWS products found:", nrow(fws_df))
  log_msg("STQ products found:", nrow(stq_df))
  
  if (nrow(fws_df) == 0 || nrow(stq_df) == 0) {
    log_msg("No FWS or STQ products found. Writing empty cache.")
    return(empty_cache())
  }
  
  spot_api_df <- map_dfr(fws_df$api_url, spot_df_fun) %>%
    group_by(spot_id) %>%
    slice_max(order_by = issuanceTime, n = 1) %>%
    ungroup()
  
  log_msg("FWS spot forecasts retained:", nrow(spot_api_df))
  
  if (nrow(spot_api_df) == 0) {
    log_msg("No spot forecasts returned from FWS products. Writing empty cache.")
    return(empty_cache())
  }
  
  request_api_df <- map_dfr(stq_df$api_url, stq_df_fun) %>%
    filter(spot_id %in% spot_api_df$spot_id) %>%
    select(spot_id:time) %>%
    distinct()
  
  log_msg("Matching STQ requests retained:", nrow(request_api_df))
  
  if (nrow(request_api_df) == 0) {
    log_msg("No matching STQ requests found. Writing empty cache.")
    return(empty_cache())
  }
  
  request_api_df <- request_api_df %>%
    mutate(tz = tz_lookup_coords(lat, lon, method = "accurate")) %>%
    rowwise() %>%
    mutate(
      forecast_date_time_obj = paste(date, time),
      forecast_date_time = as.POSIXct(
        forecast_date_time_obj,
        format = "%m/%d/%y %H%M",
        tz = tz
      )
    ) %>%
    ungroup()
  
  forecast_df <- left_join(request_api_df, spot_api_df, by = "spot_id") %>%
    mutate(
      spot_id_clean = gsub("\\..*$", "", spot_id),
      nws_spot_url = paste0("https://spot.weather.gov/forecasts/", spot_id_clean),
      issued = ifelse(date_issued == today_val, "Today", "Yesterday")
    ) %>% 
    group_by(nws_spot_url) %>% 
    arrange(desc(date_issued)) %>% 
    slice(1) %>% 
    ungroup()
  
  log_msg("Joined forecast rows:", nrow(forecast_df))
  
  if (nrow(forecast_df) == 0) {
    log_msg("Joined forecast data is empty. Writing empty cache.")
    return(empty_cache())
  }
  
  # -------------------------------------------------
  # FILTER TO REGION 8
  # -------------------------------------------------
  
  r8_bbox <- st_bbox(r8)
  
  forecast_df_bbox <- forecast_df %>%
    filter(
      lon >= r8_bbox["xmin"],
      lon <= r8_bbox["xmax"],
      lat >= r8_bbox["ymin"],
      lat <= r8_bbox["ymax"]
    )
  
  log_msg("After bbox filter:", nrow(forecast_df_bbox))
  
  if (nrow(forecast_df_bbox) == 0) {
    log_msg("No forecasts inside Region 8 bounding box. Writing empty cache.")
    return(empty_cache())
  }
  
  spots_sf <- st_as_sf(
    forecast_df_bbox,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  spots_sf <- spots_sf[lengths(st_intersects(spots_sf, r8)) > 0, ]
  forecast_df <- st_drop_geometry(spots_sf)
  
  log_msg("Inside Region 8 polygon:", nrow(forecast_df))
  
  if (nrow(forecast_df) == 0) {
    log_msg("No forecasts inside Region 8 polygon. Writing empty cache.")
    return(empty_cache())
  }
  
  # -------------------------------------------------
  # CREATE PARSED SPOT TABLES
  # -------------------------------------------------
  
  sfog_tables <- parse_spot_forecasts(forecast_df)
  
  # -------------------------------------------------
  # COMBINE OUTPUTS
  # -------------------------------------------------
  
  list(
    forecast_df = forecast_df,
    sfog_tables = sfog_tables,
    last_refresh = Sys.time()
  )
}

# -------------------------------------------------
# GENERATE AND SAVE CACHE
# -------------------------------------------------

cache_obj <- tryCatch(
  build_cache(),
  error = function(e) {
    log_msg("Cache build failed:", e$message)
    
    if (file.exists(cache_path)) {
      log_msg("Using existing cache file as fallback.")
      fallback_cache <- readRDS(cache_path)
      fallback_cache$last_refresh <- Sys.time()
      fallback_cache
    } else {
      log_msg("No existing cache file found. Writing empty fallback cache.")
      empty_cache()
    }
  }
)

log_msg("About to save cache with last_refresh =", as.character(cache_obj$last_refresh))

saveRDS(cache_obj, cache_path)

check_obj <- readRDS(cache_path)
log_msg("Read-back cache last_refresh =", as.character(check_obj$last_refresh))

log_msg("Cache updated successfully at", as.character(cache_obj$last_refresh))
# -------------------------------
# LIBRARIES
# -------------------------------

library(tidyverse)
library(lubridate)
library(kableExtra)
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
# SETUP TIMING FOR GITHUB ACTIONS
# -------------------------------
force_run <- identical(Sys.getenv("FORCE_RUN"), "true")

now_et <- with_tz(Sys.time(), "America/New_York")
hr <- lubridate::hour(now_et)
mn <- lubridate::minute(now_et)

if (!force_run && !(hr >= 6 && hr <= 10 && mn %in% c(0, 30))) {
  log_msg("Outside 06:00-10:00 ET half-hour window. Exiting.")
  quit(save = "no", status = 0)
}

if (force_run) {
  log_msg("FORCE_RUN detected. Bypassing time window.")
}


# -------------------------------
# PREVENT OVERLAPPING RUNS
# -------------------------------

dir.create("cache", showWarnings = FALSE)


# -------------------------------
# TIMESTAMPED LOGGING
# -------------------------------

log_msg("Starting cache update")
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

# -------------------------------
# API HELPERS
# -------------------------------
spot_df_fun <- function(SPOT_URL) {
  spot_api_ls <- request(SPOT_URL) %>% req_perform() %>% resp_body_json()
  as.data.frame(spot_api_ls[2:9]) %>%
    mutate(
      spot_id = str_extract(productText, "(?<=\\.TAG\\s)[0-9.]+"),
      usda = str_detect(productText, "usda\\.gov"),
      issuanceTime = ymd_hms(issuanceTime)
    ) %>%
    filter(usda == TRUE)
}

stq_df_fun <- function(STQ_URL) {
  stq_api_ls <- request(STQ_URL) %>% req_perform() %>% resp_body_json()
  as.data.frame(stq_api_ls[2:9]) %>%
    mutate(
      spot_id = str_match(productText, "OFILE:\\s*([0-9.]+)")[,2],
      project_name = str_match(productText, "PROJECT NAME:\\s*([^\\n]+)")[,2],
      lat = as.numeric(str_match(productText, "DLAT:\\s*(-?[0-9.]+)")[,2]),
      lon = -abs(as.numeric(str_match(productText, "DLON:\\s*(-?[0-9.]+)")[,2])),
      date = str_match(productText, "DATE:\\s*(\\d{2}/\\d{2}/\\d{2})")[,2],
      time = str_match(productText, "TIME:\\s*(\\d{4})")[,2]
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
  map(1:nrow(df), function(i) {
    tryCatch({
      spot_txt <- df$productText[i]
      spot_info <- df[i,]
      
      spot_chunks <- strsplit(spot_txt, "\n\n")[[1]]
      time_chunks <- spot_chunks[grepl("^(TIME|[^\\n]*TIME)", spot_chunks)]
      
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
        setNames(all_times_df[,1])
      
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
      
    }, error = function(e) NULL)
  })
}

# -------------------------------
# BUILD CACHE
# -------------------------------
build_cache <- function() {
  today_val <- today(tzone = "America/New_York")
  
  fws_ls <- request(fws_nws_base_url) %>% req_perform() %>% resp_body_json()
  stq_ls <- request(stq_nws_base_url) %>% req_perform() %>% resp_body_json()
  
  fws_df <- bind_rows(fws_ls$`@graph`) %>%
    mutate(issuingOffice = str_sub(issuingOffice, 2)) %>%
    filter(issuingOffice %in% wfo$WFO) %>%
    filter(productName == "Suppression Forecast") %>%
    rename(api_url = "@id") %>%
    mutate(date_issued = as.Date(ymd_hms(issuanceTime))) %>%
    filter(date_issued == today_val)
  
  stq_df <- bind_rows(stq_ls$`@graph`) %>%
    mutate(issuingOffice = str_sub(issuingOffice, 2)) %>%
    filter(issuingOffice %in% wfo$WFO) %>%
    filter(productName == "Spot Forecast Request") %>%
    rename(api_url = "@id") %>%
    mutate(date_issued = as.Date(ymd_hms(issuanceTime))) %>%
    filter(date_issued %in% c(today_val, today_val - 1))
  
  spot_api_df <- map_dfr(fws_df$api_url, spot_df_fun) %>%
    group_by(spot_id) %>%
    slice_max(order_by = issuanceTime, n = 1) %>%
    ungroup()
  
  request_api_df <- map_dfr(stq_df$api_url, stq_df_fun) %>%
    filter(spot_id %in% spot_api_df$spot_id) %>%
    select(spot_id:time) %>%
    distinct() %>%
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
      nws_spot_url = paste0("https://spot.weather.gov/forecasts/", spot_id_clean)
    )
  
  # -------------------------------------------------
  # FILTER TO REGION 8
  # -------------------------------------------------
  
  # 1. cheap bounding-box filter first
  r8_bbox <- st_bbox(r8)
  
  forecast_df_bbox <- forecast_df %>%
    filter(
      lon >= r8_bbox["xmin"],
      lon <= r8_bbox["xmax"],
      lat >= r8_bbox["ymin"],
      lat <= r8_bbox["ymax"]
    )
  
  # 2. precise spatial filter only on bbox candidates
  spots_sf <- st_as_sf(
    forecast_df_bbox,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  spots_sf <- spots_sf[
    lengths(st_intersects(spots_sf, r8)) > 0,
  ]
  
  forecast_df <- st_drop_geometry(spots_sf)
  
  log_msg("Total forecasts before Region 8 filter:", nrow(request_api_df))
  log_msg("After bbox filter:", nrow(forecast_df_bbox))
  log_msg("Inside Region 8:", nrow(forecast_df))
  
  
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

cache_obj <- build_cache()
saveRDS(cache_obj, "cache/superfog_cache.rds")


log_msg("Cache updated successfully at", as.character(cache_obj$last_refresh))

check_obj <- readRDS("cache/superfog_cache.rds")
log_msg("Read-back cache last_refresh =", as.character(check_obj$last_refresh))

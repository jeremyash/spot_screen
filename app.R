# PACKAGES ----------------------------------------------

library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(sf)
library(httr2)
library(jsonlite)
library(rvest)
library(xml2)
library(AirMonitor)
library(terra)
library(shinycssloaders)
library(later)
library(lutz)

APP_MODE <- Sys.getenv("APP_MODE", unset = "dev")
IS_DEV <- identical(APP_MODE, "dev")


# SOURCE HELPER FUNCTIONS ----------------------------------------------

source("R/cache_helpers.R")
source("R/map_helpers.R")
source("R/ui_helpers.R")
source("R/airnow_helpers.R")
source("R/sfog_helpers.R")
source("R/selected_info_helpers.R")
source("R/ui_assets.R")

# STATIC DATA AND CACHE----------------------------------------------

wfo <- read_csv("r8_wfo.csv", show_col_types = FALSE)

r8_forests <- readRDS("r8_forests_simplified.rds")

r8 <- st_read("region_8", quiet = TRUE) |>
  st_transform(4326)

r8_forests$forest_id <- seq_len(nrow(r8_forests))

cache_url <- "https://raw.githubusercontent.com/jeremyash/southern-area-forecast-cache/cache-data/cache/spot_superfog_cache.rds"

sfog_display_cache_url <- "https://raw.githubusercontent.com/jeremyash/southern-area-forecast-cache/cache-data/cache/ndfd_superfog_display_cache.rds"

sfog_extract_cache_url <- "https://raw.githubusercontent.com/jeremyash/southern-area-forecast-cache/cache-data/cache/ndfd_superfog_extract_cache.rds"


# INITIAL SPOT FORECAST CACHE LOAD ----------------------------------------------

initial_cache <- tryCatch(
  download_remote_cache(paste0(cache_url, "?t=", as.integer(Sys.time()))),
  error = function(e) {
    list(
      forecast_df = tibble(),
      sfog_tables = list(),
      last_refresh = as.POSIXct(NA)
    )
  }
)


# UI ----------------------------------------------

ui <- fluidPage(
  
  # HEAD / BROWSER ASSETS ----
  
  tags$head(
    
    # Favicons ----
    
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon_v2.svg"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32_v2.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16_v2.png"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon_v2.ico"),
    tags$link(rel = "shortcut icon", href = "favicon_v2.ico"),
    tags$link(rel = "mask-icon", href = "safari-pinned_v2.svg", color = "#3A3640"),
    
    # Superfog PNG overlay JavaScript ----
    sfog_overlay_js(),
    
    
    # Superfog PNG overlay CSS ----
    sfog_overlay_css(),
    
    # Spot map controls JavaScript ----
    spot_map_controls_js(),
    
    # Other CSS ---
    app_theme_css()
  ),
  
  
  # TITLE ----
  
  div(
    class = "sa-header",
    
    div(
      class = "sa-header-title",
      if (IS_DEV) {
        "USFS Southern Area Superfog Screener — DEV"
      } else {
        "USFS Southern Area Superfog Screener"
      }
    )
  ),
  
  
  # MAIN TABS ----
  
  tabsetPanel(
    id = "main_tabs",

        
        
        # SPOT MAP TAB ----
        
        tabPanel(
          "Spot Map",
          fluidRow(
            column(
              8,
              div(
                style = "position:relative;",
                
                uiOutput("spot_map_loading_overlay"),
                
                leafletOutput("forecast_map", height = "650px"),
                uiOutput("aqi_loading_overlay")
                
              ),
              div(
                class = "sa-card",
                style = "
              margin-top:10px;
              padding-top:10px;
              padding-bottom:10px;
            ",
                
                div(
                  style = "
                font-size:14px;
                color:#555555;
                font-weight:500;
              ",
                  textOutput("last_refresh_text")
                )
              )
            ),
            column(
              4,
              div(
                style = "
              height:650px;
              overflow-y:auto;
              background:#f7f9fb;
              padding:15px;
              border-radius:10px;
            ",
                
                div(
                  class = "sa-card sa-fade-in",
                  
                  uiOutput("selected_info_map")
                )
              )
            )
          )
        ),
        
        
        # SPOT TABLE TAB ----
        
        tabPanel(
          "Spot Table",
          fluidRow(
            column(
              5,
              div(
                style = "
              height:650px;
              overflow-y:auto;
              padding-right:10px;
            ",
                div(
                  style = "max-width:1200px; margin:auto; padding-top:15px;",
                  uiOutput("burn_table_grouped")
                )
              )
            ),
            column(
              7,
              div(
                style = "
              height:650px;
              overflow-y:auto;
              background:#f7f9fb;
              padding:15px;
              border-radius:10px;
            ",
                
                div(
                  class = "sa-card sa-fade-in",
                  
                  uiOutput("selected_info_table")
                )
              )
            )
          )
        ),
        
        
        # SUPERFOG RISK TAB ----
        
        tabPanel(
          "Superfog Risk",
          fluidRow(
            column(
              8,
              
              uiOutput("sfog_loading_bar"),
              
              div(
                style = "position:relative;",
                
                uiOutput("sfog_map_loading_overlay"),
                
                leafletOutput(
                  "sfog_map",
                  height = "520px"
                )
              ),
              
              uiOutput("sfog_plot_container")
            ),
            
            column(
              4,
              div(
                style = "
              background:#f7f9fb;
              padding:15px;
              border-radius:10px;
              height:700px;
              overflow-y:auto;
            ",
                
                div(
                  class = "sa-time-card",
                  
                  div(class = "sa-card-title", "Forecast Time"),
                  
                  fluidRow(
                    style = "display:flex; align-items:center;",
                    column(
                      width = 2,
                      actionButton(
                        "sfog_prev_hour",
                        label = NULL,
                        icon = icon("chevron-left"),
                        width = "100%",
                        style = "
                          height:38px;
                          display:flex;
                          align-items:center;
                          justify-content:center;
                          padding:0;
                          font-size:22px;
                        "
                      )
                    ),
                    
                    column(
                      width = 8,
                      div(
                        class = "sfog-time-card",
                        
                        div(
                          class = "sfog-time-card-value",
                          textOutput("sfog_valid_time")
                        )
                      )
                    ),
                    
                    column(
                      width = 2,
                      actionButton(
                        "sfog_next_hour",
                        label = NULL,
                        icon = icon("chevron-right"),
                        width = "100%",
                        style = "
                          height:38px;
                          display:flex;
                          align-items:center;
                          justify-content:center;
                          padding:0;
                          font-size:22px;
                        "
                      )
                    )
                  ),
                  
                  div(
                    class = "sfog-slider-wrap",
                    style = "
                      width:100%;
                      margin-top:10px;
                      margin-bottom:10px;
                    ",
                    
                    uiOutput("sfog_time_slider")
                  ),
                  
                  uiOutput("sfog_cache_message")
                ),
                
                div(
                  class = "sa-point-card",
                  
                  div(class = "sa-card-title", "Point Risk Time Series"),
                  
                  p(
                    class = "sa-muted",
                    "Enter a latitude/longitude or click the map."
                  ),
                  
                  textInput(
                    "sfog_query_lat",
                    "Latitude",
                    value = "",
                    placeholder = "e.g. 35.5951"
                  ),
                  
                  textInput(
                    "sfog_query_lon",
                    "Longitude",
                    value = "",
                    placeholder = "e.g. -82.5515"
                  ),
                  
                  actionButton(
                    "sfog_extract_point",
                    "Plot Point Risk",
                    width = "100%",
                    class = "sa-primary-btn"
                  )
                ),
                div(
                  class = "sa-sfog-sidebar-warning",
                  
                  div(
                    class = "sa-sfog-sidebar-warning-label",
                    "Warning"
                  ),
                  
                  div(
                    class = "sa-sfog-sidebar-warning-text",
                    "This visualization is intended as a situational awareness and planning aid and should not replace NWS Spot Forecasts."
                  )
                ),
              )
            )
          )
        ),
        
        
        # ABOUT TAB ----
        
        tabPanel(
          "About",
          div(
            style = "max-width:950px; margin:auto; font-size:16px; line-height:1.7;",
            div(
              class = "sa-card",
              
              h2("About the Spot Forecast Screening"),
              
              p("This tool filters the available NWS Spot Weather forecasts to those occurring on US Forest Service Southern Area National Forest System units."),
              
              p(HTML(
                "The <strong>Spot Map</strong> tab provides a spatial overview of active spot forecasts issued for Southern Area units, while the <strong>Spot Table</strong> tab organizes the same forecasts by National Forest for easier review and navigation."
              )),
              
              p(
                "Forecasts issued today are displayed separately from forecasts issued yesterday, allowing users to quickly identify newly issued spot forecasts while still maintaining visibility of recent forecast activity."
              ),
              
              p(
                "Selecting a burn unit from either the map or table displays the associated superfog screening results and highlights forecast periods where nighttime smoke and fog concerns may be elevated."
              ),
              
              p(
                HTML('Using the <a href="https://usdagcc.sharepoint.com/sites/fs-r08-sm/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Ffs%2Dr08%2Dsm%2FShared%20Documents%2FGeneral%2FGuidance%20and%20Forms%2FR8%20Smoke%20Management%20Guidelines%20%28March%202022%29%2Epdf&parent=%2Fsites%2Ffs%2Dr08%2Dsm%2FShared%20Documents%2FGeneral%2FGuidance%20and%20Forms&p=true&ga=1" target="_blank">R8 Smoke Management Guidelines</a>, each spot forecast is screened to identify weather conditions that may increase the likelihood of <strong>superfog formation</strong>. When these conditions occur, it is necessary to run a nighttime smoke dispersion model using <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont</a>.')
              ),
              
              p("The screening focuses on overnight and early morning forecast hours and evaluates four key variables:"),
              
              br(),
              
              div(
                class = "sa-card",
                style = "max-width:720px; margin:auto;",
                
                div(
                  class = "sa-card-title",
                  "Superfog Screening Thresholds"
                ),
                
                tags$table(
                  style = "
              width:100%;
              border-collapse:separate;
              border-spacing:0;
              font-size:15px;
            ",
                  
                  tags$thead(
                    tags$tr(
                      tags$th(
                        style = "
                    padding:12px;
                    text-align:left;
                    color:#2b2f36;
                    font-weight:700;
                    border:2px solid #2b2f36;
                  ",
                        "Variable"
                      ),
                      tags$th(
                        style = "
                    padding:12px;
                    text-align:center;
                    color:#2b2f36;
                    font-weight:700;
                    border:2px solid #2b2f36;
                  ",
                        "Watch Out"
                      ),
                      tags$th(
                        style = "
                    padding:12px;
                    text-align:center;
                    color:#2b2f36;
                    font-weight:700;
                    border:2px solid #2b2f36;
                  ",
                        "Critical"
                      )
                    )
                  ),
                  
                  tags$tbody(
                    tags$tr(
                      style = "background:#fafbfc;",
                      
                      tags$td(
                        style = "padding:10px; font-weight:600;",
                        "Temperature"
                      ),
                      
                      tags$td(
                        style = "
            background:#FFE066;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        "<70°F"
                      ),
                      
                      tags$td(
                        style = "
            background:#C92A2A;
            color:white;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        "<55°F"
                      )
                    ),
                    
                    tags$tr(
                      tags$td(
                        style = "padding:10px; font-weight:600;",
                        "Relative Humidity"
                      ),
                      
                      tags$td(
                        style = "
            background:#FFE066;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        ">70%"
                      ),
                      
                      tags$td(
                        style = "
            background:#C92A2A;
            color:white;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        ">90%"
                      )
                    ),
                    
                    tags$tr(
                      style = "background:#fafbfc;",
                      
                      tags$td(
                        style = "padding:10px; font-weight:600;",
                        "Surface (20 ft) Wind Speed"
                      ),
                      
                      tags$td(
                        style = "
            background:#FFE066;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        "<7 mph"
                      ),
                      
                      tags$td(
                        style = "
            background:#C92A2A;
            color:white;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        "<4 mph"
                      )
                    ),
                    
                    tags$tr(
                      tags$td(
                        style = "padding:10px; font-weight:600;",
                        "Cloud Cover"
                      ),
                      
                      tags$td(
                        style = "
            background:#FFE066;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        "<60%"
                      ),
                      
                      tags$td(
                        style = "
            background:#C92A2A;
            color:white;
            padding:10px;
            text-align:center;
            font-weight:bold;
          ",
                        "<40%"
                      )
                    )
                  )
                )
              ),
              
              br(),
              
              h3("How the Screening Works"),
              
              p(
                HTML('If any hour meets the Watch Out or Critical thresholds for <strong>all 4 criteria</strong>, it is required to examine nighttime smoke dispersion and potential superfog formation using <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont</a>.')
              ),
              
              p(
                HTML('Instances where <strong>3 out of the 4 criteria</strong> are met suggest a higher level of concern and it is recommended to be conservative and run a nighttime smoke dispersion model.')
              ),
              
              br(),
              
              div(
                class = "sa-card",
                style = "
            max-width:750px;
            margin:auto;
          ",
                h3(style = "margin-top:0; margin-bottom:15px;", "PB Piedmont Decision Guide"),
                sfog_legend_box("PB Piedmont Required", "red", "#FFDADA", "All variables in Critical or Watch Out"),
                sfog_legend_box("PB Piedmont Recommended", "orange", "#FFE8CC", "3 of 4 variables in Critical or Watch Out"),
                sfog_legend_box("PB Piedmont Not Required", "#777777", "#D9D9D9", "<3 variables in Critical or Watch Out")
              ),
              
              br(),
              
              h3("Update Cycle"),
              
              p(
                HTML('The data is set to refresh every 30 minutes, so it may take up to 30 minutes from the time your forecast is generated to display on this site. If your forecast is still not displaying on the map, visit the <a href="https://spot.weather.gov/" target="_blank">NWS Spot Forecast</a> page and conduct a manual screening using the thresholds outlined above.')
              ),
              
              
              br(),
              
            ),
            
            hr(style = "margin-top:40px; margin-bottom:40px;"),
            
            div(
              class = "sa-card",
              
              h2("About the Superfog Risk Visualization"),
              
              div(
                style = "
    max-width:950px;
    margin:0 auto 22px auto;
    padding:16px 20px;
    
    background:#fff8e1;
    border-left:6px solid #d97706;
    border-radius:10px;
    
    box-shadow:0 2px 8px rgba(0,0,0,0.06);
  ",
                
                div(
                  style = "
      font-size:13px;
      font-weight:800;
      letter-spacing:0.08em;
      text-transform:uppercase;
      color:#92400e;
      margin-bottom:6px;
    ",
                  "Important"
                ),
                
                div(
                  style = "
      font-size:17px;
      line-height:1.5;
      color:#3b2f1c;
      font-weight:600;
    ",
                  
                  "This visualization is intended as a situational awareness and planning aid and should not replace NWS Spot Forecasts."
                )
              ),
              
              p(
                HTML('The <strong>Superfog Risk</strong> tab provides an experimental regional visualization of hourly superfog risk conditions across the Southern Area using forecast data from the <a href="https://vlab.noaa.gov/web/mdl/ndfd" target="_blank">National Digital Forecast Database (NDFD)</a>.')
              ),
              
              p(
                "Unlike the Spot Map and Spot Table tabs, which screen individual NWS Spot Weather Forecasts, the Superfog Risk tab uses gridded forecast data to visualize the broader spatial distribution of nighttime superfog risk."
              ),
              
              p(
                "The visualization evaluates hourly forecast conditions using the same four screening variables applied in the spot forecast screening process:"
              ),
              
              tags$ul(
                tags$li("Temperature"),
                tags$li("Relative Humidity"),
                tags$li("Surface Wind Speed"),
                tags$li("Cloud Cover")
              ),
              
              p(
                "Each hourly grid cell is categorized into one of three superfog risk levels:"
              ),
              
              div(
                class = "sa-card",
                style = "
            max-width:750px;
            margin:auto;
          ",
                
                h3(
                  style = "margin-top:0; margin-bottom:15px;",
                  "Superfog Risk Categories"
                ),
                
                sfog_legend_box(
                  "High Risk",
                  "#CA0020",
                  "#FFDADA",
                  "Most or all screening variables meet Critical thresholds"
                ),
                
                sfog_legend_box(
                  "Moderate Risk",
                  "#FFB000",
                  "#FFE8CC",
                  "Multiple variables meet Watch Out or Critical thresholds"
                ),
                
                sfog_legend_box(
                  "Minimal Risk",
                  "#58AFDD",
                  "#DCEEFF",
                  "Few or no variables meet superfog screening thresholds"
                )
              ),
              
              br(),
              
              p(
                "The map updates every 30 minutes and displays hourly forecast guidance for the next several days. Users may click the map or manually enter latitude/longitude coordinates to generate a point-based superfog risk time series."
              ),
              
              br(),
              
              h3("Additional Resources"),
              
              p(
                HTML('For more information and additional smoke management resources, please visit the <a href="https://usdagcc.sharepoint.com/sites/fs-r08-sm" target="_blank">USFS Southern Area Smoke Management Site</a>.')
              )
            )
          )
        )
  )
)


# SERVER ----------------------------------------------


server <- function(input, output, session) {
  
  # STATE / CACHE OBJECTS -----------------------------------------------
  
  cache_data <- reactiveVal(initial_cache)
  
  spot_base_map_ready <- reactiveVal(FALSE)
  spot_markers_ready <- reactiveVal(FALSE)
  spot_cache_status <- reactiveVal("loading")
  
  airnow_today_data <- reactiveVal(NULL)
  airnow_tomorrow_data <- reactiveVal(NULL)
  
  airnow_today_added <- reactiveVal(FALSE)
  airnow_tomorrow_added <- reactiveVal(FALSE)
  
  aqi_loading <- reactiveVal(FALSE)
  
  sfog_cache <- reactiveVal(NULL)
  sfog_cache_status <- reactiveVal("not_loaded")
  
  sfog_extract_cache <- reactiveVal(NULL)
  sfog_extract_status <- reactiveVal("not_loaded")
  
  sfog_overlay_ready <- reactiveVal(FALSE)
  
  selected_burn_id <- reactiveVal(NULL)
  
  selected_sfog_point <- reactiveVal(NULL)
  
  # CACHE LOADING OBSERVERS ---------------------------------------------
  
  observe({
    invalidateLater(60 * 1000, session)
    
    try({
      fresh_cache <- download_remote_cache(
        paste0(cache_url, "?t=", as.integer(Sys.time()))
      )
      cache_data(fresh_cache)
    }, silent = TRUE)
  })
  
  observeEvent(input$main_tabs, {
    
    if (
      input$main_tabs == "Superfog Risk" &&
      !identical(sfog_cache_status(), "loaded") &&
      !identical(sfog_cache_status(), "loading")
    ) {
      
      sfog_overlay_ready(FALSE)
      
      message("Loading superfog display cache...")
      
      sfog_cache_status("loading")
      
      tryCatch({
        
        cache_obj <- download_sfog_display_cache(
          paste0(
            sfog_display_cache_url,
            "?t=",
            as.integer(Sys.time())
          )
        )
        
        sfog_cache(cache_obj)
        sfog_cache_status("loaded")
        
        message("Superfog display cache loaded successfully.")
        
      }, error = function(e) {
        
        message("Superfog display cache failed to load.")
        message(e$message)
        
        sfog_cache(NULL)
        sfog_cache_status("failed")
      })
    }
  })
  
  
  # SHARED REACTIVES ----------------------------------------------------
  
  burns_with_forest <- reactive({
    forecast_df <- cache_data()$forecast_df
    
    if (nrow(forecast_df) == 0 || !all(c("lon", "lat") %in% names(forecast_df))) {
      return(tibble())
    }
    
    burns_sf <- st_as_sf(
      forecast_df,
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )
    
    joined <- st_join(
      burns_sf,
      r8_forests[, c("forest_id", "forest")],
      join = st_intersects,
      left = TRUE
    )
    
    joined |>
      st_drop_geometry() |>
      mutate(forest = if_else(is.na(forest), "Not matched", forest))
  })
  
  sfog_point_risk <- reactive({
    
    req(identical(sfog_cache_status(), "loaded"))
    
    pt_info <- selected_sfog_point()
    
    req(pt_info)
    
    lat <- pt_info$lat
    lon <- pt_info$lon
    
    shiny::validate(
      shiny::need(!is.na(lat), "Please enter a valid latitude."),
      shiny::need(!is.na(lon), "Please enter a valid longitude.")
    )
    
    cache_ready <- load_sfog_extract_cache_if_needed()
    
    shiny::validate(
      shiny::need(
        cache_ready,
        "Superfog extraction cache is loading or unavailable. Try again in a moment."
      )
    )
    
    x <- sfog_extract_cache()
    
    
    df_extract <- x$sfog_extract_df
    
    dist_sq <- (df_extract$lon - lon)^2 +
      (df_extract$lat - lat)^2
    
    nearest_index <- which.min(dist_sq)
    
    nearest_row <- df_extract[nearest_index, ]
    
    distance_deg <- sqrt(min(dist_sq))
    
    shiny::validate(
      shiny::need(
        distance_deg < 0.25,
        "Location is outside of the Southern Area."
      )
    )
    
    risk_vals <- nearest_row |>
      dplyr::select(-cell, -lon, -lat) |>
      unlist(use.names = FALSE) |>
      as.numeric()
    
    leafletProxy("sfog_map") |>
      clearGroup("Point Query") |>
      addCircleMarkers(
        lng = lon,
        lat = lat,
        group = "Point Query",
        radius = 7,
        stroke = TRUE,
        weight = 2,
        color = "white",
        fillColor = "black",
        fillOpacity = 1,
        label = paste0(
          "Point Query: ",
          round(lat, 4),
          ", ",
          round(lon, 4)
        )
      ) |>
      fitBounds(
        lng1 = lon - 0.5,
        lat1 = lat - 0.5,
        lng2 = lon + 0.5,
        lat2 = lat + 0.5
      )
    
    valid_times <- x$valid_times
    
    point_tz <- lookup_point_tz(lat, lon)
    
    data.frame(
      time_utc = valid_times,
      time_local = lubridate::with_tz(
        valid_times,
        point_tz
      ),
      timezone = point_tz,
      risk = risk_vals,
      lat = lat,
      lon = lon
    )
  })
  
  # HELPER FUNCTIONS INSIDE SERVER ----
  set_sfog_overlay <- function(hour_index) {
    req(identical(sfog_cache_status(), "loaded"))
    
    x <- sfog_cache()
    hour_index <- as.numeric(hour_index)
    
    req(!is.null(x$overlay_info))
    req(hour_index >= 1)
    req(hour_index <= nrow(x$overlay_info))
    
    overlay_row <- x$overlay_info[hour_index, ]
    
    session$sendCustomMessage(
      type = "sfog_set_overlay",
      message = list(
        url = overlay_row$png_url,
        west = overlay_row$west,
        south = overlay_row$south,
        east = overlay_row$east,
        north = overlay_row$north
      )
    )
    
    later::later(
      function() {
        sfog_overlay_ready(TRUE)
      },
      delay = 1.5
    )
  }
  
  load_sfog_extract_cache_if_needed <- function() {
    
    if (identical(sfog_extract_status(), "loaded")) {
      return(TRUE)
    }
    
    if (identical(sfog_extract_status(), "loading")) {
      return(FALSE)
    }
    
    sfog_extract_status("loading")
    
    tryCatch({
      
      cache_obj <- download_sfog_extract_cache(
        paste0(
          sfog_extract_cache_url,
          "?t=",
          as.integer(Sys.time())
        )
      )
      
      sfog_extract_cache(cache_obj)
      sfog_extract_status("loaded")
      
      TRUE
      
    }, error = function(e) {
      
      message("Superfog extraction cache failed to load.")
      message(e$message)
      
      sfog_extract_cache(NULL)
      sfog_extract_status("failed")
      
      FALSE
    })
  }
  
  # TEXT OUTPUTS --------------------------------------------------------
  
  output$last_refresh_text <- renderText({
    lr <- cache_data()$last_refresh
    
    if (is.na(lr)) {
      "Last refreshed: cache not available"
    } else {
      paste0(
        "Last refreshed: ",
        format(with_tz(lr, "America/New_York"), "%Y-%m-%d %H:%M %Z")
      )
    }
  })
  
  
  
  # MAP OUTPUTS ---------------------------------------------------------
  
  output$forecast_map <- renderLeaflet({
    df <- cache_data()$forecast_df
    df_map <- offset_duplicate_points(df)
    
    fire_icon_url_today <- make_fire_icon_path("today")
    fire_icon_url_yesterday <- make_fire_icon_path("yesterday")
    
    fire_icon_today <- icons(
      iconUrl = fire_icon_url_today,
      iconWidth = 28,
      iconHeight = 28,
      iconAnchorX = 14,
      iconAnchorY = 14
    )
    
    fire_icon_yesterday <- icons(
      iconUrl = fire_icon_url_yesterday,
      iconWidth = 28,
      iconHeight = 28,
      iconAnchorX = 14,
      iconAnchorY = 14
    )
    
    m <- leaflet() |>
      addTiles() |>
      setView(lng = -88.11, lat = 34.95, zoom = 5) |>
      addPolygons(
        data = r8,
        color = "#5b6573",
        weight = 1.2,
        opacity = 0.55,
        fill = FALSE,
        options = pathOptions(clickable = FALSE)
      ) |>
      addPolygons(
        data = r8_forests,
        layerId = ~paste0("forest_", forest_id),
        fillColor = "#228B22",
        fillOpacity = 0.18,
        color = "#4f6f52",
        weight = 0.8,
        opacity = 0.7,
        smoothFactor = 0.5,
        options = pathOptions(clickable = TRUE)
      )
    
    
    legend_toggle_html <- spot_map_toggle_legend(
      fire_icon_url_today,
      fire_icon_url_yesterday
    )
    
    spot_base_map_ready(TRUE)
    
    m |>
      addControl(html = legend_toggle_html, position = "bottomright") |>
      addLayersControl(
        overlayGroups = c("AQI Forecast Today", "AQI Forecast Tomorrow"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomleft"
      ) |>
      addControl(
        html = spot_map_reset_button(),
        position = "topright"
      )
  })
  
  output$sfog_map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      fitBounds(
        lng1 = -96,
        lat1 = 24,
        lng2 = -74,
        lat2 = 38
      ) |>
      addPolygons(
        data = r8,
        color = "#5b6573",
        weight = 1.2,
        opacity = 0.55,
        fill = FALSE
      ) |>
      addPolygons(
        data = r8_forests,
        color = "#4f6f52",
        weight = 0.8,
        opacity = 0.55,
        fillColor = "#6b8f71",
        fillOpacity = 0.08,
        smoothFactor = 0.8,
        group = "Region 8 Forests",
        options = pathOptions(clickable = FALSE)
      ) |>
      addControl(
        html = sfog_risk_legend,
        position = "bottomright",
        layerId = "sfog_legend"
      ) |>
      addControl(
        html = sfog_map_reset_button(),
        position = "topright"
      )
  })
  
  output$sfog_plot_container <- renderUI({
    
    req(selected_sfog_point())
    
    div(
      class = "sa-card sa-fade-in",
      style = "margin-top:8px;",
      
      plotOutput(
        "sfog_point_risk_plot",
        height = "290px"
      )
    )
  })
  
  
  # MAP PROXY OBSERVERS -------------------------------------------------
  
  observeEvent(input$forecast_map_groups, {
    
    active_groups <- input$forecast_map_groups
    
    needs_today_load <- "AQI Forecast Today" %in% active_groups &&
      is.null(airnow_today_data())
    
    needs_tomorrow_load <- "AQI Forecast Tomorrow" %in% active_groups &&
      is.null(airnow_tomorrow_data())
    
    show_aqi_loading <- needs_today_load || needs_tomorrow_load
    
    if (show_aqi_loading) {
      aqi_loading(TRUE)
      
      on.exit(
        later::later(
          function() {
            aqi_loading(FALSE)
          },
          delay = 0.5
        ),
        add = TRUE
      )
    }
    
    if ("AQI Forecast Today" %in% active_groups) {
      
      if (is.null(airnow_today_data())) {
        airnow_today_data(load_airnow_kml("today"))
      }
      
      if (!airnow_today_added()) {
        leafletProxy("forecast_map") |>
          addPolygons(
            data = airnow_today_data(),
            fillColor = ~aqi_color,
            fillOpacity = 0.8,
            color = ~aqi_color,
            weight = 1,
            opacity = 0.8,
            smoothFactor = 0.5,
            popup = ~paste0("<strong>AQI Forecast Today</strong><br>", aqi_cat),
            group = "AQI Forecast Today"
          )
        
        airnow_today_added(TRUE)
      }
      
      leafletProxy("forecast_map") |>
        showGroup("AQI Forecast Today")
      
    } else {
      leafletProxy("forecast_map") |>
        hideGroup("AQI Forecast Today")
    }
    
    if ("AQI Forecast Tomorrow" %in% active_groups) {
      
      if (is.null(airnow_tomorrow_data())) {
        airnow_tomorrow_data(load_airnow_kml("tomorrow"))
      }
      
      if (!airnow_tomorrow_added()) {
        leafletProxy("forecast_map") |>
          addPolygons(
            data = airnow_tomorrow_data(),
            fillColor = ~aqi_color,
            fillOpacity = 0.8,
            color = ~aqi_color,
            weight = 1,
            opacity = 0.8,
            smoothFactor = 0.5,
            popup = ~paste0("<strong>AQI Forecast Tomorrow</strong><br>", aqi_cat),
            group = "AQI Forecast Tomorrow"
          )
        
        airnow_tomorrow_added(TRUE)
      }
      
      leafletProxy("forecast_map") |>
        showGroup("AQI Forecast Tomorrow")
      
    } else {
      leafletProxy("forecast_map") |>
        hideGroup("AQI Forecast Tomorrow")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$forecast_map_marker_click, {
    
    handle_burn_click(
      input$forecast_map_marker_click,
      selected_burn_id
    )
  })
  
  observeEvent(input$forecast_map_shape_click, {
    click <- input$forecast_map_shape_click
    req(click$id)
    
    if (startsWith(click$id, "forest_")) {
      forest_id_clicked <- sub("^forest_", "", click$id)
      
      forest_row <- r8_forests %>%
        filter(forest_id == as.integer(forest_id_clicked))
      
      req(nrow(forest_row) == 1)
      
      bb <- st_bbox(forest_row)
      
      leafletProxy("forecast_map") |>
        clearPopups() |>
        fitBounds(
          lng1 = unname(bb["xmin"]),
          lat1 = unname(bb["ymin"]),
          lng2 = unname(bb["xmax"]),
          lat2 = unname(bb["ymax"])
        ) |>
        addPopups(
          lng = click$lng,
          lat = click$lat,
          popup = paste0(
            "<div style='",
            "padding:6px 10px;",
            "font-size:17px;",
            "font-weight:700;",
            "color:#243447;",
            "text-align:center;",
            "letter-spacing:0.01em;",
            "font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Helvetica,Arial,sans-serif;",
            "'>",
            forest_row$forest[1],
            "</div>"
          )
        )
    }
  })
  
  observeEvent(input$map_layer_choice, {
    req(input$map_layer_choice)
    
    selected_burn_id(NULL)
    
    leafletProxy("forecast_map") |>
      clearPopups()
    
    if (input$map_layer_choice == "Today") {
      leafletProxy("forecast_map") |>
        showGroup("Today") |>
        hideGroup("Yesterday")
    } else if (input$map_layer_choice == "Yesterday") {
      leafletProxy("forecast_map") |>
        hideGroup("Today") |>
        showGroup("Yesterday")
    }
  })
  
  observeEvent(input$forecast_map_bounds, {
    leafletProxy("forecast_map") |>
      showGroup("Today") |>
      hideGroup("Yesterday") |>
      hideGroup("AQI Forecast Today") |>
      hideGroup("AQI Forecast Tomorrow")
    
  }, once = TRUE)
  
  observeEvent(cache_data(), {
    
    req(spot_base_map_ready())
    
    df <- cache_data()$forecast_df
    df_map <- offset_duplicate_points(df)
    
    fire_icon_today <- leaflet::icons(
      iconUrl = make_fire_icon_path("today"),
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )
    
    fire_icon_yesterday <- leaflet::icons(
      iconUrl = make_fire_icon_path("yesterday"),
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )
    
    leaflet::leafletProxy("forecast_map") |>
      leaflet::clearGroup("Today") |>
      leaflet::clearGroup("Yesterday")
    
    if (nrow(df_map) > 0 && all(c("lon", "lat") %in% names(df_map))) {
      
      df_today <- df_map |>
        dplyr::filter(issued == "Today")
      
      df_yesterday <- df_map |>
        dplyr::filter(issued == "Yesterday")
      
      if (nrow(df_today) > 0) {
        leaflet::leafletProxy("forecast_map") |>
          leaflet::addMarkers(
            data = df_today,
            lng = ~offset_lon,
            lat = ~offset_lat,
            layerId = ~spot_id,
            group = "Today",
            label = ~project_name,
            labelOptions = marker_label_opts,
            icon = fire_icon_today
          )
      }
      
      if (nrow(df_yesterday) > 0) {
        leaflet::leafletProxy("forecast_map") |>
          leaflet::addMarkers(
            data = df_yesterday,
            lng = ~offset_lon,
            lat = ~offset_lat,
            layerId = ~spot_id,
            group = "Yesterday",
            label = ~project_name,
            labelOptions = marker_label_opts,
            icon = fire_icon_yesterday
          )
      }
    }
    
    leaflet::leafletProxy("forecast_map") |>
      leaflet::showGroup("Today") |>
      leaflet::hideGroup("Yesterday")
    
    spot_markers_ready(TRUE)
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$reset_map_click, {
    selected_burn_id(NULL)
    
    leafletProxy("forecast_map") |>
      clearPopups() |>
      setView(
        lng = -88.11,
        lat = 34.95,
        zoom = 5
      )
  })
  
  observeEvent(input$sfog_hour, {
    
    req(identical(sfog_cache_status(), "loaded"))
    req(input$sfog_hour)
    
    set_sfog_overlay(input$sfog_hour)
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$sfog_map_bounds, {
    req(identical(sfog_cache_status(), "loaded"))
    
    selected_hour <- input$sfog_hour
    if (is.null(selected_hour)) selected_hour <- 1
    
    set_sfog_overlay(selected_hour)
  }, ignoreInit = FALSE)
  
  observeEvent(input$sfog_prev_hour, {
    
    req(input$sfog_hour)
    
    updateSliderInput(
      session,
      "sfog_hour",
      value = max(1, input$sfog_hour - 1)
    )
  })
  
  observeEvent(input$sfog_next_hour, {
    
    req(input$sfog_hour)
    
    x <- sfog_cache()
    
    updateSliderInput(
      session,
      "sfog_hour",
      value = min(length(x$valid_times), input$sfog_hour + 1)
    )
  })
  
  observeEvent(input$sfog_map_click, {
    
    lat <- input$sfog_map_click$lat
    lon <- input$sfog_map_click$lng
    
    updateTextInput(
      session,
      "sfog_query_lat",
      value = round(lat, 5)
    )
    
    updateTextInput(
      session,
      "sfog_query_lon",
      value = round(lon, 5)
    )
    
    selected_sfog_point(
      list(
        lat = lat,
        lon = lon,
        source = "map"
      )
    )
  })
  
  observeEvent(input$sfog_extract_point, {
    
    lat <- as.numeric(input$sfog_query_lat)
    lon <- as.numeric(input$sfog_query_lon)
    
    selected_sfog_point(
      list(
        lat = lat,
        lon = lon,
        source = "manual"
      )
    )
  })
  
  observeEvent(input$sfog_reset_map_click, {
    
    selected_sfog_point(NULL)
    
    updateTextInput(
      session,
      "sfog_query_lat",
      value = ""
    )
    
    updateTextInput(
      session,
      "sfog_query_lon",
      value = ""
    )
    
    leafletProxy("sfog_map") |>
      clearGroup("Point Query") |>
      fitBounds(
        lng1 = -96,
        lat1 = 24,
        lng2 = -74,
        lat2 = 38
      )
  })
  
  # TABLE / SELECTION OBSERVERS -----------------------------------------
  
  observeEvent(input$table_burn_click, {
    selected_burn_id(input$table_burn_click)
  })
  
  
  
  # UI OUTPUTS ----------------------------------------------------------
  
  output$burn_table_grouped <- renderUI({
    burns_tbl <- burns_with_forest()
    
    if (nrow(burns_tbl) == 0) {
      return(
        div(
          style = "
            margin-top:20px;
            padding:15px;
            border:2px dashed #cccccc;
            background:#f9f9f9;
            text-align:center;
            font-size:18px;
          ",
          "No cached burns available."
        )
      )
    }
    
    burns_tbl <- burns_tbl %>%
      mutate(
        issued_order = ifelse(issued == "Today", 0, 1),
        issued_display = if ("issuance_display" %in% names(burns_tbl)) {
          issuance_display
        } else {
          format_issued_datetime(issuanceTime)
        }
      ) %>%
      arrange(forest, issued_order, desc(issuanceTime), project_name)
    
    forest_groups <- split(burns_tbl, burns_tbl$forest)
    
    tagList(
      lapply(names(forest_groups), function(forest_name) {
        forest_df <- forest_groups[[forest_name]]
        
        tagList(
          div(
            style = "
              margin-top:20px;
              margin-bottom:12px;
              padding:12px 16px;
              background:#f7faf7;
              border:1px solid #d8e6d8;
              border-left:6px solid #228B22;
              border-radius:8px;
              box-shadow:0 1px 3px rgba(0,0,0,0.05);
              font-weight:700;
              font-size:20px;
              color:#1f3b1f;
            ",
            forest_name
          ),
          tags$table(
            style = "
              width:100%;
              border-collapse:separate;
              border-spacing:0;
              margin-bottom:20px;
              font-size:16px;
              table-layout:fixed;
            ",
            tags$colgroup(
              tags$col(style = "width:auto;"),
              tags$col(style = "width:210px;")
            ),
            tags$thead(
              tags$tr(
                tags$th(style = "text-align:left; padding:10px; border-bottom:2px solid #cccccc;", "Burn Unit"),
                tags$th(style = "text-align:left; padding:10px; border-bottom:2px solid #cccccc; width:210px; white-space:nowrap;", "Date Issued")
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(forest_df)), function(i) {
                is_selected <- identical(selected_burn_id(), forest_df$spot_id[i])
                
                row_bg <- if (is_selected) "#e8f4ea" else "transparent"
                border_color <- if (is_selected) "#228B22" else "transparent"
                text_color <- if (is_selected) "#000000" else "#1a1a1a"
                font_weight <- if (is_selected) "700" else "600"
                
                tags$tr(
                  style = "border-bottom:1px solid #e6e6e6;cursor:pointer;transition:background-color 0.15s ease;",
                  onclick = paste0(
                    "Shiny.setInputValue('table_burn_click','",
                    forest_df$spot_id[i],
                    "', {priority: 'event'});"
                  ),
                  onmouseover = if (!is_selected) "this.style.backgroundColor='#f5f5f5';" else "",
                  onmouseout = if (!is_selected) "this.style.backgroundColor='transparent';" else "",
                  tags$td(
                    style = paste0(
                      "padding:12px 10px;",
                      "color:", text_color, ";",
                      "font-weight:", font_weight, ";",
                      "background-color:", row_bg, ";",
                      "border-left:5px solid ", border_color, ";"
                    ),
                    forest_df$project_name[i]
                  ),
                  tags$td(
                    style = paste0(
                      "padding:12px 10px;",
                      "color:#1a1a1a;",
                      "background-color:", row_bg, ";",
                      "width:210px;",
                      "white-space:nowrap;"
                    ),
                    forest_df$issued_display[i]
                  )
                )
              })
            )
          )
        )
      })
    )
  })
  
  output$selected_info_map <- renderUI({
    df <- cache_data()$forecast_df
    
    active_layer <- input$map_layer_choice
    if (is.null(active_layer)) active_layer <- "Today"
    
    has_active_layer <- any(df$issued == active_layer, na.rm = TRUE)
    
    prompt_text <- if (!has_active_layer && active_layer == "Today") {
      "There are no spot forecasts issued for USFS Southern Area units today."
    } else if (!has_active_layer && active_layer == "Yesterday") {
      "There were no spot forecasts issued for USFS Southern Area units yesterday."
    } else {
      "Click a fire icon on the map to view superfog screening results."
    }
    
    build_selected_info(
      prompt_text = prompt_text,
      selected_burn_id = selected_burn_id(),
      cache_data = cache_data()
    )
  })
  
  output$selected_info_table <- renderUI({
    df <- cache_data()$forecast_df
    
    has_today <- any(df$issued == "Today", na.rm = TRUE)
    has_yesterday <- any(df$issued == "Yesterday", na.rm = TRUE)
    
    prompt_text <- if (!has_today) {
      "There are no spot forecasts issued for USFS Southern Area units today."
    } else if (!has_yesterday) {
      "There were no spot forecasts issued for USFS Southern Area units yesterday."
    } else {
      "Click a burn unit in the table to view superfog screening results."
    }
    
    build_selected_info(
      prompt_text = prompt_text,
      selected_burn_id = selected_burn_id(),
      cache_data = cache_data()
    )
  })
  
  output$spot_map_loading_overlay <- renderUI({
    
    if (
      identical(spot_base_map_ready(), TRUE) &&
      identical(spot_markers_ready(), TRUE)
    ) {
      return(NULL)
    }
    
    map_loading_overlay("Loading Spot Map...")
  })
  
  output$aqi_loading_overlay <- renderUI({
    
    if (!aqi_loading()) {
      return(NULL)
    }
    
    div(
      class = "sa-fade-in",
      style = "
      position:absolute;
      top:70px;
      right:10px;
      z-index:1000;
      background:rgba(255,255,255,0.95);
      padding:8px 12px;
      border-radius:6px;
      box-shadow:0 1px 4px rgba(0,0,0,0.3);
      font-weight:600;
    ",
      icon("spinner", class = "fa-spin"),
      " Loading AQI..."
    )
  })
  
  
  output$sfog_time_slider <- renderUI({
    
    req(identical(sfog_cache_status(), "loaded"))
    
    x <- sfog_cache()
    
    valid_times <- get_sfog_valid_times(x)
    
    sliderInput(
      inputId = "sfog_hour",
      label = NULL,
      min = 1,
      max = length(valid_times),
      value = 1,
      step = 1,
      animate = animationOptions(
        interval = 900,
        loop = TRUE
      )
    )
  })
  
  output$sfog_valid_time <- renderText({
    
    req(identical(sfog_cache_status(), "loaded"))
    req(input$sfog_hour)
    
    x <- sfog_cache()
    valid_times <- get_sfog_valid_times(x)
    
    current_time <- valid_times[input$sfog_hour]
    
    pt <- selected_sfog_point()
    
    tz <- if (
      !is.null(pt) &&
      !is.na(pt$lat) &&
      !is.na(pt$lon)
    ) {
      lookup_point_tz(pt$lat, pt$lon)
    } else {
      "America/New_York"
    }
    
    format_time_local(
      current_time,
      tz,
      "%b %d, %I:%M %p %Z"
    )
  })
  
  output$sfog_point_risk_plot <- renderPlot({
    
    req(selected_sfog_point())
    
    df <- sfog_point_risk()
    
    risk_colors <- c(
      "1" = "#58AFDD",
      "2" = "#FFB000",
      "3" = "#CA0020"
    )
    
    point_cols <- risk_colors[as.character(df$risk)]
    
    plot_tz <- df$timezone[1]
    
    par(
      mar = c(6.2, 8.2, 3.5, 2) + 0.1,
      bg = "white",
      family = "sans",
      xpd = FALSE
    )
    
    time_pad <- difftime(
      max(df$time_utc),
      min(df$time_utc),
      units = "secs"
    ) * 0.03
    
    x_lims <- c(
      min(df$time_utc) - time_pad,
      max(df$time_utc) + time_pad
    )
    
    plot(
      df$time_utc,
      df$risk,
      type = "n",
      ylim = c(0.75, 3.25),
      xlim = x_lims,
      xaxs = "i",
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      bty = "n",
      main = ""
    )
    
    usr <- par("usr")
    
    rect(
      usr[1], 0.75,
      usr[2], 1.5,
      col = adjustcolor("#58AFDD", alpha.f = 0.12),
      border = NA
    )
    
    rect(
      usr[1], 1.5,
      usr[2], 2.5,
      col = adjustcolor("#FFB000", alpha.f = 0.12),
      border = NA
    )
    
    rect(
      usr[1], 2.5,
      usr[2], 3.25,
      col = adjustcolor("#CA0020", alpha.f = 0.10),
      border = NA
    )
    
    lines(
      df$time_utc,
      df$risk,
      lwd = 2.5,
      col = "#2b2f36"
    )
    
    points(
      df$time_utc,
      df$risk,
      pch = 21,
      bg = point_cols,
      col = "#2b2f36",
      cex = 2.1,
      lwd = 1.2
    )
    
    axis(
      side = 2,
      at = c(1, 2, 3),
      labels = c("Minimal", "Moderate", "High"),
      las = 1,
      tick = FALSE,
      cex.axis = 1.05,
      font.axis = 2,
      col.axis = "#1f2933"
    )
    
    axis_times <- df$time_utc
    
    max_labels <- 9
    
    if (length(axis_times) > max_labels) {
      axis_times <- axis_times[
        unique(round(seq(1, length(axis_times), length.out = max_labels)))
      ]
    }
    
    axis.POSIXct(
      side = 1,
      at = axis_times,
      labels = format(
        lubridate::with_tz(axis_times, plot_tz),
        "%m/%d\n%H:%M"
      ),
      las = 2,
      cex.axis = 0.8,
      col.axis = "#344054"
    )
    
    title(
      main = paste0(
        "Superfog Risk at ",
        round(df$lat[1], 4),
        ", ",
        round(df$lon[1], 4),
        " (",
        plot_tz,
        ")"
      ),
      adj = 0,
      cex.main = 1.15,
      font.main = 2,
      col.main = "#243447"
    )
  })
  
  output$sfog_cache_message <- renderUI({
    
    status <- sfog_cache_status()
    
    if (status == "not_loaded") {
      return(
        div(
          class = "sa-fade-in",
          style = "
          padding:10px;
          margin-bottom:10px;
          background:#f8f8f8;
          border:1px solid #d9d9d9;
          border-radius:6px;
          color:#555;
        ",
          "Superfog cache has not loaded yet."
        )
      )
    }
    
    if (status == "loading") {
      return(
        div(
          class = "sa-fade-in",
          style = "
          padding:10px;
          margin-bottom:10px;
          background:#fff7e6;
          border:1px solid #f0ad4e;
          border-radius:6px;
          color:#7a4b00;
          font-weight:600;
        ",
          "Loading superfog cache..."
        )
      )
    }
    
    if (status == "failed") {
      return(
        div(
          class = "sa-fade-in",
          style = "
          padding:10px;
          margin-bottom:10px;
          background:#ffecec;
          border:1px solid #cc0000;
          border-radius:6px;
          color:#7a0000;
          font-weight:600;
        ",
          "Superfog cache unavailable. Try refreshing the app later."
        )
      )
    }
    
    if (status == "loaded") {
      x <- sfog_cache()
      
      return(
        div(
          class = "sa-fade-in",
          style = "
        padding:8px 10px;
        margin-bottom:10px;
        background:#eef8ee;
        border:1px solid #7fbf7f;
        border-radius:6px;
        color:#225522;
        font-size:13px;
      ",
          paste0(
            "Superfog cache loaded. Last refreshed: ",
            format_sfog_valid_time(x$last_refresh)
          )
        )
      )
    }
    
    NULL
  })
  
  output$sfog_loading_bar <- renderUI({
    
    status <- sfog_cache_status()
    
    if (status != "loading") {
      return(NULL)
    }
    
    htmltools::div(
      style = "
      margin-bottom:8px;
      padding:8px 12px;
      background:#fff7e6;
      border:1px solid #f0ad4e;
      border-radius:6px;
      color:#7a4b00;
      font-weight:600;
      display:flex;
      align-items:center;
      gap:10px;
    ",
      
      htmltools::span(
        class = "fa fa-spinner fa-spin",
        style = "font-size:16px;"
      ),
      
      htmltools::span(
        "Loading Superfog Risk map..."
      )
    )
  })
  
  output$sfog_map_loading_overlay <- renderUI({
    
    if (identical(sfog_overlay_ready(), TRUE)) {
      return(NULL)
    }
    
    if (input$main_tabs != "Superfog Risk") {
      return(NULL)
    }
    
    map_loading_overlay("Loading Superfog Risk map...")
  })
  
}


# RUN APP ----------------------------------------------
shinyApp(ui, server)
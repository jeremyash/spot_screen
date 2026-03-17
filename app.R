# app.R
library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(sf)
library(httr2)
library(jsonlite)

# -------------------------------------------------
# STATIC DATA
# -------------------------------------------------

wfo <- read_csv("r8_wfo.csv", show_col_types = FALSE)

r8_forests <- st_read("r8_forests", quiet = TRUE) |>
  st_transform(4326)

r8 <- st_read("region_8", quiet = TRUE) |>
  st_transform(4326)

r8_forests$forest_id <- seq_len(nrow(r8_forests))

cache_url <- "https://raw.githubusercontent.com/jeremyash/spot_screen/cache-data/cache/superfog_cache.rds"

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

offset_duplicate_points <- function(df, jitter_amount = 0.12) {
  if (nrow(df) == 0) return(df)
  
  df %>%
    mutate(
      issued_rank = case_when(
        issued == "Today" ~ 1L,
        issued == "Yesterday" ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    group_by(lat, lon) %>%
    arrange(issued_rank, project_name, issuanceTime, .by_group = TRUE) %>%
    mutate(
      dup_n = n(),
      dup_id = row_number(),
      same_coord_today_yesterday = any(issued == "Today") & any(issued == "Yesterday")
    ) %>%
    ungroup() %>%
    mutate(
      offset_lon = case_when(
        same_coord_today_yesterday & issued == "Today" ~ lon - jitter_amount,
        same_coord_today_yesterday & issued == "Yesterday" ~ lon + jitter_amount,
        dup_n == 1 ~ lon,
        TRUE ~ lon + (jitter_amount * 0.75) * cos(2 * pi * (dup_id - 1) / dup_n)
      ),
      offset_lat = case_when(
        same_coord_today_yesterday & issued == "Today" ~ lat + jitter_amount * 0.35,
        same_coord_today_yesterday & issued == "Yesterday" ~ lat - jitter_amount * 0.35,
        dup_n == 1 ~ lat,
        TRUE ~ lat + (jitter_amount * 0.75) * sin(2 * pi * (dup_id - 1) / dup_n)
      )
    ) %>%
    select(-issued_rank, -dup_n, -dup_id, -same_coord_today_yesterday)
}

format_issued_datetime <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_character_)
  
  x_posix <- suppressWarnings(ymd_hms(x, tz = "UTC"))
  
  if (all(is.na(x_posix))) {
    x_posix <- suppressWarnings(ymd_hm(x, tz = "UTC"))
  }
  
  if (all(is.na(x_posix))) {
    x_posix <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  }
  
  if (all(is.na(x_posix))) {
    return(as.character(x))
  }
  
  format(with_tz(x_posix, "America/New_York"), "%Y-%m-%d %H:%M %Z")
}

make_fire_icon_path <- function(type = c("today", "yesterday")) {
  type <- match.arg(type)
  
  if (type == "today") {
    "red-fire-flame.png"
  } else {
    "black-fire-flame.png"
  }
}

# -------------------------------------------------
# INITIAL CACHE LOAD
# -------------------------------------------------

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

sfog_legend_box <- function(label, border, bg, text) {
  div(
    style = "
      display:grid;
      grid-template-columns:260px 1fr;
      align-items:center;
      column-gap:18px;
      margin-bottom:10px;
    ",
    
    div(
      style = paste0(
        "border:4px solid ", border, ";",
        "background-color:", bg, ";",
        "padding:8px 14px;",
        "font-weight:bold;",
        "text-align:center;",
        "border-radius:4px;"
      ),
      label
    ),
    
    div(
      style = "font-size:16px; text-align:left;",
      text
    )
  )
}

# -------------------------------------------------
# UI
# -------------------------------------------------

ui <- fluidPage(
  titlePanel("USFS Region 8 Superfog Screener Pilot"),
  
  
  tags$script(HTML("
    $(document).on('click', '#reset_map', function () {
      Shiny.setInputValue('reset_map_click', Math.random());
    });

    $(document).on('change', 'input[name=\"date_layer_choice\"]', function () {
      Shiny.setInputValue('map_layer_choice', $(this).val(), {priority: 'event'});
    });
  ")),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "Map",
      fluidRow(
        column(
          8,
          leafletOutput("forecast_map", height = "650px"),
          div(
            style = "
              margin-top:10px;
              padding:10px 12px;
              background:#f8f8f8;
              border:1px solid #d9d9d9;
              border-radius:6px;
              font-size:14px;
              color:#666;
            ",
            textOutput("last_refresh_text")
          )
        ),
        column(
          4,
          div(
            style = "
              height:650px;
              overflow-y:auto;
              border-left:1px solid #d9d9d9;
              padding-left:15px;
              padding-right:10px;
            ",
            uiOutput("selected_info_map")
          )
        )
      )
    ),
    
    tabPanel(
      "Table",
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
              border-left:1px solid #d9d9d9;
              padding-left:15px;
              padding-right:10px;
            ",
            uiOutput("selected_info_table")
          )
        )
      )
    ),
    
    tabPanel(
      "About",
      div(
        style = "max-width:950px; margin:auto; font-size:16px; line-height:1.7;",
        
        h2("About This Tool"),
        
        p(
          "This tool filters the available NWS Spot Weather forecasts to those occurring on US Forest Service Region 8 National Forest System units."
        ),
        
        p(
          HTML('Using the <a href="https://usdagcc.sharepoint.com/sites/fs-r08-sm/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2Ffs%2Dr08%2Dsm%2FShared%20Documents%2FGeneral%2FGuidance%20and%20Forms%2FR8%20Smoke%20Management%20Guidelines%20%28March%202022%29%2Epdf&parent=%2Fsites%2Ffs%2Dr08%2Dsm%2FShared%20Documents%2FGeneral%2FGuidance%20and%20Forms&p=true&ga=1" target="_blank">R8 Smoke Management Guidelines</a>, each spot forecast is screened to identify weather conditions that may increase the likelihood of <strong>superfog formation</strong>. When these conditions occur, it is necessary to run a nighttime smoke dispersion model using <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont</a>.')
        ),
        
        p(
          "The screening focuses on overnight and early morning forecast hours and evaluates four key variables:"
        ),
        
        br(),
        
        div(
          style = "max-width:700px; margin:auto;",
          tags$table(
            style = "width:100%; border-collapse:collapse; font-size:16px;",
            tags$thead(
              tags$tr(
                tags$th(style = "padding:10px; border-bottom:2px solid black;", "Variable"),
                tags$th(style = "padding:10px; border-bottom:2px solid black;", "Watch Out"),
                tags$th(style = "padding:10px; border-bottom:2px solid black;", "Critical")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(style = "padding:8px;", "Temperature"),
                tags$td(style = "background:#FFDA00; padding:8px; text-align:center; font-weight:bold;", "<70°F"),
                tags$td(style = "background:#CA0020; color:white; padding:8px; text-align:center; font-weight:bold;", "<55°F")
              ),
              tags$tr(
                tags$td(style = "padding:8px;", "Relative Humidity"),
                tags$td(style = "background:#FFDA00; padding:8px; text-align:center; font-weight:bold;", ">70%"),
                tags$td(style = "background:#CA0020; color:white; padding:8px; text-align:center; font-weight:bold;", ">90%")
              ),
              tags$tr(
                tags$td(style = "padding:8px;", "Surface (20 ft) Wind Speed"),
                tags$td(style = "background:#FFDA00; padding:8px; text-align:center; font-weight:bold;", "<7 mph"),
                tags$td(style = "background:#CA0020; color:white; padding:8px; text-align:center; font-weight:bold;", "<4 mph")
              ),
              tags$tr(
                tags$td(style = "padding:8px;", "Cloud Cover"),
                tags$td(style = "background:#FFDA00; padding:8px; text-align:center; font-weight:bold;", "<60%"),
                tags$td(style = "background:#CA0020; color:white; padding:8px; text-align:center; font-weight:bold;", "<40%")
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
          style = "
            max-width:750px;
            margin:auto;
            padding:18px;
            border-radius:8px;
            background:#ffffff;
            box-shadow:0 0 8px rgba(0,0,0,0.08);
          ",
          h3(
            style = "margin-top:0; margin-bottom:15px;",
            "PB Piedmont Decision Guide"
          ),
          sfog_legend_box(
            "PB Piedmont Required",
            "red",
            "#FFDADA",
            "All variables in Critical or Watch Out"
          ),
          sfog_legend_box(
            "PB Piedmont Recommended",
            "orange",
            "#FFE8CC",
            "3 of 4 variables in Critical or Watch Out"
          ),
          sfog_legend_box(
            "PB Piedmont Not Required",
            "#777777",
            "#D9D9D9",
            "<3 variables in Critical or Watch Out"
          )
        ),
        
        br(),
        
        h3("Update Cycle"),
        
        p(
          HTML('The data is set to refresh every 30 minutes, so it may take up to 30 minutes from the time your forecast is generated to display on this site. If your forecast is still not diisplaying on the map, visit the <a href="https://spot.weather.gov/" target="_blank">NWS Spot Forecast</a> page and conduct a manual screening using the thresholds outlined above.')
        ),
        
        br(),
        
        h3("Additional Resources"),
        
        p(
          HTML('For more information and additional smoke management resources, please visit the <a href="https://usdagcc.sharepoint.com/sites/fs-r08-sm" target="_blank">USFS Region 8 Smoke Management Site</a>.')
        )
      )
    )
  )
)

# -------------------------------------------------
# SERVER
# -------------------------------------------------

server <- function(input, output, session) {
  
  cache_data <- reactiveVal(initial_cache)
  
  observe({
    invalidateLater(60 * 1000, session)
    try({
      fresh_cache <- download_remote_cache(
        paste0(cache_url, "?t=", as.integer(Sys.time()))
      )
      cache_data(fresh_cache)
    }, silent = TRUE)
  })
  
  selected_burn_id <- reactiveVal(NULL)
  
  observeEvent(input$main_tabs, {
    selected_burn_id(NULL)
  })
  
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
  
  output$forecast_map <- renderLeaflet({
    df <- cache_data()$forecast_df
    df_map <- offset_duplicate_points(df)
    
    fire_icon_url_today <- make_fire_icon_path("today")
    fire_icon_url_yesterday <- make_fire_icon_path("yesterday")
    
    fire_icon_today <- icons(
      iconUrl = fire_icon_url_today,
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )
    
    fire_icon_yesterday <- icons(
      iconUrl = fire_icon_url_yesterday,
      iconWidth = 24,
      iconHeight = 24,
      iconAnchorX = 12,
      iconAnchorY = 12
    )
    
    marker_label_opts <- labelOptions(
      style = list(
        "font-size" = "14px",
        "font-weight" = "bold",
        "padding" = "6px 10px"
      ),
      direction = "auto"
    )
    
    m <- leaflet() |>
      addTiles() |>
      setView(lng = -88.11, lat = 34.95, zoom = 5) |>
      addPolygons(
        data = r8,
        fill = FALSE,
        color = "#000000",
        weight = 2,
        opacity = 1,
        options = pathOptions(clickable = FALSE)
      ) |>
      addPolygons(
        data = r8_forests,
        layerId = ~paste0("forest_", forest_id),
        fillColor = "#228B22",
        fillOpacity = 0.4,
        color = "#006400",
        weight = 1,
        smoothFactor = 0.5,
        options = pathOptions(clickable = TRUE)
      )
    
    if (nrow(df_map) > 0 && all(c("lon", "lat") %in% names(df_map))) {
      df_today <- df_map %>% filter(issued == "Today")
      df_yesterday <- df_map %>% filter(issued == "Yesterday")
      
      if (nrow(df_today) > 0) {
        m <- m |>
          addMarkers(
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
        m <- m |>
          addMarkers(
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
      
      legend_toggle_html <- paste0(
        "<div style='",
        "background:white;",
        "padding:8px 10px;",
        "border-radius:6px;",
        "box-shadow:0 0 6px rgba(0,0,0,0.3);",
        "font-size:14px;",
        "line-height:1.2;",
        "min-width:180px;",
        "font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Helvetica,Arial,sans-serif;",
        "'>",
        "<div style='font-weight:600; font-size:16px; margin-bottom:6px;'>Date Issued</div>",
        
        "<label style='display:grid; grid-template-columns:30px 1fr 18px; align-items:center; column-gap:8px; margin-bottom:4px; cursor:pointer;'>",
        "<span style='display:flex; align-items:center; justify-content:center;'>",
        "<img src='", fire_icon_url_today, "' style='width:24px; height:24px;'>",
        "</span>",
        "<span style='font-size:15px;'>Today</span>",
        "<input type='radio' name='date_layer_choice' value='Today' checked>",
        "</label>",
        
        "<label style='display:grid; grid-template-columns:30px 1fr 18px; align-items:center; column-gap:8px; margin-bottom:0; cursor:pointer;'>",
        "<span style='display:flex; align-items:center; justify-content:center;'>",
        "<img src='", fire_icon_url_yesterday, "' style='width:24px; height:24px;'>",
        "</span>",
        "<span style='font-size:15px;'>Yesterday</span>",
        "<input type='radio' name='date_layer_choice' value='Yesterday'>",
        "</label>",
        "</div>"
      )
      
      m <- m |>
        addControl(
          html = legend_toggle_html,
          position = "bottomright"
        )
    }
    
    m |>
      addControl(
        html = "
          <button id='reset_map' 
            style='
              background:white;
              border:1px solid #999;
              padding:6px 10px;
              font-weight:bold;
              border-radius:4px;
              cursor:pointer;
            '>
            Reset Map View
          </button>
        ",
        position = "topright"
      )
  })
  
  handle_burn_click <- function(click) {
    req(click$id)
    
    selected_burn_id(click$id)
    
    leafletProxy("forecast_map") |>
      setView(
        lng = click$lng,
        lat = click$lat,
        zoom = 8
      )
  }
  
  observeEvent(input$forecast_map_marker_click, {
    handle_burn_click(input$forecast_map_marker_click)
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
            "font-size:18px;",
            "font-weight:600;",
            "color:#006400;",
            "text-align:center;",
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
  
  observe({
    leafletProxy("forecast_map") |>
      showGroup("Today") |>
      hideGroup("Yesterday")
  })
  
  observeEvent(input$reset_map_click, {
    leafletProxy("forecast_map") |>
      clearPopups() |>
      setView(
        lng = -88.11,
        lat = 34.95,
        zoom = 5
      )
  })
  
  observeEvent(input$table_burn_click, {
    selected_burn_id(input$table_burn_click)
  })
  
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
        issued_display = format_issued_datetime(issuanceTime)
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
                tags$th(
                  style = "text-align:left; padding:10px; border-bottom:2px solid #cccccc;",
                  "Burn Unit"
                ),
                tags$th(
                  style = "text-align:left; padding:10px; border-bottom:2px solid #cccccc; width:210px; white-space:nowrap;",
                  "Date Issued"
                )
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
                  style = paste0(
                    "border-bottom:1px solid #e6e6e6;",
                    "cursor:pointer;",
                    "transition:background-color 0.15s ease;"
                  ),
                  onclick = paste0(
                    "Shiny.setInputValue('table_burn_click','",
                    forest_df$spot_id[i],
                    "', {priority: 'event'});"
                  ),
                  onmouseover = if (!is_selected) {
                    "this.style.backgroundColor='#f5f5f5';"
                  } else {
                    ""
                  },
                  onmouseout = if (!is_selected) {
                    "this.style.backgroundColor='transparent';"
                  } else {
                    ""
                  },
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
  
  build_selected_info <- function(prompt_text) {
    clicked_id <- selected_burn_id()
    
    if (is.null(clicked_id)) {
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
          prompt_text
        )
      )
    }
    
    forecast_df <- cache_data()$forecast_df
    sfog_tables <- cache_data()$sfog_tables
    
    idx <- which(forecast_df$spot_id == clicked_id)
    
    if (length(idx) == 0) return(NULL)
    
    spot_url <- forecast_df$nws_spot_url[idx]
    project <- forecast_df$project_name[idx]
    sfog_df <- sfog_tables[[idx]]
    issued_display <- format_issued_datetime(forecast_df$issuanceTime[idx])
    
    if (is.null(sfog_df)) {
      return(
        HTML(paste0(
          'Unable to screen. Please consult your <a href="',
          spot_url,
          '" target="_blank">spot forecast</a>.'
        ))
      )
    }
    
    sfog_status <- sfog_df %>%
      rowwise() %>%
      mutate(
        critical_count = sum(
          c_across(c(sky_screen, temp_screen, rh_screen, wind_screen)) %in%
            c("critical", "watch_out")
        )
      ) %>%
      ungroup()
    
    total_max <- max(sfog_status$critical_count, na.rm = TRUE)
    
    if (total_max == 4) {
      sfog_box <- div(
        style = "border:4px solid red; background-color:#FFDADA; color:black; padding:12px; font-size:15px; margin:10px;",
        div(
          style = "font-weight:bold; font-size:18px; margin-bottom:6px;",
          "PB Piedmont Required"
        ),
        HTML(
          'Superfog criteria have been met. Please run a <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont model</a>.'
        )
      )
    } else if (total_max == 3) {
      sfog_box <- div(
        style = "border:4px solid orange; background-color:#FFE8CC; color:black; padding:12px; font-size:15px; margin:10px;",
        div(
          style = "font-weight:bold; font-size:18px; margin-bottom:6px;",
          "PB Piedmont Recommended"
        ),
        HTML(
          'Most superfog criteria have been met. Running a <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont model</a> is recommended.'
        )
      )
    } else {
      sfog_box <- div(
        style = "border:4px solid #777777; background-color:#D9D9D9; color:black; padding:12px; font-size:15px; margin:10px;",
        div(
          style = "font-weight:bold; font-size:18px; margin-bottom:6px;",
          "PB Piedmont Not Required"
        ),
        "Superfog criteria have not been met."
      )
    }
    
    kbl_table <- sfog_df %>%
      mutate(
        SKY = cell_spec(SKY, format = "html", extra_css = sapply(sky_screen, function(v) {
          css <- if (v == "critical") "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          else if (v == "watch_out") "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          else "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          paste0("display:block;width:100%;height:100%;", css)
        })),
        TEMP = cell_spec(TEMP, format = "html", extra_css = sapply(temp_screen, function(v) {
          css <- if (v == "critical") "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          else if (v == "watch_out") "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          else "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          paste0("display:block;width:100%;height:100%;", css)
        })),
        RH = cell_spec(RH, format = "html", extra_css = sapply(rh_screen, function(v) {
          css <- if (v == "critical") "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          else if (v == "watch_out") "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          else "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          paste0("display:block;width:100%;height:100%;", css)
        })),
        WIND = cell_spec(WIND, format = "html", extra_css = sapply(wind_screen, function(v) {
          css <- if (v == "critical") "background-color:#CA0020;color:white;font-weight:bold;text-align:center;"
          else if (v == "watch_out") "background-color:#FFDA00;color:black;font-weight:bold;text-align:center;"
          else "background-color:#D9D9D9;color:black;font-weight:bold;text-align:center;"
          paste0("display:block;width:100%;height:100%;", css)
        }))
      ) %>%
      select(DATETIME, TEMP, RH, WIND, SKY) %>%
      kbl(
        escape = FALSE,
        align = "c",
        col.names = c(
          "DATE-TIME",
          "Temperature<br>(°F)",
          "Relative<br>Humidity (%)",
          "Wind<br>Speed (mph)",
          "Cloud<br>Cover (%)"
        )
      ) %>%
      kable_styling(full_width = FALSE, font_size = 16)
    
    tagList(
      h3(style = "font-weight:bold; font-size:24px;", project),
      div(
        style = "margin-bottom:6px; font-size:16px;",
        a("Full Spot Weather Forecast", href = spot_url, target = "_blank")
      ),
      div(
        style = "margin-bottom:10px; font-size:16px; color:#555;",
        paste0("Date Issued: ", issued_display)
      ),
      sfog_box,
      HTML(as.character(kbl_table)),
      div(
        style = "text-align:center; font-size:18px; margin-top:8px;",
        span(style = "background-color:#CA0020;color:white;padding:8px 12px;margin-right:6px;font-weight:bold;", "Critical"),
        span(style = "background-color:#FFDA00;color:black;padding:8px 12px;margin-right:6px;font-weight:bold;", "Watch Out"),
        span(style = "background-color:#D9D9D9;color:black;padding:8px 12px;margin-right:6px;font-weight:bold;", "Minimal Concern")
      )
    )
  }
  
  output$selected_info_map <- renderUI({
    build_selected_info("Click a fire icon on the map to view superfog screening results.")
  })
  
  output$selected_info_table <- renderUI({
    build_selected_info("Click a burn unit in the table to view superfog screening results.")
  })
}

# -------------------------------------------------
# RUN APP
# -------------------------------------------------

shinyApp(ui, server)
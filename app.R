# app.R
library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(sf)
library(httr2)

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
            style = "max-width:1200px; margin:auto; padding-top:15px;",
            uiOutput("burn_table_grouped")
          )
        ),
        
        column(
          7,
          
          div(
            style = "
              min-height:650px;
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
  
  remote_cache <- reactivePoll(
    intervalMillis = 60 * 1000,
    session = NULL,
    checkFunc = function() {
      resp <- request(cache_url) |>
        req_method("HEAD") |>
        req_perform()
      
      headers <- resp_headers(resp)
      headers[["last-modified"]] %||% as.character(Sys.time())
    },
    valueFunc = function() {
      download_remote_cache(cache_url)
    }
  )
  
  selected_burn_id <- reactiveVal(NULL)
  
  observeEvent(input$main_tabs, {
    selected_burn_id(NULL)
  })
  
  # -------------------------------------------------
  # PRECOMPUTE BURNS + FOREST JOIN ONCE PER CACHE REFRESH
  # -------------------------------------------------
  
  burns_with_forest <- reactive({
    forecast_df <- remote_cache()$forecast_df
    
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
      mutate(forest = if_else(is.na(forest), "Not matched", forest)) |>
      arrange(forest, project_name)
  })
  
  # -------------------------------------------------
  # LAST REFRESH
  # -------------------------------------------------
  
  output$last_refresh_text <- renderText({
    lr <- remote_cache()$last_refresh
    
    if (is.na(lr)) {
      "Last refreshed: cache not available"
    } else {
      paste0(
        "Last refreshed: ",
        format(with_tz(lr, "America/New_York"), "%Y-%m-%d %H:%M %Z")
      )
    }
  })
  
  # -------------------------------------------------
  # MAP
  # -------------------------------------------------
  
  output$forecast_map <- renderLeaflet({
    df <- remote_cache()$forecast_df
    
    m <- leaflet() |>
      addTiles() |>
      setView(lng = -88.11, lat = 34.95, zoom = 5) |>
      
      addPolygons(
        data = r8,
        fill = FALSE,
        color = "#000000",
        weight = 2,
        opacity = 1
      ) |>
      
      addPolygons(
        data = r8_forests,
        fillColor = "#228B22",
        fillOpacity = 0.4,
        color = "#006400",
        weight = 1,
        layerId = ~forest_id,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#FFFF00",
          fillOpacity = 0.6,
          bringToFront = TRUE
        ),
        popup = ~paste0(
          "<div style='font-weight:bold;color:black;font-size:14px;padding:2px 6px;",
          "background-color:rgba(255,255,255,0.8);'>",
          forest,
          "</div>"
        )
      )
    
    if (nrow(df) > 0 && all(c("lon", "lat") %in% names(df))) {
      m <- m |>
        addMarkers(
          data = df,
          lng = ~lon,
          lat = ~lat,
          layerId = ~spot_id,
          label = ~project_name,
          labelOptions = labelOptions(
            style = list(
              "font-size" = "14px",
              "font-weight" = "bold",
              "padding" = "6px 10px"
            )
          ),
          icon = icons(
            iconUrl = "redFlame.png",
            iconWidth = 20,
            iconHeight = 24,
            iconAnchorX = 10,
            iconAnchorY = 24
          )
        ) |>
        addControl(
          html = paste0(
            "<div style='
              background:white;
              padding:8px 12px;
              border-radius:6px;
              box-shadow:0 0 6px rgba(0,0,0,0.3);
              font-weight:bold;
              font-size:16px;
            '>
            Active Spot Forecasts: ", nrow(df), "
            </div>"
          ),
          position = "topright"
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
        position = "topleft"
      )
  })
  
  observeEvent(input$forecast_map_marker_click, {
    click <- input$forecast_map_marker_click
    
    selected_burn_id(click$id)
    
    leafletProxy("forecast_map") |>
      setView(
        lng = click$lng,
        lat = click$lat,
        zoom = 8
      )
  })
  
  observeEvent(input$reset_map_click, {
    leafletProxy("forecast_map") |>
      setView(
        lng = -88.11,
        lat = 34.95,
        zoom = 5
      )
  })
  
  observeEvent(input$forecast_map_shape_click, {
    click <- input$forecast_map_shape_click
    req(click$id)
    
    forest_clicked <- r8_forests[r8_forests$forest_id == click$id, ]
    bounds <- st_bbox(forest_clicked)
    
    leafletProxy("forecast_map") |>
      fitBounds(
        lng1 = as.numeric(bounds["xmin"]),
        lat1 = as.numeric(bounds["ymin"]),
        lng2 = as.numeric(bounds["xmax"]),
        lat2 = as.numeric(bounds["ymax"])
      )
  })
  
  # -------------------------------------------------
  # TABLE
  # -------------------------------------------------
  
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
              border-collapse:collapse;
              margin-bottom:20px;
              font-size:16px;
            ",
            
            tags$tbody(
              lapply(seq_len(nrow(forest_df)), function(i) {
                
                is_selected <- identical(selected_burn_id(), forest_df$spot_id[i])
                
                bg_color <- if (is_selected) "#e8f4ea" else "transparent"
                border_color <- if (is_selected) "#228B22" else "transparent"
                text_color <- if (is_selected) "#000000" else "#1a1a1a"
                font_weight <- if (is_selected) "700" else "600"
                
                tags$tr(
                  style = "
                    border-bottom:1px solid #e6e6e6;
                    cursor:pointer;
                  ",
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
                    style = "padding:0;",
                    
                    tags$a(
                      href = "#",
                      onclick = paste0(
                        "Shiny.setInputValue('table_burn_click','",
                        forest_df$spot_id[i],
                        "', {priority: 'event'}); return false;"
                      ),
                      style = paste0(
                        "display:block;",
                        "padding:12px 10px;",
                        "color:", text_color, ";",
                        "text-decoration:none;",
                        "font-weight:", font_weight, ";",
                        "background-color:", bg_color, ";",
                        "border-left:5px solid ", border_color, ";"
                      ),
                      forest_df$project_name[i]
                    )
                  )
                )
              })
            )
          )
        )
      })
    )
  })
  
  # -------------------------------------------------
  # INFO PANEL HELPERS
  # -------------------------------------------------
  
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
    
    forecast_df <- remote_cache()$forecast_df
    sfog_tables <- remote_cache()$sfog_tables
    
    idx <- which(forecast_df$spot_id == clicked_id)
    
    if (length(idx) == 0) return(NULL)
    
    spot_url <- forecast_df$nws_spot_url[idx]
    project <- forecast_df$project_name[idx]
    sfog_df <- sfog_tables[[idx]]
    
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
        style = "border:4px solid green; background-color:#DAF5DA; color:black; padding:12px; font-size:15px; margin:10px;",
        
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
        style = "margin-bottom:10px; font-size:16px;",
        a("Full Spot Weather Forecast", href = spot_url, target = "_blank")
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
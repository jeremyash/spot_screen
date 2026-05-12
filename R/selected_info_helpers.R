build_selected_info <- function(
    prompt_text,
    selected_burn_id,
    cache_data
) {
  clicked_id <- selected_burn_id
  
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
  
  forecast_df <- cache_data$forecast_df
  sfog_tables <- cache_data$sfog_tables
  
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
      div(style = "font-weight:bold; font-size:18px; margin-bottom:6px;", "PB Piedmont Required"),
      HTML('Superfog criteria have been met. Please run a <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont model</a>.')
    )
  } else if (total_max == 3) {
    sfog_box <- div(
      style = "border:4px solid orange; background-color:#FFE8CC; color:black; padding:12px; font-size:15px; margin:10px;",
      div(style = "font-weight:bold; font-size:18px; margin-bottom:6px;", "PB Piedmont Recommended"),
      HTML('Most superfog criteria have been met. Running a <a href="https://piedmont.dri.edu/" target="_blank">PB Piedmont model</a> is recommended.')
    )
  } else {
    sfog_box <- div(
      style = "border:4px solid #777777; background-color:#D9D9D9; color:black; padding:12px; font-size:15px; margin:10px;",
      div(style = "font-weight:bold; font-size:18px; margin-bottom:6px;", "PB Piedmont Not Required"),
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
    div(style = "margin-bottom:6px; font-size:16px;", a("Full Spot Weather Forecast", href = spot_url, target = "_blank")),
    div(style = "margin-bottom:10px; font-size:16px; color:#555;", paste0("Date Issued: ", issued_display)),
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
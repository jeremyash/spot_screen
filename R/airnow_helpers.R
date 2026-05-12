#AQI Colors
namedColors <- US_AQI$colors_EPA

names(namedColors) <- c(
  US_AQI$names_eng[1:2],
  "Unhealthy for Sensitive Groups",
  US_AQI$names_eng[4:6]
)


extract_aqi_cat <- function(x) {
  vapply(
    x,
    function(txt) {
      tryCatch(
        {
          html_text(html_elements(read_html(txt), "i"))[1]
        },
        error = function(e) NA_character_
      )
    },
    FUN.VALUE = character(1)
  )
}

load_airnow_kml <- function(day = c("today", "tomorrow")) {
  day <- match.arg(day)
  
  kml_url <- if (day == "today") {
    "https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/today/forecast_today_usa.kml"
  } else {
    "https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/today/forecast_tomorrow_usa.kml"
  }
  
  sf_airnow <- sf::st_read(kml_url, quiet = TRUE) |>
    sf::st_zm()
  
  desc_col <- if ("description" %in% names(sf_airnow)) {
    "description"
  } else {
    "Description"
  }
  
  sf_airnow |>
    dplyr::mutate(
      aqi_cat = extract_aqi_cat(.data[[desc_col]]),
      aqi_color = unname(namedColors[aqi_cat])
    ) |>
    dplyr::filter(aqi_cat != "Good")
}
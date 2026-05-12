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
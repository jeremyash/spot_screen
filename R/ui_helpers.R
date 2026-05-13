make_fire_icon_path <- function(type = c("today", "yesterday")) {
  type <- match.arg(type)
  
  if (type == "today") {
    "red-fire-flame.png"
  } else {
    "black-fire-flame.png"
  }
}

marker_label_opts <- leaflet::labelOptions(
  style = list(
    "font-size" = "14px",
    "font-weight" = "bold",
    "padding" = "6px 10px"
  ),
  direction = "auto"
)


sfog_legend_box <- function(label, border, bg, text) {
  htmltools::div(
    style = "
      display:grid;
      grid-template-columns:260px 1fr;
      align-items:center;
      column-gap:18px;
      margin-bottom:10px;
    ",
    
    htmltools::div(
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
    
    htmltools::div(
      style = "font-size:16px; text-align:left;",
      text
    )
  )
}

make_fire_icon_path <- function(type = c("today", "yesterday")) {
  type <- match.arg(type)
  
  if (type == "today") {
    "red-fire-flame.png"
  } else {
    "black-fire-flame.png"
  }
}

marker_label_opts <- labelOptions(
  style = list(
    "font-size" = "14px",
    "font-weight" = "bold",
    "padding" = "6px 10px"
  ),
  direction = "auto"
)


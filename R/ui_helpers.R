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

sfog_risk_legend <- HTML('
    <div style="background:white; padding:10px; border-radius:6px;">
      <div style="font-weight:bold; margin-bottom:6px;">Superfog Risk</div>
      <div><span style="background:#58AFDD; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Minimal</div>
      <div><span style="background:#FFB000; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Moderate</div>
      <div><span style="background:#CA0020; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> High</div>
    </div>
    ')
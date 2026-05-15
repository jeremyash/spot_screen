sfog_overlay_js <- function() {
  htmltools::tags$script(htmltools::HTML("
    Shiny.addCustomMessageHandler('sfog_set_overlay', function(data) {

      var widgets = HTMLWidgets.findAll('.leaflet');
      if (!widgets.length) return;

      var map = null;

      for (var i = 0; i < widgets.length; i++) {
        if (widgets[i].getMap) {

          var candidate = widgets[i].getMap();

          if (candidate._container.id === 'sfog_map') {
            map = candidate;
            break;
          }
        }
      }

      if (!map) return;

      if (window.sfogRiskOverlay && map.hasLayer(window.sfogRiskOverlay)) {
        map.removeLayer(window.sfogRiskOverlay);
      }

      var bounds = [
        [data.south, data.west],
        [data.north, data.east]
      ];

      window.sfogRiskOverlay = L.imageOverlay(data.url, bounds, {
        opacity: 0.7,
        className: 'sfog-png-overlay'
      }).addTo(map);
    });
  "))
}

sfog_overlay_css <- function() {
  htmltools::tags$style(htmltools::HTML("
    .sfog-png-overlay {
      image-rendering: pixelated;
      image-rendering: crisp-edges;
      -ms-interpolation-mode: nearest-neighbor;
    }
  "))
}

spot_map_controls_js <- function() {
  htmltools::tags$script(htmltools::HTML("
    $(document).on('click', '#reset_map', function () {
      Shiny.setInputValue('reset_map_click', Math.random());
    });

    $(document).on('click', '#sfog_reset_map', function () {
      Shiny.setInputValue('sfog_reset_map_click', Math.random());
    });

    $(document).on('change', 'input[name=\"date_layer_choice\"]', function () {
      Shiny.setInputValue('map_layer_choice', $(this).val(), {priority: 'event'});
    });
  "))
}

spot_map_toggle_legend <- function(
    fire_icon_url_today,
    fire_icon_url_yesterday
) {
  
  paste0(
    "<div style='background:white;padding:8px 10px;border-radius:6px;",
    "box-shadow:0 0 6px rgba(0,0,0,0.3);font-size:14px;line-height:1.2;",
    "min-width:180px;font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Helvetica,Arial,sans-serif;'>",
    
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
}

spot_map_reset_button <- function() {
  
  "
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
  "
}

sfog_risk_legend <- htmltools::HTML('
    <div style="background:white; padding:10px; border-radius:6px;">
      <div style="font-weight:bold; margin-bottom:6px;">Superfog Risk</div>
      <div><span style="background:#58AFDD; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Minimal</div>
      <div><span style="background:#FFB000; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Moderate</div>
      <div><span style="background:#CA0020; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> High</div>
    </div>
    ')

sfog_map_reset_button <- function() {
  "
  <button id='sfog_reset_map' 
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
  "
}

map_loading_overlay <- function(message) {
  htmltools::div(
    style = "
      position:absolute;
      inset:0;
      z-index:900;
      background:rgba(255,255,255,0.82);
      display:flex;
      align-items:center;
      justify-content:center;
      font-size:18px;
      font-weight:600;
      color:#444;
      border:1px solid #d9d9d9;
    ",
    htmltools::span(
      class = "fa fa-spinner fa-spin",
      style = "font-size:22px; margin-right:10px;"
    ),
    message
  )
}

app_theme_css <- function() {
  htmltools::tags$style(htmltools::HTML("
    .sa-card {
      background: #ffffff;
      border: 1px solid #d9e2d9;
      border-radius: 10px;
      padding: 14px;
      margin-bottom: 14px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
    }

    .sa-card-title {
      font-size: 17px;
      font-weight: 700;
      color: #1f3b1f;
      margin-bottom: 8px;
    }

    .sa-muted {
      color: #666666;
      font-size: 14px;
      line-height: 1.45;
    }

    .sa-section-accent {
      border-left: 5px solid #228B22;
      background: #f7faf7;
    }

    .sa-time-card {
      background: linear-gradient(135deg, #f7faf7, #ffffff);
      border: 1px solid #cfe0cf;
      border-radius: 10px;
      padding: 12px;
      margin-bottom: 14px;
    }

    .sa-point-card {
      background: #f8fbff;
      border: 1px solid #cbdff2;
      border-radius: 10px;
      padding: 14px;
      margin-top: 14px;
    }

    .sa-small-label {
      font-size: 13px;
      font-weight: 700;
      color: #444444;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      margin-bottom: 6px;
    }
  "))
}

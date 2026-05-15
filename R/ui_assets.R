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
  <button
    id='reset_map'
    class='sa-primary-btn'
    style='
      padding:6px 12px;
      cursor:pointer;
      box-shadow:0 1px 4px rgba(0,0,0,0.25);
    '>
    Reset Map View
  </button>
  "
}

sfog_risk_legend <- htmltools::HTML("
  <div class='sa-map-legend'>
    <div class='sa-map-legend-title'>Superfog Risk</div>

    <div class='sa-map-legend-row'>
      <span class='sa-map-legend-swatch' style='background:#58AFDD;'></span>
      <span>Minimal</span>
    </div>

    <div class='sa-map-legend-row'>
      <span class='sa-map-legend-swatch' style='background:#FFB000;'></span>
      <span>Moderate</span>
    </div>

    <div class='sa-map-legend-row'>
      <span class='sa-map-legend-swatch' style='background:#CA0020;'></span>
      <span>High</span>
    </div>
  </div>
")

sfog_map_reset_button <- function() {
  "
  <button
    id='reset_map'
    class='sa-primary-btn'
    style='
      padding:6px 12px;
      cursor:pointer;
      box-shadow:0 1px 4px rgba(0,0,0,0.25);
    '>
    Reset Map View
  </button>
  "
}

map_loading_overlay <- function(message) {
  htmltools::div(
    class = "sa-fade-in",
    style = "
      position:absolute;
      top:50%;
      left:50%;
      transform:translate(-50%, -50%);
      z-index:2000;
      background:rgba(255,255,255,0.92);
      backdrop-filter:blur(4px);
      padding:18px 26px;
      border-radius:12px;
      box-shadow:0 4px 18px rgba(0,0,0,0.12);
      text-align:center;
      min-width:220px;
    ",
    htmltools::div(
      
      htmltools::span(
        class = "fa fa-spinner fa-spin",
        style = "
      font-size:24px;
      color:#2b2f36;
    "
      ),
      
      htmltools::div(
        style = "
      font-size:16px;
      font-weight:700;
      color:#243447;
      margin-top:10px;
      letter-spacing:0.01em;
    ",
        message
      )
    )
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
      transition:
        box-shadow 0.18s ease,
        transform 0.18s ease;
    }

      .sa-card:hover {
        box-shadow: 0 4px 14px rgba(0,0,0,0.12);
        transform: translateY(-1px);
      }

    .sa-card-title {
      font-size: 18px;
      font-weight: 700;
      color: #243447;
      margin-bottom: 10px;
      letter-spacing: 0.01em;
    }

    .sa-muted {
      color: #5f6b7a;
      font-size: 14px;
      line-height: 1.5;
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
    
    .btn-default, .btn {
      border-radius: 8px;
      font-weight: 600;
    }

    .sa-primary-btn {
      background: #2b2f36 !important;
      color: white !important;
      border: 1px solid #1f2328 !important;
      border-radius: 8px !important;
      font-weight: 700 !important;
    }

    .sa-primary-btn:hover {
      background: #1f2328 !important;
    }
    
    body {
      font-family: -apple-system, BlinkMacSystemFont,
        'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
    }
    
    .form-control {
      border-radius: 8px !important;
      border: 1px solid #cfd8e3 !important;
      box-shadow: none !important;
    }

    .form-control:focus {
      border-color: #5b7ea6 !important;
      box-shadow: 0 0 0 2px rgba(91,126,166,0.15) !important;
    }
    
    .irs-bar,
    .irs-bar-edge,
    .irs-single {
      background: #2b2f36 !important;
      border-color: #2b2f36 !important;
    }
    
    .irs-slider {
      border: 2px solid #2b2f36 !important;
      background: white !important;
    }
    
    .radio label {
      font-weight: 600;
    }
    
    .sa-fade-in {
      animation: saFadeIn 0.22s ease-out;
    }

    @keyframes saFadeIn {
      from {
        opacity: 0;
        transform: translateY(4px);
      }
    
      to {
        opacity: 1;
        transform: translateY(0);
      }
    }
    
    .sa-header {
      background: linear-gradient(
        135deg,
        #243447 0%,
        #2b2f36 100%
      );
      
      padding: 16px 22px;
      margin-bottom: 16px;
      
      border-radius: 12px;
      
      box-shadow:
        0 3px 14px rgba(0,0,0,0.12);
    }

    .sa-header-title {
      color: white;
      font-size: 30px;
      font-weight: 800;
      line-height: 1.1;
      letter-spacing: -0.01em;
    }
    
    .nav-tabs {
      border-bottom: 2px solid #d7dee7 !important;
      margin-bottom: 18px;
      padding-left: 4px;
    }
    
    .nav-tabs > li {
      margin-bottom: -2px;
    }
    
    .nav-tabs > li > a {
      border: 1px solid transparent !important;
      border-radius: 10px 10px 0 0 !important;
      
      background: #eef2f6 !important;
      color: #4c5a6a !important;
      
      font-weight: 700;
      font-size: 15px;
      
      padding: 11px 20px;
      margin-right: 6px;
      
      transition:
        background 0.18s ease,
        color 0.18s ease;
    }
    
    .nav-tabs > li > a:hover {
      background: #e3e9ef !important;
      color: #243447 !important;
    }
    
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {
      
      background: white !important;
      color: #243447 !important;
      
      border: 2px solid #243447 !important;
      border-bottom: 2px solid white !important;
      
      box-shadow: none !important;
    }
    
    .leaflet-control-zoom a {
      background: white !important;
      color: #243447 !important;
      border: none !important;
      
      box-shadow:
        0 2px 8px rgba(0,0,0,0.12);
        
      font-weight: 700;
    }

    .leaflet-control-zoom a:hover {
      background: #f3f5f7 !important;
    }
    
    .leaflet-control-layers {
      border: none !important;
      
      border-radius: 10px !important;
      
      box-shadow:
        0 3px 12px rgba(0,0,0,0.14) !important;
        
      overflow: hidden;
    }
    
    .leaflet-control-layers-toggle {
      background-color: white !important;
    }
    
    .leaflet-control-layers-expanded {
      padding: 12px !important;
      background: white !important;
    }
    
    .leaflet-control-attribution {
      background: rgba(255,255,255,0.78) !important;
      backdrop-filter: blur(4px);
    }
    
    .sa-map-legend {
      background: rgba(255,255,255,0.94);
      backdrop-filter: blur(4px);
      border: 1px solid #d9e2d9;
      border-radius: 10px;
      padding: 11px 13px;
      box-shadow: 0 3px 12px rgba(0,0,0,0.14);
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      min-width: 135px;
    }
    
    .sa-map-legend-title {
      font-size: 14px;
      font-weight: 800;
      color: #243447;
      margin-bottom: 8px;
    }
    
    .sa-map-legend-row {
      display: flex;
      align-items: center;
      gap: 8px;
      font-size: 13px;
      font-weight: 600;
      color: #344054;
      margin-bottom: 5px;
    }
    
    .sa-map-legend-row:last-child {
      margin-bottom: 0;
    }
    
    .sa-map-legend-swatch {
      width: 18px;
      height: 12px;
      border-radius: 3px;
      border: 1px solid rgba(0,0,0,0.18);
      display: inline-block;
    }
  "))
}

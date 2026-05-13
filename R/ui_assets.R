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
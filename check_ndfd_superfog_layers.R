# scripts/check_ndfd_superfog_layers.R

library(terra)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(sf)
library(tidyverse)

# -------------------------------------------------
# SETTINGS
# -------------------------------------------------

out_dir <- "debug_superfog_layers"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

r8 <- sf::st_read("region_8", quiet = TRUE) |>
  sf::st_transform(4326)

# Adjust if your local NDFD folder name differs
ndfd_dir <- "ndfd_region8"

temp_file <- file.path(ndfd_dir, "ds.temp.bin")
rh_file   <- file.path(ndfd_dir, "ds.rhm.bin")
wind_file <- file.path(ndfd_dir, "ds.wspd.bin")
sky_file  <- file.path(ndfd_dir, "ds.sky.bin")

stopifnot(file.exists(temp_file))
stopifnot(file.exists(rh_file))
stopifnot(file.exists(wind_file))
stopifnot(file.exists(sky_file))

# -------------------------------------------------
# READ NDFD RASTERS
# -------------------------------------------------

temp_c <- terra::rast(temp_file)
rh <- terra::rast(rh_file)
wind_ms <- terra::rast(wind_file)
sky <- terra::rast(sky_file)

temp_f <- (temp_c * 9 / 5) + 32
wind_mph <- wind_ms * 2.23694

# Align to temperature grid
rh <- terra::resample(rh, temp_f, method = "bilinear")
wind_mph <- terra::resample(wind_mph, temp_f, method = "bilinear")
sky <- terra::resample(sky, temp_f, method = "bilinear")

# Project to lon/lat
temp_f_ll <- terra::project(temp_f, "EPSG:4326")
rh_ll <- terra::project(rh, "EPSG:4326")
wind_ll <- terra::project(wind_mph, "EPSG:4326")
sky_ll <- terra::project(sky, "EPSG:4326")

# Clip to Southern Area
r8_vect <- terra::vect(r8)

temp_f_ll <- terra::mask(terra::crop(temp_f_ll, r8_vect), r8_vect)
rh_ll <- terra::mask(terra::crop(rh_ll, r8_vect), r8_vect)
wind_ll <- terra::mask(terra::crop(wind_ll, r8_vect), r8_vect)
sky_ll <- terra::mask(terra::crop(sky_ll, r8_vect), r8_vect)

# -------------------------------------------------
# THRESHOLD LAYERS
# 1 = Minimal, 2 = Watch Out, 3 = Critical
# -------------------------------------------------

temp_class <- terra::classify(
  temp_f_ll,
  rcl = matrix(
    c(
      -Inf, 55, 3,
      55, 70, 2,
      70, Inf, 1
    ),
    ncol = 3,
    byrow = TRUE
  ),
  include.lowest = TRUE
)

rh_class <- terra::classify(
  rh_ll,
  rcl = matrix(
    c(
      -Inf, 70, 1,
      70, 90, 2,
      90, Inf, 3
    ),
    ncol = 3,
    byrow = TRUE
  ),
  include.lowest = TRUE
)

wind_class <- terra::classify(
  wind_ll,
  rcl = matrix(
    c(
      -Inf, 4, 3,
      4, 7, 2,
      7, Inf, 1
    ),
    ncol = 3,
    byrow = TRUE
  ),
  include.lowest = TRUE
)

sky_class <- terra::classify(
  sky_ll,
  rcl = matrix(
    c(
      -Inf, 40, 3,
      40, 60, 2,
      60, Inf, 1
    ),
    ncol = 3,
    byrow = TRUE
  ),
  include.lowest = TRUE
)

# -------------------------------------------------
# QUICK NUMERIC CHECK
# -------------------------------------------------

hour <- 1

print_summary <- function(x, class_x, name) {
  cat("\n", name, "\n", sep = "")
  print(global(x[[hour]], c("min", "mean", "max"), na.rm = TRUE))
  print(freq(class_x[[hour]], digits = 0))
}

print_summary(temp_f_ll, temp_class, "Temperature °F")
print_summary(rh_ll, rh_class, "Relative Humidity %")
print_summary(wind_ll, wind_class, "Wind mph")
print_summary(sky_ll, sky_class, "Cloud Cover %")

# -------------------------------------------------
# LEAFLET VIEWER
# -------------------------------------------------

pal_raw_temp <- colorNumeric("RdYlBu", values(temp_f_ll[[hour]]), reverse = TRUE, na.color = "transparent")
pal_raw_rh <- colorNumeric("YlGnBu", values(rh_ll[[hour]]), na.color = "transparent")
pal_raw_wind <- colorNumeric("YlOrRd", values(wind_ll[[hour]]), na.color = "transparent")
pal_raw_sky <- colorNumeric("Greys", values(sky_ll[[hour]]), na.color = "transparent")

pal_class <- colorFactor(
  palette = c(
    "1" = "#58AFDD",
    "2" = "#FFB000",
    "3" = "#CA0020"
  ),
  domain = c(1, 2, 3),
  na.color = "transparent"
)

m <- leaflet() |>
  addProviderTiles(providers$OpenStreetMap.Mapnik) |>
  fitBounds(-96, 24, -74, 38) |>
  
  addRasterImage(temp_f_ll[[hour]], colors = pal_raw_temp, opacity = 0.65, group = "Raw Temperature") |>
  addRasterImage(rh_ll[[hour]], colors = pal_raw_rh, opacity = 0.65, group = "Raw RH") |>
  addRasterImage(wind_ll[[hour]], colors = pal_raw_wind, opacity = 0.65, group = "Raw Wind") |>
  addRasterImage(sky_ll[[hour]], colors = pal_raw_sky, opacity = 0.65, group = "Raw Cloud Cover") |>
  
  addRasterImage(temp_class[[hour]], colors = pal_class, opacity = 0.65, group = "Threshold Temperature") |>
  addRasterImage(rh_class[[hour]], colors = pal_class, opacity = 0.65, group = "Threshold RH") |>
  addRasterImage(wind_class[[hour]], colors = pal_class, opacity = 0.65, group = "Threshold Wind") |>
  addRasterImage(sky_class[[hour]], colors = pal_class, opacity = 0.65, group = "Threshold Cloud Cover") |>
  
  addPolygons(
    data = r8,
    color = "#5b6573",
    weight = 1.2,
    opacity = 0.7,
    fill = FALSE,
    group = "Southern Area Boundary"
  ) |>
  
  addLayersControl(
    overlayGroups = c(
      "Raw Temperature",
      "Raw RH",
      "Raw Wind",
      "Raw Cloud Cover",
      "Threshold Temperature",
      "Threshold RH",
      "Threshold Wind",
      "Threshold Cloud Cover",
      "Southern Area Boundary"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  
  hideGroup(c(
    "Raw RH",
    "Raw Wind",
    "Raw Cloud Cover",
    "Threshold Temperature",
    "Threshold RH",
    "Threshold Wind",
    "Threshold Cloud Cover"
  ))

htmlwidgets::saveWidget(
  m,
  file.path(out_dir, "ndfd_superfog_layer_check.html"),
  selfcontained = TRUE
)

browseURL(file.path(out_dir, "ndfd_superfog_layer_check.html"))
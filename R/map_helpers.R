offset_duplicate_points <- function(df, jitter_amount = 0.12) {
  if (base::nrow(df) == 0) return(df)
  
  df |>
    dplyr::mutate(
      issued_rank = dplyr::case_when(
        issued == "Today" ~ 1L,
        issued == "Yesterday" ~ 2L,
        TRUE ~ 3L
      )
    ) |>
    dplyr::group_by(lat, lon) |>
    dplyr::arrange(issued_rank, project_name, issuanceTime, .by_group = TRUE) |>
    dplyr::mutate(
      dup_n = dplyr::n(),
      dup_id = dplyr::row_number(),
      same_coord_today_yesterday = any(issued == "Today") & any(issued == "Yesterday")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      offset_lon = dplyr::case_when(
        same_coord_today_yesterday & issued == "Today" ~ lon - jitter_amount,
        same_coord_today_yesterday & issued == "Yesterday" ~ lon + jitter_amount,
        dup_n == 1 ~ lon,
        TRUE ~ lon + (jitter_amount * 0.75) * base::cos(2 * base::pi * (dup_id - 1) / dup_n)
      ),
      offset_lat = dplyr::case_when(
        same_coord_today_yesterday & issued == "Today" ~ lat + jitter_amount * 0.35,
        same_coord_today_yesterday & issued == "Yesterday" ~ lat - jitter_amount * 0.35,
        dup_n == 1 ~ lat,
        TRUE ~ lat + (jitter_amount * 0.75) * base::sin(2 * base::pi * (dup_id - 1) / dup_n)
      )
    ) |>
    dplyr::select(
      -issued_rank,
      -dup_n,
      -dup_id,
      -same_coord_today_yesterday
    )
}

handle_burn_click <- function(click, selected_burn_id) {
  shiny::req(click$id)
  
  selected_burn_id(click$id)
  
  leaflet::leafletProxy("forecast_map") |>
    leaflet::setView(
      lng = click$lng,
      lat = click$lat,
      zoom = 8
    )
}
offset_duplicate_points <- function(df, jitter_amount = 0.12) {
  if (nrow(df) == 0) return(df)
  
  df %>%
    mutate(
      issued_rank = case_when(
        issued == "Today" ~ 1L,
        issued == "Yesterday" ~ 2L,
        TRUE ~ 3L
      )
    ) %>%
    group_by(lat, lon) %>%
    arrange(issued_rank, project_name, issuanceTime, .by_group = TRUE) %>%
    mutate(
      dup_n = n(),
      dup_id = row_number(),
      same_coord_today_yesterday = any(issued == "Today") & any(issued == "Yesterday")
    ) %>%
    ungroup() %>%
    mutate(
      offset_lon = case_when(
        same_coord_today_yesterday & issued == "Today" ~ lon - jitter_amount,
        same_coord_today_yesterday & issued == "Yesterday" ~ lon + jitter_amount,
        dup_n == 1 ~ lon,
        TRUE ~ lon + (jitter_amount * 0.75) * cos(2 * pi * (dup_id - 1) / dup_n)
      ),
      offset_lat = case_when(
        same_coord_today_yesterday & issued == "Today" ~ lat + jitter_amount * 0.35,
        same_coord_today_yesterday & issued == "Yesterday" ~ lat - jitter_amount * 0.35,
        dup_n == 1 ~ lat,
        TRUE ~ lat + (jitter_amount * 0.75) * sin(2 * pi * (dup_id - 1) / dup_n)
      )
    ) %>%
    select(-issued_rank, -dup_n, -dup_id, -same_coord_today_yesterday)
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
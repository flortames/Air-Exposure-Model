#' Estimate total daily exposure to an air pollutant
#'
#' Estimates an individual's total daily exposure to an air pollutant by
#' combining exposure during trips, exposure at destinations, and exposure at
#' the home location. Travel routes are obtained from the TomTom Routing API,
#' while pollutant concentrations are extracted from hourly grid shapefiles.
#'
#' @param travel_list Data frame containing the sequence of locations visited
#'   during the day. It must contain `latitude` and `longitude` columns. The
#'   first location is assumed to be the home location and the final trip always
#'   returns to this point.
#' @param mode Character vector indicating the travel mode for each trip (for
#'   example `"car"`, `"pedestrian"` or `"bicycle"`).
#' @param dir Character. Working directory containing the pollutant grids and
#'   temporary files.
#' @param key Character. TomTom API key.
#' @param selection Integer or character vector indicating which alternative
#'   route is selected for each trip.
#' @param output_exp Character. Output type. One of `"df"`, `"plot"` or
#'   `"polyline"`.
#' @param departure_time_home Character. Departure date and time from the home
#'   location.
#' @param activity_minutes Numeric vector indicating the duration (minutes) of
#'   the activity performed at each destination.
#' @param pollutant Character. Pollutant name used for map classification (for
#'   example `"PM2.5"`).
#' @param shapeValue Character. Name of the pollutant concentration field in the
#'   grid shapefiles.
#' @param gridID Character. Name of the grid identifier field.
#' @param units Character. Pollutant concentration units displayed in the map.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Retrieves alternative routes between consecutive locations using the
#'   TomTom Routing API.
#'   \item Selects one route for each trip according to `selection`.
#'   \item Estimates exposure during travel using the selected route.
#'   \item Estimates exposure while remaining at each destination.
#'   \item Estimates exposure at the home location before the first trip.
#'   \item Calculates total daily exposure.
#'   \item Optionally returns an interactive map showing routes and pollutant
#'   concentrations.
#' }
#'
#' This function relies on the helper functions
#' `alternative_trajectories()`, `temporary_grid_search()`,
#' `points_to_line()`, `map_colors()`, and `function_hours()`.
#'
#' @return
#' Depending on `output_exp`:
#' \describe{
#'   \item{`"df"`}{A data frame summarizing exposure for each trip and the total
#'   daily exposure.}
#'   \item{`"plot"`}{An interactive `leaflet` map showing the selected routes,
#'   visited locations and pollutant concentrations.}
#'   \item{`"polyline"`}{An `sf` object containing the route polylines.}
#' }
#'
#' @examples
#' \dontrun{
#' exposure <- total_exposure(
#'   travel_list = travel_list,
#'   mode = c("car", "pedestrian"),
#'   dir = data_dir,
#'   key = api_key,
#'   selection = c(1, 1),
#'   output_exp = "plot",
#'   departure_time_home = "2019-08-01 08:00:00",
#'   activity_minutes = c(480),
#'   pollutant = "PM2.5",
#'   shapeValue = "value",
#'   gridID = "ID",
#'   units = expression(mu * g/m^3)
#' )
#' }
#'
#' @export

if (!is.data.frame(travel_list)) {
  stop("'travel_list' must be a data.frame.")
}

if (nrow(travel_list) < 2) {
  stop("'travel_list' must contain at least two locations.")
}

if (length(mode) != nrow(travel_list)) {
  stop("'mode' must have one element for each trip.")
}

if (!output_exp %in% c("df", "plot", "polyline")) {
  stop("'output_exp' must be one of 'df', 'plot' or 'polyline'.")
}

if (!dir.exists(dir)) {
  stop("The directory specified in 'dir' does not exist.")
}

total_exposure <- function(
  travel_list,
  mode,
  dir,
  key,
  selection,
  output_exp,
  departure_time_home,
  activity_minutes,
  pollutant,
  shapeValue,
  gridID,
  units
) {
  df_output <- data.frame()
  rbind_df_1 <- data.frame()

  rbind_route_select <- data.frame()

  # Calculate exposure for each daily trip

  for (i in 1:length(travel_list$long)) {
    print(paste("Point", i, sep = " "))

    # First trip: use the departure time provided by the user

    if (i == 1) {
      origin_coords <- paste(
        travel_list[i, 2],
        travel_list[i, 1],
        sep = ","
      )

      destination_coords <- paste(
        travel_list[i + 1, 2],
        travel_list[i + 1, 1],
        sep = ","
      )

      selection_route <- alternative_trajectories(
        origin = origin_coords,
        dest = destination_coords,
        mode = mode[i],
        dir,
        key = key,
        output = "df",
        hours = departure_time_home,
        gridID,
        shapeValue,
        units
      )
    } else if (i == length(travel_list$long)) {
      # Last trip: return to the first location (home)

      origin_coords <- paste(
        travel_list[i, 2],
        travel_list[i, 1],
        sep = ","
      )

      origin_coords_1 <- paste(
        travel_list[1, 2],
        travel_list[1, 1],
        sep = ","
      )

      selection_route <- alternative_trajectories(
        origin = origin_coords,
        dest = origin_coords_1,
        mode = mode[i],
        dir,
        key = key,
        output = "df",
        hours = prox_hour_output,
        gridID,
        shapeValue,
        units
      )
    } else {
      # Intermediate trips

      origin_coords <- paste(
        travel_list[i, 2],
        travel_list[i, 1],
        sep = ","
      )

      destination_coords <- paste(
        travel_list[i + 1, 2],
        travel_list[i + 1, 1],
        sep = ","
      )

      selection_route <- alternative_trajectories(
        origin = origin_coords,
        dest = destination_coords,
        mode = mode[i],
        dir,
        key = key,
        output = "df",
        hours = prox_hour_output,
        gridID,
        shapeValue,
        units
      )
    }

    # Select the chosen alternative

    selection_route <- selection_route[
      selection_route$type == selection[i],
    ]

    selection_route$i <- paste("Route", i, sep = " ")

    # Destination data

    data_destination <- selection_route[
      selection_route$ID == length(selection_route$ID),
    ]

    lat_destination <- data_destination$lat
    long_destination <- data_destination$long

    # Time interval used to retrieve the destination grid

    if (i == 1) {
      # First trip: arrival time + activity duration

      arrival_time <- as.POSIXct(
        strptime(
          selection_route$arrivalTime[1],
          format = "%Y-%m-%dT%H:%M:%S"
        )
      )

      prox_hour_output <- as.POSIXct(
        strptime(
          selection_route$arrivalTime[1],
          format = "%Y-%m-%dT%H:%M:%S"
        )
      ) +
        minutes(activity_minutes[i, ])

      destination_time <- as.numeric(
        difftime(
          prox_hour_output,
          arrival_time,
          unit = "mins"
        )
      )
    } else if (i == nrow(activity_minutes) + 1) {
      # Last activity: from arrival until 23:59

      prox_hour_output <- as.POSIXct(
        strptime(
          paste(
            substr(departure_time_home, 1, 10),
            "23:59:59",
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S"
        )
      )

      arrival_time <- as.POSIXct(
        strptime(
          selection_route$arrivalTime[1],
          format = "%Y-%m-%dT%H:%M:%S"
        )
      )

      destination_time <- as.numeric(
        difftime(
          prox_hour_output,
          arrival_time,
          unit = "mins"
        )
      )
    } else {
      # Intermediate activities: arrival time + activity duration

      prox_hour_output <- as.POSIXct(
        strptime(
          selection_route$arrivalTime[1],
          format = "%Y-%m-%dT%H:%M:%S"
        )
      ) +
        minutes(activity_minutes[i, ])

      arrival_time <- as.POSIXct(
        strptime(
          selection_route$arrivalTime[1],
          format = "%Y-%m-%dT%H:%M:%S"
        )
      )

      destination_time <- as.numeric(
        difftime(
          prox_hour_output,
          arrival_time,
          unit = "mins"
        )
      )
    }

    # Trip information

    trip_time <- selection_route$travelTimeInMinutes[1] # minutes
    trip_distance <- selection_route$lengthInMeters[1]

    trip_conc <- mean(
      selection_route$daily_pol_value_mean,
      na.rm = TRUE
    )

    # Origin data

    data_origin <- selection_route[
      selection_route$ID == 1,
    ]

    lat_origin <- data_origin$lat
    long_origin <- data_origin$long

    # Destination concentrations

    df_concentractions <- rbind(
      data_origin,
      data_destination
    )

    df_concentractions <- st_as_sf(
      df_concentractions,
      coords = c("long", "lat"),
      crs = 4326
    )

    dir_point <- paste(dir, "/temp/", sep = "")
    name_point <- paste(dir_point, "temp_point.shp", sep = "")

    st_write(
      df_concentractions,
      paste(dir_point, "temp_point.shp", sep = ""),
      driver = "ESRI Shapefile",
      quiet = TRUE
    )

    point <- st_read(name_point, quiet = TRUE)

    # Retrieve the pollutant grid for the selected time interval

    grid <- temporary_grid_search(
      start_hour = arrival_time,
      end_hour = prox_hour_output,
      dir = dir,
      time_format = "%Y-%m-%d %H:%M:%S",
      gridID,
      shapeValue
    )

    intersection_point <- sf::st_intersection(point, grid)

    names(intersection_point) <- c(
      "altrntv",
      "ID",
      "dprtrTm",
      "arrvlTm",
      "lngthIM",
      "trffLIM",
      "travlMd",
      "trffDIS",
      "trvlTIS",
      "lTITTIS",
      "hsTTTIS",
      "nTrTTIS",
      "value",
      "exposur",
      "type",
      "i",
      "gridID",
      "value.1",
      "geometry"
    )

    conc_destination <- which(
      intersection_point$ID == max(intersection_point$ID)
    )

    conc_destination <- intersection_point[conc_destination, ]
    conc_destination <- conc_destination$value.1

    if (i == 1) {
      # Origin concentrations

      zone <- substr(selection_route$arrivalTime[1], 20, 22)

      start_time <- paste(
        substr(arrival_time, 1, 10),
        "00:00:01 ",
        zone,
        sep = " "
      )

      grid_origin <- temporary_grid_search(
        start_hour = start_time,
        end_hour = departure_time_home,
        dir = dir,
        time_format = "%Y-%m-%d %H:%M:%S",
        gridID,
        shapeValue
      )

      intersection_point <- sf::st_intersection(
        point,
        grid_origin
      )

      names(intersection_point) <- c(
        "altrntv",
        "ID",
        "dprtrTm",
        "arrvlTm",
        "lngthIM",
        "trffLIM",
        "travlMd",
        "trffDIS",
        "trvlTIS",
        "lTITTIS",
        "hsTTTIS",
        "nTrTTIS",
        "value",
        "exposur",
        "type",
        "i",
        "gridID",
        "value.1",
        "geometry"
      )

      conc_origin <- which(
        intersection_point$ID == min(intersection_point$ID)
      )

      conc_origin <- intersection_point[conc_origin, ]
      conc_origin <- conc_origin$value.1

      time_origin <- as.numeric(
        difftime(
          departure_time_home,
          start_time,
          unit = "mins"
        )
      )
    } else {
      conc_origin <- NA
      time_origin <- NA
    }

    point <- sf::st_read(name_point, quiet = TRUE)

    file.remove(
      file.path(
        dir_point,
        dir(path = dir_point, pattern = "temp_point.*")
      )
    )

    df_1 <- data.frame(
      lat_origin,
      long_origin,
      lat_destination,
      long_destination,
      conc_origin,
      trip_conc,
      conc_destination,
      trip_time,
      destination_time,
      time_origin,
      i
    )

    rbind_df_1 <- rbind(rbind_df_1, df_1)

    # Store selected route for plotting

    rbind_route_select <- rbind(
      rbind_route_select,
      selection_route
    )
  }

  # Final variables

  # Total exposure during travel, at destinations and at the origin

  rbind_df_1$exp_tot_trajectory <-
    rbind_df_1$trip_conc * rbind_df_1$trip_time

  rbind_df_1$exp_tot_destination <-
    rbind_df_1$conc_destination * rbind_df_1$destination_time

  rbind_df_1$exp_tot_origin <-
    rbind_df_1$conc_origin * rbind_df_1$time_origin

  # Mean exposure (minutes)

  exp_origin <- mean(
    rbind_df_1$exp_tot_origin,
    na.rm = TRUE
  )

  exp_trajectory <- mean(
    rbind_df_1$exp_tot_trajectory,
    na.rm = TRUE
  )

  exp_destination <- mean(
    rbind_df_1$exp_tot_destination,
    na.rm = TRUE
  )

  exp_tot <- round(
    mean(
      c(exp_origin, exp_destination, exp_trajectory),
      na.rm = TRUE
    ) /
      60,
    2
  )

  rbind_df_1$exp_tot <- exp_tot

  # Total accumulated exposure (Version 2)

  exp_origin2 <- sum(
    rbind_df_1$exp_tot_origin,
    na.rm = TRUE
  )

  exp_trajectory2 <- sum(
    rbind_df_1$exp_tot_trajectory,
    na.rm = TRUE
  )

  exp_destination2 <- sum(
    rbind_df_1$exp_tot_destination,
    na.rm = TRUE
  )

  exp_tot2 <- round(
    sum(
      c(exp_origin2, exp_destination2, exp_trajectory2),
      na.rm = TRUE
    ) /
      60,
    2
  )

  rbind_df_1$exp_tot_sum <- exp_tot2
  rbind_df_1$route <- paste("Route", rbind_df_1$i, sep = " ")
  rbind_df_1$mode <- mode

  time_origin_function <- function_hours(
    sum(
      rbind_df_1$destination_time[max(rbind_df_1$i)] +
        rbind_df_1$time_origin[
          !is.na(rbind_df_1$time_origin)
        ]
    )
  )

  trip_time_function <- function_hours(
    sum(rbind_df_1$trip_time)
  )

  destination_time_function <- function_hours(
    sum(
      rbind_df_1$destination_time[
        1:(length(rbind_df_1$destination_time) - 1)
      ]
    )
  )

  # Convert route points to polylines

  route_line <- points_to_line(
    data = rbind_route_select,
    long = "long",
    lat = "lat",
    id_field = "i",
    sort_field = "ID"
  )

  # HTML map style

  tag.map.title <- htmltools::tags$style(
    HTML(
      "
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"
    )
  )

  # Map title

  title <- htmltools::tags$div(
    tag.map.title,
    HTML(
      paste(
        sep = "<br/>",
        paste0("<center><b>Total daily exposure estimate</b></center>"),
        paste0("<b>Exposure: </b>", exp_tot2, " ", units, "/24 h"),
        paste0("<b>Origin time: </b>", time_origin_function, " h"),
        paste0("<b>Activities time: </b>", destination_time_function, " h"),
        paste0("<b>Travel time: </b>", trip_time_function, " h")
      )
    )
  )

  # Travel list IDs

  num_rows <- nrow(travel_list)
  id <- c(1:num_rows)

  travel_list <- cbind(id, travel_list)

  input_hour <- as.POSIXct(
    strptime(
      departure_time_home,
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

  init_day <- paste(date(input_hour), "00:00:01", sep = " ")
  finish_day <- paste(date(input_hour), "23:59:01", sep = " ")

  grid_tot <- temporary_grid_search(
    start_hour = init_day,
    end_hour = finish_day,
    dir = dir,
    time_format = "%Y-%m-%d %H:%M:%S",
    gridID,
    shapeValue
  )

  # Assign AQI categories

  grid_tot <- map_colors(grid_tot, pollutant)

  # Color palettes

  palette_route <- c(
    "#023858",
    "#49006a",
    "#00441b",
    "#e7298a",
    "#feb24c",
    "#3690c0",
    "#016c59",
    "#8c510a",
    "#f03b20"
  )

  palfac <- leaflet::colorFactor(
    unique(grid_tot$color),
    domain = grid_tot$category
  )

  pal <- leaflet::colorFactor(
    palette_route,
    domain = rbind_df_1$i
  )

  circle_pal <- leaflet::colorFactor(
    palette_route,
    domain = travel_list$id
  )

  # Create interactive map

  mapa <- leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addAwesomeMarkers(
      travel_list,
      lng = travel_list$long,
      lat = travel_list$lat,
      label = paste("Point", travel_list$id, sep = " ")
    ) |>
    leaflet::addPolylines(
      data = route_line,
      opacity = 0.8,
      stroke = TRUE,
      weight = c((rbind_df_1$i) + 5),
      color = c(pal(rbind_df_1$i)),
      group = as.character(rbind_df_1$route)
    ) |>
    leaflet::addPolygons(
      data = grid_tot,
      color = "#636363",
      group = "Concentrations",
      weight = 2,
      smoothFactor = 0.1,
      opacity = 0.1,
      fillOpacity = 0.5,
      fillColor = grid_tot$color
    ) |>
    leaflet::addTiles() |>
    leaflet::addControl(
      title,
      position = "topleft",
      className = "map-title"
    ) |>
    leaflet::addLegend(
      data = grid_tot,
      position = "bottomleft",
      pal = palfac,
      values = grid_tot$category,
      title = paste(
        "US AQI Level",
        pollutant,
        units,
        sep = " "
      )
    ) |>
    leaflet::addLayersControl(
      overlayGroups = c(
        "Concentrations",
        c(rbind_df_1$route)
      )
    )

  if (output_exp == "df") {
    file.remove(
      file.path(
        dir_point,
        dir(path = dir_point, pattern = "temp.*")
      )
    )
    return(rbind_df_1)
  }

  if (output_exp == "plot") {
    file.remove(
      file.path(
        dir_point,
        dir(path = dir_point, pattern = "temp.*")
      )
    )
    return(mapa)
  }

  if (output_exp == "polyline") {
    file.remove(
      file.path(
        dir_point,
        dir(path = dir_point, pattern = "temp.*")
      )
    )
    return(route_line)
  }
}

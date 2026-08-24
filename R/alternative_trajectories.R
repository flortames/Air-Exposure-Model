#' Compare alternative travel routes using pollutant concentrations
#'
#' Retrieves alternative routes between an origin and a destination using the
#' TomTom Routing API and estimates the pollutant exposure associated with each
#' route by intersecting the trajectories with hourly pollutant concentration
#' grids.
#'
#' The function identifies the fastest, shortest, least polluted, most
#' polluted, lowest exposure, and highest exposure routes. Results can be
#' returned either as a data frame or as an interactive map.
#'
#' @param origin Character string containing the origin coordinates in the
#'   format `"latitude,longitude"`.
#' @param dest Character string containing the destination coordinates in the
#'   format `"latitude,longitude"`.
#' @param mode Character. Travel mode accepted by the TomTom Routing API
#'   (e.g., `"car"`, `"truck"`, `"pedestrian"`).
#' @param hours Character. Departure date and time in the format
#'   "YYYY-mm-dd HH:MM:SS".
#' @param key Character. TomTom API key.
#' @param dir Character. Directory containing the hourly pollutant grid
#'   shapefiles.
#' @param gridID Character. Name of the grid identifier field.
#' @param shapeValue Character. Name of the pollutant concentration field in
#'   the grid.
#' @param pollutant Character. Pollutant name used for map labels and legends.
#' @param units Character. Units of the pollutant concentration
#'   (e.g., `"µg/m³"`).
#' @param output Character. Output type. Either `"df"` to return a data frame
#'   or `"plot"` to return an interactive `leaflet` map.
#'
#' @details
#' This function combines route information obtained from the TomTom Routing
#' API with hourly pollutant concentration grids. If the trip spans multiple
#' hourly grids, pollutant concentrations are averaged using
#' `temporary_grid_search()`. Exposure is estimated as the product of the
#' average pollutant concentration along each route and the travel time.
#'
#' A valid TomTom API key and an active internet connection are required.
#'
#' @return
#' If `output = "df"`, a data frame summarizing the selected alternative
#' routes and their associated travel metrics, pollutant concentrations and
#' exposure estimates.
#'
#' If `output = "plot"`, an interactive `leaflet` map displaying the pollutant
#' grid together with the selected alternative routes.
#'
#' @examples
#' \dontrun{
#' routes <- alternative_trajectories(
#'   origin = "-31.4201,-64.1888",
#'   dest = "-31.4300,-64.2000",
#'   mode = "car",
#'   hours = "2019-08-01 08:00:00",
#'   key = "YOUR_API_KEY",
#'   dir = "path/to/grids",
#'   gridID = "ID",
#'   shapeValue = "PM25",
#'   pollutant = "PM2.5",
#'   units = "µg/m³",
#'   output = "df"
#' )
#' }
#'
#' @export

alternative_trajectories <- function(
  origin,
  dest,
  mode,
  dir,
  key,
  output,
  hours = NULL,
  gridID,
  shapeValue,
  units,
  pollutant
) {
  # Retrieve alternative routes from the TomTom Routing API

  trajectory <- trajectories_tomtom(
    origin,
    dest,
    mode,
    hours,
    key
  )

  # Convert route points to LINESTRING geometries

  v_lines <- points_to_line(
    data = trajectory,
    long = "long",
    lat = "lat",
    id_field = "alternative",
    sort_field = "ID"
  )

  id_df <- data.frame()

  trajectory |>
    dplyr::group_by(alternative) |>
    dplyr::group_split() -> dat_agrupado

  for (x in seq_len(nrow(v_lines))) {
    departureTime <- dat_agrupado[[x]][["departureTime"]][1]

    arrivalTime <- dat_agrupado[[x]][["arrivalTime"]][1]

    lengthInKM <- dat_agrupado[[x]][["lengthInKM"]][1]

    trafficLengthInKM <- dat_agrupado[[x]][["trafficLengthInKM"]][1]

    travelMode <- dat_agrupado[[x]][["travelMode"]][1]

    trafficDelayInMinutes <-
      dat_agrupado[[x]][["trafficDelayInMinutes"]][1]

    travelTimeInMinutes <-
      dat_agrupado[[x]][["travelTimeInMinutes"]][1]

    liveTrafficIncidentsTravelTimeInMinutes <-
      dat_agrupado[[x]][[
        "liveTrafficIncidentsTravelTimeInMinutes"
      ]][1]

    historicTrafficTravelTimeInMinutes <-
      dat_agrupado[[x]][[
        "historicTrafficTravelTimeInMinutes"
      ]][1]

    noTrafficTravelTimeInMinutes <-
      dat_agrupado[[x]][[
        "noTrafficTravelTimeInMinutes"
      ]][1]

    alternative <- dat_agrupado[[x]][["alternative"]][1]
  }

  df2 <- dplyr::left_join(v_lines, id_df, by = "alternative")

  # Retrieve the pollutant grid corresponding to the travel period

  sf::st_write(
    obj = df2,
    dsn = "./temp",
    layer = "temp",
    driver = "ESRI Shapefile",
    quiet = TRUE
  )

  df3 <- sf::st_read(
    dsn = "temp/temp.shp",
    quiet = TRUE
  )

  # Search the pollutant grid corresponding to the selected time

  grid <- temporary_grid_search(
    start_hour = df3$dprtrTm[1],
    end_hour = df3$arrvlTm[length(df3$arrvlTm)],
    dir = dir,
    time_format = "%Y-%m-%dT%H:%M:%S",
    gridID = gridID,
    shapeValue = shapeValue
  )

  intersection_grid <- sf::st_intersection(df3, grid)

  # Delete temporary files

  file.remove(
    file.path(
      "./temp",
      dir(path = "./temp", pattern = "temp.*")
    )
  )

  dataSplit_intersection <- intersection_grid |>
    dplyr::group_by(altrntv) |>
    dplyr::group_split()

  sum_df <- data.frame()

  for (i in seq_along(dataSplit_intersection)) {
    origin <- dataSplit_intersection[[i]][["origin"]][1]

    destination <- dataSplit_intersection[[i]][["destination"]][1]

    alternative <- dataSplit_intersection[[i]][["altrntv"]][1]

    daily_pol_value_mean <- round(
      mean(
        dataSplit_intersection[[i]][["value"]],
        na.rm = TRUE
      ),
      2
    )

    df <- data.frame(
      alternative = alternative,
      daily_pol_value_mean = daily_pol_value_mean
    )

    sum_df <- rbind(sum_df, df)
  }

  df_merge <- merge(
    trajectory,
    sum_df,
    by = "alternative"
  )

  trajectory <- df_merge

  # Calculate mean exposure for each route

  trajectory$exposure_value_mean <- round(
    (trajectory$daily_pol_value_mean *
      trajectory$travelTimeInMinutes) /
      60,
    2
  )

  # 01. Faster route

  faster_route <- trajectory[
    trajectory$travelTimeInMinutes == min(trajectory$travelTimeInMinutes),
  ]

  faster_route <- faster_route[
    faster_route$alternative == faster_route$alternative[1],
  ]

  faster_route$type <- "fast"

  # 02. Shorter route

  shorter_route <- trajectory[
    trajectory$lengthInKM == min(trajectory$lengthInKM),
  ]

  shorter_route <- shorter_route[
    shorter_route$alternative == shorter_route$alternative[1],
  ]

  shorter_route$type <- "short"

  # 03. Least polluted route

  less_polluted_route <- trajectory[
    trajectory$daily_pol_value_mean == min(trajectory$daily_pol_value_mean),
  ]

  less_polluted_route <- less_polluted_route[
    less_polluted_route$alternative == less_polluted_route$alternative[1],
  ]

  less_polluted_route$type <- "lesspol"

  # 04. Most polluted route

  more_polluted_route <- trajectory[
    trajectory$daily_pol_value_mean == max(trajectory$daily_pol_value_mean),
  ]

  more_polluted_route <- more_polluted_route[
    more_polluted_route$alternative == more_polluted_route$alternative[1],
  ]

  more_polluted_route$type <- "morepol"

  # 05. Highest exposure route

  more_exposure_route <- trajectory[
    trajectory$exposure_value_mean == max(trajectory$exposure_value_mean),
  ]

  more_exposure_route <- more_exposure_route[
    more_exposure_route$alternative == more_exposure_route$alternative[1],
  ]

  more_exposure_route$type <- "moreexpos"

  # 06. Lowest exposure route

  less_exposure_route <- trajectory[
    trajectory$exposure_value_mean == min(trajectory$exposure_value_mean),
  ]

  less_exposure_route <- less_exposure_route[
    less_exposure_route$alternative == less_exposure_route$alternative[1],
  ]

  less_exposure_route$type <- "lessexpos"

  df_output <- data.frame()

  # Return a data frame with the selected routes

  if (output == "df") {
    df_output <- rbind(
      faster_route,
      shorter_route,
      more_polluted_route,
      less_polluted_route,
      more_exposure_route,
      less_exposure_route
    )

    return(df_output)
  }

  # Create LINESTRING objects for the selected routes

  less_polluted_route_line <- points_to_line(
    data = less_polluted_route,
    long = "long",
    lat = "lat",
    id_field = NULL,
    sort_field = "ID"
  )

  more_polluted_route_line <- points_to_line(
    data = more_polluted_route,
    long = "long",
    lat = "lat",
    id_field = NULL,
    sort_field = "ID"
  )

  shorter_route_line <- points_to_line(
    data = shorter_route,
    long = "long",
    lat = "lat",
    id_field = NULL,
    sort_field = "ID"
  )

  faster_route_line <- points_to_line(
    data = faster_route,
    long = "long",
    lat = "lat",
    id_field = NULL,
    sort_field = "ID"
  )

  more_exposure_route_line <- points_to_line(
    data = more_exposure_route,
    long = "long",
    lat = "lat",
    id_field = NULL,
    sort_field = "ID"
  )

  less_exposure_route_line <- points_to_line(
    data = less_exposure_route,
    long = "long",
    lat = "lat",
    id_field = NULL,
    sort_field = "ID"
  )

  # Output: interactive map

  if (output == "plot") {
    # Map title

    tag.map.title <- htmltools::tags$style(
      htmltools::HTML(
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

    title <- htmltools::tags$div(
      tag.map.title,
      htmltools::HTML(
        paste(
          "<center><b>Travel alternative with mode:</b></center>",
          mode
        )
      )
    )

    # Popup content

    content_more_cont <- paste(
      sep = "<br/>",
      paste0("<center><b>More polluted route: </b></center>"),
      paste0(
        "<b>Duration: </b>",
        more_polluted_route$travelTimeInMinutes,
        " min"
      ),
      paste0(
        "<b>Distance: </b>",
        more_polluted_route$lengthInKM,
        " km"
      ),
      paste0(
        "<b>Concentrations: </b>",
        more_polluted_route$value,
        " ",
        units
      ),
      paste0(
        "<b>Exposure: </b>",
        more_polluted_route$exposure,
        " ",
        units,
        "/h"
      )
    )

    content_less_cont <- paste(
      sep = "<br/>",
      paste0("<center><b>Less polluted route: </b></center>"),
      paste0(
        "<b>Duration: </b>",
        less_polluted_route$travelTimeInMinutes,
        " min"
      ),
      paste0(
        "<b>Distance: </b>",
        less_polluted_route$lengthInKM,
        " km"
      ),
      paste0(
        "<b>Concentrations: </b>",
        less_polluted_route$value,
        " ",
        units
      ),
      paste0(
        "<b>Exposure: </b>",
        less_polluted_route$exposure,
        " ",
        units,
        "/h"
      )
    )

    content_short <- paste(
      sep = "<br/>",
      paste0("<center><b>Shorter route: </b></center>"),
      paste0(
        "<b>Duration: </b>",
        shorter_route$travelTimeInMinutes,
        " min"
      ),
      paste0(
        "<b>Distance: </b>",
        shorter_route$lengthInKM,
        " km"
      ),
      paste0(
        "<b>Concentrations: </b>",
        shorter_route$value,
        " ",
        units
      ),
      paste0(
        "<b>Exposure: </b>",
        shorter_route$exposure,
        " ",
        units,
        "/h"
      )
    )

    content_fast <- paste(
      sep = "<br/>",
      paste0("<center><b>Faster route: </b></center>"),
      paste0(
        "<b>Duration: </b>",
        faster_route$travelTimeInMinutes,
        " min"
      ),
      paste0(
        "<b>Distance: </b>",
        faster_route$lengthInKM,
        " km"
      ),
      paste0(
        "<b>Concentrations: </b>",
        faster_route$value,
        " ",
        units
      ),
      paste0(
        "<b>Exposure: </b>",
        faster_route$exposure,
        " ",
        units,
        "/h"
      )
    )

    content_less_exp <- paste(
      sep = "<br/>",
      paste0("<center><b>Less exposure route: </b></center>"),
      paste0(
        "<b>Duration: </b>",
        less_exposure_route$travelTimeInMinutes,
        " min"
      ),
      paste0(
        "<b>Distance: </b>",
        less_exposure_route$lengthInKM,
        " km"
      ),
      paste0(
        "<b>Concentrations: </b>",
        less_exposure_route$value,
        " ",
        units
      ),
      paste0(
        "<b>Exposure: </b>",
        less_exposure_route$exposure,
        " ",
        units,
        "/h"
      )
    )

    content_more_exp <- paste(
      sep = "<br/>",
      paste0("<center><b>More exposure route: </b></center>"),
      paste0(
        "<b>Duration: </b>",
        more_exposure_route$travelTimeInMinutes,
        " min"
      ),
      paste0(
        "<b>Distance: </b>",
        more_exposure_route$lengthInKM,
        " km"
      ),
      paste0(
        "<b>Concentrations: </b>",
        more_exposure_route$value,
        " ",
        units
      ),
      paste0(
        "<b>Exposure: </b>",
        more_exposure_route$exposure,
        " ",
        units,
        "/h"
      )
    )

    grid <- map_colors(grid, pollutant)

    # Grid colors

    palette_grid <- c(
      "#abdda4",
      "#f8fd66",
      "#fdde61",
      "#d74a4c",
      "#b687ba",
      "#590e63"
    )

    palfac <- leaflet::colorFactor(
      unique(grid$color),
      domain = grid$category
    )

    # Build the interactive map

    map <- leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addPolygons(
        data = grid,
        color = ~color,
        fillColor = ~color,
        fillOpacity = 0.6,
        weight = 0.3,
        popup = ~ paste0(
          pollutant,
          ": ",
          value,
          " ",
          units
        )
      ) |>
      leaflet::addPolylines(
        data = faster_route_line,
        color = "#1f78b4",
        weight = 5,
        popup = content_fast
      ) |>
      leaflet::addPolylines(
        data = shorter_route_line,
        color = "#33a02c",
        weight = 5,
        popup = content_short
      ) |>
      leaflet::addPolylines(
        data = less_polluted_route_line,
        color = "#ff7f00",
        weight = 5,
        popup = content_less_cont
      ) |>
      leaflet::addPolylines(
        data = more_polluted_route_line,
        color = "#e31a1c",
        weight = 5,
        popup = content_more_cont
      ) |>
      leaflet::addPolylines(
        data = less_exposure_route_line,
        color = "#6a3d9a",
        weight = 5,
        popup = content_less_exp
      ) |>
      leaflet::addPolylines(
        data = more_exposure_route_line,
        color = "#b15928",
        weight = 5,
        popup = content_more_exp
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors = palette_grid,
        labels = levels(grid$category),
        title = pollutant,
        opacity = 1
      ) |>
      leaflet::addControl(
        title,
        position = "topright",
        className = "map-title"
      )

    return(map)
  }
}

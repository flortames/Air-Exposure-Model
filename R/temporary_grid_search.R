#' Retrieve Pollutant Grid(s) for a Given Time Period
#'
#' Retrieves the pollutant grid corresponding to a given date and time.
#' When a time interval spanning multiple hours is provided, the function
#' averages the pollutant values across all hourly grids and returns a
#' single grid.
#'
#' @param start_hour Character. Start date and time.
#' @param end_hour Optional character. End date and time. If `NULL`, only
#'   the grid corresponding to `start_hour` is returned.
#' @param dir Character. Directory containing the hourly grid shapefiles.
#' @param time_format Character. Format used to parse `start_hour` and
#'   `end_hour`.
#' @param gridID Character. Name of the field containing the grid cell
#'   identifier.
#' @param shapeValue Character. Name of the field containing the pollutant
#'   concentration values.
#'
#' @details
#' If `start_hour` and `end_hour` span multiple hours, the function
#' retrieves every corresponding hourly grid, calculates the mean pollutant
#' concentration for each grid cell, and returns the resulting averaged
#' grid as an `sf` object.
#'
#' The returned object is always transformed to the WGS84 geographic
#' coordinate reference system (EPSG:4326).
#'
#' @return
#' An `sf` object containing the pollutant grid corresponding to the
#' requested hour or the average pollutant grid over the requested time
#' interval.
#'
#' @examples
#' \dontrun{
#' temporary_grid_search(
#'   start_hour = "2019-08-01 08:00:00",
#'   end_hour = "2019-08-01 10:00:00",
#'   dir = "path/to/grids",
#'   time_format = "%Y-%m-%d %H:%M:%S",
#'   gridID = "ID",
#'   shapeValue = "PM25"
#' )
#' }
#'
#' @export

temporary_grid_search <- function(
  start_hour,
  end_hour = NULL,
  dir,
  time_format,
  gridID,
  shapeValue
) {
  # Retrieve the pollutant grid corresponding to the requested hour(s)

  if (!dir.exists(dir)) {
    stop("Directory specified in 'dir' does not exist.")
  }

  trajectory_grid_rbind <- data.frame()

  # Start hour

  only_start_hour <- lubridate::hour(
    as.POSIXct(
      strptime(start_hour, format = time_format)
    )
  )

  if (is.na(only_start_hour)) {
    stop("'start_hour' could not be parsed using 'time_format'.")
  }

  # End hour

  if (!is.null(end_hour)) {
    only_end_hour <- lubridate::hour(
      as.POSIXct(
        strptime(end_hour, format = time_format)
      )
    )

    if (is.na(only_end_hour)) {
      stop("'end_hour' could not be parsed using 'time_format'.")
    }
  }

  # Only one hour requested

  if (is.null(end_hour)) {
    df_start_grids <- sf::st_read(
      hourly_grid(
        hour = start_hour,
        time_format = time_format,
        dir = dir
      ),
      quiet = TRUE
    )

    df_start_grid <- sf::st_transform(
      df_start_grids,
      crs = 4326
    )

    names(df_start_grid)[names(df_start_grid) == gridID] <- "ID"
  } else if (only_start_hour == only_end_hour) {
    # Start and end hours are the same

    trajectory_grid <- sf::st_read(
      hourly_grid(
        hour = start_hour,
        time_format = time_format,
        dir = dir
      ),
      quiet = TRUE
    )

    salida <- sf::st_transform(
      trajectory_grid,
      crs = 4326
    )

    names(salida)[names(salida) == gridID] <- "ID"
  } else {
    # Average pollutant concentration across multiple hourly grids

    for (j in only_start_hour:only_end_hour) {
      if (j < 10) {
        j_hour <- paste0("0", j)
      } else {
        j_hour <- j
      }

      zone <- substr(
        start_hour,
        nchar(start_hour) - 2,
        nchar(start_hour)
      )

      day <- paste(
        substr(start_hour, 1, 10),
        paste0(j_hour, ":00:00"),
        zone
      )

      trajectory_grid <- sf::st_read(
        hourly_grid(
          hour = day,
          time_format = "%Y-%m-%d %H:%M:%S",
          dir = dir
        ),
        quiet = TRUE
      )

      trajectory_grid$hour <- day

      trajectory_grid_rbind <- rbind(
        trajectory_grid_rbind,
        trajectory_grid
      )
    }

    # Group by grid cell and calculate the mean concentration

    ID <- sf::st_drop_geometry(
      trajectory_grid_rbind
    )

    names(ID) <- "ID"

    trajectory_grid_rbind <- cbind(
      trajectory_grid_rbind,
      ID
    )

    data_grilla <- trajectory_grid_rbind |>
      dplyr::group_by(ID) |>
      dplyr::group_split()

    df_grilla <- data.frame()

    for (p in seq_along(data_grilla)) {
      ID <- data_grilla[[p]][["ID"]][1]

      value <- mean(
        data_grilla[[p]][[shapeValue]],
        na.rm = TRUE
      )

      geometry <- data_grilla[[p]][["geometry"]][1]

      df <- data.frame(
        ID = ID,
        value = value,
        geometry = geometry
      )

      df_grilla <- rbind(
        df_grilla,
        df
      )
    }

    sf::st_write(
      df_grilla,
      "./temp/temp_grid.shp",
      delete_layer = TRUE,
      quiet = TRUE
    )

    trajectory_grid <- sf::st_read(
      "./temp/temp_grid.shp",
      quiet = TRUE
    )

    salida <- sf::st_transform(
      trajectory_grid,
      crs = 4326
    )
  }

  if (is.null(end_hour)) {
    return(df_start_grid)
  } else {
    return(salida)
  }
}

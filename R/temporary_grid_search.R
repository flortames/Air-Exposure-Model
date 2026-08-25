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
  # Validate inputs

  if (!dir.exists(dir)) {
    stop("Directory specified in 'dir' does not exist.")
  }

  if (!is.character(gridID) || length(gridID) != 1) {
    stop("'gridID' must be a single character value.")
  }

  if (!is.character(shapeValue) || length(shapeValue) != 1) {
    stop("'shapeValue' must be a single character value.")
  }

  # Parse start time

  start_time <- as.POSIXct(
    strptime(start_hour, format = time_format)
  )

  if (length(start_time) != 1 || is.na(start_time)) {
    stop("'start_hour' could not be parsed using 'time_format'.")
  }

  # Parse end time

  if (is.null(end_hour)) {
    end_time <- start_time
  } else {
    end_time <- as.POSIXct(
      strptime(end_hour, format = time_format)
    )

    if (length(end_time) != 1 || is.na(end_time)) {
      stop("'end_hour' could not be parsed using 'time_format'.")
    }

    if (end_time < start_time) {
      stop("'end_hour' must not be earlier than 'start_hour'.")
    }
  }

  # Generate every required hour, including periods crossing midnight

  requested_hours <- seq(
    from = lubridate::floor_date(start_time, unit = "hour"),
    to = lubridate::floor_date(end_time, unit = "hour"),
    by = "hour"
  )

  grids <- vector("list", length(requested_hours))

  for (i in seq_along(requested_hours)) {
    hour_text <- format(
      requested_hours[i],
      format = "%Y-%m-%d %H:%M:%S"
    )

    grid_name <- hourly_grid(
      hour = hour_text,
      time_format = "%Y-%m-%d %H:%M:%S",
      dir = dir
    )

    grid_path <- file.path(dir, grid_name)

    grid <- sf::st_read(
      grid_path,
      quiet = TRUE
    )

    if (!gridID %in% names(grid)) {
      stop(
        paste0(
          "Column specified in 'gridID' was not found in ",
          grid_name,
          "."
        )
      )
    }

    if (!shapeValue %in% names(grid)) {
      stop(
        paste0(
          "Column specified in 'shapeValue' was not found in ",
          grid_name,
          "."
        )
      )
    }

    names(grid)[names(grid) == gridID] <- "ID"
    names(grid)[names(grid) == shapeValue] <- "value"

    grid <- sf::st_transform(grid, crs = 4326)

    grids[[i]] <- grid[, c("ID", "value")]
  }

  # A single requested grid requires no averaging

  if (length(grids) == 1) {
    return(grids[[1]])
  }

  # Combine grids and calculate the hourly mean for each cell

  combined_grid <- do.call(rbind, grids)

  output_grid <- combined_grid |>
    dplyr::group_by(.data$ID) |>
    dplyr::summarise(
      value = mean(.data$value, na.rm = TRUE),
      .groups = "drop"
    )

  return(output_grid)
}

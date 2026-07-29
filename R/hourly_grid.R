#' Retrieve the Hourly Grid File for a Given Date and Time
#'
#' Searches a directory containing hourly pollutant grid shapefiles and
#' returns the filename corresponding to the requested date and hour.
#'
#' @param hour Character. Date and time used to identify the grid file.
#' @param time_format Character. Format used to parse `hour`.
#' @param dir Character. Directory containing the hourly grid shapefiles.
#'
#' @details
#' Hourly grid shapefiles are expected to follow the naming convention
#' `"YYYY-mm-dd_HHMM.shp"`.
#'
#' @return
#' A character string containing the name of the matching shapefile.
#'
#' @examples
#' \dontrun{
#' hourly_grid(
#'   hour = "2019-08-01 08:00:00",
#'   time_format = "%Y-%m-%d %H:%M:%S",
#'   dir = "path/to/grids"
#' )
#' }
#'
#' @export

hourly_grid <- function(hour, time_format, dir) {
  if (!dir.exists(dir)) {
    stop("Directory specified in 'dir' does not exist.")
  }

  input_hour <- as.POSIXct(
    strptime(hour, format = time_format)
  )

  if (is.na(input_hour)) {
    stop("'hour' could not be parsed using 'time_format'.")
  }

  hour_exposure <- lubridate::hour(input_hour)
  exposure_day <- lubridate::date(input_hour)

  # List available shapefiles

  file_list <- list.files(
    path = dir,
    pattern = "\\.shp$",
    full.names = FALSE
  )

  if (length(file_list) == 0) {
    stop("No shapefiles were found in 'dir'.")
  }

  # Shapefile names must follow the format YYYY-mm-dd_HHMM.shp

  table_files <- as.POSIXct(
    strptime(
      substr(file_list, 1, 15),
      format = "%Y-%m-%d_%H%M"
    )
  )

  searched_date <- which(
    lubridate::date(table_files) == exposure_day
  )

  table_files <- table_files[searched_date]

  searched_hour <- which(
    lubridate::hour(table_files) == hour_exposure
  )

  file <- table_files[searched_hour]

  name_file <- paste(
    substr(file, 1, 10),
    "_",
    substr(file, 12, 13),
    substr(file, 15, 16),
    ".shp",
    sep = ""
  )

  return(name_file)
}

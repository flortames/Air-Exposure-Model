#' Estimate Daily Exposure Using the Traditional Model
#'
#' Estimates daily exposure assuming that an individual remains at the
#' origin location throughout the entire day. Exposure is calculated by
#' intersecting the origin point with the daily average pollutant grid and
#' multiplying the resulting concentration by 24 hours.
#'
#' @param origin_point A data frame containing the coordinates of the origin
#'   location. It must include the columns `longitude` and `latitude`.
#' @param date Character. Date of interest in the format `"YYYY-mm-dd"`.
#' @param dir Character. Directory containing the hourly pollutant grid
#'   shapefiles.
#' @param gridID Character. Name of the field containing the grid cell
#'   identifier.
#' @param shapeValue Character. Name of the field containing the pollutant
#'   concentration values.
#'
#' @details
#' The function retrieves all hourly pollutant grids for the requested day,
#' computes the daily average concentration using
#' `temporary_grid_search()`, identifies the grid cell containing the
#' origin location, and estimates daily exposure assuming continuous
#' residence at that location.
#'
#' @return
#' A numeric value representing the estimated daily exposure.
#'
#' @examples
#' \dontrun{
#' traditional_model(
#'   origin_point = home_location,
#'   date = "2019-08-01",
#'   dir = "path/to/grids",
#'   gridID = "ID",
#'   shapeValue = "PM25"
#' )
#' }
#'
#' @export

# Traditional exposure model
# Estimate exposure assuming that the individual remains at the origin
# location throughout the entire day.

traditional_model <- function(
  origin_point,
  date,
  dir,
  gridID,
  shapeValue
) {
  if (!dir.exists(dir)) {
    stop("Directory specified in 'dir' does not exist.")
  }

  required_columns <- c("longitude", "latitude")

  if (!all(required_columns %in% names(origin_point))) {
    stop(
      "'origin_point' must contain the columns 'longitude' and 'latitude'."
    )
  }

  # Convert the origin point to a spatial object

  sp::coordinates(origin_point) <- ~ longitude + latitude

  sp::proj4string(origin_point) <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  )

  # Convert to an sf object

  origin_point <- sf::st_as_sf(
    origin_point,
    crs = 4326
  )

  # Retrieve the daily average pollutant grid

  hour_00 <- paste(date, "T00:01:00", sep = "")
  hour_23 <- paste(date, "T23:59:00", sep = "")

  grid_search <- temporary_grid_search(
    start_hour = hour_00,
    end_hour = hour_23,
    dir = dir,
    time_format = "%Y-%m-%dT%H:%M:%S",
    gridID = gridID,
    shapeValue = shapeValue
  )

  # Identify the grid cell containing the origin point

  intersection_point <- sf::st_intersection(
    origin_point,
    grid_search
  )

  # Calculate daily exposure (24 h)

  daily_exposure <- intersection_point$value * 24

  return(daily_exposure)
}

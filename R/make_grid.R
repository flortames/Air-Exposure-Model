#' Create example hourly pollutant grids
#'
#' Creates a regular spatial grid over a user-defined bounding box and exports
#' 24 hourly shapefiles. Each grid cell is assigned a random value, making this
#' function useful for generating example datasets and testing workflows.
#'
#' @param ymin Numeric. Minimum latitude of the grid extent.
#' @param ymax Numeric. Maximum latitude of the grid extent.
#' @param xmin Numeric. Minimum longitude of the grid extent.
#' @param xmax Numeric. Maximum longitude of the grid extent.
#' @param pixelSize Numeric. Grid cell size in meters.
#' @param Dir Character. Directory where the shapefiles will be written.
#' @param date Character. Date used in the output filenames
#'   (`"YYYY-mm-dd"`).
#' @param values Optional. Currently unused.
#'
#' @details
#' The function creates one shapefile for each hour of the day. Pollutant
#' values are generated randomly and are intended only for examples and
#' testing.
#'
#' @return
#' This function is called for its side effect of writing shapefiles to disk.
#' No value is returned.
#'
#' @examples
#' \dontrun{
#' make_grid(
#'   ymin = -31.6,
#'   ymax = -31.3,
#'   xmin = -64.3,
#'   xmax = -64.0,
#'   pixelSize = 1000,
#'   Dir = "example_grids/",
#'   date = "2019-08-01"
#' )
#' }
#'
#' @export

# Create example hourly grids

make_grid <- function(
  ymin,
  ymax,
  xmin,
  xmax,
  pixelSize,
  Dir,
  date,
  values = NULL
) {
  if (
    !is.numeric(pixelSize) ||
      length(pixelSize) != 1 ||
      pixelSize <= 0
  ) {
    stop("'pixelSize' must be a positive numeric value.")
  }

  if (xmin >= xmax) {
    stop("'xmin' must be smaller than 'xmax'.")
  }

  if (ymin >= ymax) {
    stop("'ymin' must be smaller than 'ymax'.")
  }

  if (!dir.exists(Dir)) {
    stop("Directory specified in 'Dir' does not exist.")
  }

  # Convert the grid resolution from meters to degrees

  latitude <- (ymin + ymax) / 2

  # Earth's circumference at the equator (meters)

  circumference_earth_equator <- 40075000

  degree_longitud <- (360 /
    (circumference_earth_equator *
      cos(latitude * pi / 180))) *
    pixelSize

  # Define the spatial extent

  bbox <- sf::st_bbox(
    c(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax
    ),
    crs = sf::st_crs(4326)
  )

  # Define grid resolution

  res <- degree_longitud

  # Create polygon grid

  grid <- sf::st_make_grid(
    bbox,
    cellsize = c(res, res),
    what = "polygons"
  )

  ID <- seq_along(grid)

  for (i in 0:23) {
    random_values <- sample(
      0:500,
      length(grid),
      replace = TRUE
    )

    sfc_polygon_values <- sf::st_sf(
      ID,
      random_values,
      geometry = grid
    )

    names(sfc_polygon_values) <- c(
      "ID",
      "value",
      "geometry"
    )

    if (i == 0) {
      name <- paste(date, "_", "0", i, "01.shp", sep = "")
    } else if (i < 10 & i != 0) {
      name <- paste(date, "_", "0", i, "00.shp", sep = "")
    } else {
      name <- paste(date, "_", i, "00.shp", sep = "")
    }

    # Export shapefile

    sf::st_write(
      sfc_polygon_values,
      file.path(Dir, name)
    )
  }
}

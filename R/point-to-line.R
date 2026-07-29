#' Convert Points to LINESTRING Geometries
#'
#' Converts a set of ordered point coordinates into one or more LINESTRING
#' geometries. If a grouping variable is provided, one LINESTRING is created
#' for each group.
#'
#' @param data A data frame containing point coordinates.
#' @param long Character. Name of the longitude column.
#' @param lat Character. Name of the latitude column.
#' @param id_field Optional character. Name of the column used to group
#'   points into different LINESTRING geometries.
#' @param sort_field Optional character. Name of the column used to order
#'   points before creating the LINESTRING geometries.
#'
#' @return
#' If `id_field` is `NULL`, returns an `sfc_LINESTRING` object with CRS
#' EPSG:4326.
#'
#' If `id_field` is provided, returns an `sf` object containing one
#' LINESTRING for each group and a column with the corresponding group
#' identifier.
#'
#' @examples
#' \dontrun{
#' line <- points_to_line(
#'   data = trajectory,
#'   long = "long",
#'   lat = "lat",
#'   id_field = "alternative",
#'   sort_field = "ID"
#' )
#' }
#'
#' @export

points_to_line <- function(
  data,
  long,
  lat,
  id_field = NULL,
  sort_field = NULL
) {
  # Check input

  if (!long %in% names(data)) {
    stop("Column specified in 'long' was not found.")
  }

  if (!lat %in% names(data)) {
    stop("Column specified in 'lat' was not found.")
  }

  if (!is.null(id_field) && !id_field %in% names(data)) {
    stop("Column specified in 'id_field' was not found.")
  }

  if (!is.null(sort_field) && !sort_field %in% names(data)) {
    stop("Column specified in 'sort_field' was not found.")
  }

  if (nrow(data) < 2) {
    stop("At least two points are required to create a LINESTRING.")
  }

  # Sort points if requested

  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }

  # Create a single LINESTRING

  if (is.null(id_field)) {
    sp_lines <- sf::st_linestring(
      as.matrix(data[, c(long, lat)])
    )

    sp_lines <- sf::st_sfc(sp_lines)
    sf::st_crs(sp_lines) <- 4326

    return(sp_lines)
  }

  # Create multiple LINESTRING geometries

  paths <- split(data, data[[id_field]])

  sp_lines <- sf::st_linestring(
    as.matrix(paths[[1]][, c(long, lat)])
  )

  sp_lines <- sf::st_sfc(sp_lines)
  sf::st_crs(sp_lines) <- 4326

  alternative <- paths[[1]][[id_field]][1]

  sp_lines <- sf::st_sf(
    geometry = sp_lines,
    alternative = alternative
  )

  if (length(paths) > 1) {
    for (p in 2:length(paths)) {
      line <- sf::st_linestring(
        as.matrix(paths[[p]][, c(long, lat)])
      )

      line <- sf::st_sfc(line)
      sf::st_crs(line) <- 4326

      alternative <- paths[[p]][[id_field]][1]

      sp_lines2 <- sf::st_sf(
        geometry = line,
        alternative = alternative
      )

      sp_lines <- rbind(sp_lines, sp_lines2)
    }
  }

  return(sp_lines)
}

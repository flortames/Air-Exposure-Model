#' Assign AQI categories and colors to a pollutant grid
#'
#' Classifies pollutant concentrations according to the U.S. Environmental
#' Protection Agency (EPA) Air Quality Index (AQI) breakpoints and assigns a
#' color to each grid cell for visualization.
#'
#' @param grid An `sf` object containing a pollutant concentration field named
#'   `value`.
#' @param pollutant Character. Pollutant name. Supported pollutants are
#'   `"PM2.5"`, `"PM10"`, `"CO"`, `"SO2"`, `"NO2"` and `"O3"`.
#'
#' @details
#' Two new columns are added to the input grid:
#' \describe{
#'   \item{category}{AQI category assigned according to the pollutant
#'   concentration.}
#'   \item{color}{Hexadecimal color associated with the AQI category.}
#' }
#'
#' AQI breakpoints follow the U.S. EPA Air Quality Index.
#'
#' @return
#' The input `sf` object with two additional columns: `category` and `color`.
#'
#' @examples
#' \dontrun{
#' colored_grid <- map_colors(
#'   grid = pollutant_grid,
#'   pollutant = "PM2.5"
#' )
#' }
#'
#' @export

# Assign AQI categories and colors to a pollutant grid

map_colors <- function(grid, pollutant) {
  if (pollutant == "PM2.5") {
    print(c("The entered pollutant is:", pollutant))

    grid$category <- dplyr::case_when(
      grid$value <= 12.1 ~ "Good",
      grid$value > 12.1 & grid$value <= 35.4 ~ "Moderate",
      grid$value > 35.4 & grid$value <= 55.4 ~
        "Unhealthy for sensible groups",
      grid$value > 55.4 & grid$value <= 150.4 ~ "Unhealthy",
      grid$value > 150.4 & grid$value <= 250.4 ~
        "Very unhealthy",
      grid$value > 250.4 ~ "Hazardous"
    )

    grid$color <- dplyr::case_when(
      grid$category == "Good" ~ "#abdda4",
      grid$category == "Moderate" ~ "#f8fd66",
      grid$category == "Unhealthy for sensible groups" ~ "#fdde61",
      grid$category == "Unhealthy" ~ "#d74a4c",
      grid$category == "Very unhealthy" ~ "#b687ba",
      grid$category == "Hazardous" ~ "#590e63"
    )
  } else if (pollutant == "PM10") {
    # PM10 (μg/m3), 24-hour

    print(c("The entered pollutant is:", pollutant))

    grid$category <- dplyr::case_when(
      grid$value <= 54 ~ "Good",
      grid$value > 54 & grid$value <= 154 ~ "Moderate",
      grid$value > 154 & grid$value <= 254 ~
        "Unhealthy for sensible groups",
      grid$value > 254 & grid$value <= 354 ~ "Unhealthy",
      grid$value > 354 & grid$value <= 424 ~
        "Very unhealthy",
      grid$value > 424 ~ "Hazardous"
    )

    grid$color <- dplyr::case_when(
      grid$category == "Good" ~ "#abdda4",
      grid$category == "Moderate" ~ "#f8fd66",
      grid$category == "Unhealthy for sensible groups" ~ "#fdde61",
      grid$category == "Unhealthy" ~ "#d74a4c",
      grid$category == "Very unhealthy" ~ "#b687ba",
      grid$category == "Hazardous" ~ "#590e63"
    )
  } else if (pollutant == "CO") {
    # CO (ppm), 8-hour

    print(c("The entered pollutant is:", pollutant))

    grid$category <- dplyr::case_when(
      grid$value <= 4.4 ~ "Good",
      grid$value > 4.4 & grid$value <= 9.4 ~ "Moderate",
      grid$value > 9.4 & grid$value <= 12.4 ~
        "Unhealthy for sensible groups",
      grid$value > 12.4 & grid$value <= 15.4 ~ "Unhealthy",
      grid$value > 15.4 & grid$value <= 30.4 ~
        "Very unhealthy",
      grid$value > 30.4 ~ "Hazardous"
    )

    grid$color <- dplyr::case_when(
      grid$category == "Good" ~ "#abdda4",
      grid$category == "Moderate" ~ "#f8fd66",
      grid$category == "Unhealthy for sensible groups" ~ "#fdde61",
      grid$category == "Unhealthy" ~ "#d74a4c",
      grid$category == "Very unhealthy" ~ "#b687ba",
      grid$category == "Hazardous" ~ "#590e63"
    )
  } else if (pollutant == "SO2") {
    # SO2 (ppb), 1-hour

    print(c("The entered pollutant is:", pollutant))

    grid$category <- dplyr::case_when(
      grid$value <= 35 ~ "Good",
      grid$value > 35 & grid$value <= 75 ~ "Moderate",
      grid$value > 75 & grid$value <= 185 ~
        "Unhealthy for sensible groups",
      grid$value > 185 & grid$value <= 304 ~
        "Unhealthy",
      # 1-hour SO2 values do not define AQI values >= 200.
      # Higher AQI values are calculated using 24-hour SO2 concentrations.
      grid$value > 304 & grid$value <= 604 ~
        "Very unhealthy",
      # 1-hour SO2 values do not define AQI values >= 200.
      # Higher AQI values are calculated using 24-hour SO2 concentrations.
      grid$value > 604 ~ "Hazardous"
      # 1-hour SO2 values do not define AQI values >= 200.
      # Higher AQI values are calculated using 24-hour SO2 concentrations.
    )

    grid$color <- dplyr::case_when(
      grid$category == "Good" ~ "#abdda4",
      grid$category == "Moderate" ~ "#f8fd66",
      grid$category == "Unhealthy for sensible groups" ~ "#fdde61",
      grid$category == "Unhealthy" ~ "#d74a4c",
      grid$category == "Very unhealthy" ~ "#b687ba",
      grid$category == "Hazardous" ~ "#590e63"
    )
  } else if (pollutant == "NO2") {
    # NO2 (ppb), 1-hour

    print(c("The entered pollutant is:", pollutant))

    grid$category <- dplyr::case_when(
      grid$value <= 53 ~ "Good",
      grid$value > 53 & grid$value <= 100 ~ "Moderate",
      grid$value > 100 & grid$value <= 360 ~
        "Unhealthy for sensible groups",
      grid$value > 360 & grid$value <= 649 ~ "Unhealthy",
      grid$value > 649 & grid$value <= 1249 ~
        "Very unhealthy",
      grid$value > 1250 ~ "Hazardous"
    )

    grid$color <- dplyr::case_when(
      grid$category == "Good" ~ "#abdda4",
      grid$category == "Moderate" ~ "#f8fd66",
      grid$category == "Unhealthy for sensible groups" ~ "#fdde61",
      grid$category == "Unhealthy" ~ "#d74a4c",
      grid$category == "Very unhealthy" ~ "#b687ba",
      grid$category == "Hazardous" ~ "#590e63"
    )
  } else if (pollutant == "O3" || pollutant == "o3") {
    # O3 (ppm), 1-hour

    print(c("The entered pollutant is:", pollutant))

    grid$category <- dplyr::case_when(
      grid$value > 0.125 & grid$value <= 0.164 ~
        "Unhealthy for sensible groups",
      grid$value > 0.164 & grid$value <= 0.204 ~
        "Unhealthy",
      grid$value > 0.204 & grid$value <= 0.404 ~
        "Very unhealthy",
      grid$value > 0.404 ~ "Hazardous"
    )

    grid$color <- dplyr::case_when(
      grid$category == "Good" ~ "#abdda4",
      grid$category == "Moderate" ~ "#f8fd66",
      grid$category == "Unhealthy for sensible groups" ~ "#fdde61",
      grid$category == "Unhealthy" ~ "#d74a4c",
      grid$category == "Very unhealthy" ~ "#b687ba",
      grid$category == "Hazardous" ~ "#590e63"
    )
  } else {
    stop("The exposure to this pollutant cannot be modeled.")
  }

  return(grid)
}

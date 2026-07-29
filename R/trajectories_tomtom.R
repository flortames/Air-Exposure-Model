#' Retrieve Alternative Routes from the TomTom Routing API
#'
#' Retrieves up to five alternative routes between an origin and destination
#' using the TomTom Routing API. The function returns the route geometry as
#' point coordinates together with travel time, distance, and traffic-related
#' metrics for each alternative route.
#'
#' @param origin Character string containing the origin coordinates in the
#'   format `"latitude,longitude"`.
#' @param dest Character string containing the destination coordinates in the
#'   format `"latitude,longitude"`.
#' @param mode Character. Travel mode accepted by the TomTom Routing API
#'   (e.g., `"car"`, `"truck"`, `"pedestrian"`, `"bicycle"`).
#' @param hour_trajectory Character. Departure date and time in the format
#'   `"YYYY-mm-dd HH:MM:SS"`.
#' @param key Character. Valid TomTom Routing API key.
#'
#' @details
#' This function requires a valid TomTom Routing API key and an active
#' internet connection. The output includes one row for each point along
#' each alternative route returned by the API.
#'
#' @return
#' A data frame with one row per route point and the following variables:
#' \describe{
#'   \item{ID}{Sequential point identifier within each route.}
#'   \item{long}{Longitude of the route point.}
#'   \item{lat}{Latitude of the route point.}
#'   \item{departureTime}{Departure time returned by the API.}
#'   \item{arrivalTime}{Arrival time returned by the API.}
#'   \item{lengthInKM}{Route length in kilometers.}
#'   \item{trafficLengthInKM}{Length affected by traffic in kilometers.}
#'   \item{travelMode}{Travel mode used for routing.}
#'   \item{trafficDelayInMinutes}{Estimated traffic delay in minutes.}
#'   \item{travelTimeInMinutes}{Estimated travel time in minutes.}
#'   \item{liveTrafficIncidentsTravelTimeInMinutes}{Travel time considering
#'   live traffic incidents.}
#'   \item{historicTrafficTravelTimeInMinutes}{Travel time estimated from
#'   historical traffic conditions.}
#'   \item{noTrafficTravelTimeInMinutes}{Estimated travel time assuming no
#'   traffic.}
#'   \item{alternative}{Alternative route identifier.}
#' }
#'
#' @examples
#' \dontrun{
#' routes <- trajectories_tomtom(
#'   origin = "-31.4201,-64.1888",
#'   dest = "-31.4300,-64.2000",
#'   mode = "car",
#'   hour_trajectory = "2019-08-01 08:00:00",
#'   key = "YOUR_API_KEY"
#' )
#' }
#'
#' @export

trajectories_tomtom <- function(
  origin,
  dest,
  mode,
  hour_trajectory,
  key
) {
  if (missing(origin) || missing(dest)) {
    stop("'origin' and 'dest' must be provided.")
  }

  if (missing(key) || key == "") {
    stop("A valid TomTom API key must be provided.")
  }

  if (length(strsplit(origin, ",")[[1]]) != 2) {
    stop("'origin' must be in the format 'latitude,longitude'.")
  }

  if (length(strsplit(dest, ",")[[1]]) != 2) {
    stop("'dest' must be in the format 'latitude,longitude'.")
  }

  num_alternative <- 5

  hour_trajectory <- as.POSIXct(
    strptime(
      hour_trajectory,
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

  if (is.na(hour_trajectory)) {
    stop(
      "'hour_trajectory' could not be parsed. ",
      "Expected format: 'YYYY-mm-dd HH:MM:SS'."
    )
  }

  day <- substr(hour_trajectory, 1, 10)
  hours <- substr(hour_trajectory, 12, 13)
  minutes <- substr(hour_trajectory, 15, 16)

  zone_tot <- substr(format(hour_trajectory, "%z"), 1, 3)

  if (substr(zone_tot, 1, 1) == "-") {
    zone <- substr(format(hour_trajectory, "%z"), 1, 3)

    hour_format <- paste(
      day,
      "T",
      hours,
      "%3A",
      minutes,
      "%3A00",
      zone,
      "%3A00",
      sep = ""
    )
  } else {
    zone <- substr(format(hour_trajectory, "%z"), 2, 3)

    hour_format <- paste(
      day,
      "T",
      hours,
      "%3A",
      minutes,
      "%3A00%2B",
      zone,
      "%3A00",
      sep = ""
    )
  }

  origin_lat <- strsplit(origin, ",")[[1]][1]
  origin_long <- strsplit(origin, ",")[[1]][2]

  destination_lat <- strsplit(dest, ",")[[1]][1]
  destination_long <- strsplit(dest, ",")[[1]][2]

  url <- paste0(
    "https://api.tomtom.com/routing/1/calculateRoute/",
    origin_lat,
    "%2C",
    origin_long,
    "%3A",
    destination_lat,
    "%2C",
    destination_long,
    "/json?maxAlternatives=",
    num_alternative,
    "&departAt=",
    hour_format,
    "&routeRepresentation=polyline",
    "&computeTravelTimeFor=all",
    "&traffic=true",
    "&travelMode=",
    mode,
    "&vehicleEngineType=combustion",
    "&key=",
    key
  )

  # API request

  response <- httr::GET(url)

  httr::stop_for_status(response)

  resp_json <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
  )

  if (length(resp_json$routes) == 0) {
    stop("TomTom API returned no routes.")
  }

  df_rbind <- data.frame()
  df_rbind_output <- data.frame()

  for (j in seq_along(resp_json[["routes"]][["legs"]])) {
    resp <- data.frame(
      long = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][[
        "longitude"
      ]],

      lat = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["latitude"]],

      departureTime = resp_json[["routes"]][["legs"]][[j]][["summary"]][[
        "departureTime"
      ]],

      arrivalTime = resp_json[["routes"]][["legs"]][[j]][["summary"]][[
        "arrivalTime"
      ]],

      lengthInKM = resp_json[["routes"]][["legs"]][[j]][["summary"]][[
        "lengthInMeters"
      ]] /
        1000,

      trafficLengthInKM = resp_json[["routes"]][["legs"]][[j]][["summary"]][[
        "trafficLengthInMeters"
      ]] /
        1000,

      travelMode = resp_json[["routes"]][["sections"]][[1]][["travelMode"]][1],

      trafficDelayInMinutes = round(
        resp_json[["routes"]][["legs"]][[j]][["summary"]][[
          "trafficDelayInSeconds"
        ]] /
          60,
        2
      ),

      travelTimeInMinutes = round(
        resp_json[["routes"]][["legs"]][[j]][["summary"]][[
          "travelTimeInSeconds"
        ]] /
          60,
        2
      ),

      liveTrafficIncidentsTravelTimeInMinutes = round(
        resp_json[["routes"]][["legs"]][[j]][["summary"]][[
          "liveTrafficIncidentsTravelTimeInSeconds"
        ]] /
          60,
        2
      ),

      historicTrafficTravelTimeInMinutes = round(
        resp_json[["routes"]][["legs"]][[j]][["summary"]][[
          "historicTrafficTravelTimeInSeconds"
        ]] /
          60,
        2
      ),

      noTrafficTravelTimeInMinutes = round(
        resp_json[["routes"]][["legs"]][[j]][["summary"]][[
          "noTrafficTravelTimeInSeconds"
        ]] /
          60,
        2
      ),

      alternative = paste0("alternative_", j)
    )

    ID <- seq_len(nrow(resp))

    data_frame_resp <- cbind(ID, resp)

    df_rbind <- rbind(df_rbind, data_frame_resp)
  }

  df_rbind_output <- rbind(df_rbind_output, df_rbind)

  names(df_rbind_output) <- c(
    "ID",
    "long",
    "lat",
    "departureTime",
    "arrivalTime",
    "lengthInKM",
    "trafficLengthInKM",
    "travelMode",
    "trafficDelayInMinutes",
    "travelTimeInMinutes",
    "liveTrafficIncidentsTravelTimeInMinutes",
    "historicTrafficTravelTimeInMinutes",
    "noTrafficTravelTimeInMinutes",
    "alternative"
  )

  return(df_rbind_output)
}

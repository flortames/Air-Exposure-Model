fake_alternative_routes <- data.frame(
  ID = c(1, 2),

  long = c(
    -64.1888,
    -64.2000
  ),

  lat = c(
    -31.4201,
    -31.4300
  ),

  departureTime = c(
    "2019-08-01T08:00:00",
    "2019-08-01T08:00:00"
  ),

  arrivalTime = c(
    "2019-08-01T08:15:00",
    "2019-08-01T08:15:00"
  ),

  travelTimeInMinutes = c(
    15,
    15
  ),

  daily_pol_value_mean = c(
    10,
    10
  ),

  type = c(
    "fast",
    "fast"
  ),

  alternative = c(
    "alternative_1",
    "alternative_1"
  )
)

fake_exposure_grid <- sf::st_sf(
  ID = 1,
  value = 10,
  geometry = sf::st_sfc(
    sf::st_polygon(
      list(
        rbind(
          c(-65, -32),
          c(-63, -32),
          c(-63, -30),
          c(-65, -30),
          c(-65, -32)
        )
      )
    ),
    crs = 4326
  )
)

test_that("travel_list must be a data.frame", {
  expect_error(
    total_exposure(
      travel_list = "invalid",
      mode = c("car"),
      dir = tempdir(),
      key = "fake",
      selection = 1,
      output_exp = "df",
      departure_time_home = "2019-08-01 08:00:00",
      activity_minutes = data.frame(x = 60),
      pollutant = "PM2.5",
      shapeValue = "value",
      gridID = "ID",
      units = "ug/m3"
    ),
    "data.frame"
  )
})

test_that("travel_list must contain at least two locations", {
  travel_list <- data.frame(
    long = -64,
    lat = -31
  )

  expect_error(
    total_exposure(
      travel_list = travel_list,
      mode = c("car"),
      dir = tempdir(),
      key = "fake",
      selection = 1,
      output_exp = "df",
      departure_time_home = "2019-08-01 08:00:00",
      activity_minutes = data.frame(x = 60),
      pollutant = "PM2.5",
      shapeValue = "value",
      gridID = "ID",
      units = "ug/m3"
    ),
    "at least two locations"
  )
})

test_that("mode must have one element for each trip", {
  travel_list <- data.frame(
    long = c(-64, -65),
    lat = c(-31, -32)
  )

  expect_error(
    total_exposure(
      travel_list = travel_list,
      mode = c("car"),
      dir = tempdir(),
      key = "fake",
      selection = c(1, 1),
      output_exp = "df",
      departure_time_home = "2019-08-01 08:00:00",
      activity_minutes = data.frame(x = 60),
      pollutant = "PM2.5",
      shapeValue = "value",
      gridID = "ID",
      units = "ug/m3"
    ),
    "mode"
  )
})

test_that("output_exp must be valid", {
  travel_list <- data.frame(
    long = c(-64, -65),
    lat = c(-31, -32)
  )

  expect_error(
    total_exposure(
      travel_list = travel_list,
      mode = c("car", "car"),
      dir = tempdir(),
      key = "fake",
      selection = c(1, 1),
      output_exp = "table",
      departure_time_home = "2019-08-01 08:00:00",
      activity_minutes = data.frame(x = 60),
      pollutant = "PM2.5",
      shapeValue = "value",
      gridID = "ID",
      units = "ug/m3"
    ),
    "output_exp"
  )
})

test_that("directory must exist", {
  travel_list <- data.frame(
    long = c(-64, -65),
    lat = c(-31, -32)
  )

  expect_error(
    total_exposure(
      travel_list = travel_list,
      mode = c("car", "car"),
      dir = "fake_directory",
      key = "fake",
      selection = c(1, 1),
      output_exp = "df",
      departure_time_home = "2019-08-01 08:00:00",
      activity_minutes = data.frame(x = 60),
      pollutant = "PM2.5",
      shapeValue = "value",
      gridID = "ID",
      units = "ug/m3"
    ),
    "does not exist"
  )
})

test_that("returns a dataframe when output_exp is df", {
  testthat::local_mocked_bindings(
    alternative_trajectories = function(...) {
      fake_alternative_routes
    },

    temporary_grid_search = function(...) {
      fake_exposure_grid
    },

    .package = "AirExposure"
  )

  travel_list <- data.frame(
    long = c(-64.18, -64.20),
    lat = c(-31.42, -31.43)
  )

  expect_no_error(
    total_exposure(
      travel_list = travel_list,
      mode = c("car", "car"),
      dir = tempdir(),
      key = "fake",
      selection = c("fast", "fast"),
      output_exp = "df",
      departure_time_home = "2019-08-01 08:00:00",
      activity_minutes = data.frame(x = 60),
      pollutant = "PM2.5",
      shapeValue = "value",
      gridID = "ID",
      units = "ug/m3"
    )
  )
})

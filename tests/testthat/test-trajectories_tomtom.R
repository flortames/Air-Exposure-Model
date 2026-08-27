test_that("origin and destination must be provided", {
  expect_error(
    trajectories_tomtom(
      mode = "car",
      hour_trajectory = "2019-08-01 08:00:00",
      key = "fake"
    ),
    "must be provided"
  )
})

test_that("origin must be a character string", {
  expect_error(
    trajectories_tomtom(
      origin = c(-31, -64),
      dest = "-31.5,-64.5",
      mode = "car",
      hour_trajectory = "2019-08-01 08:00:00",
      key = "fake"
    ),
    "origin.*character string"
  )
})

test_that("destination must be a character string", {
  expect_error(
    trajectories_tomtom(
      origin = "-31,-64",
      dest = c(-31.5, -64.5),
      mode = "car",
      hour_trajectory = "2019-08-01 08:00:00",
      key = "fake"
    ),
    "dest.*character string"
  )
})

test_that("coordinates must have the expected format", {
  expect_error(
    trajectories_tomtom(
      origin = "wrong_coordinate",
      dest = "-31.5,-64.5",
      mode = "car",
      hour_trajectory = "2019-08-01 08:00:00",
      key = "fake"
    ),
    "origin.*latitude,longitude"
  )
  
  expect_error(
    trajectories_tomtom(
      origin = "-31,-64",
      dest = "wrong_coordinate",
      mode = "car",
      hour_trajectory = "2019-08-01 08:00:00",
      key = "fake"
    ),
    "dest.*latitude,longitude"
  )
})

test_that("a valid API key must be provided", {
  expect_error(
    trajectories_tomtom(
      origin = "-31,-64",
      dest = "-31.5,-64.5",
      mode = "car",
      hour_trajectory = "2019-08-01 08:00:00",
      key = ""
    ),
    "API key"
  )
})

test_that("transport mode must be supported", {
  expect_error(
    trajectories_tomtom(
      origin = "-31,-64",
      dest = "-31.5,-64.5",
      mode = "plane",
      hour_trajectory = "2019-08-01 08:00:00",
      key = "fake"
    ),
    "mode.*must be one of"
  )
})

test_that("hour_trajectory must be a character string", {
  expect_error(
    trajectories_tomtom(
      origin = "-31,-64",
      dest = "-31.5,-64.5",
      mode = "car",
      hour_trajectory = 123,
      key = "fake"
    ),
    "hour_trajectory.*character string"
  )
})

test_that("hour_trajectory must have the expected format", {
  expect_error(
    trajectories_tomtom(
      origin = "-31,-64",
      dest = "-31.5,-64.5",
      mode = "car",
      hour_trajectory = "not-a-date",
      key = "fake"
    ),
    "could not be parsed"
  )
})

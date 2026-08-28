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

test_that("processes a mocked TomTom response", {
  webmockr::enable()
  on.exit(webmockr::disable(), add = TRUE)
  on.exit(webmockr::webmockr_reset(), add = TRUE)
  
  fixture_path <- testthat::test_path(
    "fixtures",
    "tomtom-route.json"
  )
  
  response_body <- paste(
    readLines(fixture_path, warn = FALSE),
    collapse = "\n"
  )
  
  stub <- webmockr::stub_request(
    method = "get",
    uri_regex = "api\\.tomtom\\.com/routing/1/calculateRoute/"
  )
  
  webmockr::to_return(
    stub,
    status = 200,
    body = response_body,
    headers = list(
      "Content-Type" = "application/json"
    )
  )
  
  result <- trajectories_tomtom(
    origin = "-31.4201,-64.1888",
    dest = "-31.4300,-64.2000",
    mode = "car",
    hour_trajectory = "2019-08-01 08:00:00",
    key = "fake-key"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  expect_true(
    all(
      c(
        "ID",
        "long",
        "lat",
        "departureTime",
        "arrivalTime",
        "lengthInKM",
        "travelTimeInMinutes",
        "alternative"
      ) %in% names(result)
    )
  )
})
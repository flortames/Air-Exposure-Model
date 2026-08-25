library(testthat)
library(sf)

example_grid <- sf::st_as_sf(
  data.frame(
    value = c(5, 20, 50, 100, 200, 300),
    lon = 1:6,
    lat = 1:6
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

test_that("map_colors adds category and color columns for PM2.5", {
  result <- map_colors(example_grid, "PM2.5")

  expect_true("category" %in% names(result))
  expect_true("color" %in% names(result))
})

test_that("PM2.5 categories are assigned correctly", {
  result <- map_colors(example_grid, "PM2.5")

  expect_equal(
    result$category,
    c(
      "Good",
      "Moderate",
      "Unhealthy for sensible groups",
      "Unhealthy",
      "Very unhealthy",
      "Hazardous"
    )
  )
})

test_that("PM2.5 colors are assigned correctly", {
  grid <- sf::st_as_sf(
    data.frame(
      value = c(5, 300),
      lon = c(1, 2),
      lat = c(1, 2)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  result <- map_colors(grid, "PM2.5")

  expect_equal(result$color[1], "#abdda4")
  expect_equal(result$color[2], "#590e63")
})

test_that("PM10 is accepted", {
  grid <- sf::st_as_sf(
    data.frame(
      value = 10,
      lon = 1,
      lat = 1
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  result <- map_colors(grid, "PM10")

  expect_equal(result$category, "Good")
})

test_that("O3 values below AQI breakpoint produce a warning", {
  grid <- sf::st_as_sf(
    data.frame(
      value = 0.10,
      lon = 1,
      lat = 1
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  expect_warning(
    map_colors(grid, "O3"),
    "assigned NA"
  )
})

test_that("invalid pollutants throw an error", {
  expect_error(
    map_colors(example_grid, "ABC"),
    "must be one of"
  )
})

test_that("grid must be an sf object", {
  expect_error(
    map_colors(
      data.frame(value = 10),
      "PM2.5"
    ),
    "sf object"
  )
})

test_that("grid must contain value column", {
  grid <- sf::st_as_sf(
    data.frame(
      x = 1,
      lon = 1,
      lat = 1
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  expect_error(
    map_colors(grid, "PM2.5"),
    "column named 'value'"
  )
})

test_that("pollutant must be character", {
  expect_error(
    map_colors(example_grid, 123),
    "character"
  )
})

test_that("pollutant must be length one", {
  expect_error(
    map_colors(
      example_grid,
      c("PM2.5", "PM10")
    ),
    "single character value"
  )
})

test_that("pollutant names are case sensitive", {
  expect_error(
    map_colors(example_grid, "o3"),
    "must be one of"
  )
})

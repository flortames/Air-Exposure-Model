test_that("fails when directory does not exist", {
  expect_error(
    temporary_grid_search(
      start_hour = "2019-08-01 08:00:00",
      dir = "fake_directory",
      time_format = "%Y-%m-%d %H:%M:%S",
      gridID = "ID",
      shapeValue = "value"
    ),
    "does not exist"
  )
})

test_that("fails when start_hour cannot be parsed", {
  expect_error(
    temporary_grid_search(
      start_hour = "not_a_date",
      dir = tempdir(),
      time_format = "%Y-%m-%d %H:%M:%S",
      gridID = "ID",
      shapeValue = "value"
    ),
    "start_hour"
  )
})

test_that("fails when end_hour cannot be parsed", {
  expect_error(
    temporary_grid_search(
      start_hour = "2019-08-01 08:00:00",
      end_hour = "not_a_date",
      dir = tempdir(),
      time_format = "%Y-%m-%d %H:%M:%S",
      gridID = "ID",
      shapeValue = "value"
    ),
    "end_hour"
  )
})

test_that("fails when end_hour is earlier than start_hour", {
  expect_error(
    temporary_grid_search(
      start_hour = "2019-08-01 10:00:00",
      end_hour = "2019-08-01 08:00:00",
      dir = tempdir(),
      time_format = "%Y-%m-%d %H:%M:%S",
      gridID = "ID",
      shapeValue = "value"
    ),
    "earlier than"
  )
})

test_that("gridID must be a single character value", {
  expect_error(
    temporary_grid_search(
      start_hour = "2019-08-01 08:00:00",
      dir = tempdir(),
      time_format = "%Y-%m-%d %H:%M:%S",
      gridID = 123,
      shapeValue = "value"
    ),
    "gridID"
  )
})

test_that("shapeValue must be a single character value", {
  expect_error(
    temporary_grid_search(
      start_hour = "2019-08-01 08:00:00",
      dir = tempdir(),
      time_format = "%Y-%m-%d %H:%M:%S",
      gridID = "ID",
      shapeValue = 123
    ),
    "shapeValue"
  )
})

test_that("returns grid for a single hour", {
  temp_folder <- tempfile()
  dir.create(temp_folder)

  grid <- sf::st_as_sf(
    data.frame(
      ID = 1,
      value = 100,
      lon = -64,
      lat = -31
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  sf::st_write(
    grid,
    file.path(
      temp_folder,
      "2019-08-01_0800.shp"
    ),
    delete_layer = TRUE,
    quiet = TRUE
  )

  result <- temporary_grid_search(
    start_hour = "2019-08-01 08:00:00",
    dir = temp_folder,
    time_format = "%Y-%m-%d %H:%M:%S",
    gridID = "ID",
    shapeValue = "value"
  )

  expect_s3_class(result, "sf")
  expect_equal(result$ID, 1)
  expect_equal(result$value, 100)
})

test_that("averages pollutant values across multiple hours", {
  temp_folder <- tempfile()
  dir.create(temp_folder)

  grid1 <- sf::st_as_sf(
    data.frame(
      ID = 1,
      value = 10,
      lon = -64,
      lat = -31
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  grid2 <- sf::st_as_sf(
    data.frame(
      ID = 1,
      value = 20,
      lon = -64,
      lat = -31
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  sf::st_write(
    grid1,
    file.path(
      temp_folder,
      "2019-08-01_0800.shp"
    ),
    delete_layer = TRUE,
    quiet = TRUE
  )

  sf::st_write(
    grid2,
    file.path(
      temp_folder,
      "2019-08-01_0900.shp"
    ),
    delete_layer = TRUE,
    quiet = TRUE
  )

  result <- temporary_grid_search(
    start_hour = "2019-08-01 08:00:00",
    end_hour = "2019-08-01 09:00:00",
    dir = temp_folder,
    time_format = "%Y-%m-%d %H:%M:%S",
    gridID = "ID",
    shapeValue = "value"
  )

  expect_s3_class(result, "sf")
  expect_equal(result$ID, 1)
  expect_equal(result$value, 15)
})

test_that("fails when directory does not exist", {
  expect_error(
    hourly_grid(
      hour = "2019-08-01 08:00:00",
      time_format = "%Y-%m-%d %H:%M:%S",
      dir = "fake_directory"
    ),
    "does not exist"
  )
})

test_that("fails when hour cannot be parsed", {
  expect_error(
    hourly_grid(
      hour = "invalid_date",
      time_format = "%Y-%m-%d %H:%M:%S",
      dir = tempdir()
    ),
    "could not be parsed"
  )
})

test_that("fails when no shapefiles are found", {
  temp_folder <- tempfile()
  dir.create(temp_folder)

  expect_error(
    hourly_grid(
      hour = "2019-08-01 08:00:00",
      time_format = "%Y-%m-%d %H:%M:%S",
      dir = temp_folder
    ),
    "No shapefiles"
  )
})

test_that("returns matching shapefile name", {
  temp_folder <- tempfile()
  dir.create(temp_folder)

  file.create(
    file.path(
      temp_folder,
      "2019-08-01_0800.shp"
    )
  )

  expect_equal(
    hourly_grid(
      hour = "2019-08-01 08:00:00",
      time_format = "%Y-%m-%d %H:%M:%S",
      dir = temp_folder
    ),
    "2019-08-01_0800.shp"
  )
})

test_that("hour must be a character string", {
  expect_error(
    hourly_grid(
      hour = 123,
      time_format = "%Y-%m-%d %H:%M:%S",
      dir = tempdir()
    ),
    "character string"
  )
})

test_that("fails when no shapefile matches requested hour", {
  temp_folder <- tempfile()
  dir.create(temp_folder)

  file.create(
    file.path(
      temp_folder,
      "2019-08-01_0900.shp"
    )
  )

  expect_error(
    hourly_grid(
      hour = "2019-08-01 08:00:00",
      time_format = "%Y-%m-%d %H:%M:%S",
      dir = temp_folder
    ),
    "No shapefile was found"
  )
})

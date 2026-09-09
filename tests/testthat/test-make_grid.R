test_that("pixelSize must be positive", {
  expect_error(
    make_grid(
      ymin = -31.6,
      ymax = -31.3,
      xmin = -64.3,
      xmax = -64.0,
      pixelSize = 0,
      dir = tempdir(),
      date = "2019-08-01"
    ),
    "pixelSize"
  )
})

test_that("xmin must be smaller than xmax", {
  expect_error(
    make_grid(
      ymin = -31.6,
      ymax = -31.3,
      xmin = -64.0,
      xmax = -64.3,
      pixelSize = 1000,
      dir = tempdir(),
      date = "2019-08-01"
    ),
    "xmin"
  )
})

test_that("ymin must be smaller than ymax", {
  expect_error(
    make_grid(
      ymin = -31.3,
      ymax = -31.6,
      xmin = -64.3,
      xmax = -64.0,
      pixelSize = 1000,
      dir = tempdir(),
      date = "2019-08-01"
    ),
    "ymin"
  )
})

test_that("creates 24 hourly shapefiles", {
  out_dir <- tempfile()
  dir.create(out_dir)

  make_grid(
    ymin = -31.6,
    ymax = -31.3,
    xmin = -64.3,
    xmax = -64.0,
    pixelSize = 1000,
    dir = out_dir,
    date = "2019-08-01"
  )

  shp_files <- list.files(
    out_dir,
    pattern = "\\.shp$"
  )

  expect_equal(length(shp_files), 24)
})

test_that("generated grids contain ID and value fields", {
  out_dir <- tempfile()
  dir.create(out_dir)

  make_grid(
    ymin = -31.6,
    ymax = -31.3,
    xmin = -64.3,
    xmax = -64.0,
    pixelSize = 1000,
    dir = out_dir,
    date = "2019-08-01"
  )

  grid <- sf::st_read(
    file.path(
      out_dir,
      "2019-08-01_0800.shp"
    ),
    quiet = TRUE
  )

  expect_true("ID" %in% names(grid))
  expect_true("value" %in% names(grid))
})

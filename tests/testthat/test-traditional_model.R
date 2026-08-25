test_that("fails when directory does not exist", {
  expect_error(
    traditional_model(
      origin_point = data.frame(
        longitude = -64,
        latitude = -31
      ),
      date = "2019-08-01",
      dir = "fake_directory",
      gridID = "ID",
      shapeValue = "value"
    ),
    "does not exist"
  )
})

test_that("origin_point must contain longitude and latitude", {
  expect_error(
    traditional_model(
      origin_point = data.frame(x = -64, y = -31),
      date = "2019-08-01",
      dir = tempdir(),
      gridID = "ID",
      shapeValue = "value"
    ),
    "longitude"
  )
})

test_that("date must have format YYYY-mm-dd", {
  expect_error(
    traditional_model(
      origin_point = data.frame(
        longitude = -64,
        latitude = -31
      ),
      date = "not_a_date",
      dir = tempdir(),
      gridID = "ID",
      shapeValue = "value"
    ),
    "YYYY-mm-dd"
  )
})

test_that("origin_point must contain exactly one location", {
  expect_error(
    traditional_model(
      origin_point = data.frame(
        longitude = c(-64, -65),
        latitude = c(-31, -32)
      ),
      date = "2019-08-01",
      dir = tempdir(),
      gridID = "ID",
      shapeValue = "value"
    ),
    "exactly one location"
  )
})

test_that("calculates daily exposure correctly", {
  temp_folder <- tempfile()
  dir.create(temp_folder)

  for (i in 0:23) {
    poly <- sf::st_polygon(list(rbind(
      c(-65, -32),
      c(-63, -32),
      c(-63, -30),
      c(-65, -30),
      c(-65, -32)
    )))

    grid <- sf::st_sf(
      ID = 1,
      value = 10,
      geometry = sf::st_sfc(poly, crs = 4326)
    )

    if (i == 0) {
      file_name <- "2019-08-01_0001.shp"
    } else if (i < 10) {
      file_name <- paste0(
        "2019-08-01_0",
        i,
        "00.shp"
      )
    } else {
      file_name <- paste0(
        "2019-08-01_",
        i,
        "00.shp"
      )
    }

    sf::st_write(
      grid,
      file.path(
        temp_folder,
        file_name
      ),
      delete_layer = TRUE,
      quiet = TRUE
    )
  }

  origin <- data.frame(
    longitude = -64,
    latitude = -31
  )

  result <- traditional_model(
    origin_point = origin,
    date = "2019-08-01",
    dir = temp_folder,
    gridID = "ID",
    shapeValue = "value"
  )

  expect_equal(result, 240)
})

test_that("fails when origin point is outside the grid", {
  temp_folder <- tempfile()
  dir.create(temp_folder)

  poly <- sf::st_polygon(list(rbind(
    c(-65, -32),
    c(-63, -32),
    c(-63, -30),
    c(-65, -30),
    c(-65, -32)
  )))

  grid <- sf::st_sf(
    ID = 1,
    value = 10,
    geometry = sf::st_sfc(poly, crs = 4326)
  )

  for (i in 0:23) {
    if (i == 0) {
      file_name <- "2019-08-01_0001.shp"
    } else if (i < 10) {
      file_name <- paste0("2019-08-01_0", i, "00.shp")
    } else {
      file_name <- paste0("2019-08-01_", i, "00.shp")
    }

    sf::st_write(
      grid,
      file.path(temp_folder, file_name),
      delete_layer = TRUE,
      quiet = TRUE
    )
  }

  origin <- data.frame(
    longitude = -80,
    latitude = -50
  )

  expect_error(
    traditional_model(
      origin_point = origin,
      date = "2019-08-01",
      dir = temp_folder,
      gridID = "ID",
      shapeValue = "value"
    ),
    "does not intersect any grid cell"
  )
})

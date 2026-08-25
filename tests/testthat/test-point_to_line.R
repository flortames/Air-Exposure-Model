test_that("creates a single LINESTRING", {
  df <- data.frame(
    lon = c(-64.1, -64.2),
    lat = c(-31.4, -31.5)
  )

  result <- points_to_line(
    data = df,
    long = "lon",
    lat = "lat"
  )

  expect_s3_class(result, "sfc_LINESTRING")
  expect_equal(sf::st_crs(result)$epsg, 4326)
})


test_that("creates multiple LINESTRING objects", {
  df <- data.frame(
    lon = c(-64.1, -64.2, -64.3, -64.4),
    lat = c(-31.4, -31.5, -31.6, -31.7),
    route = c("A", "A", "B", "B")
  )

  result <- points_to_line(
    data = df,
    long = "lon",
    lat = "lat",
    id_field = "route"
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
})


test_that("sort_field orders the points correctly", {
  df <- data.frame(
    lon = c(2, 1),
    lat = c(2, 1),
    order = c(2, 1)
  )

  expect_no_error(
    points_to_line(
      data = df,
      long = "lon",
      lat = "lat",
      sort_field = "order"
    )
  )
})


test_that("fails when longitude column does not exist", {
  df <- data.frame(
    lat = c(1, 2)
  )

  expect_error(
    points_to_line(
      data = df,
      long = "lon",
      lat = "lat"
    ),
    "long"
  )
})


test_that("fails when latitude column does not exist", {
  df <- data.frame(
    lon = c(1, 2)
  )

  expect_error(
    points_to_line(
      data = df,
      long = "lon",
      lat = "lat"
    ),
    "lat"
  )
})


test_that("fails when id_field does not exist", {
  df <- data.frame(
    lon = c(1, 2),
    lat = c(1, 2)
  )

  expect_error(
    points_to_line(
      data = df,
      long = "lon",
      lat = "lat",
      id_field = "route"
    ),
    "id_field"
  )
})


test_that("fails when sort_field does not exist", {
  df <- data.frame(
    lon = c(1, 2),
    lat = c(1, 2)
  )

  expect_error(
    points_to_line(
      data = df,
      long = "lon",
      lat = "lat",
      sort_field = "order"
    ),
    "sort_field"
  )
})


test_that("fails with fewer than two points", {
  df <- data.frame(
    lon = 1,
    lat = 2
  )

  expect_error(
    points_to_line(
      data = df,
      long = "lon",
      lat = "lat"
    ),
    "At least two points"
  )
})

test_that("each group must contain at least two points", {
  df <- data.frame(
    lon = c(1, 2, 3),
    lat = c(1, 2, 3),
    route = c("A", "B", "B")
  )

  expect_error(
    points_to_line(
      data = df,
      long = "lon",
      lat = "lat",
      id_field = "route"
    ),
    "Each group specified in 'id_field' must contain at least two points"
  )
})

test_that("Converts whole hours correctly", {
  expect_equal(function_hours(60), "01:00")
  expect_equal(function_hours(120), "02:00")
})

test_that("Converts hours and minutes correctly", {
  expect_equal(function_hours(90), "01:30")
  expect_equal(function_hours(61), "01:01")
  expect_equal(function_hours(125), "02:05")
})

test_that("Rounds decimal minutes correctly", {
  expect_equal(function_hours(89.4), "01:29")
  expect_equal(function_hours(89.5), "01:30")
  expect_equal(function_hours(89.6), "01:30")
})

test_that("Handles zero minutes", {
  expect_equal(function_hours(0), "00:00")
})

test_that("Returns a character string", {
  expect_type(function_hours(30), "character")
})

test_that("Maintains leading zeros", {
  expect_equal(function_hours(5), "00:05")
  expect_equal(function_hours(9), "00:09")
})

test_that("minutes must be numeric", {
  expect_error(
    function_hours("hola"),
    "must be numeric"
  )
})

test_that("minutes must have length one", {
  expect_error(
    function_hours(c(60, 120)),
    "single numeric value"
  )
})

test_that("minutes cannot be NA", {
  expect_error(
    function_hours(NA),
    "cannot be NA"
  )
})

test_that("minutes must be non-negative", {
  expect_error(
    function_hours(-10),
    "non-negative"
  )
})

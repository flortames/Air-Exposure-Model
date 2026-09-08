

# Test function: alternative_trajectories
# Validate that origin and destination coordinates follow the expected format

# Add to function
#if (!is.character(origin) || length(origin) != 1) {stop("origin must be a character string")}
#if (!grepl("^-?[0-9.]+,-?[0-9.]+$", origin)) { stop("origin must have format 'latitude,longitude'")}
#if (!is.character(dest) || length(dest) != 1) {stop("dest must be a character string")}
#if (!grepl("^-?[0-9.]+,-?[0-9.]+$", dest)) {stop("dest must have format 'latitude,longitude'")}

test_that("origin and destination have valid coordinate format", {
  expect_error(
    alternative_trajectories(
      origin = "wrong_coordinate", dest = "-32.9,-68.8", mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") ) 

  expect_error(
    alternative_trajectories(
      origin = "-32.9,-68.8" , dest = "wrong_coordinate", mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") )

  expect_error(
    alternative_trajectories(
      origin = "-32.79679" , dest = "-32.90212,-68.761" , mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") )

  expect_error(
    alternative_trajectories(
      origin = c(-32.79679,-68.816), dest = "-32.90212,-68.761" , mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") )
  })
# ------------------------------------------------------------------------------

# Input validation: supported transportation modes
# Add to function
#allowed_modes <- c("car","truck","pedestrian","bicycle","motorcycle")
#if (!mode %in% allowed_modes){stop("mode must be one of: car, truck, pedestrian, bicycle, motorcycle")}

test_that("transport mode must be supported", {
  expect_error(alternative_trajectories(
      origin="-32.79,-68.81", dest="-32.90,-68.76", mode="plane", dir="test",
      key="fake", output="df", gridID="ID", shapeValue="value", units="ug/m3",
      pollutant="PM2.5"))})



# ------------------------------------------------------------------------------
#Input validation: output format selection

# Add function
# if(!output %in% c("df","plot")){stop("output must be 'df' or 'plot'")}

test_that("output argument must be either df or plot", {
  expect_error( alternative_trajectories(
      origin="-32.79,-68.81", dest="-32.90,-68.76", mode="car", dir="test",
      key="fake", output="table", gridID="ID", shapeValue="value", units="ug/m3",
      pollutant="PM2.5"))})



# ------------------------------------------------------------------------------
# Validate returned object structure
test_that("alternative trajectories returns expected dataframe", {
  skip_on_cran()
  skip_on_ci()
  skip_if(Sys.getenv("TOMTOM_KEY") == "","TomTom API key not available")
  
  result <- alternative_trajectories(
    origin="-32.895559,-68.862029",
    dest="-32.887536,-68.834832",
    mode="car",
    dir=system.file("extdata", package="Air-Exposure-Model"),
    key=Sys.getenv("TOMTOM_KEY"),
    output="df",
    gridID="GRI1_ID",
    shapeValue="value",
    units="ug/m3",
    pollutant="PM2.5")
  expect_s3_class(result,"data.frame")})

# ------------------------------------------------------------------------------
# Validate required output variables

test_that("output contains required columns", {
  skip_if_not(exists("result"))
  expect_true(all(c("alternative", "daily_pol_value_mean", "exposure_value_mean",
        "type")%in% names(result)))})



# ------------------------------------------------------------------------------
# Check non-negative pollutant concentration and exposure estimates
test_that("pollutant concentration values are non-negative", {
  expect_true(all(result$daily_pol_value_mean >= 0))})


test_that("exposure values are non-negative", {
  expect_true(all(result$exposure_value_mean >= 0))})



# ------------------------------------------------------------------------------
#Validate selected route categories
test_that("six alternative route categories are returned", {
  expect_equal(sort(unique(result$type)), sort(
      c("fast", "short", "morepol", "lesspol","moreexpos","lessexpos")))})




# Test function: alternative_trajectories
# Validate that origin and destination coordinates follow the expected format

# Add to function
#if (!is.character(origin) || length(origin) != 1) {stop("origin must be a character string")}
#if (!grepl("^-?[0-9.]+,-?[0-9.]+$", origin)) { stop("origin must have format 'latitude,longitude'")}
#if (!is.character(dest) || length(dest) != 1) {stop("dest must be a character string")}
#if (!grepl("^-?[0-9.]+,-?[0-9.]+$", dest)) {stop("dest must have format 'latitude,longitude'")}

test_that("origin and destination have valid coordinate format", {
  expect_error(
    alternative_trajectories(
      origin = "wrong_coordinate", dest = "-32.9,-68.8", mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") ) 

  expect_error(
    alternative_trajectories(
      origin = "-32.9,-68.8" , dest = "wrong_coordinate", mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") ) })

  expect_error(
    alternative_trajectories(
      origin = "-32.79679" , dest = "-32.90212,-68.761" , mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") ) })

  expect_error(
    alternative_trajectories(
      origin = c(-32.79679,-68.816), dest = "-32.90212,-68.761" , mode = "car", dir = "test",
      key = "fake", output = "df", gridID = "ID", shapeValue = "value", units = "ug/m3",
      pollutant = "PM2.5") )
  })
# ------------------------------------------------------------------------------

# Input validation: supported transportation modes
# Add to function
#allowed_modes <- c("car","truck","pedestrian","bicycle","motorcycle")
#if (!mode %in% allowed_modes){stop("mode must be one of: car, truck, pedestrian, bicycle, motorcycle")}

test_that("transport mode must be supported", {
  expect_error(alternative_trajectories(
      origin="-32.79,-68.81", dest="-32.90,-68.76", mode="plane", dir="test",
      key="fake", output="df", gridID="ID", shapeValue="value", units="ug/m3",
      pollutant="PM2.5"))})



# ------------------------------------------------------------------------------
#Input validation: output format selection

# Add function
# if(!output %in% c("df","plot")){stop("output must be 'df' or 'plot'")}

test_that("output argument must be either df or plot", {
  expect_error( alternative_trajectories(
      origin="-32.79,-68.81", dest="-32.90,-68.76", mode="car", dir="test",
      key="fake", output="table", gridID="ID", shapeValue="value", units="ug/m3",
      pollutant="PM2.5"))})



# ------------------------------------------------------------------------------
# Validate returned object structure
test_that("alternative trajectories returns expected dataframe", {
  skip_on_cran()
  skip_on_ci()
  skip_if(Sys.getenv("TOMTOM_KEY") == "","TomTom API key not available")
  
  result <- alternative_trajectories(
    origin="-32.895559,-68.862029",
    dest="-32.887536,-68.834832",
    mode="car",
    dir=system.file("extdata", package="Air-Exposure-Model"),
    key=Sys.getenv("TOMTOM_KEY"),
    output="df",
    gridID="GRI1_ID",
    shapeValue="value",
    units="ug/m3",
    pollutant="PM2.5")
  expect_s3_class(result,"data.frame")})

# ------------------------------------------------------------------------------
# Validate required output variables

test_that("output contains required columns", {
  skip_if_not(exists("result"))
  expect_true(all(c("alternative", "daily_pol_value_mean", "exposure_value_mean",
        "type")%in% names(result)))})



# ------------------------------------------------------------------------------
# Check non-negative pollutant concentration and exposure estimates
test_that("pollutant concentration values are non-negative", {
  expect_true(all(result$daily_pol_value_mean >= 0))})


test_that("exposure values are non-negative", {
  expect_true(all(result$exposure_value_mean >= 0))})



# ------------------------------------------------------------------------------
#Validate selected route categories
test_that("six alternative route categories are returned", {
  expect_equal(sort(unique(result$type)), sort(
      c("fast", "short", "morepol", "lesspol","moreexpos","lessexpos")))})




# ------------------------------------------------------------------------------
# Validate input directory containing pollutant grids
# Validate required pollutant grid fields
# Validate pollutant name and measurement units inputs
# Validate API key availability and format
# Validate temporal compatibility between trajectory and pollutant grids
# Validate that pollutant exposure is correctly calculated

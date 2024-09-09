# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- HOURLY_GRID FUNCTION EXAMPLE ----------------------------- 
# Variable
# Latitude, Longitude of the origin and destination points.
# 01. Date-time that will be searched in the local path to find the grid.
input_hour <- "2019-08-01 15:50:00"
hour_input<- "2019-08-01 15:50:00"
# 02. Format of the input_hour
input_format <- "%Y-%m-%d %H:%M:%S"
# 03. Local path where the shapefiles are located. Example dataset.
#dir <-system.file("data", package = "AirExposure") 
dir <- system.file("extdata", package = "AirExposure") 

#-- Run the function
test <-AirExposure::hourly_grid(hour_input = input_hour,  time_format = input_format, dir = dir)
test 
class(test)


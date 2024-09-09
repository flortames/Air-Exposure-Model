# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- TEMPORARY_GRID_SEARCH FUNCTION EXAMPLE ----------------------------- 
# 01. Local path. Example dataset
dir <-system.file("data", package = "AirExposure") 
dir <- system.file("extdata", package = "AirExposure")

setwd(dir)
# 02. Field name of the grid ID.
gridID <- "GRI1_ID"
# 03. Field name of the concentration value.
shapeValue <- "value"
# 04. Format to convert on POSIXct or POSIXt object
time_format = "%Y-%m-%d %H:%M:%S"

# -- Run example
# With an only start hour, return an only grid  
test_1<-AirExposure::temporary_grid_search(start_hour="2019-08-01 00:50:00 -03",end_hour=NULL ,dir=dir,time_format=time_format, gridID, shapeValue)  
#' With a different start and end time, return the mean of all the grids  
test_2<-AirExposure::temporary_grid_search(start_hour="2019-08-01 00:50:00",end_hour="2019-08-01 02:50:00",dir=dir, time_format=time_format, gridID, shapeValue)  
#' With the same start and end time, return an only grid  
test_3<-AirExposure::temporary_grid_search(start_hour="2019-08-01 00:10:00",end_hour="2019-08-01 00:50:00 -03" ,dir = dir, time_format=time_format, gridID, shapeValue)  
class(test_3) 



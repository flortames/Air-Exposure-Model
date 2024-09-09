
#######################################################################
# ------------        TOTAL EXPOSURE ESTIMATE    -------------  
# ------------        Traditional method    -------------  
# Estimation of exposure assuming that the person remains at the place of origin 24 hours a day
traditional_model <- function (origin_point,date,dir, gridID,	shapeValue ){
  #----    Spatial transformation
  coordinates(origin_point) <- ~longitude+latitude
  proj4string(origin_point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #----    Put the Coordinate reference system
  origin_point <- sf::st_as_sf(origin_point, crs = 4326)
  #----    Use the "temporary_grid_search" function to search and estimate the daily concentration.
  
  hour_00 <- paste(date,"T00:01:00",sep="")
  hour_23 <- paste(date,"T23:59:00",sep="")
  grid_search<-temporary_grid_search(start_hour = hour_00, end_hour = hour_23,dir,time_format="%Y-%m-%dT%H:%M:%S", gridID,	shapeValue)
  
  
  #----    Take the point where the person with the average concentration of PM lives
  intersection_point <- sf::st_intersection(origin_point ,grid_search)
  
  # Calculate the daily expossure -  μg m-3 - 24hs
  
  daily_exposure <- (intersection_point$value * 24)
  return(print(daily_exposure))
}



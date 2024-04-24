# ---- Function to generate grids
#The function generates 24 hourly grids based on the entered boundaries 
# (xmin, ymin, xmax, ymax) with a specified resolution. The pixel size
# should be in meters. Coordinates are in latitude-longitude. 
# The files will be saved in shapefile format in the selected directory 
# 'dir' and named according to the entered date and the specified hour 
# (00-23h). 
#The output values in each file correspond to random values 
# between 0-500 according to the EPA AQI.

make_grid <- function(ymin,ymax,xmin,xmax, pixelSize,Dir, date, values=NULL){  
  #Conversion of values entered in meters to degrees
  latitude <- (ymin + ymax )/2
  # The circumference of the Earth at the equator in meters
  circumference_earth_equator <- 40075000  # Aprox. 40,075 km
  # Conversion
  degree_longitud <- (360 / (circumference_earth_equator * cos(latitude * pi / 180))) * pixelSize
  
  # Grid Generation
  # Defining spatial boundaries.Creating a 'bbox' object
  bbox <- sf::st_bbox(c(xmin = xmin, ymin= ymin, xmax= xmax, ymax = ymax),crs = st_crs(4326))
  
  # Defining the grid resolution (in degrees)
  res <- degree_longitud
  
  # Creating 24 polygon grids
  grid <- sf::st_make_grid(bbox, cellsize = c(res, res), what = "polygons")
    
  ID <- c(1:length(grid))
  # grid <- cbind(c(1:length(grid)) , grid) 
  for (i in 0:23){
    print(i)
    random_values <- sample(0:500, length(grid), replace = TRUE)
    
    sfc_polygon_values<- sf::st_sf(ID,random_values, geometry = grid)
    names(sfc_polygon_values) <- c("ID","value","geometry")
    if(i==0){
      name <- paste(date,"_","0",i,"01.shp",sep="")
    }
    else if(i<10 & i!=0){
    name <- paste(date,"_","0",i,"00.shp",sep="")
    }
    else{
      name <- paste(date,"_",i,"00.shp",sep="")
    }
    # Shapefile export
    sf::st_write(sfc_polygon_values, paste(Dir,name,sep=""))
    }
    
  }


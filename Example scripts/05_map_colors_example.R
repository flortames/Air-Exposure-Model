# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- MAKE_GRID FUNCTION EXAMPLE ----------------------------- 

# Read one shapefile for the example
# Path
#dir <- system.file("data", package = "AirExposure") 
dir <- system.file("extdata", package = "AirExposure") 
file_list <- dir(dir,pattern = ".shp")
setwd(dir)
#Read the first grid.
grid <- sf::st_read(file_list[1])
pollutant <- "PM2.5"

names(grid)
# [1] "GRI1_ID"   "date"     "value"    "geometry"
# Run the function
test <- map_colors (grid,pollutant) 

#See the new variables
names(test)
# [1] "GRI1_ID"    "date"       "value"      "geometry"   "category"   "color"
unique(test$category)
# [1] "Good"   "Moderate" "Unhealthy for sensible groups" "Unhealthy" "Very Unhealthy" 
# " Hazardous"
 # Color according to the concentration values.
unique(test$color)
# [1] "#abdda4" "#f8fd66" "#fdde61" "#d74a4c" "#b687ba" "#590e63" 
            
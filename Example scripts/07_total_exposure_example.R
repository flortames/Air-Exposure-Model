# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- TOTAL EXPOSURE FUNCTION EXAMPLE ----------------------------- 
#library(dplyr)
library(sf)
library(htmltools)
library(leaflet)
library(lubridate)
library(httr)
library(jsonlite)

# 01. Origin-Destination points 
# One destination
travel_list <- data.frame(long = c(-68.86805,-68.833412),
                          lat = c(-32.940088,-32.92984))

travel_list <- data.frame(long = c(-68.83042773649689,-68.833412),
                          lat = c(-32.955952520947136,-32.92984))
travel_list <- data.frame(long = c(-68.83040353504914,-68.833412),
                          lat = c(-32.95792197293608,-32.92984))
# 02. Key tom-tom 
#key <- ## 
# 03. Transport mode - Always consider the round trip 
mode = c("car","car") 
mode = c("bicycle","pedestrian") 

# 04. Path with the hourly grids
#Example CALPUFF grids for 01-08-2019 for Metropolitan Area of Mendoza, Argentina. The grids must be by day-hour 
dir <- system.file("data", package = "AirExposure") 
dir <- system.file("extdata", package = "AirExposure") 
setwd(dir) 


concentrations_grid<- "D:/Josefina/paper_git/paper_exposure_model/grid_example"
dir <- concentrations_grid
setwd(concentrations_grid)
#dir <- "D:/Josefina/paper_git/paper_exposure_model/code_07/"
#setwd(dir)

# 05. Type of route selected 
selection <- c("fast","fast")
selection <- c("short","short")
selection <- c("lessexpos","lesspol") 
selection <- c("moreexpos", "moreexpos") 
## 07. Departure time from home for the first time. 
departure_time_home <- "2019-08-01 08:00:00" 
## 08. Duration of each activity. 
activity_minutes<-data.frame(activity_minutes = 300) 

## 09. Pollutant
pollutant <- "PM2.5"
## 10. Name ID grid
gridID <- "GRI1_ID"
## 11. Name value grid
shapeValue <- "value"
## 12. Pollutant unit
units <-  "Ug/m3 24-hour"
key <- "YOdvX5qKwpk9YRl9v0JzqC5qSYNOwbDc"###


# 13. Check and set the time zone
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")
## Run the function 
# output DF 
test_df <- total_exposure (travel_list, mode, dir,key,selection,output_exp="df",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
test_df2 <- test_df[,5:18]
write.csv(test_df,"example_df.csv") 
# output plot 
test_plot <- total_exposure (travel_list=travel_list, mode, dir,key,selection,output_exp="plot",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
htmlwidgets::saveWidget(test_plot , "D:/Josefina/paper_git/paper_exposure_model/code_08/prueba_salidas/example_plots_18-06-2024_CAR.html") 

# output polyline
test_polyline <- total_exposure (travel_list, mode, dir,key,selection,output_exp="polyline",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
plot(test_polyline$geometry)
st_write(test_polyline, "test_polyline .shp")
st_write(test_polyline, "D:/Josefina/paper_git/paper_exposure_model/code_08/prueba_salidas/file_test_poly_tot_exposure.shp")


#####
# ----- Example with two destination
# 01. Origin-Destination points 
travel_list <- data.frame(long = c(-68.86805,-68.833412,-68.84409972459794),
                          lat = c(-32.940088,-32.92984,-32.90106309945855)) 
# ----- Example with 3 destination
# 01. Origin-Destination points 
travel_list <- data.frame(long = c(-68.86805,-68.833412,-68.84409972459794, -68.85676403385452),
                          lat = c(-32.940088,-32.92984,-32.90106309945855,-32.9293315009014)) 

# 02. Key tom-tom 
key <- "YOdvX5qKwpk9YRl9v0JzqC5qSYNOwbDc"###
#03. Transport mode - Always consider the round trip 
mode <- c("car","car","car") 
mode <- c("car","car","car","car") 
# 04. Path with the hourly grids
#Example CALPUFF grids for 01-08-2019 for Metropolitan Area of Mendoza, Argentina. The grids must be by day-hour 
#dir <- system.file("data", package = "exposureModelP") 
concentrations_grid<- "D:/Josefina/paper_git/paper_exposure_model/grid_example"
dir <- concentrations_grid
setwd(concentrations_grid) 
# 05. Type of route selected 
selection <- c("fast","short","short")
selection <- c("fast","short","short","short")
## 07. Departure time from home for the first time. 
departure_time_home <- "2019-08-01 08:00:00" 
## 08. Duration of each activity. 
activity_minutes<-data.frame(activity_minutes = c(300,90)) 
activity_minutes<-data.frame(activity_minutes = c(300,90,60)) 

## 09. Pollutant
pollutant <- "PM2.5"
## 10. Name ID grid
gridID <- "GRI1_ID"
## 11. Name value grid
shapeValue <- "value"
## 12. Pollutant unit
units <-  "ug/m3 24-hour"


## Run the function 
# output DF 

# output plot 
test_plot_02 <- total_exposure (travel_list, mode, dir,key,selection,output_exp="plot",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
htmlwidgets::saveWidget(test_plot_02 , "D:/Josefina/paper_git/paper_exposure_model/code_08/prueba_salidas/example_plots_18-06-2024_CAR-FAST-SHORT-SHORT_3destins.html") 




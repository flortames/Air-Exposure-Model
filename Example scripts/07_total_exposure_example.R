# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- TOTAL EXPOSURE FUNCTION EXAMPLE ----------------------------- 
#
# 01. Origin-Destination points 
# One destination
travel_list <- data.frame(long = c(-68.86805,-68.833412),
                          lat = c(-32.940088,-32.92984))

# 02. Key tom-tom 
key <- ## 
# 03. Transport mode - Always consider the round trip 
mode = c("car","car") 
mode = c("bicycle","car") 

# 04. Path with the hourly grids
#Example CALPUFF grids for 01-08-2019 for Metropolitan Area of Mendoza, Argentina. The grids must be by day-hour 
dir <- system.file("data", package = "exposureModelP") 
setwd(dir) 
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
units <-  "μg/m3 24-hour"

# 13. Check and set the time zone
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")
## Run the function 
# output DF 
test_df <- total_exposure (travel_list, mode, dir,key,selection,output_exp="df",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)

write.csv(test_df,"example_df.csv") 
# output plot 
test_plot <- total_exposure (travel_list, mode, dir,key,selection,output_exp="plot",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
htmlwidgets::saveWidget(test_plot , "example_plots.html") 

# output polyline
test_polyline <- total_exposure (travel_list, mode, dir,key,selection,output_exp="polyline",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
plot(test_polyline$geometry)
st_write(test_polyline, "test_polyline .shp")

#####
# ----- Example with two destination
# 01. Origin-Destination points 
travel_list <- data.frame(long = c(-68.86805,-68.833412,-68.84409972459794),
                          lat = c(-32.940088,-32.92984,-32.90106309945855)) 
# 02. Key tom-tom 
key <- ## 
#03. Transport mode - Always consider the round trip 
mode <- c("pedestrian","bicycle","car") 

# 04. Path with the hourly grids
#Example CALPUFF grids for 01-08-2019 for Metropolitan Area of Mendoza, Argentina. The grids must be by day-hour 
dir <- system.file("data", package = "exposureModelP") 
setwd(dir) 
# 05. Type of route selected 
selection <- c("fast","short","lesspol")
## 07. Departure time from home for the first time. 
departure_time_home <- "2019-08-01 08:00:00" 
## 08. Duration of each activity. 
activity_minutes<-data.frame(activity_minutes = c(300,90)) 

## 09. Pollutant
pollutant <- "PM2.5"
## 10. Name ID grid
gridID <- "GRI1_ID"
## 11. Name value grid
shapeValue <- "value"
## 12. Pollutant unit
units <-  "μg/m3 24-hour"


## Run the function 
# output DF 
test_df_02 <- total_exposure (travel_list, mode, dir,key,selection,output_exp="df",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
write.csv(test_df,"example_df.csv") 
# output plot 
test_plot_02 <- total_exposure (travel_list, mode, dir,key,selection,output_exp="plot",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
htmlwidgets::saveWidget(test_plot_02 , "example_plot.html") 

# output polyline
test_polyline_02 <- total_exposure (travel_list, mode, dir,key,selection,output_exp="polyline",departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units)
plot(test_polyline$geometry)
st_write(test_polyline, "test_polyline .shp")


# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- ALTERNATIVE_TRAJECTORIES FUNCTION EXAMPLE ----------------------------- 
# Variable
# Latitude, Longitude of the origin and destination points.
# 01. Latitude and longitude of the origin. 
origin <-"-32.79679,-68.816" 
# 02. Latitude and longitude of the destination 
dest <- "-32.90212,-68.761" 

# 03. Transport
mode <- "car"
# 04. Date and hour of departure to destination
# 05. Check and set the time zone
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")
# 06. Local path where the shapefiles are located. Example dataset
dir <-system.file("data", package = "AirExposure") 
#To run the function you must be located in the local path where the shapefiles are.
setwd(dir) 
# 07. Departure date-time
hour_time <- "2019-08-01 07:50:00"
hour_time <- "2019-08-01 12:50:00"
hour_time <- "2019-08-01 16:50:00"
hour_time <- "2019-08-01 20:50:00"
# 08. Field name of the grid ID.
gridID <- "GRI1_ID"
# 09. Field name of the concentration value.
shapeValue <- "value"
# 10. Measurement units
units <- "ug/m3"
# 11. The pollutant name 
pollutant <- "PM2.5"
# 12. Tom-Tom password. 
key = #*****
  

# -- Run the function
# Dataframe test 
test_df_1650 <- alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                   key=key,output="df",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)

write.csv(test_df_1250, "test_df_1250_df.csv") 
# Plot test
test_plot_2050 <- alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                       key=key,output="plot",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)
htmlwidgets::saveWidget(test_plot_2050 , "test_plot_2050.html")

# Polyline test
test_poly<- alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                      key=key,output="polyline",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)
plot(test_poly$geometry)
st_write(test_poly, "file_test_poly.shp")


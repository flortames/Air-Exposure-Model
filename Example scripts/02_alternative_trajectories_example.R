# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- ALTERNATIVE_TRAJECTORIES FUNCTION EXAMPLE ----------------------------- 
# Variable
# Latitude, Longitude of the origin and destination points.
# 01. Latitude and longitude of the origin. 
origin <-"-32.79679,-68.816" 
origin <-"-32.895559140440355,-68.86202901786527"
# 02. Latitude and longitude of the destination 
dest <- "-32.90212,-68.761" 
dest <- "-32.887536148728756,-68.83483295230235"
# 03. Transport
mode <- "car"
# 04. Date and hour of departure to destination
# 05. Check and set the time zone
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")
# 06. Local path where the shapefiles are located. Example dataset
#dir <-system.file("data", package = "AirExposure") 
# dir <- system.file("extdata", package = "AirExposure") 
dir <- "D:/Josefina/paper_git/paper_exposure_model/grid_example"
#To run the function you must be located in the local path where the shapefiles are.
setwd(dir) 
# 07. Departure date-time
hour_time <- "2019-08-01 04:10:00"
hour_time <- "2019-08-01 06:10:00"
hour_time <- "2019-08-01 08:10:00"
hour_time <- "2019-08-01 10:10:00"
hour_time <- "2019-08-01 12:10:00"
hour_time <- "2019-08-01 14:10:00"
hour_time <- "2019-08-01 16:10:00"
hour_time <- "2019-08-01 18:10:00"
hour_time <- "2019-08-01 20:10:00"
hour_time <- "2019-08-01 22:10:00"
hour_time <- "2019-08-01 00:10:00"
# 08. Field name of the grid ID.
gridID <- "GRI1_ID"
# 09. Field name of the concentration value.
shapeValue <- "value"
# 10. Measurement units
units <- "ug/m3"
# 11. The pollutant name 
pollutant <- "PM2.5"
# 12. Tom-Tom password. 
key <- "YOdvX5qKwpk9YRl9v0JzqC5qSYNOwbDc"###
  

# -- Run the function
# Dataframe test 
test_df <- AirExposure::alternative_trajectories (origin=origin,dest=dest,mode=mode,
                                                       key=key,dir=dir,hour=hour_time, 
                                                       gridID=gridID, 
                                   shapeValue=shapeValue, units=units, pollutant=pollutant,output="df")
View(test_df)
write.csv(test_df_1250, "test_df_1250_df.csv") 
p_2 <- test_df[test_df$alternative=="alternative_6",]
test_df_Df <- data.frame(alternative = test_df$alternative,
                         id = test_df$ID,
                         lon = test_df$long,
                         lat =test_df$lat,
                         
                         departureTime = test_df$departureTime,
                         arrivalTime = test_df$arrivalTime,
                         len = test_df$lengthInKM,
                         travelMode = test_df$travelMode,
                         time = test_df$travelTimeInMinutes,
                         daily_pol_value_mean= test_df$daily_pol_value_mean,
                         exposure_value_mean=test_df$exposure_value_mean,
                         tipe=test_df$type)
"fast"      "short"     "morepol"   "lesspol"   "moreexpos" "lessexpos"
test_df_Df2 = test_df_Df[test_df_Df$tipe=="lessexpos",]
test_df_Df2$daily_pol_value_mean[1]
test_df_Df2$exposure_value_mean[1]


# Plot test
test_plot <- AirExposure::alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                       key=key,output="plot",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)
htmlwidgets::saveWidget(test_plot , "D:/Josefina/paper_git/paper_exposure_model/code_08/prueba_salidas/test_plot_17-06-2024_car-0000hs.html")

# Polyline test
test_poly<- alternative_trajectories (origin=origin,dest=dest,mode=mode,dir=dir,
                                      key=key,output="polyline",hour=hour_time,gridID=gridID, shapeValue=shapeValue, units=units, pollutant=pollutant)
plot(test_poly)
st_write(test_poly, "D:/Josefina/paper_git/paper_exposure_model/code_08/prueba_salidas/test_plot_17-06-2024.shp")


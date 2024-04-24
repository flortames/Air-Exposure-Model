# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- TRAJECTORIES_TOMTOM FUNCTION EXAMPLE ----------------------------- 
#
# 01. Latitude and longitude of the origin. 
origin <-"-32.79679,-68.816" 
# 02. Latitude and longitude of the destination 
dest <- "-32.90212,-68.761" 
# 03. Transport
mode <- "car"
# 04. Date and hour of departure to destination
hour_trajectory <- "2019-08-01 15:50:00 -03"
# 05. Check and set the time zone
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")

# 06. TomTom key
#key <- xxxx

# Run example
test <- trajectories_tomtom(origin, dest, mode, hour_trajectory, key)
head(test,2)

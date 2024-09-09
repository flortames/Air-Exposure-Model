#Function that allows coloring grids according to the EPA AQI (Air Quality Index)
# This function is used in the 'tom-tom' function and in the 'total exposure' function.
# It requires inputting a grid with pollutant information as well as the name of the pollutant. 
# As output, it provides the same input grid with a new column containing AQI information


map_colors <- function(grid,pollutant){ 
  #PM2.5 (μg/m3) 24-hour
  if (pollutant == "PM2.5"){
    print(c("The entered pollutant is: ",pollutant))
    grid$category = dplyr::case_when(grid$value<=12.1 ~ 'Good',                           
                              grid$value>12.1 & grid$value <= 35.4  ~ 'Moderate',
                              grid$value >35.4 & grid$value <= 55.4  ~ 'Unhealthy for sensible groups',
                              grid$value > 55.4 & grid$value <= 150.4  ~ 'Unhealthy',
                              grid$value > 150.4 & grid$value <= 250.4  ~ 'Very unhealthy',
                              grid$value > 250.4 ~ 'Hazardous' )
    grid$color = dplyr::case_when(grid$category == 'Good' ~ "#abdda4",                           
                           grid$category == 'Moderate'~"#f8fd66",
                           grid$category == 'Unhealthy for sensible groups'~"#fdde61",
                           grid$category == 'Unhealthy'~"#d74a4c",
                           grid$category == 'Very unhealthy'~"#b687ba",
                           grid$category == 'Hazardous' ~"#590e63")
  }
  
  
  else if (pollutant == "PM10"){ #PM10 (μg/m3) 24-hour
    print(c("The entered pollutant is: ",pollutant))
    grid$category = dplyr::case_when(grid$value<=54 ~ 'Good',
                              grid$value>54 & grid$value <= 154  ~ 'Moderate',
                              grid$value >154 & grid$value <= 254  ~ 'Unhealthy for sensible groups',
                              grid$value > 254 & grid$value <= 354  ~ 'Unhealthy',
                              grid$value > 354 & grid$value <= 424  ~ 'Very unhealthy',
                              grid$value > 424 ~ 'Hazardous' )
    grid$color = dplyr::case_when(grid$category == 'Good' ~ "#abdda4",                           
                           grid$category == 'Moderate'~"#f8fd66",
                           grid$category == 'Unhealthy for sensible groups'~"#fdde61",
                           grid$category == 'Unhealthy'~"#d74a4c",
                           grid$category == 'Very unhealthy'~"#b687ba",
                           grid$category == 'Hazardous' ~"#590e63")
  }
  else if (pollutant == "CO"){ #CO (ppm) 8-hour
    print(c("The entered pollutant is: ",pollutant))
    grid$category = dplyr::case_when(grid$value<= 4.4 ~ 'Good',
                              grid$value> 4.4 & grid$value <= 9.4  ~ 'Moderate',
                              grid$value > 9.4 & grid$value <= 12.4  ~ 'Unhealthy for sensible groups',
                              grid$value > 12.4 & grid$value <= 15.4  ~ 'Unhealthy',
                              grid$value > 15.4 & grid$value <= 30.4  ~ 'Very unhealthy',
                              grid$value > 30.4 ~ 'Hazardous' )
    grid$color = dplyr::case_when(grid$category == 'Good' ~ "#abdda4",                           
                           grid$category == 'Moderate'~"#f8fd66",
                           grid$category == 'Unhealthy for sensible groups'~"#fdde61",
                           grid$category == 'Unhealthy'~"#d74a4c",
                           grid$category == 'Very unhealthy'~"#b687ba",
                           grid$category == 'Hazardous' ~"#590e63")
  }
  else if (pollutant == "SO2"){ #SO2 (ppb) 1-hour
    print(c("The entered pollutant is: ",pollutant))
    grid$category = dplyr::case_when(grid$value<= 35 ~ 'Good',
                              grid$value> 35 & grid$value <= 75  ~ 'Moderate',
                              grid$value > 75 & grid$value <= 185  ~ 'Unhealthy for sensible groups',
                              grid$value > 185 & grid$value <= 304  ~ 'Unhealthy', ##4 1-hour SO2 values do not define higher AQI values (≥ 200). AQI values of 200 or greater are calculated with 24-hour SO2 concentrations.
                              grid$value > 304 & grid$value <= 604  ~ 'Very unhealthy', #4 1-hour SO2 values do not define higher AQI values (≥ 200). AQI values of 200 or greater are calculated with 24-hour SO2 concentrations.
                              grid$value > 604 ~ 'Hazardous' ) #4 1-hour SO2 values do not define higher AQI values (≥ 200). AQI values of 200 or greater are calculated with 24-hour SO2 concentrations.
    grid$color = dplyr::case_when(grid$category == 'Good' ~ "#abdda4",                           
                           grid$category == 'Moderate'~"#f8fd66",
                           grid$category == 'Unhealthy for sensible groups'~"#fdde61",
                           grid$category == 'Unhealthy'~"#d74a4c",
                           grid$category == 'Very unhealthy'~"#b687ba",
                           grid$category == 'Hazardous' ~"#590e63")
  }
  else if (pollutant == "NO2"){ #NO2 (ppb) 1-hour
    print(c("The entered pollutant is: ",pollutant))
    grid$category = dplyr::case_when(grid$value<= 53 ~ 'Good',
                              grid$value> 53 & grid$value <= 100  ~ 'Moderate',
                              grid$value > 100 & grid$value <= 360  ~ 'Unhealthy for sensible groups',
                              grid$value > 360 & grid$value <= 649  ~ 'Unhealthy', 
                              grid$value > 649 & grid$value <= 1249  ~ 'Very unhealthy', 
                              grid$value > 1250 ~ 'Hazardous' ) 
    grid$color = dplyr::case_when(grid$category == 'Good' ~ "#abdda4",                           
                           grid$category == 'Moderate'~"#f8fd66",
                           grid$category == 'Unhealthy for sensible groups'~"#fdde61",
                           grid$category == 'Unhealthy'~"#d74a4c",
                           grid$category == 'Very unhealthy'~"#b687ba",
                           grid$category == 'Hazardous' ~"#590e63")
  }
  else if (pollutant == "O3" ||pollutant == "o3"){ #O3 (ppm) 1-hour
    print(c("The entered pollutant is: ",pollutant))
    grid$category = dplyr::case_when(
      grid$value >0.125 & grid$value <= 0.164 ~ 'Unhealthy for sensible groups',
      grid$value > 0.164 & grid$value <= 0.204  ~ 'Unhealthy',
      grid$value > 0.204 & grid$value <= 0.404  ~ 'Very unhealthy',
      grid$value > 0.404 ~ 'Hazardous' )
    grid$color = dplyr::case_when(grid$category == 'Good' ~ "#abdda4",                           
                           grid$category == 'Moderate'~"#f8fd66",
                           grid$category == 'Unhealthy for sensible groups'~"#fdde61",
                           grid$category == 'Unhealthy'~"#d74a4c",
                           grid$category == 'Very unhealthy'~"#b687ba",
                           grid$category == 'Hazardous' ~"#590e63")
  }
  else{
    stop("The exposure to this pollutant cannot be modeled.")
  }
  return(grid)
}

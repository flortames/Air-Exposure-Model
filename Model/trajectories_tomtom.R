trajectories_tomtom <- function(origin,dest,mode,hour_trajectory=hour,key){
  num_alternative <- 5

  #---  hour de departure

  hour_trajectory<-as.POSIXct(strptime(hour_trajectory, format = "%Y-%m-%d %H:%M:%S"))

  day <- substr (hour_trajectory,1,10)
  hour <- substr(hour_trajectory,12,13)
  minutes <- substr(hour_trajectory,15,16)
  zone_tot <- substr(format(hour_trajectory, "%z"),1,3)

  if(substr(zone_tot,1,1)=="-"){
    
    zone <- substr(format(hour_trajectory, "%z"),1,3)
    
    hour_format <- paste(day,"T",hour,"%3A",minutes,"%3A00",zone,"%3A00",sep = "")
  }else{
    
    zone <- substr(format(hour_trajectory, "%z"),2,3)
    hour_format <- paste(day,"T",hour,"%3A",minutes,"%3A00%2B",zone,"%3A00",sep = "")
    
  }
 
 
 
  #mode_transp <- mode
  
  df_rbind <- data.frame()
  df_rbind_output <- data.frame()

  origin_lat  <- strsplit(origin, ",")[[1]][1]
  origin_long<- strsplit(origin, ",")[[1]][2]
  destination_lat  <- strsplit(dest, ",")[[1]][1]
  destination_long<- strsplit(dest, ",")[[1]][2]
  url<- paste0("https://api.tomtom.com/routing/1/calculateRoute/",origin_lat,"%2C",origin_long,"%3A",destination_lat,"%2C",destination_long,
               "/json?maxAlternatives=",num_alternative,"&departAt=",hour_format,"&routeRepresentation=polyline&computeTravelTimeFor=all&traffic=true&travelMode=",mode,"&vehicleEngineType=combustion&key=",key)
  
  #---  Request en la API
  
  response <- httr::GET(url)
  resp_json <- jsonlite::fromJSON(content(response, as = "text"))
  for (j in 1:length(resp_json[["routes"]][["legs"]])){
    resp<- data.frame( long = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["longitude"]],
                       lat = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["latitude"]],
                       # --- Arrival and departure time --
                       departureTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["departureTime"]],
                       arrivalTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["arrivalTime"]],
                       # --   Distance  --- 
                       lengthInKM = (resp_json[["routes"]][["legs"]][[j]][["summary"]][["lengthInMeters"]]/1000),
                       trafficLengthInKM= (resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficLengthInMeters"]]/1000),
                       travelMode=resp_json[["routes"]][["sections"]][[1]][["travelMode"]][1],
                       # --- Delay Time 
                       trafficDelayInMinutes= round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficDelayInSeconds"]]/60),2),
                       
                       # ---  Real Time with traffic ---   
                       
                       travelTimeInMinutes = round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["travelTimeInSeconds"]]/60),2),
                       liveTrafficIncidentsTravelTimeInMinutes=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["liveTrafficIncidentsTravelTimeInSeconds"]]/60),2),
                       # ---  Historic Traffic time  --- 
                       historicTrafficTravelTimeInMinutes=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["historicTrafficTravelTimeInSeconds"]]/60),2),
                       #   ---  Time without traffic  --- 
                       noTrafficTravelTimeInMinutes= round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["noTrafficTravelTimeInSeconds"]]/60),2),
                       alternative = paste("alternative_",j,sep=""))
    
    num_rows<-  nrow(resp)
    ID <- c(1:num_rows)
    data_frame_resp <- cbind(ID , resp)
    df_rbind <- rbind(data_frame_resp,df_rbind)  
    
  }
  
  df_rbind_output<- rbind(df_rbind,df_rbind_output)  
  names(df_rbind_output) <- c("ID" , "long","lat" ,"departureTime", 
                              "arrivalTime", "lengthInKM", 
                              "trafficLengthInKM","travelMode", 
                              "trafficDelayInMinutes","travelTimeInMinutes" ,                   
                              "liveTrafficIncidentsTravelTimeInMinutes",
                              "historicTrafficTravelTimeInMinutes",
                              "noTrafficTravelTimeInMinutes",           
                              "alternative")
  return(df_rbind_output)
}

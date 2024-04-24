#######################################################################
# ------------        TOTAL EXPOSURE ESTIMATE    -------------     

#This code allows estimating the total daily exposure
# The idea is to enter a list() of coordinates with the points of O-D
# The last trip will always be home, that is, the first point entered
#
total_exposure <- function (travel_list, mode, dir,key,selection,output_exp,
                            departure_time_home, activity_minutes,pollutant,shapeValue ,gridID,units){
 
  df_output <- data.frame()
  rbind_df_1 <- data.frame()
  
  rbind_route_select <- data.frame()
  # ------- Calculation for more than one daily trip
  for (i in 1:length(travel_list$long)){
    print(paste("Point",i,sep = " "))
    #if it is 1 it is the output value
    if(i==1){
      
      # If i=1 consider the departure time entered in the function
      origin_coords <- paste((travel_list[i,2]),(travel_list[i,1]),sep = ",")
      destination_coords <- paste((travel_list[i+1,2]),(travel_list[i+1,1]),sep = ",")
      selection_route <-  alternative_trajectories (origin=origin_coords,dest=destination_coords,mode=mode[i], dir,key=key,output = "df",hour = departure_time_home,gridID,shapeValue, units)
      
    }
    
    else if (i == length(travel_list$long)){
      # If the coordinates correspond to the last point entered, destination returns to point 1 (home)
      origin_coords <- paste((travel_list[i,2]),(travel_list[i,1]),sep = ",")
      origin_coords_1 <- paste((travel_list[1,2]),(travel_list[1,1]),sep = ",")
      selection_route <-  alternative_trajectories (origin=origin_coords,dest=origin_coords_1,mode=mode[i], dir,key=key,output = "df",hour =prox_hour_output,gridID,shapeValue,units )
      
    }else {
      
      origin_coords <- paste((travel_list[i,2]),(travel_list[i,1]),sep = ",")
      destination_coords <- paste((travel_list[i+1,2]),(travel_list[i+1,1]),sep = ",")
      selection_route <-  alternative_trajectories (origin=origin_coords,dest=destination_coords,mode=mode[i], dir,key=key,output = "df",hour = prox_hour_output,gridID,shapeValue,units )
      
    }
    # --------select the chosen alternative
    selection_route <- selection_route[selection_route$type == selection[i],]
    selection_route$i <- paste ("Route",i, sep = " ")
    # ------- data destination
    data_destination <-  selection_route[selection_route$ID == (length(selection_route$ID)),]
    lat_destination <- data_destination$lat
    long_destination <- data_destination$long
    
    # ------- hours that we use to get the destination grid
    
    if(i==1){
      
      # If it is the first trip, arrival time + activity time
      arrival_time <-  as.POSIXct(strptime(selection_route$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))
      prox_hour_output<-as.POSIXct(strptime(selection_route$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))+minutes(activity_minutes[i,])
      destination_time <- as.numeric(difftime(prox_hour_output,arrival_time ,unit ="mins"))
      
    }
    
    else if (i == nrow(activity_minutes)+1){
      # If it is the last entered time ==> entered time - 23:59
      prox_hour_output <-  as.POSIXct(strptime((paste(substr(departure_time_home,1,10)," 23:59:59",sep ="")), format = "%Y-%m-%d %H:%M:%S"))
      arrival_time  <-  as.POSIXct(strptime(selection_route$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))
      destination_time <- as.numeric(difftime(prox_hour_output,arrival_time ,unit ="mins"))
    }else{
      
      # else ==> arrival time - hour + 1
      prox_hour_output<-as.POSIXct(strptime(selection_route$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))+minutes(activity_minutes[i,])
      arrival_time <-  as.POSIXct(strptime(selection_route$arrivalTime[1], format = "%Y-%m-%dT%H:%M:%S"))
      destination_time <- as.numeric(difftime(prox_hour_output,arrival_time ,unit ="mins"))
      
    }
    
    # ------- Data trajectories
    
    trip_time <- selection_route$travelTimeInSeconds[1] # in minutes
    trip_distance <- selection_route$lengthInMeters[1]
    trip_conc <- mean (selection_route$daily_pol_value_mean, na.rm = T)
    # ------- Data origin
    data_origin <-  selection_route[selection_route$ID == 1,]
    lat_origin <- data_origin$lat
    long_origin <- data_origin$long
    
    ## ------- Destination concentrations
    df_concentractions <- rbind(data_origin,data_destination)
    df_concentractions <- st_as_sf(df_concentractions, coords = c("long", "lat"), crs = 4326)
    dir_point <- paste (dir,"/temp/",sep="")
    name_point <- paste(dir_point ,"temp_point.shp",sep="")
    st_write(df_concentractions,paste(dir_point,"temp_point.shp",sep = ""), driver="ESRI Shapefile", quiet = TRUE)
    
    point <- st_read(name_point,quiet = TRUE)
    # Function that looks for the grids of the hours of interest
    grid <- temporary_grid_search (start_hour = arrival_time,end_hour = prox_hour_output ,dir=dir,time_format = "%Y-%m-%d %H:%M:%S",gridID, shapeValue )
    
    intersection_point <- sf::st_intersection(point,grid)
    names(intersection_point)<-  c("altrntv" ,"ID" , "dprtrTm","arrvlTm","lngthIM" , "trffLIM" , "travlMd",
                                   "trffDIS", "trvlTIS", "lTITTIS",   "hsTTTIS",   "nTrTTIS" , "value", "exposur" ,
                                   "type","i" ,"gridID","value.1", "geometry")
    conc_destination <- which(intersection_point$ID == max(intersection_point$ID))
    conc_destination<- intersection_point[conc_destination,]
    conc_destination <- conc_destination$value.1
    if (i==1){
      
      # ------- Concentrations in O-D points in origin
      zone <- substr(selection_route$arrivalTime[1],20,22)
      start_time <- paste (substr(arrival_time,1,10)," 00:00:01 ", zone,sep="")
      grid_origin <- temporary_grid_search (start_hour=start_time , end_hour=departure_time_home,dir=dir,time_format = "%Y-%m-%d %H:%M:%S",gridID, shapeValue )
      intersection_point <- sf::st_intersection(point,grid_origin)
      names(intersection_point)<-  c("altrntv" ,"ID" , "dprtrTm","arrvlTm","lngthIM" , "trffLIM" , "travlMd","trffDIS", "trvlTIS",
                                     "lTITTIS",   "hsTTTIS",   "nTrTTIS" , "value", "exposur" ,"type","i" ,"gridID","value.1", "geometry")
      conc_origin <- which(intersection_point$ID == min(intersection_point$ID))
      conc_origin<- intersection_point[conc_origin,]
      conc_origin <- conc_origin$value.1
      time_origin <- as.numeric(difftime(departure_time_home,start_time  ,unit ="mins"))
      
    }else{
      conc_origin <- NA
      time_origin <- NA
    }
    point <- sf::st_read(name_point,quiet = TRUE)
    file.remove(file.path(dir_point, dir(path=dir_point,pattern="temp_point.*")))
    df_1 <- data.frame(lat_origin,long_origin, lat_destination, long_destination,
                       conc_origin, trip_conc, conc_destination,
                       trip_time,destination_time,time_origin,i)
    rbind_df_1<- rbind(rbind_df_1, df_1)
    
    # ---- If you want a plot
    rbind_route_select<- rbind(rbind_route_select,selection_route)
    
  }
  # ------- Final variables
  #Total exposure route
  rbind_df_1$exp_tot_trajectory <- rbind_df_1$trip_conc *  rbind_df_1$trip_time
  rbind_df_1$exp_tot_destination <-   rbind_df_1$conc_destination *   rbind_df_1$destination_time
  rbind_df_1$exp_tot_origin <-   rbind_df_1$conc_origin *   rbind_df_1$time_origin
  
  # ------- Calculate expossure per hour (mins)
  exp_origin <- mean (rbind_df_1$exp_tot_origin, na.rm=T)
  exp_trajectory <- mean (rbind_df_1$exp_tot_trajectory, na.rm=T)
  exp_destination <- mean (rbind_df_1$exp_tot_destination, na.rm=T)
  exp_tot <- round((mean(c(exp_origin,exp_destination,exp_trajectory), na.rm=T)/60),2)
  rbind_df_1$exp_tot <- exp_tot
  
  # -------Calculate exposures per hour mins VERSION 2
  exp_origin2 <- sum(rbind_df_1$exp_tot_origin, na.rm=T)
  exp_trajectory2 <- sum(rbind_df_1$exp_tot_trajectory, na.rm=T)
  exp_destination2 <- sum (rbind_df_1$exp_tot_destination, na.rm=T)
  exp_tot2 <- round((sum(c(exp_origin2,exp_destination2,exp_trajectory2), na.rm=T)/60),2)
  rbind_df_1$exp_tot_sum <- exp_tot2
  rbind_df_1$route <- paste("Route",rbind_df_1$i,sep=" " )
  rbind_df_1$mode <- mode
  time_origin_function <- function_hours(sum(rbind_df_1$destination_time[max(rbind_df_1$i)]+rbind_df_1$time_origin[!is.na(rbind_df_1$time_origin)]))
  trip_time_function <- function_hours((sum(rbind_df_1$trip_time)))
  destination_time_function <- function_hours(sum(rbind_df_1$destination_time[1:(length(rbind_df_1$destination_time)-1)]))
  
  #  --- Point to line transformation
  route_line<- points_to_line(data = rbind_route_select,
                              long = "long",
                              lat = "lat",
                              id_field = "i",
                              sort_field = "ID")
  #  --- HTML plot features
  
  tag.map.title <- htmltools::tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))
  #  --- Map title
  title <-  htmltools::tags$div(tag.map.title, HTML(paste(sep = "<br/>",
                                              paste0("<center><b>Total daily exposure estimate</b></center>"),
                                              
                                              paste0("<b>Exposure: </b>",  exp_tot2 ," ",units, "/ 24hs"),
                                              paste0("<b>Origin time: </b>",  time_origin_function,"hs"),
                                              paste0("<b>Activities time: </b>",  destination_time_function ," hs"),
                                              paste0("<b>Travel time: </b>",  trip_time_function ," hs"))))
  #print(title)
  #  --- Travel list ID
  num_rows<-  nrow(travel_list)
  id <- c(1:num_rows)
  travel_list<- cbind(id , travel_list)#
  input_hour <- as.POSIXct(strptime(departure_time_home, format = "%Y-%m-%d %H:%M:%S"))
  
  init_day <- paste(date(input_hour),"00:00:01", sep=" ")
  finish_day <- paste(date(input_hour),"23:59:01", sep=" ")
  grid_tot <- temporary_grid_search (start_hour =init_day ,end_hour =finish_day  ,dir=dir,time_format = "%Y-%m-%d %H:%M:%S",gridID, shapeValue )
  #  --- Grid categories
  grid_tot  <- map_colors(grid_tot,pollutant)
  #  --- Color grid
  palette_route <- c("#023858","#49006a","#00441b","#e7298a","#feb24c","#3690c0","#016c59","#8c510a","#f03b20")
  palfac <- leaflet::colorFactor(unique(grid_tot$color), domain = grid_tot$category)
  pal <- leaflet::colorFactor(palette_route, domain = rbind_df_1$i)
  circle_pal<- leaflet::colorFactor(palette_route, domain = travel_list$id)
  # ---  Plot
  mapa <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addAwesomeMarkers(travel_list,lng=travel_list$long,lat =travel_list$lat,label = paste("Point",travel_list$id,sep = " ")) %>%
    leaflet::addPolylines(data = route_line,opacity = 0.8,stroke = TRUE,weight = c((rbind_df_1$i)+5), color = c(pal(rbind_df_1$i)),group = (as.character(rbind_df_1$route))) %>%
    leaflet::addPolygons(data = grid_tot,color = "#636363" ,#opacity = 0.8,weight = c((rbind_df_1$i)+3)
                group = "Concentrations",
                weight = 2,
                smoothFactor = 0.1,
                opacity = 0.1,
                fillOpacity = 0.5,
                fillColor = grid_tot$color,
    )%>%
    leaflet::addTiles() %>%
    leaflet::addControl(title, position = "topleft", className="map-title")%>%
    leaflet::addLegend(data = grid_tot,position = "bottomleft", pal = palfac, values = grid_tot$category,
              title = paste("US AQI Level",pollutant, units,sep=" ")) %>%
    
    
    # Layers control
    leaflet::addLayersControl(
      overlayGroups = c("Concentrations",c(rbind_df_1$route)))
  
  if (output_exp == "df"){
    file.remove(file.path(dir_point, dir(path=dir_point,pattern="temp.*")))
    return(rbind_df_1)
  }
  if (output_exp == "plot"){
    file.remove(file.path(dir_point, dir(path=dir_point,pattern="temp.*")))
    return(mapa)
    
  }
  if (output_exp == "polyline"){
    file.remove(file.path(dir_point, dir(path=dir_point,pattern="temp.*")))
    return( route_line)
    
  }
}

###############################################################################




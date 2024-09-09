## ----------  Alternativas de viaje 
alternative_trajectories <- function(origin,dest,mode,dir,key,output,hours = NULL,gridID, shapeValue, units, pollutant){
# ------------             Search of trajectories      ---------------- 
# Search for alternative according to tom-tom

  trajectory <- trajectories_tomtom(origin,dest,mode = mode, hour_trajectory = hours, key)

  # ------------             PASO DE PUNTOS A LINEAS      ----------------
  data_trajectory <- trajectory # renombramos
  v_lines <- points_to_line(data = data_trajectory, 
                            long = "long", 
                            lat = "lat", 
                            id_field = "alternative",
                            sort_field = "ID")
  
  id_df <- data.frame()
  data_trajectory%>%
    dplyr::group_by(alternative) %>%  
    dplyr::group_split() -> dat_agrupado
  for (x in 1:nrow(v_lines)){
    origin <- origin
    destination <- dest
    departureTime <- dat_agrupado[[x]][["departureTime"]][1]
    arrivalTime<- dat_agrupado[[x]][["arrivalTime"]][1]
    lengthInKM<- dat_agrupado[[x]][["lengthInKM"]][1]
    #trafficLengthInKM <- dat_agrupado[[x]][["trafficLengthInKM"]][1]
    travelMode <- dat_agrupado[[x]][["travelMode"]][1]
    #trafficDelayInMinutes<-  dat_agrupado[[x]][["trafficDelayInMinutes"]][1]
    travelTimeInMinutes<- dat_agrupado[[x]][["travelTimeInMinutes"]][1]
    #liveTrafficIncidentsTravelTimeInMinutes<- dat_agrupado[[x]][["liveTrafficIncidentsTravelTimeInMinutes"]][1]
    #historicTrafficTravelTimeInMinutes <- dat_agrupado[[x]][["historicTrafficTravelTimeInMinutes"]][1]
    #noTrafficTravelTimeInMinutes<- dat_agrupado[[x]][["noTrafficTravelTimeInMinutes"]][1]
    alternative<- dat_agrupado[[x]][["alternative"]][1]
    data_frame_1 <- data.frame( origin,destination ,departureTime, 
                                arrivalTime, lengthInKM, 
                                #trafficLengthInKM,
                                travelMode, 
                                #trafficDelayInMinutes,
                                travelTimeInMinutes ,                   
                                #liveTrafficIncidentsTravelTimeInMinutes,
                                #historicTrafficTravelTimeInMinutes,
                                #noTrafficTravelTimeInMinutes,           
                                alternative)
    names (data_frame_1)<- c( "origin","destination" ,"departureTime", 
                              "arrivalTime", "lengthInKM", 
                              #"trafficlengthInKM",
                              "travelMode", 
                              #"trafficDelayInMinutes",
                              "travelTimeInMinutes",                   
                              #"liveTrafficIncidentstravelTimeInMinutes",
                              #"historicTraffictravelTimeInMinutes",
                              #"noTraffictravelTimeInMinutes",           
                              "alternative")
    
    id_df <- rbind(id_df,data_frame_1)
  }

  df2 <- dplyr::left_join(v_lines, id_df, by = "alternative")
  # ------------                  SELECTION ROUTE        ----------------
  

  sf::st_write(df2,"./temp","temp", driver="ESRI Shapefile", quiet = TRUE) 
  df3 <- sf::st_read("temp/temp.shp",quiet = TRUE,crs = 4326)
  
    
  #---  Search of the grid of interest according to the hour entered
  grid<- temporary_grid_search(start_hour = df3$dprtrTm[1],end_hour=df3$arrvlTm[length(df3$arrvlTm)],dir = dir,time_format="%Y-%m-%dT%H:%M:%S",gridID=gridID, shapeValue = shapeValue )
  intersection_grid <- sf::st_intersection(df3,grid)
  # Delete files
  file.remove(file.path("./temp", dir(path="./temp" ,pattern="temp.*")))
    
   intersection_grid%>%
     dplyr::group_by(altrntv) %>% 
     dplyr::group_split() -> dataSplit_intersection
   sum_df <- data.frame()

   for (i in 1:length(dataSplit_intersection)){
      origin <- dataSplit_intersection[[i]][["origin"]][1]
      destination <- dataSplit_intersection[[i]][["destination"]][1]
      alternative<- dataSplit_intersection[[i]][["altrntv"]][1]
      daily_pol_value_mean <- round(mean(dataSplit_intersection[[i]][["value"]],na.rm=T),2)
      df <- data.frame(alternative = alternative,daily_pol_value_mean = daily_pol_value_mean)
      sum_df<- rbind(sum_df,df)
      }
    df_merge <- merge(trajectory,sum_df, 
                      by = "alternative")
    trajectory<- df_merge
    trajectory$exposure_value_mean <- round(((trajectory$daily_pol_value_mean * trajectory$travelTimeInMinutes)/60),2)

    # ------------ 01. FASTER ROUTE
    faster_route <- trajectory[trajectory$travelTimeInMinutes == min(trajectory$travelTimeInMinutes),]
    faster_route <- faster_route[faster_route$alternative == faster_route$alternative[1],]
    faster_route$type <- "fast" 
    # ------------ 02. SHORTER ROUTE
    shorter_route <- trajectory[trajectory$lengthInKM == min(trajectory$lengthInKM),]
    shorter_route <- shorter_route[shorter_route$alternative == shorter_route$alternative[1],]
    shorter_route$type <- "short"
    # ------------ 03. LESS CONTAMINATED ROUTE
    less_polluted_route<- trajectory[trajectory$daily_pol_value_mean == min(trajectory$daily_pol_value_mean),]
    less_polluted_route <- less_polluted_route[less_polluted_route$alternative == less_polluted_route$alternative[1],]
    less_polluted_route$type <- "lesspol"
    
    # ------------ 04. MORE CONTAMINATED ROUTE
    more_polluted_route <- trajectory[trajectory$daily_pol_value_mean == max(trajectory$daily_pol_value_mean),]
    more_polluted_route <- more_polluted_route[more_polluted_route$alternative == more_polluted_route$alternative[1],]
    more_polluted_route$type <- "morepol"
    # ------------ 0.5 MORE EXPOSURE
    more_exposure_route <- trajectory[trajectory$exposure_value_mean == max(trajectory$exposure_value_mean),]
    more_exposure_route <- more_exposure_route[more_exposure_route$alternative == more_exposure_route$alternative[1],]
    more_exposure_route$type <- "moreexpos"
    
    # ------------ 0.7 LESS EXPOSURE
    less_exposure_route <- trajectory[trajectory$exposure_value_mean == min(trajectory$exposure_value_mean),]
    less_exposure_route <- less_exposure_route[less_exposure_route$alternative == less_exposure_route$alternative[1],]
    less_exposure_route$type <- "lessexpos"
    
    df_output <- data.frame()
    # ------- If you want a dataframe output with  trajectory exposure and origin-destination exposure
    if (output=="df"){
    df_output <- rbind(faster_route,shorter_route,more_polluted_route,less_polluted_route,more_exposure_route,less_exposure_route)#,
                       
    return(df_output)
    }
  # ------------                  ROUTE PLOT        ----------------
  # Plot the different alternatives considered
    less_polluted_route_line<- points_to_line(data = less_polluted_route, 
                                long = "long", 
                                lat = "lat", 
                                id_field = NULL,
                                sort_field = "ID")
     more_polluted_route_line<- points_to_line(data = more_polluted_route, 
                                             long = "long", 
                                             lat = "lat", 
                                             id_field = NULL,
                                             sort_field = "ID")
     shorter_route_line<- points_to_line(data = shorter_route, 
                                        long = "long", 
                                        lat = "lat", 
                                        id_field = NULL,
                                        sort_field = "ID")
    faster_route_line<- points_to_line(data = faster_route, 
                                        long = "long", 
                                        lat = "lat", 
                                        id_field = NULL,
                                        sort_field = "ID")
    
    more_exposure_route_line<- points_to_line(data = more_exposure_route, 
                                      long = "long", 
                                      lat = "lat", 
                                      id_field = NULL,
                                      sort_field = "ID")
    less_exposure_route_line<- points_to_line(data = less_exposure_route, 
                                      long = "long", 
                                      lat = "lat", 
                                      id_field = NULL,
                                      sort_field = "ID")
    # ------- output: map
    #  --- Title map
    if (output == "plot"){
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
      #  --- Title
     title <- htmltools::tags$div(tag.map.title, 
                        HTML(paste("<center><b>travel alternative with mode: </b></center>",mode)))

    #  --- Plot content
      content_more_cont <- paste(sep = "<br/>",
                                paste0("<center><b>More polluted route: </b></center>"),
                                paste0("<b>Duration: </b>", more_polluted_route$travelTimeInMinutes," min"),
                                paste0("<b>Distance: </b>", more_polluted_route$lengthInKM," km"),
                                paste0("<b>Concentrations: </b>", more_polluted_route$value," ", units),
                                paste0("<b>Exposure: </b>", more_polluted_route$exposure," ", units,"/h"))
      content_less_cont <- paste(sep = "<br/>",
                                paste0("<center><b>Less polluted route: </b></center>"),
                                paste0("<b>Duration: </b>", less_polluted_route$travelTimeInMinutes," min"),
                                paste0("<b>Distance: </b>", less_polluted_route$lengthInKM," km"),
                                paste0("<b>Concentrations: </b>", less_polluted_route$value," ", units),
                                paste0("<b>Exposure: </b>", less_polluted_route$exposure," ", units,"/h"))
      content_short <- paste(sep = "<br/>",
                                paste0("<center><b>Shorter route: </b></center>"),
                                paste0("<b>Duration: </b>",shorter_route$travelTimeInMinutes," min"),
                                paste0("<b>Distance: </b>", shorter_route$lengthInKM," km"),
                             paste0("<b>Concentrations: </b>", shorter_route$value," ", units),
                             paste0("<b>Exposure: </b>", shorter_route$exposure," ", units,"/h"))
      
      content_fast <- paste(sep = "<br/>",
                                paste0("<center><b>Faster route: </b></center>"),
                                paste0("<b>Duration: </b>", faster_route$travelTimeInMinutes," min"),
                                paste0("<b>Distance: </b>", faster_route$lengthInKM," km"),
                                paste0("<b>Concentrations: </b>", faster_route$value," ", units),
                                paste0("<b>Exposure: </b>", faster_route$exposure," ", units,"/h"))

      content_less_exp<- paste(sep = "<br/>",
                              paste0("<center><b>Less exposure route: </b></center>"),
                              paste0("<b>Duration: </b>", less_exposure_route$travelTimeInMinutes," min"),
                              paste0("<b>Distance: </b>", less_exposure_route$lengthInKM," km"),
                              paste0("<b>Concentrations: </b>", less_exposure_route$value," ", units),
                              paste0("<b>Exposure: </b>", less_exposure_route$exposure," ", units,"/h"))
      
      
      content_more_exp <- paste(sep = "<br/>",
                              paste0("<center><b>More exposure route: </b></center>"),
                              paste0("<b>Duration: </b>", more_exposure_route$travelTimeInMinutes," min"),
                              paste0("<b>Distance: </b>", more_exposure_route$lengthInKM," km"),
                              paste0("<b>Concentrations: </b>", more_exposure_route$value," ",units),#" ?g m-3"),
                              paste0("<b>Exposure: </b>", more_exposure_route$exposure," ",units,"/h"))#" ?g m-3/h"))

      grid <- map_colors(grid,pollutant)
      #  --- Colorgrid
      palette_grid <- c("#abdda4","#f8fd66","#fdde61","#d74a4c","#b687ba","#590e63")
      palfac <- leaflet::colorFactor(unique(grid$color), domain = grid$category)
      # ---  Plot
    
       map <- leaflet::leaflet() %>%
         leaflet::addTiles() %>%

         leaflet::addAwesomeMarkers(
          
          lat = as.numeric(strsplit(origin, ",")[[1]][1]),
          lng = as.numeric(strsplit(origin, ",")[[1]][2]),
          label = "origin") %>%
          
         leaflet::addAwesomeMarkers(
          
          lat = as.numeric(strsplit(dest, ",")[[1]][1]),
          lng = as.numeric(strsplit(dest, ",")[[1]][2]), 
          label = "destination") %>%
         
         leaflet::addPolylines(data = faster_route_line,weight = 5,stroke = TRUE, color ="#FF0000FF",label = "Faster route",popup=content_fast,group="Faster route") %>%
         leaflet::addPolylines(data = shorter_route_line,weight = 5,stroke = TRUE,color ="#ae017e",label = "Shorter route",popup=content_short,group="Shorter route") %>%
         leaflet::addPolylines(data = more_polluted_route_line,weight = 5,stroke = TRUE, color ="#00FF66FF",label = "More polluted route",popup=content_more_cont,group="More polluted route")%>%
         leaflet::addPolylines(data = less_polluted_route_line,weight = 5, color ="#08306b",label = "Less polluted route",popup=content_less_cont,group="Less polluted route")%>%
         leaflet::addPolylines(data = less_exposure_route_line,weight = 5, color ="#016c59",label = "Less exposure route",popup=content_less_exp,group="Less exposure route")%>%
         leaflet::addPolylines(data = more_exposure_route_line,weight = 5, color ="#cc4c02",label = "More exposure route",popup=content_more_exp,group="More exposure route")%>%
         leaflet::addPolygons(data = grid,color = "#636363" ,
                   group = "Concentrations",
                   weight = 2,
                   smoothFactor = 0.1,
                   opacity = 0.1,
                   fillOpacity = 0.5,
                   fillColor = grid$color#~palfac(grid$category)
         )%>%
         leaflet::addTiles() %>%
         leaflet::addControl(title, position = "topleft", className="map-title")%>%
         leaflet::addLegend(data = grid,position = "bottomleft", pal = palfac, values = ~grid$category,
                   title = paste("US AQI Level",pollutant,units, sep=" ")) %>%
         # Layers control
         leaflet::addLayersControl(
         overlayGroups = c("Concentrations","Less polluted route", "More polluted route", "Shorter route","Faster route", "Less exposure route","More exposure route"))#,"More polluted route Sum", "Less polluted route Sum"))#,

       alternative_map <- map
       return(alternative_map)
  }
  #################################################################################
  # ------- output POLYLINE
       if (output == "polyline"){
         df_output <- rbind(faster_route,shorter_route,more_polluted_route,less_polluted_route, more_exposure_route, less_exposure_route)
         
         polyline_output<- points_to_line(data = df_output, 
                                          long = "long", 
                                          lat = "lat", 
                                          id_field = "type",
                                          sort_field = "ID")
         
         id_df_output <- data.frame()
         df_output%>%
           dplyr::group_by(type) %>%  
           dplyr::group_split() -> group_dat_output
         
         for (p in 1:nrow(polyline_output)){
           origin <- origin
           destination <- dest
           
           departureTime <- group_dat_output[[p]][["departureTime"]][1]
           arrivalTime<- group_dat_output[[p]][["arrivalTime"]][1]
           lengthInKM<- group_dat_output[[p]][["lengthInKM"]][1]
           #trafficlengthInKM <- group_dat_output[[p]][["trafficlengthInKM"]][1]
           travelMode <- group_dat_output[[p]][["travelMode"]][1]
           #trafficDelayInMinutes<-  group_dat_output[[p]][["trafficDelayInMinutes"]][1]
           travelTimeInMinutes<- group_dat_output[[p]][["travelTimeInMinutes"]][1]
           #liveTrafficIncidentstravelTimeInMinutes<- group_dat_output[[p]][["liveTrafficIncidentstravelTimeInMinutes"]][1]
           #historicTraffictravelTimeInMinutes <- group_dat_output[[p]][["historicTraffictravelTimeInMinutes"]][1]
           #noTraffictravelTimeInMinutes<- group_dat_output[[p]][["noTraffictravelTimeInMinutes"]][1]
           alternative<-group_dat_output[[p]][["alternative"]][1]
           type <- group_dat_output[[p]][["type"]][1]
           value <- group_dat_output[[p]][["exposure_value_mean"]][1] #group_dat_output[[p]][["value"]][1]
           daily_pol_value_mean <- group_dat_output[[p]][["daily_pol_value_mean"]][1] #group_dat_output[[p]][["value"]][1]
           
           
           data_frame_output <- data.frame( origin,destination ,departureTime, 
                                            arrivalTime, lengthInKM, 
                                            #trafficlengthInKM,
                                            travelMode, 
                                            #trafficDelayInMinutes,
                                            travelTimeInMinutes ,                   
                                            #liveTrafficIncidentstravelTimeInMinutes,
                                            #historicTraffictravelTimeInMinutes,
                                            #noTraffictravelTimeInMinutes,
                                            daily_pol_value_mean,
                                            alternative, type, value)
           names (data_frame_output)<- c("origin","destination" ,"departureTime", 
                                         "arrivalTime", "lengthInKM", 
                                         #"trafficlengthInKM",
                                         "travelMode", 
                                         #"trafficDelayInMinutes",
                                         "travelTimeInMinutes",                   
                                         #"liveTrafficIncidentstravelTimeInMinutes",
                                         #"historicTraffictravelTimeInMinutes",
                                         #"noTraffictravelTimeInMinutes", 
                                         "daily_pol_value_mean",
                                         "alternative","type","value")
           
           id_df_output <- rbind(id_df_output,data_frame_output)
         }
         polyline_output$type <- polyline_output$alternative
         df2_output <- dplyr::left_join(polyline_output, id_df_output, by = "type")
       }
    file.remove(file.path("./temp", dir(path="./temp" ,pattern="temp.*")))
    return(df2_output)
}

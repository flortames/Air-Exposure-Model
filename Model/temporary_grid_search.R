temporary_grid_search <- function(start_hour, end_hour=NULL,dir,time_format, gridID, shapeValue){  
  # --- Function that looks for the grid (.shp) corresponding to the hour of interest entered
  trajectory_grid_rbind <- data.frame()
  # Init time
  only_start_hour <- lubridate::hour(as.POSIXct(strptime(start_hour, format = time_format)))
  # End time
  only_end_hour <- lubridate::hour(as.POSIXct(strptime(end_hour, format = time_format)))

  #  --- 
  if (is.null(end_hour)){

    df_start_grids <- sf::st_read(hourly_grid(hour = start_hour, time_format = time_format,dir),quiet = TRUE)
    df_start_grid <- sf::st_transform(df_start_grids,crs = 4326)# Always transform the CRS in 4326
    names(df_start_grid)[names(df_start_grid) == gridID] <- "ID"
    #ID <- st_drop_geometry(df_start_grid)[gridID]
    #names(ID) <- "ID"
    #df_start_grid <- cbind(df_start_grid,ID)
    }
  #  --- When there is only one grid
  else if (only_start_hour == only_end_hour ){

    trajectory_grid <- sf::st_read(hourly_grid(start_hour, time_format = time_format,dir),quiet = TRUE)
    salida<- sf::st_transform(trajectory_grid,crs = 4326)
    names(salida)[names(salida) == gridID] <- "ID"
    
  }else{
    # --- When there are several grids we do an average per pixel
    for(j in only_start_hour:only_end_hour){

      if (j < 10){
        j_hour <- paste("0",j,sep = "")
      }else{
        j_hour <- j
      }
      zone <- substr(start_hour, nchar(start_hour) - 2, nchar(start_hour))
      day <- paste(substr(start_hour,1,10),paste(j_hour,":00:00",sep = ""), zone,sep = " ")
      trajectory_grid <- sf::st_read(hourly_grid(day, time_format = "%Y-%m-%d %H:%M:%S",dir),quiet = TRUE)
      trajectory_grid$hour <- day
      trajectory_grid_rbind <- rbind(trajectory_grid_rbind,trajectory_grid)
    }
    ## ------------ Group by the ID of the grid and make the mean of each pixel

    ID <- sf::st_drop_geometry(trajectory_grid_rbind)#[gridID]
    names(ID) <- "ID"
    trajectory_grid_rbind <- cbind(trajectory_grid_rbind,ID)
    trajectory_grid_rbind %>%
      dplyr::group_by(ID) %>%  
      dplyr::group_split() -> data_grilla
    
    df_grilla <- data.frame()
    for (p in 1:length(data_grilla)){
      ID <- data_grilla[[p]][["ID"]][1]
      value <- mean(data_grilla[[p]][[shapeValue]],na.rm = T)
      geometry <- data_grilla[[p]][["geometry"]][1]
      df <- data.frame(ID = ID, value = value, 
                       geometry = geometry)
      df_grilla <- rbind(df_grilla ,df)
      
    }
    sf::st_write(df_grilla,"./temp/temp_grid.shp",delete_layer = TRUE,quiet = TRUE)
    trajectory_grid<- sf::st_read("./temp/temp_grid.shp",quiet = TRUE)
    salida<- sf::st_transform(trajectory_grid,crs = 4326)
    
  }
  if(is.null(end_hour)){

    return(df_start_grid)
    }else{
      return(salida)
    }
}

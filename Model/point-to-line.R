
# ------- Function for transforming points into lines
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  # Convert to SpatialPointsDataFrame
  # If there is a sort field...
  
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    sp_lines <- sf::st_linestring(as.matrix(data[, c(long, lat)]))
    
    sp_lines <- sf::st_sfc(sp_lines)
    sf::st_crs(sp_lines) <- 4326
    return(sp_lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    paths <- split(data, data[[id_field]])
    sp_lines <- sf::st_linestring(as.matrix(paths[[1]][, c(long,lat)]))
    sp_lines <- sf::st_sfc(sp_lines)
    sf::st_crs(sp_lines) <- 4326
    alternative <- paths[[1]][[id_field]][1]
    
    sp_lines <- sf::st_sf(geometry = sp_lines,alternative = alternative)
    for (p in 2:length(paths)) {
      #l <- sf::st_linestring(as.matrix(paths[[p]][, c("long", "lat")]))
      l <- sf::st_linestring(as.matrix(paths[[p]][, c(long, lat)]))
      l <- sf::st_sfc(l)
      sf::st_crs(l) <- 4326
      alternative <- paths[[p]][[id_field]][1]
      sp_lines2 <- sf::st_sf(geometry = l,alternative = alternative)
      sp_lines <- rbind(sp_lines, sp_lines2)
    }
    return(sp_lines)
  }
}





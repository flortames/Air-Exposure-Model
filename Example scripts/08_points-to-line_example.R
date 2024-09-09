data <- data.frame (lat = c(-32.922778,-32.921596,-32.921062,-32.919904,-32.918786),
                    long = c(-68.847785,-68.847513,-68.847316,-68.847043,-68.846816),
                    sort_field = c(1,2,3,4,5))

test <- points_to_line (data, long = "long", lat = "lat", 
                        id_field = NULL, sort_field = "sort_field")

 

data2 <- data.frame (lat = c(-32.922778,-32.921596,-32.921062,-32.919904,-32.918786,
                            -32.914315,-32.914362,-32.914262,-32.914208),
                    long = c(-68.847785,-68.847513,-68.847316,-68.847043,-68.846816,
                             -68.856012, -68.855111, -68.853812, -68.852847),
                    id_field = c("line1","line1","line1","line1","line1",
                                 "line2","line2","line2","line2"),
                    sort_field = c(1,2,3,4,5,1,2,3,4))
test2 <- points_to_line (data2, long = "long", lat = "lat", 
                        id_field = "id_field", sort_field = "sort_field")
# 01. Generate a data.frame with the geographic coordinates of the spatial points.
data_one_line <- data.frame (lat = c(-32.922778,-32.921596,-32.921062,-32.919904,-32.918786),
                             long = c(-68.847785,-68.847513,-68.847316,-68.847043,-68.846816),
                             sort_field = c(1,2,3,4,5))
test_one_line <- points_to_line (data = data_one_line, long = "long", lat = "lat", id_field = NULL, sort_field = "sort_field")
View(test_one_line)
plot(test_one_line)

# Example for two spatial line
# 01. Generate a data.frame with the geographic coordinates of the spatial points.
data_two_line <- data.frame (lat = c(-32.92277,-32.92156,-32.92106,-32.91990,-32.91878,-32.91431,-32.91436,-32.91426,-32.91420),
                             long = c(-68.84778,-68.84753,-68.84731,-68.847043,-68.84681, -68.85601, -68.85511, -68.85381, -68.85284),
                             id_field = c("line1","line1","line1","line1","line1",
                                          "line2","line2","line2","line2"),
                             sort_field = c(1, 2, 3, 4, 5, 1, 2, 3, 4))
test_two_line <- points_to_line (data = data_two_line, long = "long", lat = "lat", id_field = "id_field", sort_field = "sort_field")
View(test_two_line)
plot(test_two_line)

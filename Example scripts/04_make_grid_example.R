# ---The example is for the Metropolitan Area of Mendoza in Argentina. - - -


# -----------------------------------------------------------------------------
# --------------------------- MAKE_GRID FUNCTION EXAMPLE ----------------------------- 

# Coordinates: Example Cordoba, Argentina
xmax = -64.08641884588135
xmin = -64.27816073530852 
ymax =  -31.320897547807217
ymin = -31.48431967197022  
# Coordinates: Example Mendoza, Argentina
xmin = -68.93274
xmax = -68.67807
ymin= -33.05988
ymax = -32.78414
# Coordinates: Example Mexico
xmin = -99.26994216858604
xmax = -98.94861920239204 
ymin= 19.23859719684508
ymax = 19.541088423908583

# Coordinates: Example Madrid
xmin = -3.7752605696761523
xmax = -3.5700588517546783
ymin= 40.347562816507136
ymax = 40.49667032936084

# Pixel size in meters
pixelSize <- 500
# Local path
dir <- "D:/Josefina/paper_git/paper_exposure_model/code_08/prueba_salidas/Grillas/Mexico/"

# Date to estimate air exposure
date <- "2024-06-01"

#-- Run example
make_grid(ymin, ymax, xmin,xmax, pixelSize, dir, date)

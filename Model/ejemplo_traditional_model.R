##------ Example

origin <- data.frame(longitude = c(-68.804999),
                     latitude = c(-32.891357))
date <- "2019-08-01"
#directorio local donde estan los archivos .shp
dir_grid <- "/media/alfredo/d326c7b6-4226-42b2-b413-a56dbcc88137/cuem/PROYECTOS/paqueteR/grid_example"

example <- traditional_model (origin_point = origin,date,dir_grid)
#Si genera error,generar una carpeta llamada "temp" dentro del directorio "dir_grid"
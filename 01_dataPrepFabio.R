# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, 
               tidyverse, terra, foreach, fs, future.apply, furrr, fst, glue, compiler)

rm(list = ls())

# Load data --------------------------------------------------
path <- 'inputs/predictions'
spcs <- fs::dir_ls(path, type = 'directory') # lista todas las carpetas type es para los archivos o directories ## ??regexp = '^a'
length(spcs)
print(spcs)


# Making an example for one specie ----------------------------
make_average_reps <- function(sp){
  # sp <- spcs[4]
  cat('Start\n')
  fls <- fs::dir_ls(sp) #lista archivos
  dir <- sp # directorio de trabajo
  spn <- basename(sp) #tiene 4 letras de codigo de especies despues del ultimo _
  nvt <- data.frame(specie = spn, raster = fls) %>%  # crea un df con los datos de cada raster
    as_tibble() %>%  #saca las primeras filas del df
    mutate(name = basename(raster), # crea nuevas columnas
           year = str_sub(name, start = nchar(name) - 7, end = nchar(name) - 4)) # crea el nombre 
  spl <- str_split(pull(nvt, name), pattern = '_') # divide y saca un vector del nombre y corta con base en el _
  nvt <- nvt %>% 
    mutate(run = sapply(1:length(spl), function(i) spl[[i]][2]), # posicion 2  # sapply devuelve un vector que puedo incluir en una columna
           gcm = sapply(1:length(spl), function(i) spl[[i]][1])) # posicion 1 donde empieza el nombre
  
  nvt <- nvt %>% dplyr::select(specie, year, gcm) # nvt va tener 6 columnas pero me interesan solo 3
  dst <- nvt %>% distinct(specie, year, gcm)  #elimina datos duplicados divididos en specie, year, gcm   
  
  cat('To create the average\n')
  
  rsl <- map(.x = 1:nrow(dst), .f = function(k){
    
    ds <- dst %>% slice(k) #slice extraer fila similar a dst[1,]
    sp <- pull(ds, 1) # son objetos tipo caracter
    yr <- pull(ds, 2)
    gc <- pull(ds, 3)
    
    fl <- grep(sp, fls, value = TRUE) %>% #filtro de fls todos los rasters, extraigo solo especie
      grep(yr, ., value = TRUE) %>%  #solo a;o
      grep(gc, ., value = TRUE)  # solo gcm
    
    tr <- terra::rast(fl) #leer como raster comando de terra
    av <- mean(tr) # saca la media 
    ou <- glue('./outputs/{sp}/mean_{sp}_{yr}_{gc}.tif') # crea la ruta del archivo de resultado {sp} cambia al igual el a;o
    dr <- dirname(ou) ## extrae lo primero del ultimo _
    ifelse(!dir.exists(ou), dir.create(dr), print('Folder already exist')) # revisa si existe el file
    cat('To write the final raster\n')
    writeRaster(x = av, filename = ou, overwrite = TRUE) #guarda el raster 
    cat('Done ', k, '\n')
    
    
  })
  
  cat('Done\n') # imprime mensaje en pantalla 
}


# Apply the function -----------------------------------------------------
map(spcs, make_average_reps)





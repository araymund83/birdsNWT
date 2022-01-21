#The aim of this script is to convert the  bird density rasters to a probability 
##of occurrence. 
##
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rcartocolor, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, hrbrthemes, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)


g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Function -----------------------------------------------------------------
get_probOcc <- function(spc){
  
 #spc <- spcs[1] # Run and erase
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- fls <- grep('mean', fls, value = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  
  
  occurRas<- map(.x = 1:length(gcm), .f = function(k){
    message(crayon::green('Loading files for', gcm[k]))
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- as.character(fl)
    stk <- raster::stack(fl)
    dps <- calc(x = stk, fun = function(pxl){1- dpois(x = 0, lambda = pxl)})
    ou <- glue('./outputs/{spc}/occur/occu_{spc}_{gcm}.tif')
    dr <- dirname(name)
    writeRaster(x = dps, filename = ou[k], overwrite = TRUE )

    proOccRas<-  map(.x = 1:length(yrs), .f = function(yr){
      message(crayon::green('Year', yrs[yr]))
      sfl <- grep(yrs[yr], fl, value = TRUE)
      rst <- raster::raster(sfl)
      dps <- calc(x = rst, fun = function(pxl){1- dpois(x = 0, lambda = pxl)})
      out <- glue('./outputs/{spc}/occur')
      ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
      writeRaster(x = dps, 
                  filename = glue('./outputs/{spc}/occur/occu_{spc}_{yrs[yr]}_{gcm[k]}.tif'),
                  overwrite = TRUE)
      cat('Done!\n')
   })
 })
}

# Apply the function ------------------------------------------------------
map(.x = spcs, .f = get_probOcc)

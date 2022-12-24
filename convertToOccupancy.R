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
spcs <- spcs[1:72]

# Function -----------------------------------------------------------------
get_probOcc <- function(spc){
  
 #spc <- spcs[1] # Run and erase
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- fls <- grep('mean', fls, value = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- yrs[2:7]
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  gcm <- gcm[2:4]
  
  
  occurRas<- map(.x = 1:length(gcm), .f = function(k){
    message(crayon::green('Loading files for', gcm[k]))
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- as.character(fl)
    stk <- raster::stack(fl)
    cellArea = 6.25
   # dps <- calc(x = stk, fun = function(pxl){1- dpois(x = 0, lambda = pxl * cellArea)}) # multiply for cell area to produce poccurrence at cell level
    #names(dps)<- yrs
    #ou <- glue('./outputs/{spc}/occur/occu_{spc}_{gcm}_6.25.tif')
    #dr <- dirname(name)
    #writeRaster(x = dps, filename = ou[k], overwrite = TRUE )

    proOccRas<-  map(.x = 1:length(yrs), .f = function(yr){
      message(crayon::green('Year', yrs[yr]))
      sfl <- grep(yrs[yr], fl, value = TRUE)
      rst <- raster::raster(sfl)
      dps <- calc(x = rst, fun = function(pxl){1- dpois(x = 0, lambda = pxl * cellArea)})
      out <- glue('./outputs/{spc}/occur')
      ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
      writeRaster(x = dps, 
                  filename = glue('./outputs/{spc}/occur/occu_{spc}_{yrs[yr]}_{gcm[k]}_6.25.tif'),
                  overwrite = TRUE)
      cat('Done!\n')
   })
 })
}

# Apply the function ------------------------------------------------------
map(.x = spcs, .f = get_probOcc)

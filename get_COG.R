# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(colorspace, furrr, future, future.apply, fs, fst, ggpubr, ggspatial, glue, gridExtra,
               RColorBrewer, raster, rgdal, rgeos, RStoolbox, sf, stringr, terra, tidyverse, trend  )

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]

targetCRS <-  paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                    "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Function to use ---------------------------------------------------------
get_COG<- function(spc){
  
  spc <- spcs[2] # Run and erase
  
  cat('Start ', spc, '\n')
  dir <- grep(spc,dirs, value = TRUE)
  fls <- list.files(dir, pattern = 'mean', full.names = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- yrs[2:7]
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  gcm <- gcm[2:4]
  
  cat('To apply to each gcm\n')
  cog <- map(.x = 1:length(gcm), function(k){
    message(crayon::green('Loading files for', gcm[k]))
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- as.character(fl)
    getYrs <-  map(.x = 1:length(yrs), .f = function(yr){
      message(crayon::green('Year', yrs[yr]))
      sfl <- grep(yrs[yr], fl, value = TRUE)
      rst <- terra::rast(sfl)
  
  }
    }
  
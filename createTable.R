# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(parallel, foreach, doSNOW,raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, terra, stringr, glue, sf, tidyverse, 
               RStoolbox, fs, fst, trend, colorspace, hrbrthemes,exactextractr, furrr, future, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './tables/qs'
files <- list.files(root, pattern = '*.qs')
spcs

spcs <- str_sub(string = basename(files), 1, 4)
prd <- unique(prd)

## to read qs
table<- qs::qread(file = glue('./tables/qs/{spc}_mean_tbl.qs'))


dataList <- lapply(files, function(x) qs::qread(x))

spcs <- basename(dirs)

create_table <- function(spc){
  
  spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('To start ', spc, '\n')
  cat('-------------------------------------------------------------\n')
  
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./tables/qs/{spc}_mean_tbl.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  
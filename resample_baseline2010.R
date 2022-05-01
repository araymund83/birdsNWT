# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Resampling baseline files 
  spc <- spcs[1] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- grep('mean', fls, value = TRUE)
  
  
  cat('resampling\n')
  baseline_files <- list.files('./outputs',pattern = '_baseline.tif$', 
                               full.names = TRUE, recursive = TRUE)
  baseline_files<- lapply(baseline_files, raster)
  nfile <- length(baseline_files)
  r1 <- raster(baseline_files[i])
  reference<- raster('./outputs/ALFL/mean_ALFL_2031_CanESM2.tif')
  # Resampling data
  rs<- list(reference)
  for (i in 1:length(baseline_files)) {
    rs[[i]] <- raster::resample(baseline_files[[i]], reference, method = "bilinear")
    ou <- glue('./outputs/resample/mean_{sp}_{yr}_{gc}.tif')
    writeRaster (rs[[i]], filename = names(rs[[1]]) )
  }
  name<- names(rs[[]])
  for (i in 1:length(rs)){writeRaster(rs[[i]],  filename =glue(names(rs[[i]]),'resample'),
                                      format= 'GTiff', overwrite = TRUE)}


reference<- raster('./outputs/ALFL/mean_ALFL_2031_CanESM2.tif')
ALFL_2010<- raster('./outputs/ALFL/mean_ALFL_2010_baselineresample.tif')

chg1031<-overlay(ALFL_2010,reference, fun = function(r1,r2){return(r1-r2)})



root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
fls <- fs::dir_ls(dir, regexp = '.tif$')
fls <- grep('mean', fls, value = TRUE)
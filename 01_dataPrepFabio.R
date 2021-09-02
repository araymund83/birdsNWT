# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(raster, rgdal, rgeos, readxl, stringr, sf, tidyverse, terra, foreach, fs)

# Load data --------------------------------------------------
path <- 'inputs/predictions'
spcs <- fs::dir_ls(path, type = 'directory')
length(spcs)
print(spcs)

# Making an example for one specie ----------------------------
make_average_reps <- function(sp){
  
  sp <- spcs[1]
  cat('Start\n')
  fls <- fs::dir_ls(sp)
  dir <- sp
  spn <- basename(sp)
  nvt <- data.frame(specie = spn, raster = fls) %>% 
    as_tibble() %>% 
    mutate(name = basename(raster),
           year = str_sub(name, start = nchar(name) - 7, end = nchar(name) - 4))
  spl <- str_split(pull(nvt, name), pattern = '_')
  nvt <- nvt %>% 
    mutate(run = sapply(1:length(spl), function(i) spl[[i]][2]),
           gcm = sapply(1:length(spl), function(i) spl[[i]][1]))
  
  nvt <- nvt %>% dplyr::select(specie, year, run, gcm)
  dst <- nvt %>% distinct(specie, year, gcm)         
  basename(nvt$raster)
  nvt
  
  cat('Done\n')
}

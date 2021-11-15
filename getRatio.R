# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(parallel, foreach, doSNOW,raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, terra, stringr, glue, sf, tidyverse, 
               RStoolbox, fs, fst, trend, colorspace, hrbrthemes,exactextractr, furrr, future, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('inputs/ecoregions/ecoregions.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)
ecrg_limt <- sf::st_intersection(x = ecrg, y = limt)
plot(st_geometry(ecrg_limt))
plot(st_geometry(limt))


calcul_change91_11 <- function (spc) {
  
  spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('To start ', spc, '\n')
  cat('-------------------------------------------------------------\n')
  
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/tbl_yrs_{spc}.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
  
  tbl <- mutate(tbl, change = y2091 - y2011)
  
  cat('To estimate the change (ratio), initial and final year\n')
  tbl <- mutate(tbl, ratio = ((y2091 - y2011) / y2011 ) * 100) #change 2100 for 2091
  std <- tbl %>% group_by(gc) %>% summarise(std = sd(ratio)) %>% ungroup()
  tbl <- map(.x = 1:3, .f = function(i){
    st <- std %>% filter(gc == gcm[i]) %>% pull(std)
    st <- st / 4
    tb <- tbl %>% 
      filter(gc == gcm[i]) %>% 
      mutate(rt_bn = case_when(ratio >= st * -1 & ratio <= st ~ 'None',
                               ratio > st ~ 'Positive',
                               ratio < st * -1 ~ 'Negative'))
  })
  tbl <- bind_rows(tbl)
  
  
}
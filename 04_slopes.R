# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(parallel, foreach, doSNOW,raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, terra, stringr, glue, sf, tidyverse, 
               RStoolbox, fs, fst, trend, colorspace, hrbrthemes,exactextractr, furrr, future, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())


###Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

 


calcSlopes<- function(spc){
#spc <- spcs[7]
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

cat('Table to raster\n')
rst <- map(.x = 1:length(gcm), .f = function(k){
  
  cat(k)
  sub <- tbl %>% 
    filter(gc == gcm[k]) %>% 
    dplyr::select(lon, lat, y2011:y2100)
  
  rsr <- map(.x = 3:ncol(sub), .f = function(z){
    sub %>% dplyr::select(1, 2, z) %>% rasterFromXYZ()
  })
  
  rsr <- raster::stack(rsr)
  cat('Done\n')
  return(rsr)

})

cat('To calculate the slopes\n')
plan(cluster, workers = 3, gc = TRUE)
options(future.globals.maxSize= 4194304000) ## this option helps with  the error about global sizes 
slpe <- furrr::future_map(.x = 1:length(rst), .f = function(k){
  library(spatialEco); library(raster)
  cat('Start\n')
  slp <- raster.kendall(x = rst[[k]], p.value = TRUE)
  raster::writeRaster(x = slp[[1]], filename = glue('./outputs/{spc}/slp_{gcm[k]}.tif'), overwrite = TRUE)
  raster::writeRaster(x = slp[[2]], filename = glue('./outputs/{spc}/pvl_{gcm[k]}.tif'), overwrite = TRUE)
  cat('Done\n')
  return(slp)
})
future:::ClusterRegistry('stop')


slpe.tble <- map(.x = 1:length(slpe), .f = function(k){
  cat(k, '\n')
  rsl <- slpe[[k]] %>% 
    rasterToPoints(., spatial = FALSE) %>% 
    as_tibble() %>% 
    mutate(model = gcm[k]) %>% 
    setNames(c('x', 'y', 'slp', 'pvl', 'model')) %>% 
    mutate(model = gcm[k])
  return(rsl)
})

slpe.tble <- bind_rows(slpe.tble)

cat('To make the map\n')
gslp <- ggplot() + 
  geom_tile(data = slpe.tble, aes(x = x, y = y, fill = slp)) + 
  facet_wrap(.~model, ncol = 3, nrow = 1) +
  scale_fill_binned_sequential(palette = 'blues') + 
  theme_void() +
  coord_sf() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line')) +
  labs(x = 'Lon', y = 'Lat', fill = 'Slope')

gpvl <- ggplot() + 
  geom_tile(data = slpe.tble, aes(x = x, y = y, fill = pvl)) + 
  facet_wrap(.~model, ncol = 3, nrow = 1) +
  scale_fill_binned_sequential(palette = 'ag_GrnYl', rev = FALSE, breaks = c(0.05, 0.25, 0.5, 0.75)) + 
  theme_void() +
  coord_sf() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line')) +
  labs(x = 'Lon', y = 'Lat', fill = 'p-value')


gall <- ggarrange(gslp, gpvl, ncol = 1, nrow = 2)

ggsave(plot = gall, 
       filename = glue('./graphs/maps/{spc}_slp_pvl.png'), 
       units = 'in', width = 13, height = 10, dpi = 300)

cat('------------------------------------------------------------------------------------------------------\n')
cat('------------------------------------------------- Done -----------------------------------------------\n')
cat('------------------------------------------------------------------------------------------------------\n')

}


# Apply the function ------------------------------------------------------

map(.x = spcs[7:8], .f = calcSlopes)





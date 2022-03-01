# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra,
               hrbrthemes, colorspace, exactextractr)

rm(list = ls())


# Load data ---------------------------------------------------------------
fles <- dir_ls('./qs', regexp = 'table_ratio')

limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('inputs/ecoregions/EcoRegions_NWT_gov/ecoRegionsNT1_BCR6.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)

shpf <- as(ecrg, 'Spatial')
colnames(shpf@data)
znes <- unique(shpf@data$ECO3_NAM_1)
znes <- tibble(name = znes, value = 1:length(znes))
shpf <- st_as_sf(shpf)
shpf <- inner_join(shpf, znes, by = c('ECO3_NAM_1' = 'name'))
shpf <- as(shpf, 'Spatial')
dssl <- aggregate(shpf, 'value')
plot(dssl)
dssl <- st_as_sf(dssl)
dssl <- inner_join(dssl, znes, by = c('value' = 'value'))
                   


# Function ----------------------------------------------------------------
get_max_min <- function(fle){
  
  #fle <- fles[1]
  cat('Start\n')
  spc <- str_sub(basename(fle), 1, 4)
  qst <- qs::qread(fle)
  gcm <- unique(qst$gc)
  
  rsl <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    tbl <- filter(qst, gc == gcm[k])
    tbl <- dplyr::select(tbl, lon, lat, logRatio)
    rst <- rasterFromXYZ(tbl)
    # plot(rst) # Run and erase
    crs(rst) <- targetCRS
    znl.avg <- exactextractr::exact_extract(rst, dssl, 'mean')
    znl.sdt <- exactextractr::exact_extract(rst, dssl, 'stdev')
    dfm <- data.frame(region = pull(dssl, 'name'), average = znl.avg, sdt = znl.sdt)
    
    dfm <- as_tibble(dfm)
    dfm <- mutate(dfm, model = gcm[k])
    cat('Done ', gcm[k], '\n')
    return(dfm)
    
  })
  rsl <- bind_rows(rsl)
  qs::qsave(x = rsl, file = glue('./qs/zonal/{spc}_logZonal2.qs'))
  cat('Done!\n')
  return(rsl)
}

# Apply the function ------------------------------------------------------
cat('Calculating zonal\n')
plan(cluster, workers = 3, gc = TRUE)
options(future.globals.maxSize= 4194304000) ## this option helps with  the error about global sizes 
znl_all <- furrr::future_map(.x = 1:length(fles), .f = function(i){
  cat('Start\n')
  znl <- get_max_min(fle = fles[i])
  cat('Done!\n')
  return(znl)
})
future:::ClusterRegistry('stop')                       
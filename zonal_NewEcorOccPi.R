# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra,
               hrbrthemes, colorspace, exactextractr)

rm(list = ls())


# Load data ---------------------------------------------------------------
path <- './qs/occurpi'
fls <- list.files(path, pattern = '2011-2031', full.names = TRUE)

limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('inputs/ecoregions/NWT_ecoregions_dissolvec.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Function ----------------------------------------------------------------
get_mean_zonal <- function(fle){
  
  fle <- fls[1]
  cat('Start\n')
  spc <- str_sub(basename(fle), start = 25, end = -4)
  qst <- qs::qread(fle)
  gcm <- unique(qst$gc)
  
  rsl <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    tbl <- filter(qst, gc == gcm[k])
    tbl <- dplyr::select(tbl, lon, lat, change)
    rst <- rasterFromXYZ(tbl)
    # plot(rst) # Run and erase
    crs(rst) <- targetCRS
    znl.avg <- exactextractr::exact_extract(rst, ecrg, 'mean')
    znl.sdt <- exactextractr::exact_extract(rst, ecrg, 'stdev')
    dfm <- data.frame(region = ecrg$region, average = znl.avg, sdt = znl.sdt)
    
    dfm <- as_tibble(dfm)
    dfm <- mutate(dfm, model = gcm[k],
                  specie = spc)
    cat('Done ', gcm[k], '\n')
    return(dfm)
    
  })
  rsl <- bind_rows(rsl)
  out <- ('./qs/zonalOccPi')
  ifelse(!dir.exists(out), dir.create(out, recursive = TRUE),'folder already exists')
  qs::qsave(x = rsl, file = glue('{out}/{spc}_occPiZonal1131.qs'))
  cat('Done!\n')
  return(rsl)
}

# Apply the function ------------------------------------------------------
cat('Calculating zonal\n')
future::plan(cluster, workers = 3, gc = TRUE)
options(future.globals.maxSize= 4194304000) ## this option helps with  the error about global sizes 
znl_all <- furrr::future_map(.x = 1:length(fls), .f = function(i){
  cat('Start\n')
  znl <- get_mean_zonal(fle = fls[i])
  cat('Done!\n')
  return(znl)
})
future:::ClusterRegistry('stop')                       

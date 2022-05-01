require(pacman)
pacman::p_load(parallel, foreach, doSNOW,raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, stringr, glue, sf, tidyverse, fasterize,
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

ecrg_geog <- sf::st_transform(x = ecrg, crs = targetCRS)
ca_geo <- sf::st_transform(x = ca_prov, crs = targetCRS)

ecrg_geog %>% st_geometry() %>% plot
plot(ecrg_geog['ECO3_NAM_1'])


# Function to use ---------------------------------------------------------
get_diff <- function(spc){
  
  #spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('To start ', spc, '\n')
  cat('-------------------------------------------------------------\n')
  
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl_base <- qs::qread(file = glue('./outputs/{spc}/tbl_2010_base72_{spc}.qs'))
  tbl_base <- dplyr::select(tbl_base, x, y, y2010)
  tbl_yrs <- qs::qread(file= glue('./outputs/{spc}/tbl_yrs_{spc}.qs'))
  gcm <- unique(tbl_yrs$gc)
  
  cat('Apply to each gcm\n')
  dfm <- map(.x = 1:length(gcm), function(k){
    
    tbl_base <-  tbl_base %>% group_by(x, y)
    tbl_yrs <- tbl_yrs %>% select(x, y, y2031, y2091, gc) %>% group_by(x, y)
    tbl_diff <- full_join(tbl_yrs, tbl_base, by_group = TRUE)
    tbl_diff <- na.omit(tbl_diff)
    tbl_diff <- tbl_diff %>% select(x, y, y2010,y2031, y2091, gc)
    
    return(tbl_diff)
  })
  rsl <- bind_rows(dfm)
  #browser
  #fst::write_fst(x = rsl, path = glue('./outputs/{spc}/tbl_yrs_{spc}.fst')) ## saving with .fst creates very big files! 
  qs::qsave(x = rsl, file = glue('./outputs/{spc}/tbl_basediff_{spc}.qs'))
  
  cat('------- Done -------\n')
  return(rsl)
  }
### Raster to table ---------------------------------------------------------
dfrm <- map(.x = spcs, .f = get_diff)
dim(dfrm)
object.size(dfrm)
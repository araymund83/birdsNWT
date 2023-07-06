
# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, qs, sf, fasterize, tidyverse, 
               fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#st_write(ecrg_limt, './inputs/NWT_ecoregion.gpkg')

# Load data ---------------------------------
mask <- raster('./outputs/1km/mean_ALFL_2011_CanESM2_1km.tif') # Leer el raster de los poligonos 
crs(mask) <- targetCRS
plot(st_geometry(ecrg))
shpf <- st_read('inputs/ecoregions/NWT_ecoregions_dissolvec.shp') # Leer el shape de las ecoregiones
shpf$gid <- 1:nrow(shpf)
limt <- sf::st_transform(x = shpf, crs = targetCRS)
ecrg <- sf::st_transform(x = shpf, crs = targetCRS)

# Fasterize 
mask <- mask * 0 
fstr <- fasterize::fasterize(ecrg, mask, field = 'gid')
#writeRaster(fstr, './inputs/polyRasNWTIII.tif') #saves the raster with the polygons 
znes <- sort(as.numeric(na.omit(unique(fstr[]))))
Nsample <- 20000
# Getting the sample n
cntr <- fstr %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'value')) %>% 
  group_by(value) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%
  mutate(porc = count / sum(count) * 100, 
         n = (porc * Nsample) / 100)
nmrs <- cntr
head(cntr)

# Get the cell IDS for each polygon ---------------------------------------
test <- fstr
# check for NA's 
table(is.na(test[]))
test[is.na(test[])]<- 999
rast[is.na(rast[])]<- 999
plot(rast,colNA = 'red')
plot(rast)
#Extract which cells are within each polygon 
rast <- terra::crop(fstr, ecrg, mask = T)
df<- exact_extract(rast, ecrg, include_xy = TRUE)
# convert list to a dataframe 
cellsDF <- df %>% map_df(as_tibble)  
cellsDF <- cellsDF %>% rename(region = value) %>% 
  mutate(pixelID = 1:nrow(cellsDF)) 
test<- cellsDF %>% dplyr::select(region, x, y, pixelID) %>% drop_na(region)

region1_df<- test %>% filter(region == 1) # 189553 cells
region2_df<- test %>% filter(region == 2) # 170835 cells
region3_df<- test %>% filter(region == 3) # 175430 cells


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
mask <- raster('./outputs/ALFL/mean_ALFL_2011_CanESM2.tif') # Leer el raster de los poligonos 
crs(mask) <- targetCRS
shpf <- st_read('./inputs/NWT_ecoregion.gpkg') # Leer el shape de las ecoregiones
shpf$gid <- 1:nrow(shpf)
crs(mask) <- targetCRS

# Fasterize 
mask <- mask * 0 
fstr <- fasterize::fasterize(shpf, mask, field = 'gid')
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
test[which(test[] != 1)] <- NA
test
plot(test)

temp<- raster::extract(test, rasterToPoints(test)[,1:2], cellnumbers = TRUE)
clss<- sample_n(tbl = as.data.frame(temp), size = 37, replace = FALSE)
test[!clls] <- NA
plot(test)

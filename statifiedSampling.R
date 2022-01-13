# Load libraries ----------------------------------------------------------
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

##load a raster to be used as a 

plot(ras)
plot(st_geometry(ecrg_limt), add = TRUE)
ecrg_limt$polyID <- as.factor(ecrg_limt$REGION_NAM)
##names of ecoregions
ecrgDF <- data.frame(className = unique(ecrg_limt$REGION_NAM), classValue = 1:30)
#now ratify (RAT = 'Raster Attribute Table)
tmp <- ras
tmp[] <- NA
tmp[] <- 1:ncell(ras)
ecrg_limt$polyID <- as.factor(ecrg_limt$REGION_NAM)
ecrg_limt <- st_cast(ecrg_limt, 'MULTIPOLYGON')
polyRas <- fasterize::fasterize(ecrg_limt, tmp, field = 'polyID')

tmp <- ratify(polyRas)
rat <- levels(polyRas)[[1]]

#sampling
smp <- sampleStratified(polyRas, size = 200, na.rm = TRUE, sp = TRUE)
## number of samples in each class 
table(smp$layer)

#plot the sample sites 
library(rasterVis)
plt<- levelplot(tmp, col.regions = main = 'sample sites')

##https://rspatial.org/raster/rs/5-supclassification.html
# Extract the layer values for the locations
sampvals <- extract(landsat5, samp2011, df = TRUE)
# sampvals no longer has the spatial information. To keep the spatial information 
#you use `sp=TRUE` argument in the `extract` function.
# drop the ID column
sampvals <- smp[, -1]
# combine the class information with extracted values
sampdata <- data.frame(classvalue = smp@data$layer, sampvals)

##implementing proportional stratified sampling 
library(dplyr)
library(tidyr)
cellsDF <- cellsDF |> mutate(cellID = 1:nrow(cellsDF))
##convert the cell values into a wide format to use 'stratified.R'
cellsDF %>% 
  pivot_wider(names_from = polyID, values_from = polyID)


 Nsamp <- 20000
 Npix<- ncell(polyRas)
 nSamp <- 
 

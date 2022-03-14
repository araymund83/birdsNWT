# Load libraries ----------------------------------------------------------
library(tidyverse)
library(sf)
library(mapview)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

#limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 
ecrg <- sf::st_read('inputs/ecoregions/EcoRegions_NWT_gov/ecoRegionsNT1_BCR6.shp')
#cntr <- sf::st_read('inputs/world/all_countries.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# To make the map ---------------------------------------------------------
limt_geog <- sf::st_transform(x = limt, crs = st_crs(4326))
ecrg_geog <- sf::st_transform(x = ecrg, crs = targetCRS)

ecrg_geog %>% st_geometry() %>% plot
plot(ecrg_geog['ECO3_NAM_1'])

# usign ggplot 
# simplest plot
ggplot(ecrg_geog) 
# plot the regiones and fill each polygon based on the Level 3 classification
ggplot(ecrg_geog) + geom_sf(aes(fill = ECO3_NAM_1))

map <- mapview::mapview(ecrg_geog, zcol = 'ECO3_NAM_1')

#define colors for a palette
pal <- viridis::magma(n = length(unique(ecrg_geog$ECO3_NAM_1)), direction = -1)

mapSA <- mapview::mapview(ecrg_geog, 
                        map.types = c('Esri.WorldShadedRelief','OpenStreetMap.DE'),
                        zcol = 'ECO3_NAM_1', 
                        col.regions = RColorBrewer::brewer.pal(5, 'Dark2'),
                        legend = TRUE)
##Using tmap for static maps code from https://geocompr.robinlovelace.net/adv-map.html
#https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html
# Add fill layer to nwt shape
tm_shape(ecrg) +
  tm_fill() 
# Add border layer to nwt shape
tm_shape(ecrg) +
  tm_borders() 
# Add fill and border layers to nwt shape
map_nwt<- tm_shape(ecrg)+ 
          tm_fill(col = 'ECO3_NAM_1', title = 'Ecoregions Level 3', 
                  palette = 'Dark2', alpha = 0.7) +
          tm_borders() 

map_nwt <- map_nwt + 
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.5, position = c('right', 'bottom'))+
  tm_style('classic') +
  tm_graticules()
##save the map 
tmap_save(tm = map_nwt, filename = './inputs/studyAreaMap_classic.png')

map_nwt + tm_basemap(server = "OpenStreetMap", alpha = 0.5)+
  tmap_options(check.and.fix = TRUE)


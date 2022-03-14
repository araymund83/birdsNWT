# Load libraries ----------------------------------------------------------
library(sf) # the base package manipulating shapes
library(rgdal) # geo data abstraction library
library(geojsonio) # geo json input and output
library(spdplyr) # the `dplyr` counterpart for shapes
library(rmapshaper) # the package that allows geo shape transformation
library(magrittr) # data wrangling
library(dplyr)
library(tidyr)
library(ggplot2)
library(tmap)
library(mapview)
library(grid)
library(cowplot)

#limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 
ecrg <- sf::st_read('inputs/ecoregions/EcoRegions_NWT_gov/ecoRegionsNT1_BCR6.shp')
ca_prov <- st_read('./inputs/canada_cd_sim.geojson')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# To make the map ---------------------------------------------------------
limt_geog <- sf::st_transform(x = limt, crs = st_crs(4326))
ecrg_geog <- sf::st_transform(x = ecrg, crs = targetCRS)
ca_geo <- sf::st_transform(x = ca_prov, crs = targetCRS)

ecrg_geog %>% st_geometry() %>% plot
plot(ecrg_geog['ECO3_NAM_1'])

# usign ggplot 
# simplest plot
ggplot(ecrg_geog) 
# plot the regiones and fill each polygon based on the Level 3 classification
ggplot(ecrg_geog) + geom_sf(aes(fill = ECO3_NAM_1))



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
map_nwt<- tm_shape(ecrg_geog)+ tm_polygons() + 
          tm_layout(title = 'Ecoregions Northwest Territories')
          tm_fill(col = 'ECO3_NAM_1', title = 'Ecoregions Level 3', 
                  palette = 'Dark2', alpha = 0.7) +
          tm_borders() 

map_nwt <- map_nwt + 
  tm_compass(type = "arrow", size = 1.2, position = c("left", "bottom"), text.size = 0.6) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 0.5, position = c('right', 'bottom'))+
  tm_style('classic') +
  tm_graticules()
##save the map 
tmap_save(tm = map_nwt, filename = './inputs/studyAreaMap_classic.png')

map_nwt + tm_basemap(server = "OpenStreetMap", alpha = 0.5)+
  tmap_options(check.and.fix = TRUE)

##CREATING AN INSET
# compute the bounding box for the Canada
ca_box <- st_bbox(ca_geo ) %>%
  st_as_sfc() 

bigmap <- map_nwt +
  tm_shape(ca_box) +  #here we are adding the little inset box on the overview map
  tm_borders(lwd = 3)
bigmap

##get map for whole canada from :https://cran.r-project.org/web/packages/mapcan/vignettes/choropleth_maps_vignette.html
library(mapcan)
mapcan(boundaries = province,
       type = standard) %>%
  head()

pr_map <- mapcan(boundaries = province,
                 type = standard) %>%
  ggplot(aes(x = long, y = lat, group = group))


pr_map <- pr_map +
  geom_polygon() +
  coord_fixed()

##create an inset map 
#define the are of interest 
mapcan(boundaries = province,
       type = standard) %>%
  head()
pr_map <- mapcan(boundaries = province,
                 type = standard) %>%
  ggplot(aes(x = long, y = lat, group = group))
pr_map
pr_map <- pr_map +
  geom_polygon() +
  coord_fixed()

pr_map +
  theme_mapcan() +
  ## Add a title
  ggtitle("Map of Canada with Provincial/Territorial Boundaries")

pr_geographic <- mapcan(boundaries = province,
                        type = standard)
pr_geographic$
pr_geographic <- st_as_sf(pr_geographic, coords = c('long', 'lat'))

pr_geographic %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() +
  scale_fill_viridis_c(name = "Population") +
  ggtitle("Canadian Population by Province")

NWT_ridings <- mapcan(boundaries = ridings,
                     type = standard,
                     province = NT)
ggplot(NWT_ridings, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() +
  ggtitle("British Columbia \nFederal Electoral Ridings")

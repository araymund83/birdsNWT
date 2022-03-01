# Load libraries ----------------------------------------------------------
library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(sf)

limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 
ecrg <- sf::st_read('inputs/ecoregions/EcoRegions_NWT_gov/ecoRegionsNT1_BCR6.shp')
cntr <- sf::st_read('inputs/world/all_countries.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# To make the map ---------------------------------------------------------
limt_geog <- sf::st_transform(x = limt, crs = st_crs(4326))
ecrg_geog <- sf::st_transform(x = ecrg, crs = st_crs(4326))
cntr_geog <- sf::st_transform(x = cntr, crs = st_crs(4326))

world <- getMap(resolution = "low")

(with_world <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    coord_quickmap() +  # Prevents stretching when resizing
    theme_classic() +  # Remove ugly grey background
    xlab =("Longitude") +
    ylab= ("Latitude") 
   )


gwrl <- ggplot() + 
  geom_sf(data = cntr_geog, fill = NA, col = 'grey50') +
  xlab = ('Longitude') + ylab = ('Latitude')

ecrg_simp_shp <- ecrg %>% st_simplify(dTolerance = 1e-03)

ecrg_simp_shp %>% ggplot(fill =NA) + geom_sf() +
  theme_minimal() +
  xlab('Longitude')+
  ylab('Latitude')

clipper_ca <- as(extent(-141.006, -52.61889, 41.67693, 83.11042), 'SpatialPolygons')
proj4string(clipper_ca) <- CRS(proj4string(world))
world_clip <- raster::intersect(world, clipper_ca)
world_clip_f <- fortify(world_clip)

(map <- ggplot() + 
    geom_polygon(data = world_clip_f, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    theme_minimal() +
    xlab("Longitude") +
    ylab("Latitude") + 
    coord_quickmap())

shpdata_NWT <- spTransform(ecrg_shp, CRS("+proj=longlat +datum=WGS84"))

(map_NWT <- ggplot() +
    geom_polygon(data = shpdata_NWT,
                 aes(x = X, y = Y, group = group fill = as.factor('ID')),
                 color = "gray90", size = 0.5) +
    theme_classic() +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank()) + 
    xlab("Longitude") +
    ylab("Latitude") + 
    coord_quickmap())
library('ggmap')

nwt_basemap <- get_map(location=c(lon = -120, lat = 60), zoom=11, maptype = 'terrain-background', source = 'stamen')

ggmap(ph_basemap)


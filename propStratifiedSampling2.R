
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
plot(test,colNA = 'red')
test[which(test[] != NA )] <- NA
test
plot(test)
#Extract which cells are within each polygon 
df<- exact_extract(fstr, ecrg, include_xy = TRUE)
# convert list to a dataframe 
cellsDF <- df %>% map_df(as_tibble)  
cellsDF <- cellsDF %>% mutate(pixelID = 1:nrow(cellsDF),
                              region = value) 
test<- cellsDF %>% drop_na(value) %>%  dplyr::select(region, x, y, pixelID)
 
region1_df<- test %>% filter(region == 1) # 3003976 cells
region2_df<- test %>% filter(region == 2) # 2714243 cells
region3_df<- test %>% filter(region == 3) # 2793494 cells



CCSM4_r1_2011 <- left_join( region1_df,CCSM4_2011,by = 'pixelID')
CCSM4_r2_2011 <- left_join( region2_df, CCSM4_2011,by = 'pixelID')
CanESM2_region3 <- left_join( region3_df, CCSM4_2011,by = 'pixelID')

NA_r1<- left_join(as.data.frame(b), test, by = 'pixelID')


CCSM4_r1_2011 <- CCSM4_r1_2011 %>%   mutate(pixelID = paste0('S_', pixelID))
colnames(CCSM4_r1_2011) <- gsub('2011_', '', colnames(CCSM4_r1_2011))
CCSM4_r1_2011 <- subset(CCSM4_r1_2011, select= -c(BARS,NOFL,PIWO))
CCSM4_r1_2011 <- as_tibble(CCSM4_r1_2011)
CCSM4_r1_2011 <- CCSM4_r1_2011 %>% remove_rownames()
CCSM4_r1_2011<-  column_to_rownames(CCSM4_r1_2011, var = 'pixelID')
colnames(CCSM4_r1_2011) <- gsub('2011_', '', colnames(CCSM4_r1_2011))
CCSM4_r1_2011<- CCSM4_r1_2011 %>% dplyr::select(-c(region, x, y))
CCSM4_r1_2011 <- subset(CCSM4_r1_2011, select= -c(BARS,NOFL,PIWO))

CanESM2_2011 <- as_tibble(CanESM2_2011)
CanESM2_2011 <- CanESM2_2011 %>% remove_rownames()
CanESM2_2011<-  column_to_rownames(CanESM2_2011, var = 'pixelID')
test <- as.matrix(CanESM2_2011)

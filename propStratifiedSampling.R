# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#st_write(ecrg_limt, './inputs/NWT_ecoregion.gpkg')

# Load data ---------------------------------
nmrs <- read_csv('./inputs/stratifiedSampling.csv')[,1:4] # Leer la tabla de las proporciones de area por pixel 
mask <- raster('./outputs/ALFL/mean_ALFL_2011_CanESM2.tif') # Leer el raster de los poligonos 
crs(mask) <- targetCRS
mask <- mask * 0
shpf <- st_read('./inputs/NWT_ecoregion.gpkg') # Leer el shape de las ecoregiones
shpf$gid <- 1:nrow(shpf)

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


# Function to use ----------------------------
make_sample <- function(zne){
  
  #zne <- znes[1]
  cat('Start ', zne, '\n')
  nmr <- filter(nmrs, value == zne)
  pxl <- ceiling(nmr$n)
  lim <- filter(shpf, gid == zne)
  lim <- as(lim, 'Spatial')
  rst <- raster::crop(fstr, lim)
  rst <- raster::mask(rst , lim)
  tbl <- rasterToPoints(rst, spatial = FALSE)
  tbl <- as_tibble(tbl)
  
  cat('To replicate 10000 times\n')
  dfm <- map(.x = 1:10000, .f = function(k){
    cat('Replicating :', k, '\n')
    rsl <- slice_sample(tbl, n = pxl)
    colnames(rsl) <- c('x', 'y', 'PolyID')
    rsl <- mutate(rsl, rep = k)
    return(rsl)
  })
  
  dfm <- bind_rows(dfm)
  cat('To write the result\n')
  out <- glue('./qs/sample')
  qsave(x = dfm, file = glue('{out}/PolyID_{zne}.qs'))
  cat('Done!\n')
  return(dfm)
}
# Apply the function ------------------------------------------------------
smpls <- map(.x = znes, .f = make_sample)

# Read the results --------------------------------------------------------
smpls <- dir_ls('./qs/sample')
smpls
smpls <- map(smpls, qread)

nrw <- map(smpls, nrow) %>% unlist() %>% as.numeric() 
which(nrw == 0)
dir_ls('./qs/sample')[which(nrw == 0)]

miss <- parse_number(dir_ls('./qs/sample')[which(nrw == 0)])



# Masking
cntr <- fstr %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'value')) %>% 
  group_by(value) %>% 
  summarise(count = n()) %>% 
  ungroup() 

as.data.frame(cntr)

# Intersection 
mask_poly <- raster::rasterToPolygons(mask)

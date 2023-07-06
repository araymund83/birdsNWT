# Load libraries  ---------------------------------------------------------
library(pacman)
p_load( dplyr,fs, ggplot2, glue,qs,sf,tidyverse)

# Load data ---------------------------------------------------------------
path <- './tables/coGpi/'
fles <- list.files(path, pattern = 'coGpi')
fles <- fles[1:72]
spcs <- str_sub(basename(fles), 1, 4)
spcs <- spcs[1:72]
yrs <- c('2011', '2031','2091')
gcm <- c('CanESM2', 'CCSM4', 'INM-CM4')
targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Make graph function -----------------------------------------------------
get_dist_coG <- function(spc){
  #spc <- spcs[6]
  cat(spc, '\t')
  fle <- grep(spc, fles, value = TRUE)
  fle <- as.character(fle)
  tbl <- qs::qread(glue ('./tables/coGpi/{spc}_coGpi.qs'))
  tbl <- tbl %>% group_by(model) %>% mutate(year = as.factor(year))
  tbl<- tbl %>% filter(year %in% c('2011', '2031', '2091')) %>% 
    rename('lon' = 'COGx', 'lat' = 'COGy') %>% 
    dplyr::select(year, lon, lat, species, model)
  gcm <- unique(tbl$model)
  cogDistBear <- map(.x = 1:length(gcm), .f = function(k){
    message(crayon::green('Loading files for', gcm[k]))
    df <- tbl %>% filter(model == gcm[k])
    cogDF <- st_as_sf(df, coords = c('lon','lat'), crs = targetCRS)
    cogDF <- cogDF %>% st_transform(crs = 4326) 
    #extract the lat lon vectors for each of the points from the geometry
    cogDF <- as_tibble(cogDF) %>% mutate(lon =unlist(map(cogDF$geometry, 1)),
                              lat = unlist(map(cogDF$geometry,2)))
    points <- as_tibble(cogDF) %>% dplyr::select(year, species, model, lat, lon) %>% 
      mutate(distance = geosphere::distVincentyEllipsoid(cbind(lon, lat),
                                                         cbind(lag(lon), lag(lat)))/1000,
             distancefrom2011 = geosphere::distVincentyEllipsoid(cbind(first(lon), first(lat)),
                                                           cbind((lon), (lat)))/1000,
             bearing = geosphere::bearing(cbind(lon, lat),
                                          cbind(lag(lon), lag(lat))),
             bearingfrom2011 = geosphere::bearing(cbind(first(lon), first(lat)),
                                                  cbind(lon, lat)),
             course = (bearing + 360) %% 360, ##add full circle and determine module for 360
             coursefrom2011 = (bearingfrom2011 + 360) %% 360) # https://stackoverflow.com/questions/51030060/in-geosphere-package-in-r-why-arent-bearings-in-0-360-degrees
    #https://stackoverflow.com/questions/31838855/how-do-i-easily-convert-a-line-angle-to-a-navigational-bearing-scale-i-e-with
             return(points)
  })
  rsl<- bind_rows(cogDistBear)
  out <- glue('./tables/distBear_coGpi')
  ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
  qs::qsave(x =rsl, file = glue('{out}/{spc}_distBear_coGpi.qs'))
  return(rsl)
}
# Apply the function ------------------------------------------------------
distCogTable<- map(.x = spcs, .f = get_dist_coG)
qs::qsave(x =distBearCogTable, file = glue('./tables/distBear_coGpi/distBear_coGpi_allspp.qs'))


# Read all dist COG tables  ----------------------------------------------------
read_table <- function(specie){
   specie <- spcs[2]
  message(crayon::blue('Starting\n', specie, '\n'))
  path <- glue('./tables/distance_coG')
  table <- qs::qread(file = glue('{path}/{specie}_distance_coG.qs'))
  cat('Done \n')
  return(table)
}  

YRWA <- qs::qread(file = glue('./tables/distBear_coGpi/YRWA_distBear_coGpi.qs'))



# Load libraries  ---------------------------------------------------------
library(pacman)
p_load( dplyr,fs, ggplot2, glue,qs,sf,tidyverse)
g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]

# Read all COG tables  ----------------------------------------------------
read_table <- function(specie){
  specie <- spcs[1]
  message(crayon::blue('Starting\n', specie, '\n'))
  path <- glue('./tables/coGpi')
  table <- qs::qread(file = glue('{path}/{specie}_coGpi.qs'))
  cat('Done \n')
  return(table)
}  
# Apply the function ------------------------------------------------------
# coG_Table <- map(.x = spcs, .f = read_table)
# coG_dist_all_Table <- bind_rows(coG_Table)  
# 
# 
out <-'./tables/distBear_coGpi'
# ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
# qs::qsave(coG_dist_all_Table, glue('{out}/coGpiDistBearTable_allsp.qs'))



limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 
ecrg <- sf::st_read('inputs/ecoregions/NWT_ecoregions_dissolvec.shp')


targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

spName <- read.csv('./inputs/SppNames.csv') 

cogTable <-  qs::qread(file = glue('./tables/coGpiTable_allsp.qs'))
cogDF <- st_as_sf(cogTable, coords = c('COGx','COGy'), crs = targetCRS)

cogDistTable <-qs::qread(file = glue('tables/coGpiDistBearTable_72allsp.qs'))
#YRWA <- qs::qread(file = glue('tables/distBear_coGpi/YRWA_distBear_coGpi.qs'))

coG_Map <- function(spc){
 # spc <- spcs[2] # Run and comment (after)
  message(crayon::green('Files for specie', spc))
  dir <- glue('./outputs/{spc}/occurpi')
  #dir <- grep(spc, dirs, value = TRUE)
  fls <- list.files(dir, pattern = 'pi', full.names = TRUE)
  yrs <- c('2011','2031', '2091')
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 7)
  gcm <- unique(gcm)
  yr <- yrs[1]
  message(crayon::green('Year', yr))
  sfl <- grep(yr, fls, value = TRUE)
  rst <- terra::rast(sfl)
  names(rst) <- gcm
  tbl <- as.data.frame(rst, xy = TRUE)
  tbl2 <- tbl %>% pivot_longer(!c(x, y), names_to = 'gc', values_to = 'pOcc')
  tbl2 <- tbl2 %>% rename('lon' = x, 'lat' = y) %>%
    mutate(yr = yr,
           spc = spc)
  breaks <- seq(0, 1, by = 0.25)
  name<- filter(spName, species == spc)
  tbl2 <- full_join(tbl2, name, by = c('spc'= 'species'))
    #plotting
    message(crayon::green('Making map for', yr))
    ggCOG <-  ggplot() + 
      geom_tile(data = tbl2, (aes(x = lon, y = lat, fill = pOcc))) +
      scale_fill_gradientn(colours = colorspace::sequential_hcl(n = 10, palette = 'Blues', rev = TRUE),
        na.value = 'transparent', breaks = breaks, limits = c(0, 1)) +
      geom_sf(data = ecrg,fill = NA, col = '#b3b3b3') +
      ggtitle(label = glue('{name} ({spc})'), subtitle = yr) +
      theme_bw() +
      theme(legend.position = 'bottom', legend.key.width = unit(2, 'line'),
            plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 11),
            legend.title = element_text(size = 12, face = 'bold'), 
            #text = element_text(size = 10, color = 'gray'),
            strip.text = element_text(size = 12)) +
      labs(x = 'Longitude', y = 'Latitude', fill = 'Probability of Occurrence') +
      coord_sf() + #ims_method = 'geometry_bbox') +
      facet_wrap(~ gc, ncol = 3) 
    ##adding centroids
     p2 <- ggCOG + 
      geom_sf(data = subset(cogDF, year == yr & species == spc), col = '#821196',  size = 3) +
      #geom_sf(data = subset(cogDF, year == '2011' & species == spc), col = 'plum',  size = 3)+
      geom_sf_text(data = subset(cogDF, year == '2011' & species == spc), 
                   aes(label = '2011'), size = 3, colour = 'gray37') +
     #geom_sf(data = subset(cogDF, year == '2031' &species == spc), col = 'plum',  size = 3) +
      #geom_sf_text(data = subset(cogDF, year == '2031' & species == spc), 
       #            aes(label = '2031'), size = 3,colour = 'gray37') +
       coord_sf() + #ims_method = 'geometry_bbox') +
       facet_wrap(~ model, ncol = 3)
      
      
    out <- glue('./maps/cogPi3/{yr}')
    ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
    ggsave(plot = p2,
           filename = glue('{out}/cogPi_{spc}_{yr}.png'),
           units = 'in', width = 12, height = 8, dpi = 300)
    message(crayon::blue('Done!')) 
  
}

# Apply the function ------------------------------------------------------
cogMaps <- map(.x = spcs, .f = coG_Map)


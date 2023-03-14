# Load libraries  ---------------------------------------------------------
library(pacman)
p_load( dplyr,fs, ggplot2, glue,qs,sf,tidyverse)

# Read all COG tables  ----------------------------------------------------
read_table <- function(specie){
  # specie <- spcs[2]
  message(crayon::blue('Starting\n', specie, '\n'))
  path <- glue('./tables/coG/Reclass')
  table <- qs::qread(file = glue('{path}/{specie}_coG_reclass.qs'))
  cat('Done \n')
  return(table)
}  
# Apply the function ------------------------------------------------------
coG_Table <- map(.x = spcs, .f = read_table)
coG_dist_all_Table <- bind_rows(distCogTable)  


out <-'./tables/distBear_coGpi'
ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
qs::qsave(coG_dist_all_Table, glue('{out}/coGpiDistBearTable_allsp.qs'))


limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 
ecrg <- sf::st_read('inputs/ecoregions/NWT_ecoregions_dissolvec.shp')


targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
cogTable <-  qs::qread(file = glue('./tables/coGpi/coGpiTable_allsp.qs'))
cogDF <- st_as_sf(cogTable, coords = c('COGx','COGy'), crs = targetCRS)

cogDistTable <-qs::qread(file = glue('tables/distBear_coGpi/coGpiDistBearTable_allsp.qs'))
YRWA <- qs::qread(file = glue('tables/distBear_coGpi/YRWA_distBear_coGpi.qs'))

coG_Map <- function(spc){
 # spc <- spcs[22] # Run and comment (after)
  message(crayon::green('Files for specie', spc))
  dir <- glue('./outputs/{spc}/occurpi')
  #dir <- grep(spc, dirs, value = TRUE)
  fls <- list.files(dir, pattern = 'pi', full.names = TRUE)
  yrs <- c('2011','2031', '2091')
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 7)
  gcm <- unique(gcm)
  
  cogMap <- map(.x = 1:length(gcm), .f = function(k){
    message(crayon::green('Loading files for', gcm[k]))
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- as.character(fl)
    cogYrs <-  map(.x = 1:length(yrs), .f = function(yr){
      #message(crayon::green('Year', yrs[yr]))
      sfl <- grep(yrs[yr], fl, value = TRUE)
      rst <- terra::rast(sfl) 
      tbl <- as.data.frame(rst, xy= TRUE) %>% 
        rename('lon' = x, 'lat' = y, 'pOcc' = layer) %>% 
        mutate(gc = gcm[k],
               yr = yrs[yr],
               spc = spc)
      tbl <- dplyr::select(tbl, lon, lat, gc, everything())
      cogPoint <- cogDF %>% filter(species == spc & model == gcm[k] & year == yrs[yr])
      cogPoint11 <- cogDF %>% filter(species == spc & model == gcm[k] & year == '2011')
      cogPoint31 <- cogDF %>% filter(species == spc & model == gcm[k] & year == '2031')
      breaks <- seq(0, 1, by = 0.25)
      #plotting
      message(crayon::green('Making map for', yrs[yr]))
      ggCOG <- ggplot() + geom_tile(data = tbl, (aes(x = lon, y = lat, fill = pOcc))) +
        scale_fill_gradientn(colours = colorspace::sequential_hcl(n = 10, palette = 'Blues', rev = TRUE),
                             na.value = 'transparent',breaks = breaks,limits = c(0, 1)) +
        geom_sf(data = ecrg, fill = NA, col = '#b3b3b3') +
        geom_sf(data = cogPoint, col = '#821196', size = 3)+
       # ggsflabel::geom_sf_text(data = cogPoint, aes(label = '2091'), size = 3, colour = 'gray37') +
        geom_sf(data = cogPoint11, col = 'plum', size = 3) +
        ggsflabel::geom_sf_text(data = cogPoint11, aes(label = '2011'), size = 3, colour = 'gray37') +
        #ggsflabel::geom_sf_label_repel(data = cogPoint11, aes(label = '2011')) +
        geom_sf(data = cogPoint31, col = 'plum', size = 3) +
        #ggsflabel::geom_sf_label_repel(data = cogPoint31, aes(label = '2031')) +
        geom_sf_text(data = cogPoint31, aes(label = '2031'), size = 3, colour = 'gray37') +
        #geom_segment(data = cogDist, aes(x =lon, y = lat, xend = lon + delta.x  , yend= lat + delta.y),
         #            arrow = arrow(length = unit(0.02, 'cm')), alpha = 0.6, size = 0.5, color = 'darkblue') +
        facet_wrap(. ~ gc, ncol = 3, nrow = 1) +
        coord_sf(lims_method = 'geometry_bbox') +
        ggtitle(label = spc, subtitle = yrs[yr]) +
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
        labs(x = 'Longitude', y = 'Latitude', fill = 'pOcc')
     
      out <- glue('./maps/cogPi') 
      ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
      ggsave(plot = ggCOG,filename = glue('{out}/cogPi_{spc}_{gcm[k]}_{yrs[yr]}.png'),
             units = 'in', width = 12, height = 9, dpi = 700 )
    })
    
  })
  return(cogMap)
  message(crayon::blue('Done!'))
}

# Apply the function ------------------------------------------------------
cogMaps <- map(.x = spcs[72], .f = coG_Map)


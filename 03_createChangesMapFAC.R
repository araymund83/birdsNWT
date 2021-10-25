# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(parallel, foreach, doSNOW,raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, terra, stringr, glue, sf, tidyverse, 
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
plot(st_geometry(limt))

# Function to use ---------------------------------------------------------
see_changes <- function(spc){
  
 #spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('To start ', spc, '\n')
  cat('-------------------------------------------------------------\n')

  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/tbl_yrs_{spc}.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
  
  cat('To see the average in a raster file\n')
  rst.avg <- map(.x = 1:length(gcm), .f = function(k){
    cat('Start -- ', k, '\n')
    rs <- tbl %>% 
      filter(gc == gcm[k]) %>% 
      dplyr::select(lon, lat, avg) %>% 
      rasterFromXYZ()
    return(rs)
  })
  
  cat('To make a simple map\n')
  gavg <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = avg)) + 
    #geom_sf(data = limt, fill = NA, col = 'grey') +
    #geom_sf(data = ecrg_limt, fill = NA) +
    coord_sf() + 
    facet_wrap(.~gc, nrow = 1, ncol = 3) +
    # scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 8, name = 'YlOrBr')) + 
    scale_fill_binned_sequential(palette = 'Heat') +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5), 
          legend.position = 'bottom') + 
    labs(x = 'Longitude', y = 'Latitude', fill = 'Mean') 
  
  ggsave(plot = gavg, filename = glue('./graphs/maps/avg_gcm_{spc}.png'), 
         units = 'in', width = 13, height = 8, dpi = 300)
  
  cat('To estimate the change (ratio), initial and final year\n')
  tbl <- mutate(tbl, ratio = (y2100 - y2011) / y2011 * 100)
  std <- tbl %>% group_by(gc) %>% summarise(std = sd(ratio)) %>% ungroup()
  tbl <- map(.x = 1:3, .f = function(i){
    st <- std %>% filter(gc == gcm[i]) %>% pull(std)
    st <- st / 4
    tb <- tbl %>% 
      filter(gc == gcm[i]) %>% 
      mutate(rt_bn = case_when(ratio >= st * -1 & ratio <= st ~ 'None',
                               ratio > st ~ 'Positive',
                               ratio < st * -1 ~ 'Negative'))
  })
  tbl <- bind_rows(tbl)
  tbl <- mutate(tbl, rt_bn = factor(rt_bn, levels = c('Negative', 'None', 'Positive')))
  tbl %>% group_by(gc, rt_bn) %>% summarise(count = n()) %>% ungroup()
  qs::qsave(x = tbl, file = glue('./qs/{spc}_table_ratio.qs'))
  
  cat('To make the map binary\n')
  gbn <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = rt_bn)) + 
    facet_wrap(.~gc, ncol = 3, nrow = 1) + 
    scale_fill_manual(values = c("#e97D56", "#999999", "#afceeb")) + 
    ggtitle(label = spc) +
    theme_ipsum_es() + 
    theme(legend.position = 'bottom', 
          axis.text.y = element_text(angle = 90, vjust = 0.5)) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Change')
  
  ggsave(plot = gbn, filename = glue('./graphs/maps/bin_gcm_change_{spc}.png'),
         units = 'in', width = 12, height = 9, dpi = 300)
}


# Apply the function ------------------------------------------------------

map(.x = spcs[61:75], .f = see_changes)

zonal_Changes <- function(spc){   
  
  #spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('Making zonal statistics\n', spc, '\n')
  cat('-------------------------------------------------------------\n')
 
  znl <- map(.x = 1:length(rst.avg), .f = function(k){
    
    cat('To start\n')
    cat(k, '\n')
    znl <- exact_extract(rst.avg[[k]], ecrg, c('mean', 'stdev'))
    znl <- round(znl, digits = 2)
    znl <- mutate(znl, mdl = gcm[k], ecoregion = ecrg$REGION_NAM)
    cat('Done\n')
    return(znl)
    
  })
  
  znl <- bind_rows(znl) 
  znl <- drop_na(znl)
  
  cat('To make the graph\n')
  gbr <- ggplot(data = znl, aes(x = ecoregion, y = mean)) + 
    geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev), width = .2, position = position_dodge(.9)) +
    geom_bar(position = position_dodge(), stat = 'identity') + 
    scale_fill_manual(values = c('#00AFAF', '#00B084', '#66A182')) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
    facet_wrap(.~mdl, ncol = 1, nrow = 3) +
    theme_bw() +
    theme(legend.position = 'bottom', 
          axis.text.x = element_text(size = 7)) + 
    labs(x = 'Ecoregion', y = 'Change', fill = 'GCM')
  
  ogb <- glue('./graphs/figs/bar_ratio_{spc}.png')
  ggsave(plot = gbr, filename = ogb, units = 'in', width = 13, height = 6.8, dpi = 300)
}  


# Apply the function ------------------------------------------------------

map(.x = spcs[1:20], .f = zonal_Changes)


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(parallel, raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, terra, stringr, glue, sf, tidyverse, 
               RStoolbox, fs, fst, trend, colorspace, exactextractr, furrr, future)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('inputs/bcr6_NTYU/bcr6NTYU.shp') 
ecpr <- sf::st_read('inputs/ecoregions/ecoregions.shp') # TODO: ADD ECOprovicnes

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecpr))
ecpr <- sf::st_transform(x = ecpr, crs = st_crs(limt))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecpr_limt <- sf::st_intersection(x = ecpr, y = limt)
plot(st_geometry(ecpr_limt))

# Function to use ---------------------------------------------------------
see_changes <- function(spc){
  
 # spc <- spcs[2]
  
  cat('To start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  # fle <- fs::dir_ls(dir, regexp = '.fst')
  fle <- fs::dir_ls(dir, regexp = '.qs')
  #tbl <- fst::read_fst(path = fle)
  tbl<- qs::qread(file = glue('./outputs/{spc}/tbl_yrs_{spc}.qs'))
  # tbl <- fst::read_fst(path = fle)
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
  
  cat('To see the average in a raster file\n')
  rst.avg <- map(.x = 1:length(gcm), .f = function(k){
    cat('Start -- ', k, '\n')
    rs <- tbl %>% filter(gc == gcm[k]) %>% dplyr::select(lon, lat, avg) %>% rasterFromXYZ()
    return(rs)
  })
  
  
  cat('To make a simple map\n')
  gavg <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = avg)) + 
    geom_sf(data = limt, fill = NA, col = 'grey') +
    #geom_sf(data = ecpr_limt, fill = NA) +
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
}
cat('To estimate the change (ratio), initial and final year\n')
tbl <- mutate(tbl, ratio = (y2100 - y2011) / y2011 * 100)

cat('Now to make the zonal statistical\n')
znl <- map(.x = 1:length(rst.avg), .f = function(k){
  
  cat('To start\n')
  cat(k, '\n')
  znl <- exact_extract(rst.avg[[k]], ecpr_limt, c('mean', 'stdev'))
  znl <- round(znl, digits = 2)
  znl <- mutate(znl, gcm = gcm[k], ecoprovince = ecpr_limt$ECOPROVINC)
  cat('Done\n')
  return(znl)
  
})

znl <- bind_rows(znl)  ##une las tres listas de znl en un solo DF

cat('To make the graph\n')
gbr <- ggplot(data = znl, aes(x = ecoprovince, y = mean, fill = gcm, group = gcm)) + 
  geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev), width = .2, position = position_dodge(.9)) +
  geom_bar(position = position_dodge(), stat = 'identity')  + 
  scale_fill_manual(values = c('#38610B', '#FF8000', '#29088A')) +
  theme_bw() +
  theme(legend.position = 'bottom') + 
  labs(x = 'Ecoprovince', y = 'Change', fill = 'GCM')

ogb <- glue('./graphs/figs/bar_ratio_{spc}.png')
ggsave(plot = gbr, filename = ogb, units = 'in', width = 9, height = 6.8, dpi = 300)

cat('To calculate the slopes\n')
tbl <- map(.x = 1:3, function(k){tbl %>% filter(gc == gcm[k]) %>% 
    mutate(gid = 1:nrow(.))}) %>% bind_rows()
gds <- tbl %>% pull(gid) %>% unique()gds <- tbl %>% pull(gid) %>% unique()  # pull convierte la columna en un vector

cat('To sentence the function\n')
run_slope <- function(pix){
  
  cat(j, '\n')
  tb <- tbl %>% filter(gid == gds[j])
  
  rs <- map(.x = 1:3, .f = function(g){
    
    cat(gcm[g], '\n')
    df <- tb %>% filter(gc == gcm[g]) 
    ts <- df %>% dplyr::select(contains('y'))
    ts <- ts %>% gather(year, value) %>% mutate(year = parse_number(year))
    tm <- ts %>% pull(value) %>% ts()
    sl <- sens.slope(tm)
    df <- data.frame(gcm = gcm[g], gid = gds[j], slp = sl$estimates, pvl = sl$p.value)
    df <- as_tibble(df)
    
  })
  
  rs <- bind_rows(rs)
  cat('Done\n')
  return(rs)
  
}

cat('To estimate the slopes\n')
plan(multicore, workers = 30)
options(future.globals.maxSize = 3460300800)
rsl <- future.apply::future_lapply(X = gds, FUN = run_slope)
# Update

}


# Apply the function ------------------------------------------------------
map(spcs[3:10], see_changes)

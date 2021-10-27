# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, hrbrthemes, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)


g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('./inputs/NT1_BCR6/NT1_BCR6_poly.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

limt <- sf::st_transform(x = limt, crs = targetCRS)

# Make   --------------------------------------------------------
raster_to_table <- function(spc){
  
  # Proof
  spc <- spcs[1] # Run and comment (after)
  message(crayon::green("Loading data for: ", spc))
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- grep('slpe', fls, value = TRUE)
  gcm <- str_sub(basename(fls), start = 6, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)

  slopes <- list()
  for (i in 1:length(fls)){
    slopes[[i]] <- raster(fls[i])
  }

slpe.tble <- map(.x = 1:length(slopes), .f = function(k){
  cat(k, '\n')
  rsl <- slopes[[k]] %>% 
    rasterToPoints(., spatial = FALSE) %>% 
    as_tibble() %>% 
    mutate(model = gcm[k]) %>% 
    setNames(c('x', 'y', 'slp', 'model')) %>% 
    mutate(model = gcm[k])
  return(rsl)
})

slpe.tble <- bind_rows(slpe.tble)
min <- min(slpe.tble$slp) ##getting the min and max values for the limits in ggplot filling scale
max <- max(slpe.tble$slp)

brks <- c(-Inf,-1:1,Inf)
discr_colors <- scales::div_gradient_pal( low =, mid = 'ligthgray', high= 'green')

cat('Making the map\n')
gslp <- ggplot() + 
  geom_tile(data = slpe.tble, aes(x = x, y = y, fill = slp)) + 
  geom_sf(data = limt, fill = NA) +
  facet_wrap(.~model, ncol = 3, nrow = 1) +
  #scale_fill_manual(values = brewer.pal(name = 'YlOrRd', n = 7)) + 
  #scale_fill_binned_diverging(palette = 'Red-Green', n.breaks = 6) +
  #scale_fill_continuous_diverging(palette = 'Red-Green', p1 = -0.02, p2 = 0.02) +
  scalle_fill_gradient2 (low = 'darkred', mid = 'grey67', high = 'green',
                         guide = 'legend', breaks ) 
 #theme_void() +  
  #theme(plot.title =  element_text(hjust = 0.5, face = 'bold')) +
  theme_ipsum_es() + 
  coord_sf() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line')) +
  labs(x = 'Longitud', y = 'Latitude', fill = 'Slope',
       title = spc) 
  
  
  ggsave(plot = gslp, 
         filename = glue('./graphs/maps/slopes/{spc}_slpe4.png'), 
         units = 'in', width = 13, height = 10, dpi = 700)
  
  message(crayon::green("Done: ", spc))
}

# Apply the function ------------------------------------------------------
map(.x = spcs, .f = raster_to_table)



# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(colorspace, furrr, future, future.apply, fs, fst, ggpubr, ggspatial, glue, gridExtra,
               RColorBrewer, raster, rgdal, rgeos, RStoolbox, sf, stringr, terra, tidyverse, trend  )

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './inputs/predictions'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read("./inputs/bcr6_NTYU/bcr6_NTYU.shp") ## TODO: PUT NAME SHAPEFILE
targetCRS <-  paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                    "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
limt <- st_transform(x = limt, targetCRS)
qntl <- readRDS(file = './outputs/rds/all_qntl.rds')

# Function to use ---------------------------------------------------------
make_sum <- function(spc){
  
  #spc <- spcs[2] # Run and erase
  
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, directory = '.tif$')
  
  cat('To get the name of each gcm\n')
  gcm <- str_split(basename(fls), '_')
  gcm <- sapply(gcm, function(x) x[1])
  gcm <- unique(gcm)
  
  cat('To apply to each gcm\n')
  rsl <- map(.x = 1:length(gcm), function(k){
    
    prd <- str_sub(fls, start = nchar(fls) - 7, end = nchar(fls) - 4)
    prd <- unique(prd)
    fl <- grep(gcm[k], fls, value = TRUE)
    
    rs <- map(.x = 1:length(prd), .f = function(i){
      
      cat(gcm[k], '\n')
      fl <- grep(prd[i], fl, value = TRUE)
      rs <- terra::rast(fl)
      tb <- terra::as.points(rs)
      df <- terra::as.data.frame(x = tb)
      sm <- apply(X = df, MARGIN = 1, FUN = sum)
      sd <- apply(X = df, MARGIN = 1, FUN = sd)
      gm <- terra::geom(tb)
      df <- cbind(gm[,3:4], df)
      df <- mutate(df, sma = sm, std = sd)
      rs.sm <- dplyr::select(df, x, y, sma) %>% rasterFromXYZ()
      rs.sd <- dplyr::select(df, x, y, std) %>% rasterFromXYZ()
      dr <- glue('./outputs/{spc}')
      writeRaster(x = rs.sm, filename = glue('{dr}/sum_{spc}_{gcm[k]}_{prd[i]}.tif'), overwrite = T)
      writeRaster(x = rs.sd, filename = glue('{dr}/std_{spc}_{gcm[k]}_{prd[i]}.tif'), overwrite = T)
      cat('Done\n')
      
    })
    
  })
  
  cat(' --------------------- Finish --------------------\n')
  
}

# Apply the function to make the sum --------------------------------------
#make_sum(spc = spcs[2])
map(.x = spcs[43:length(spcs)], .f = make_sum)

# To make the maps --------------------------------------------------------

fldr <- './outputs'
spcs <- fs::dir_ls(fldr, type = 'directory')

make_maps <- function(spc){
  
  #spc <- spcs[1] # Run and comment
  
  fld <- grep(spc, spcs, value = TRUE)
  fls <- list.files(fld, full.names = TRUE, pattern = '.tif')
  fls <- grep(paste0(c('sum', 'std'), collapse = '|'), fls, value = TRUE)
  print(fls)
  gcm <- str_split(basename(fls), '_')
  gcm <- sapply(gcm, function(x) x[3])
  gcm <- unique(gcm)
  
  map(.x = 1:length(gcm), .f = function(i){
    
   # i <- 1 # Run and comment
    
    # Start reading the rasters
    fle <- grep(gcm[i], fls, value = TRUE)
    sum <- grep('sum', fle, value = TRUE) 
    std <- grep('std', fle, value = TRUE)
    sum <- raster::stack(sum) # tsm <- terra::rast(sum)
    std <- raster::stack(std) # tsd <- terra::rast(std)
    crs(sum) <- targetCRS
    crs(std) <- targetCRS
    
    # Sum raster - Table
    tbl.sum <- rasterToPoints(sum, spatial = FALSE) %>% as_tibble()
    tbl.sum <- mutate(tbl.sum, gid = 1:nrow(tbl.sum))
    tbl.sum <- gather(tbl.sum, var, value, -gid, -x, -y)
    tbl.sum <- separate(data = tbl.sum, col = var, into = c('type', 'spc', 'gcm', 'year'), sep = '_')
    
    # Std raster - Table
    tbl.std <- rasterToPoints(std, spatial = FALSE) %>% as_tibble()
    tbl.std <- mutate(tbl.std, gid = 1:nrow(tbl.std))
    tbl.std <- gather(tbl.std, var, value, -gid, -x, -y)
    tbl.std <- separate(data = tbl.std, col = var, into = c('type', 'spc', 'gcm', 'year'), sep = '_')
    
    cat('To make the maps')
    
    # Sum map
    ggp.sum <- ggplot() + 
      geom_tile(data = tbl.sum, aes(x = x, y = y, fill = value)) +
      facet_wrap(.~ year) +
      scale_fill_gradient(colors = brewer.pal(name = 'PiYG', n = 5)) +
      geom_sf(data = limt, fill = NA, col = 'grey20') +
      coord_sf() + 
      theme_bw() + 
      theme(legend.position = 'bottom', 
            legend.key.width = unit(1.5, 'line')) + 
      labs(x = 'Lon', y = 'Lat', fill = 'Sum values')
    
    ggsave(plot = ggp.sum, filename = glue('./graphs/maps/sum/sum_{basename(spc)}.png'),
           units = 'in', width = 10, height = 8, dpi = 300) ### TESTING
    
    # Std maps
    ggp.std <- ggplot() + 
      geom_tile(data = tbl.std, aes(x = x, y = y, fill = value)) + 
      facet_wrap(.~ year) + 
      scale_fill_gradientn(colors = brewer.pal(name = 'PiYG', n = 5)) +
      geom_sf(data = limt, fill = NA, col = 'grey20') + 
      coord_sf() + 
      theme_bw() + 
      theme(legend.position = 'bottom', 
            legend.key.width = unit(1.5, 'line')) + 
      labs(x = 'Lon', y = 'Lat', fill = 'Std values')
    
    ggsave(plot = ggp.std, filename = glue('./graphs/maps/sum/std_{basename(spc)}.png'), 
           units = 'in', width = 10, height = 8, dpi = 300)
    
    cat('----Done----\n')
    
  })
  
  
}





# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(caTools, colorspace,compiler, furrr, future, future.apply, fs, fst, ggpubr, ggspatial, glue, gridExtra,
               RColorBrewer, raster, rgdal, rgeos, RStoolbox, sf, stringr, terra, tidyverse, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './inputs/predictions'
spcs <- dir_ls(root)

# Get values  -------------------------------------------------------------
get_extreme_values <- function(spc){
  
  #spc <- spcs[1]
  
  cat('Start\n', spc, '\n')
  fls <- dir_ls(spc, regexp = '.tif$')

  vls <- mclapply(X = 1:length(fls), FUN = function(i){
   
   cat('Start ', fls[i], '\n')
   fl <- fls[i]
   rs <- raster(fl)
   vl <- getValues(rs)
   vl <- na.omit(vl)
   vl <- as.numeric(vl)
   cat('Done\n')
   return(vl)
   
 }, mc.cores = 30)
  
  all <- do.call(c, vls)
  prc <- quantile(all, seq(0, 1, 0.2))
  prc <- as.numeric(prc)
  dfm <- data.frame(specie = basename(spc), quantile = seq(0, 1, 0.2),  intervals = prc)
  cat('Done\n')
  return(dfm)

}

# Apply all the function --------------------------------------------------
system.time(alfl <- get_extreme_values(spc = spcs[1]))
saveRDS(object = alfl, './outputs/rds/alfl_qntl.rds')

rsl <- map(.x = spcs, .f = get_extreme_values)
rsl <- bind_rows(rsl)
saveRDS(object = rsl, file = './outputs/rds/all_qntl.rds')
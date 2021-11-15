# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# See the changes  --------------------------------------------------------
raster_to_table <- function(spc){
  
  # Proof
  #spc <- spcs[2] # Run and comment (after)

  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- fls <- grep('mean', fls, value = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- na.omit(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  
  message(crayon::blue(paste0('Making table for', spc)))
  dfm <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    fl <- grep(gcm[k], fls, value = TRUE)
    
    cat('----- Terra library functions -----\n')
    tr <- terra::rast(fl)
    tb <- terra::as.points(tr)
    df <- terra::as.data.frame(x = tb)
    names(df) <- paste0('y', yrs)
    
    gm <- terra::geom(tb)
    df <- cbind(gm[,3:4], df)
    df <- as_tibble(df)
    df <- mutate(df, gc = gcm[k])
    df <- mutate(df, sp = spc)
    return(df)
    
  })
  
  rsl <- bind_rows(dfm)
  #browser
  #fst::write_fst(x = rsl, path = glue('./outputs/{spc}/tbl_yrs_{spc}.fst')) ## saving with .fst creates very big files! 
  qs::qsave(x = rsl, file = glue('./tables/qs/{spc}_mean_tbl.qs'))
  
  cat('------- Done -------\n')
  return(rsl)
  
}

# Apply the function ------------------------------------------------------

map(.x = spcs[71:75], .f = raster_to_table)




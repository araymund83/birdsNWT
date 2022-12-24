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
change_raster_res <- function(spc){
  
  # Proof
#spc <- spcs[2] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- grep('mean', fls, value = TRUE)
 # fls <- grep( gcm, fls, value = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- yrs[2:7]
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  gcm <- gcm[2:4]
  cat('Changing resolution to 1km')
  dfm <- map(.x = 1:length(gcm), .f = function(gc){
    
    cat(gcm[gc], '\n')
    fl <- grep(gcm[gc], fls, value = TRUE)
    
    rs <- map(.x = 1:length(yrs), .f = function(yr){

      fl <- grep(yrs[yr], fl, value = TRUE)
    
    cat('----- Terra library functions -----\n')
    tr <- terra::rast(fl)
    tr2 <- aggregate(tr,  fun = 'mean', fact = 40, na.rm = TRUE, cores = 16) # change factor to 40 to make pixels 10x10 km , 16 makes 4 x 4 km
    ou <- glue('./outputs/10km/mean_{spc}_{yrs[yr]}_{gcm[gc]}_10km.tif') # crea la ruta del archivo de resultado {sp} cambia al igual el a;o
    dr <- dirname(ou) ## extrae lo primero del ultimo _
    ifelse(!dir.exists(ou), dir.create(dr), print('Folder already exist')) # revisa si existe el file
    cat('writing the final raster\n')
    writeRaster(x = tr2, filename = ou, overwrite = TRUE) #guarda el raster 
    cat('Done ', gc, '\n')
    df <- terra::as.data.frame(x = tr2, xy = TRUE)
 
    df <- as_tibble(df)
    df <- mutate(df, gc = gcm[gc],
                 year = yrs[yr])
    return(df)
    
  })
  })
  rsl <- bind_rows(dfm)
  #browser
  #fst::write_fst(x = rsl, path = glue('./outputs/{spc}/tbl_yrs_{spc}.fst')) ## saving with .fst creates very big files! 
  qs::qsave(x = rsl, file = glue('./qs/4km/tbl_yrs_{spc}_4km.qs'))
  
  cat('------- Done -------\n')
  return(rsl)
  
}
### Raster to table ---------------------------------------------------------
dfrm <- map(.x = spcs[1:72], .f = change_raster_res)
dim(dfrm)
object.size(dfrm)

## to read qs
table<- qs::qread(file = glue('./qs/1km/tbl_yrs_{spc}_1km.qs'))

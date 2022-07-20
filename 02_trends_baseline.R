# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# See the changes  --------------------------------------------------------
raster_to_table <- function(spc){
  
  # Proof
 spc <- spcs[1] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- grep('baselineresample', fls, value = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- na.omit(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  
  cat('Raster to table\n')
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
    return(df)
    
  })
  
  rsl <- bind_rows(dfm)
  #browser
  #fst::write_fst(x = rsl, path = glue('./outputs/{spc}/tbl_yrs_{spc}.fst')) ## saving with .fst creates very big files! 
  qs::qsave(x = rsl, file = glue('./outputs/{spc}/tbl_2010_base72_{spc}.qs'))
  
  cat('------- Done -------\n')
  return(rsl)
  
}
### Raster to table ---------------------------------------------------------
dfrm <- map(.x = spcs[52:72], .f = raster_to_table)
dim(dfrm)
object.size(dfrm)

## to read qs
tabl2<- qs::qread(file = glue('./outputs/ALFL/tbl_2010_base72_ALFL.qs'))
tab1<- qs::qread(file = glue('./outputs/ALFL/tbl_yrs_ALFL.qs'))
tabl<-tab1 %>% group_by(x, y)
tb2 <- tabl2 %>% select(x, y, y2010) %>% group_by(x, y)

test <- tab1 %>%  filter(gc == 'CanESM2') %>% select(x,y, y2031, y2091, gc) %>% 
  group_by(x, y)

diff_1031<- full_join(test, tb2, by_group = TRUE)

dif_short <- diff_1031 %>% select(x, y, y2010, y2031, y2091, gc)

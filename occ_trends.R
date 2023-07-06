#This script converts the occurrence rasters to a table 
# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
thrs <- read_csv('./inputs/prevOcc.csv')
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]
dirs <- glue('{dirs}/occurpi')
dirs <- as.character(dirs)
dirs <- dirs[1:72]

# See the changes  --------------------------------------------------------
raster_to_table <- function(spc){
  
  # Proof
  #spc <- spcs[6] # Run and comment (after)
  message(crayon::green("Starting with:", spc))
  dir <- grep(spc,dirs, value = TRUE)
  fls <- list.files(dir, pattern = 'pi', full.names = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- na.omit(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 7)
  gcm <- unique(gcm)

  cat('Raster to table\n')
  dfm <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    fl <- grep(gcm[k], fls, value = TRUE)
    rs <- terra::rast(fl)
    df <- terra::as.data.frame(x = rs ,xy = TRUE, na.rm = TRUE)
    colnames(df)[3:8]<- glue('y{yrs}')
    df <- as_tibble(df)
    df <- df %>% mutate(gc = gcm[k],
                        specie = spc)
    return(df)
    
  })
  
  rsl <- bind_rows(dfm)
  out<- glue ('./qs/occurpi')
  ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Dir already exists'))
  qs::qsave(x = rsl, file = glue('{out}/occ_yrs_{spc}_pi.qs'))
  
  cat('------- Done -------\n')
  return(rsl)
  
}
### Raster to table ---------------------------------------------------------
dfrm <- map(.x = spcs[57:72], .f = raster_to_table)



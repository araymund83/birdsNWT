# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(biomod2,exactextractr, glue, fs, fst)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('./inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('./inputs/ecoregions/NWT_ecoregions_dissolvec.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)
ecrg_limt <- sf::st_intersection(x = ecrg, y = limt)
plot(st_geometry(ecrg_limt))
plot(st_geometry(limt))

path <- './maps/binnary/occuBinnary'
fls <- list.files(path, pattern = 'occbin', full.names = TRUE)

getRangeSize <- function(spc){
  #spc <- spcs[11] # Run and erase
  fls <- grep(spc,fls, value = TRUE)
  gcm <- str_sub(basename(fls), start = 13, end = nchar(basename(fls)) - 9)
  gcm <- unique(gcm)
  yrs <- c('2011','2031', '2091')
  
  dfm <- map(.x = 1:length(gcm), .f = function(k){
    
  message(crayon::green('Loading files for', gcm[k]))
  fl <- grep(gcm[k], fls, value = TRUE)
  fl_pres <- grep(yrs[1], fl, value = TRUE)
  rst_pres <- rast(fl_pres)
  rangeYrs <-  map(.x = 2:length(yrs), .f = function(yr){
    message(crayon::green('Year', yrs[yr]))
    fl_fut <- grep(yrs[yr], fl, value = TRUE)
    rst_fut <- terra::rast(fl_fut) 
    diff_ras <- rst_fut - rst_pres
    freq_table <- as.data.frame(freq(diff_ras)) 
    freq_table <- freq_table %>% mutate(specie = spc, 
                                        gcm = gcm[k],
                                        yr = yrs[yr])
    out <- './maps/binnary/diffRas'
    ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
    writeRaster(diff_ras, glue('{out}/diffRas_{yrs[yr]}_{spc}_{gcm[k]}.tif'), 
                overwrite = TRUE)
    return(freq_table)
   })
  return(rangeYrs)
  rSize <- bind_rows(rangeYrs)
  })
  df<-  bind_rows(dfm)
  out <- glue('./tables/freqPixBin')
  ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
  qs::qsave(x =df, file = glue('{out}/{spc}_freqPixBin.qs'))
  return(df)
}

# Apply the function ------------------------------------------------------
rangeGL <- map(.x = spcs, .f = getRangeSize)

rangeBinn <- bind_rows(rangeGL)
cellSize = 250 * 250

rangeBinn <- rangeBinn %>% mutate((count * 6.25) /10000)
write.csv(rangeBinn, file = './tables/rangeBinn3191_allsp.csv')
rangeBinn31 <- rangeBinn %>% group_by(gcm) %>%  filter(yr == '2031')
rangeBinnSumm <- rangeBinn31 %>% group_by(gcm,specie, value) %>%  
  summarize(avg_pix = mean(count))
rangeBinn31_sppGain <- rangeBinn31 %>% filter(value == 1)
rangeBinn31_sppLoss <- rangeBinn31 %>% filter(value == -1)
length(unique(rangeBinn31_sppLoss$specie))



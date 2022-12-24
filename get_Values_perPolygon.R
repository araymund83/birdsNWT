# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(exactextractr, parallel, foreach, doSNOW,raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, stringr, glue, sf, tidyverse, fasterize,
               RStoolbox, fs, fst, trend, colorspace, hrbrthemes,furrr, future, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('./inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('./inputs/ecoregions/EcoNWT_poly_dissolved/ecoregionsIII_disolved.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)
ecrg_limt <- sf::st_intersection(x = ecrg, y = limt)
plot(st_geometry(ecrg_limt))
plot(st_geometry(limt))

##dissolve polygons 
ecrg_diss <- ecrg %>% mutate(region = case_when(name %in% c('High Boreal', 'Mid-Boreal')~ 'South',
                                      name %in% c('Low Subarctic')~ 'Middle',
                                      name %in% c('Low Arctic north', 'High Subarctic')~'North',))

ecrg_diss <- ecrg_diss %>% group_by(region) %>% summarise()
glimpse(ecrg_diss)

# Function to use ---------------------------------------------------------
getPixperPoly <- function(spc){
    
    #spc <- spcs[1] # Run and erase
    dir <- grep(spc,dirs, value = TRUE)
    fles <- fs::dir_ls(dir, regexp = 'occur')
    fls <- fs::dir_ls(fles, regexp = '.tif$')
    yrs <- parse_number(basename(fls))
    yrs <- unique(yrs)
    gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
    gcm <- unique(gcm)
    cat('Start getting area for', spc, '\n')
    
    getVals<- map(.x = 1:length(gcm), .f = function(k){
      message(crayon::green('Loading files for', gcm[k]))
      fl <- grep(gcm[k], fls, value = TRUE)
      fl <- as.character(fl)
      valsYears <-  map(.x = 1:length(yrs), .f = function(yr){
        message(crayon::green('Year', yrs[yr]))
        sfl <- grep(yrs[yr], fl, value = TRUE)
        rst <- terra::rast(sfl)
        #extract the area of each cell that is contained within each polygon    
        tbl <- exactextractr::exact_extract(rst,ecrg_diss,coverage_area = TRUE)
        #add polygon names that the results will be grouped by
        names(tbl) <- c('Middle', 'North', 'South')
       #bind the list output into a df and calculate the proportion cover for each category
        vals <- bind_rows(tbl, .id = 'region') %>%  group_by(region) %>% 
          summarize(total_area = sum(coverage_area)/10000) %>% 
          mutate(proportion = total_area/sum(total_area))
        vals <- vals %>%  mutate(species = spc,
                                 model = gcm[k], 
                                 year = yrs[yr])
        return(vals)
      })
      df <- bind_rows(valsYears)
      out <- glue('./qs/polyArea')
      qsave(x = df, file = glue('{out}/{spc}_areaPoly.qs'))
      cat('Done!\n')
      return(df)
     })
      }
# Apply the function ------------------------------------------------------
area <- map(.x = spcs, .f = getPixperPoly)


areaPolys <- qs::qread('./qs/polyArea/areaPolyAll.qs')
areaPol <- bind_rows(area)

      
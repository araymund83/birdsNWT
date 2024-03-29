# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(parallel, foreach, doSNOW,raster, rgdal, rgeos, reproducible, RColorBrewer, ggspatial, 
               ggpubr, gridExtra, stringr, glue, sf, tidyverse, fasterize,
               fs, fst, trend, colorspace, hrbrthemes,exactextractr, furrr, future, spatialEco)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('inputs/ecoregions/NWT_ecoregions_dissolvec.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
spName <- read.csv('./inputs/SppNames.csv') 

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)
plot(st_geometry(ecrg))
plot(st_geometry(limt))


# Function to use ---------------------------------------------------------
make_changesMap_pOcc <- function(spc, yr1, yr2){
  
 #spc <- spcs[1]
 message(crayon::green("Calculating changes for: ", spc))
 path <- glue('./qs/occurpi')
 fls <- list.files(path, pattern =  'occ_yrs', full.names = TRUE)
 fle <- grep(spc, fls, value = TRUE)
 tbl <- qs::qread(file = glue('{path}/occ_yrs_{spc}_pi.qs'))
 tbl <- dplyr::select(tbl, x, y, gc, everything())
 names(tbl)[1:2] <- c('lon', 'lat')
 tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
 tbl <- as_tibble(tbl)
 gcm <- unique(tbl$gc)
 name<- filter(spName, species == spc)
  
 message(crayon::green(glue('Estimating change from {yr1} to {yr2} year\n')))
 tbl <- mutate(tbl, change = tbl$y2031 - tbl$y2011)  #change 2100 for 2031
 tbl <- mutate(tbl, perctChange = ((tbl$y2031- tbl$y2011) / tbl$y2011) * 100) # use if you want to calculate the percent change from baselinea
 tbl <- mutate(tbl, ratio = tbl$y2031/tbl$y2011)
 tbl <- mutate(tbl, logRatio = log2(ratio))
 tbl <- mutate(tbl, gc = as.factor(gc))
 tbl <- full_join(tbl, name, by = c('specie'= 'species'))
 out <- glue('./qs/occurpi/')
 ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
 qs::qsave(x = tbl, file = glue('{out}/occur_changes_{yr1}-{yr2}_{spc}.qs'))
 breaks <- seq(-1, 1, by = 0.25)
 message(crayon::green('Making map for: ',spc))
 ggRatio <- ggplot() +
   geom_tile(data = tbl, aes(x = lon, y = lat, color = change)) + # use logRatio or change
   geom_sf(data = limt, fill = NA, col = 'gray10') + # !!! use only when studyAreaName = 'dehcho'
   geom_sf(data = ecrg, fill= NA, col = 'gray10') +
   #   scale_fill_gradientn(colours = colorspace::diverging_hcl(n = 10, palette = 'Green-Brown'),rev = TRUE,
   #                       na.value = '#999999',breaks = breaks,limits = c(-1, 1) * max(abs(tbl$change))) +
   scale_colour_gradientn(colours = brewer.pal(n = 10, name = 'BrBG'), limit = c(-1,1))+#limit = c(-1, 1) * max(abs(tbl$change)))+
   facet_wrap(. ~ gc, ncol = 3, nrow = 1) +
   ggtitle(label = glue('{name} ({spc})'), subtitle = glue('{yr1}-{yr2}')) +
   theme_bw() +
   theme(legend.position = 'bottom', 
         legend.key.width = unit(4, 'line'),
         plot.title = element_text(size = 16, face = 'bold', hjust = 0, vjust = 0.7), 
         plot.subtitle = element_text(size = 14, hjust = 0, vjust = 0.7),
         axis.title = element_text(size = 14),
         axis.text.x = element_text(size = 12), 
         axis.text.y = element_text(size = 12), 
         legend.text = element_text(size = 10), 
         legend.title = element_text(size = 12, face = 'bold'), 
         strip.text = element_text(size =12)) +
   labs(x = 'Longitude', y = 'Latitude', fill = 'Change')
 out <- glue('./maps/occurpi2/changes_{yr1}-{yr2}/') 
 ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), 'Folder already exist')
 ggsave(plot = ggRatio,filename = glue('{out}/ocurr_change_{yr1}-{yr2}_{spc}.png'),
        units = 'in', width = 11, height = 8, dpi = 300)
}

# Apply the function -----------------------------------------------------
purrr::map(.x= spcs, yr1 ='2011', yr2 = '2031', .f = make_changesMap_pOcc)

# Apply the function ------------------------------------------------------

map(.x = spcs, .f = see_changes)

zonal_Changes <- function(spc){   
  
  #spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('Making zonal statistics\n', spc, '\n')
  cat('-------------------------------------------------------------\n')
 
  znl <- map(.x = 1:length(rst.avg), .f = function(k){
    
    cat('To start\n')
    cat(k, '\n')
    znl <- exact_extract(rst.avg[[k]], ecrg, c('mean', 'stdev'))
    znl <- round(znl, digits = 2)
    znl <- mutate(znl, mdl = gcm[k], ecoregion = ecrg$REGION_NAM)
    cat('Done\n')
    return(znl)
    
  })
  
  znl <- bind_rows(znl) 
  znl <- drop_na(znl)
  
  cat('To make the graph\n')
  gbr <- ggplot(data = znl, aes(x = ecoregion, y = mean)) + 
    geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev), width = .2, position = position_dodge(.9)) +
    geom_bar(position = position_dodge(), stat = 'identity') + 
    scale_fill_manual(values = c('#00AFAF', '#00B084', '#66A182')) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
    facet_wrap(.~mdl, ncol = 1, nrow = 3) +
    theme_bw() +
    theme(legend.position = 'bottom', 
          axis.text.x = element_text(size = 7)) + 
    labs(x = 'Ecoregion', y = 'Change', fill = 'GCM')
  
  ogb <- glue('./graphs/figs/bar_ratio_{spc}.png')
  ggsave(plot = gbr, filename = ogb, units = 'in', width = 13, height = 6.8, dpi = 300)
}  


# Apply the function ------------------------------------------------------

map(.x = spcs[1:20], .f = zonal_Changes)


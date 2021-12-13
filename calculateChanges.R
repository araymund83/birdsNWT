# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 

ecrg <- sf::st_read('inputs/ecoregions/ecoregions.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
limt <- sf::st_transform(x = limt, crs = targetCRS)
ecrg <- sf::st_transform(x = ecrg, crs = targetCRS)
ecrg_limt <- sf::st_intersection(x = ecrg, y = limt)
plot(st_geometry(ecrg_limt))
plot(st_geometry(limt))


logRatio_rasters <- function(spc){
  spc <- spcs[27] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/tbl_yrs_{spc}.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
 
  cat('Estimating change from initial (2011) and final (2091) year\n')
  tbl <- mutate(tbl, change = y2091 - y2011 )  #change 2100 for 2091
  #tbl <- mutate(tbl, perctChange = ((y2091 - y2011) / y2011) * 100) 
  tbl <- mutate(tbl, ratio = (y2091/y2011))
  tbl <- mutate(tbl, logRatio = log2(ratio))
  tbl <- mutate(tbl, gc = as.factor(gc))
  
  #tst <- tbl %>%  filter(gc == 'CCSM4') %>% dplyr::select(lon, lat, logRatio) %>% rasterFromXYZ()
  ## para verificar que los datos se comportan de esa manera, y no hay error de indexacion.
  
  qs::qsave(x = tbl, file = glue('./qs/{spc}_table_ratio.qs'))
    
  cat('Making a ratio map for:', spc, '\n')
  ggRatio <- ggplot() +
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = logRatio)) +
    scale_fill_binned_diverging(palette= 'Blue-Red', name = expression('log'[2]~'ratio'), rev = TRUE, n.breaks = 5) +
    facet_wrap(. ~ gc, ncol = 3, nrow = 1) +
    #geom_tile(aes(fill = logRatio)) +
    geom_sf(data = limt, fill = NA, col = '#999999') +
    geom_sf(data = ecrg_limt, fill = NA, col = '#bfbfbf') +
    #scale_fill_manual(values = c('#D73027','#f4f4f4', '#1A9870')) + ## change green from #1A9850
    ggtitle(label = spc) +
    #theme_ipsum_es() +
    theme_bw() +
    theme(legend.position = 'bottom',
          legend.key.width = unit(2, 'line'),## aumenta la longitud de la leyenda
          axis.text.y = element_text(angle = 90, vjust = 0.5)) +
    labs(x = 'Longitude', y = 'Latitude')
  
  ggsave(plot = ggRatio,filename = glue('./graphs/maps/ratio/gcm_ratio_{spc}.png'),
         units = 'in', width = 12, height = 9, dpi = 700)
 }

# Apply the function -----------------------------------------------------
map(spcs, logRatio_rasters)

# files <- fs::dir_ls('./qs')
# files <- grep('changes', files, value = TRUE)
# #dat_list <- lapply(files, qs::qread)
# #saveRDS(dat_list,'./tables/qs/changesTables.RDS')
# 
# # Read list of changes Tables ---------------------------------------------
# dat_list <- loadRDS('./tables/qs/changesTables.RDS')
# g
# a <- dat_list[[1]]
# a %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
# a %>% 
# 
# 
# # Create summary table summing all rows for each species ------------------
# 
#   
#  sum_table <- function(specie){
#   # specie <- spcs[1]
#    message(crayon::blue('Starting\n',specie, '\n'))
#    table <- qs::qread(file = glue('./qs/{specie}_table_changes.qs'))
#    sumDF <- table %>% group_by(gc) %>% summarise(across(y2011:y2100, sum))
#    sumDF <- mutate(sumDF, species = specie)
#    cat('Done \n')
#    return(sumDF)
#  }  
# 
# 
# sumTable <- map(.x = spcs, .f = sum_table)
# 
# totalTable <- bind_rows(sumTable)  ## join all rows into a same table 
# 
# qsave(totalTable, './tables/totalTable.qs')
# 
# 
# long <- totalTable %>%  gather(Year, Value, - c(gc, species))
# long <- long %>% mutate(Year = as.factor(Year))
# levels(long$Year) <- c('2011','2031','2051', '2071', '2091', '2100')
# long <- long %>% 
#   arrange(gc, species, Year) 
# 
# # create table for plotting rate of change --------------------------------
# totalTable <- qs::qread(file = glue('./tables/totalTable.qs'))
# 
# change <- long %>% 
#   group_by(gc, species) %>% 
#   mutate(Total = Value * 6.25,
#          Previous = lag(Total), 
#          Next = lead(Total),
#          Change <- (Total - Previous),
#          pctChange = (Total - Previous)/Previous) %>% 
#   ungroup()
#  #qs::qsave(change, file = glue('./tables/yr_changeTable.qs'))
# change <- qs::qread('./tables/yr_changeTable.qs')
# canESM <- filter(change, gc == 'CanESM2')
# # making a bar plot -------------------------------------------------------
# 
# plot <- ggplot(data = canESM, aes(x = Year, y = pctChange)) +
#         geom_col(position = 'dodge') +
#         #scale_y_continuous(labels = scales::percent) +
#         #facet_wrap(~  species, scale = 'free_y', ncol = 7) + 
#         facet_wrap(~  species, ncol = 10, nrow = 8) + 
#         theme_bw() +
#         labs(title = 'Percent change per Year') +
#         theme(panel.grid.major = element_blank(),
#         axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
#         axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
#         labs(y =' % change') 
#         
# ggsave(plot = plot, filename = glue('./graphs/maps/ratio/test_{spc}.png'),
#        units = 'in', width = 12, height = 9, dpi = 700)       
# 
# loli<- ggplot(data = change, aes(x = Year, y = pctChange, color = gc)) +
#   geom_point(size = 4) +
#   geom_segment(aes(x = Year, y = 0, xend = Year, yend= pctChange), 
#                color = 'grey50', size = 1) +
#   theme_light() +
#   theme(panel.grid.major.x = element_blank(),
#         panel.border = element_blank(),
#         axis.ticks.x = element_blank())+
#   xlab('') +
#   ylab('% change') +
#   facet_wrap(~ species, scale = 'free') 
# 
# ggsave(plot = loli, filename = glue('./graphs/maps/ratio/lolitest_{spc}.png'),
#        units = 'in', width = 12, height = 9, dpi = 700) 
#   
# change <- qs::qread('../qs/yr_changeTable.qs')
# subd <- filter(change, species == 'ALFL')
# 
# gtst <- ggplot(data = subd, aes(x = Year, y = pctChange,))+ 
#   geom_point(size = 8, aes(col = gc, group = gc)) + 
#   geom_segment(
#     aes(x = Year, y = 0, xend = Year, yend = pctChange, group = gc, 
#         colour = gc), size = 1) + 
#   facet_wrap(.~gc) +
#   theme_bw()

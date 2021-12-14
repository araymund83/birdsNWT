# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rcartocolor, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, hrbrthemes, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon, ggridges)


g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

meansTable <- function(spc){
  #spc <- spcs[4] # Run and comment (after)
  message(crayon::blue('Starting table for', spc, '\n'))
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/tbl_yrs_{spc}.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- as_tibble(tbl)
  tbl <- select(tbl, -c(lon, lat))
  gcm <- unique(tbl$gc)
  
  tblLong <- tbl %>% pivot_longer( !gc, names_to = 'year', values_to = 'value')

# ##  make density plot with ggplot 
  message(crayon::blue('Making plot for:', spc, '\n'))
  
  violinPlot <- ggplot(tblLong, aes(x =gc, y = value, fill = gc)) + 
    geom_violin(alpha = 0.7) +   ## add alpha = 0.4 to add transparency
    geom_boxplot(width = 0.1) +
    scale_fill_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
    #coord_flip() +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = 'bold'),
          legend.title = element_blank(),
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.spacing.x = unit(5, 'mm'),
          axis.text.x = element_text(size = 8)) +
    labs(y = '', x  = '') +
    ggtitle(label = spc)
  
  ggsave(plot = violinPlot, 
         filename = glue('./graphs/figs/densityYrMeans/{spc}_violinMeans.png'), 
         units = 'in', width = 13, height = 10, dpi = 700)
 

message(crayon::blue('Done!'))
}
map(.x = spcs, .f = meansTable)

# densityPlot<- ggplot(tblLong, aes(x = value, fill = gc)) + 
#               geom_density(adjust = 1.5, alpha = 0.4) +
#               scale_fill_manual(values = c("#FF6A00","#C15CCB", "#00868B")) +
#               theme_bw() +
#               theme(plot.title = element_text(size = 14, face = 'bold'),
#                     legend.title = element_blank(),
#                     legend.position = 'right',
#                     panel.grid.major = element_blank(),
#                     panel.spacing.x = unit(5, 'mm'),
#                     axis.text.x = element_text(size = 8)) +
#               ggtitle(label = spc)
# 
# ggsave(plot = densityPlot, 
#        filename = glue('./graphs/figs/densityYrMeans/{spc}_densityMeans.png'), 
#        units = 'in', width = 13, height = 10, dpi = 700)
  

# histPlot <- ggplot(tblLong, aes(x = value, fill = gc)) + 
#   geom_histogram(bindwith = 1, alpha = 0.4)+
#   scale_fill_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 14, face = 'bold'),
#         legend.title = element_blank(),
#         legend.position = 'right',
#         panel.grid.major = element_blank(),
#         panel.spacing.x = unit(5, 'mm'),
#         axis.text.x = element_text(size = 8)) +
#   ggtitle(label = spc)
# 
# ggsave(plot = histPlot, 
#        filename = glue('./graphs/figs/densityYrMeans/{spc}_histMeans.png'), 
#        units = 'in', width = 13, height = 10, dpi = 700)

# boxPlot <- ggplot(tblLong, aes(x = gc, y = value, fill = gc)) + 
#   geom_boxplot() +   ## add alpha = 0.4 to add transparency
#   scale_fill_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
#   coord_flip() +
#   theme_bw() +
#   theme(plot.title = element_text(size = 14, face = 'bold'),
#         legend.title = element_blank(),
#         legend.position = 'none',
#         panel.grid.major = element_blank(),
#         panel.spacing.x = unit(5, 'mm'),
#         axis.text.x = element_text(size = 8)) +
#   labs(y = '', x  = '') +
#   ggtitle(label = spc)
# 
# ggsave(plot = boxPlot, 
#        filename = glue('./graphs/figs/densityYrMeans/{spc}_boxPlotMeans.png'), 
#        units = 'in', width = 13, height = 10, dpi = 700)




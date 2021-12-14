# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Function ----------------------------------------------------------------
get_scatterplot <- function(spc){
  
  spc <- spcs[2] # Run and erase or use #
  
  cat('Start\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./qs/{spc}_table_ratio.qs'))
  tbl <- dplyr::select(tbl, lon, lat, everything())
  tbl <- as_tibble(tbl)
  tbl <- dplyr::select(tbl, lon, lat, gc, avg)
  ##tbl <- .Last.value  ## sirve para poder recuperar el ultimo objeto ejecutado 
  tbl <- tbl %>% spread(gc, avg)
  colnames(tbl) <- gsub('-', '_', colnames(tbl)) 
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  
  
  set.seed(1234)
  smp <- sample_n(tbl = tbl, size = nrow(tbl) * 0.01, replace = FALSE)
  smp$gid
  
  
  cat('Done!\n')
  
}

b <- ggplot(data = tbl) + geom_point(aes(x = y2011, y = y2091, fill = gc)) + 
  scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                                'INM-CM4' = "#00868B")) + theme_bw()

ggsave(plot = b, filename = glue('./graphs/figs/scatter/scatterplot_change{spc}.png'),
       units = 'in', width = 12, height = 9, dpi = 700)

scatterPlot <- ggplot(tbl, aes(x = y2011, y = y2091, color = gc)) +
  geom_point(size = 1)+
  theme_ipsum()
                      
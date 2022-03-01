# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst, exactextractr,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
fles <- dir_ls('./qs/zonal', regexp = 'log2')

# Function ----------------------------------------------------------------
make_graph <- function(fle){
  
  #fle <- fles[1]
  spc <- str_sub(basename(fle), 1, 4)
  cat('Start\n')
  tbl <- qs::qread(file = fle)
  tbl
  grp <- ggplot(data = tbl, aes(x = region, y = average, col = model)) + 
    geom_point(size = 3) + 
    scale_color_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                                   'INM-CM4' = "#00868B")) +
    coord_flip() +
    ggtitle(label = glue('Zonal mean log2 density {spc}')) +
    #theme_ipsum_es() + 
    theme_bw() +
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 14)) + 
    labs(x = 'Ecoregion', y = 'Mean', col = '')
  
  ggsave(plot = grp,
         filename = glue('./graphs/figs/zonal_ecoregions/zonalEco2_{spc}.png'), 
         units = 'in', width = 12, height = 9, dpi = 300)
  
  cat('Done!\n')
}

# Apply the function ------------------------------------------------------

map(.x = fles, .f = make_graph)

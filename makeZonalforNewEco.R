
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr,tidyverse, terra, yaImpute )


g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './qs/zonal'
fles <- dir_ls(root, regexp = '2.qs$')
head(fles); length(fles); tail(fles)

spcs <- str_sub(basename(fles), 1, 4)
spcs <- unique(spcs)

# Make graph function -----------------------------------------------------
make_graph <- function(spc){
  #spc <- spcs[1]
  cat(spc, '\t')
  fle <- grep(spc, fles, value = TRUE)
  tbl <- qs::qread(fle)
  tbl <- as_tibble(tbl)
  gpn <- ggplot(data = tbl, aes(x = region, y = average, group = model, col = model)) +
    geom_point(size = 3) + 
    scale_color_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                                  'INM-CM4' = "#00868B")) +
    coord_flip() + 
    ggtitle(label = glue('Zonal mean log2 density {spc}')) +
    theme_bw() +
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5), 
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 13, face = 'bold')) +
    labs(x = 'Region', y = 'logRatio', color = 'GCM') +
    guides(color = guide_legend(override.aes = list(size = 12)))
  
  out <- glue('./graphs/figs/zonal_ecoregions/zonalNewEco_{spc}.jpg')
  ggsave(plot = gpn, filename = out, units = 'in', width = 9, height = 7, dpi = 700)
} 

# Apply the function ------------------------------------------------------
map(spcs, make_graph)



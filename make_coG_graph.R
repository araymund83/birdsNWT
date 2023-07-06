# Load libraries ----------------------------------------------------------
library(pacman)
p_load(dplyr, fs, ggplot2,glue, qs, tidyverse)
# Load data ---------------------------------------------------------------
root <- './tables/coG'
fles <- dir_ls(root, regexp = '.qs$')
spcs <- str_sub(basename(fles), 1, 4)
spcs <- unique(spcs)
yrs <- c('2011', '2031','2091')
# Make graph function -----------------------------------------------------
make_coGgraph <- function(spc){
  #spc <- spcs[1]
  cat(spc, '\t')

  fle <- grep(spc, fles, value = TRUE)
  tbl <- qs::qread(fle)
  tbl <- as_tibble(tbl)
  tbl <- tbl %>% group_by(model) %>% mutate(year = as.factor(year))
  tbl<- tbl %>% filter(year %in% c('2011', '2031', '2091'))
  gpn <- ggplot(data = tbl, aes(x = year, y = COGy, group = model, col = model)) +
   # geom_errorbar( aes(ymin = COGy - COGy.sd, ymax = COGy + COGy.sd), width = 0.2,
    #               position = position_dodge2(0.5))+
    geom_point(size = 3) + 
    scale_color_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                                  'INM-CM4' = "#00868B")) +
    facet_wrap(~model) + 
    ggtitle(label =spc)+
    labs(x = 'Year', y= 'Centroid y-coordinate', col = '') +
    scale_x_discrete(labels = yrs) +
    theme_bw() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 16, face = 'bold', hjust = 0, vjust = 0.7), 
          axis.title = element_text(size = 14, face = 'bold'),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 12, face = 'bold'), 
          strip.text = element_text(size =12)) 
  ggsave(plot = gpn,
         filename = glue('./graphs/figs/coG/coG_{spc}.png'), 
         units = 'in', width = 12, height = 9, dpi = 700)
  
  #return(gpn)
}

# Apply the function ------------------------------------------------------
map(.x = spcs, .f = make_coGgraph )



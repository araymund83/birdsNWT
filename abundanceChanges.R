# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
options(scipen = 999)

data<- qs::qread( './tables/totalAbundance.qs')

abunChange<- data %>% 
  group_by(gcm, specie) 
 
gcm <- unique(abunChange$gcm)

group.colors <- c(CanESM2 = '#FF6A00', CCSM4 = '#C15CCB', INM.CM4 = '#00868B')

make_loliPlot <- function(sp){
 # sp <- spcs[1]
  subd <- filter(data, specie == sp)
  years <- c('2011', '2031', '2051', '2071','2091','2100')
  
  message(crayon::green('Making lolipop plot for:', sp))  
  
  loliplot <- ggplot(data = subd, aes(x = year, y = value)) + 
    geom_point(size = 8, aes(col = gcm, group = gcm)) + 
    scale_color_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
    geom_segment(aes(x = year, y = 0, xend = year, yend = value, group = gcm, 
                     colour = gcm), size = 1) + 
    geom_hline(yintercept = 0, color = 'black', size = 0.5) +
    #scale_y_continuous(labels = scales::percent) +
    facet_wrap(.~gcm) +
    theme_bw() +
    ggtitle(label = sp) +
    theme(plot.title = element_text(size = 14, face = 'bold'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0.5),
          legend.position = 'none') +
    labs( y = 'Population Size', x = 'Year') +
    scale_x_discrete(labels = years)
  
  ggsave(plot = loliplot, filename = glue('./graphs/figs/yrChange/abund/lolipopPlot2_{sp}.png'),  ## the notation is not scientific
         units = 'in', width = 12, height = 9, dpi = 700)
  return(loliplot)
}
# Apply the function ------------------------------------------------------
ggs <- map(.x = spcs, .f = make_loliPlot)

saveRDS(ggs, file = './ggs.rds')


gal <- ggarrange(plotlist = ggs, ncol = 7, nrow = 11)
ggsave(plot = gal, filename = './graphs/figs/yrChange/all_species_change2.png', 
       units = 'in', width = 30, height = 28, dpi = 700)

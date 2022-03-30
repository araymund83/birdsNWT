##this script calculates the mean  of the 5 replicates for each species/gcm/year

# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, 
               tidyverse, terra, foreach, fs, future.apply, furrr, fst, glue, compiler)

rm(list = ls())


# create table for plotting rate of change --------------------------------
totalTable <- qs::qread(file = glue('./tables/totalTable.qs'))

long <- totalTable %>%  gather(Year, Value, - c(gc, species))
long <- long %>% mutate(Year = as.factor(Year))
levels(long$Year) <- c('2011','2031','2051', '2071', '2091', '2100')
long <- long %>% 
  arrange(gc, species, Year) 


cumm_change <- long %>% 
  arrange(gc, Year, species) %>% 
  group_by(gc, species) %>% 
  mutate(Total = Value * 6.25,
         lag =lag(Total),
         change = (Total - lag) * 100 /lag ,
         First = head(Total, 1),
         cumChange = case_when(Total != First ~(Total - First) * 100/First,
                               TRUE ~ 1 * NA)) %>% 
  select(Year, gc, Total, change,cumChange) %>% 
  ungroup()

qs::qsave(cumm_change, file = glue('./tables/cumm_changeTable.qs'))
write.csv(change, './tables/abundcumm_changeTable.csv')

change <- qs::qread('./tables/cumm_changeTable.qs')
change <- change %>%  replace_na((list(cumChange = 0)))
change <- change %>% group_by(gc,species)
qs::qsave(cumm_change, file = glue('./tables/cumm_changeTable.qs'))
group.colors <- c(CanESM2 = '#FF6A00', CCSM4 = '#C15CCB', INM.CM4 = '#00868B')
make_loliPlot <- function(sp){
  #sp <- spcs[1]
  subd <- filter(change, species == sp)
  years <- c('2011', '2031', '2051', '2071','2091','2100')
  
  message(crayon::green('Making lolipop plot for:', sp))  
  
  loliplot <- ggplot(data = subd, aes(x = Year, y = cumChange))+ 
    geom_point(size = 8, aes(col = gc, group = gc)) + 
    scale_color_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
    geom_segment(aes(x = Year, y = 0, xend = Year, yend = cumChange, group = gc, 
                     colour = gc), size = 1) + 
    geom_hline(yintercept = 0, color = 'black', size = 0.5)+
    #scale_y_continuous(labels = scales::percent) +
    facet_wrap(.~gc) +
    theme_bw() +
    ggtitle(label = sp) +
    theme(plot.title = element_text(size = 16, face = 'bold'),
          text= element_text(size =14),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.title.x = element_text(size = 16, face = 'bold'),
          axis.title.y = element_text(size = 16, face = 'bold'),
          axis.text.y = element_text(size = 14), 
          axis.text.x = element_text(size = 14, angle = 90, vjust= 0.5, hjust = 0.5),
          legend.position = 'none') +
    labs( y = 'Change (%)', x = 'Year') +
    scale_x_discrete(labels = years) 
  
  
  ggsave(plot = loliplot, filename = glue('./graphs/figs/cummChange/cummchange_{sp}.png'),
         units = 'in', width = 12, height = 9, dpi = 700)
  return(loliplot)
}
# Apply the function ------------------------------------------------------
ggs <- map(.x = spcs, .f = make_loliPlot)

saveRDS(ggs, file = './ggs.rds')


gal <- ggarrange(plotlist = ggs, ncol = 7, nrow = 11)
ggsave(plot = gal, filename = './graphs/figs/yrChange/all_species_change2.png', 
       units = 'in', width = 30, height = 28, dpi = 700)
library(RColorBrewer)
library(scales)
library("rcartocolor")
divColors <-brewer.pal(6, "RdYlGn") %>%
  colorRampPalette()
gslp <- ggplot() + 
  geom_tile(data = slpe.tble, aes(x = x, y = y, fill = slp)) + 
  geom_sf(data = limt, fill = NA) +
  facet_wrap(.~model, ncol = 3, nrow = 1) +
  #scale_fill_manual(values = brewer.pal(name = 'YlOrRd', n = 7)) + 
  #scale_fill_binned_diverging(palette = 'Red-Green', n.breaks = 6) +
  #scale_fill_continuous_diverging(palette = 'Red-Green', p1 = -0.02, p2 = 0.02) +
  #scale_fill_gradientn (name = "slope", colours = divColors(5)) +
  #scale_color_gradient( low = '#D73027', high = '#1A9850'
  #scale_fill_gradientn(colours= c('#D73027', '#e9e9e9','#1A9850'), limits = c(min,max), oob = scales::squish) +  
  #rcartocolor::scale_fill_Carto_c(name = 'slope', type = 'diverging', palette = 'Geyser', direction = -1) +
  scale_fill_gradient2(limits = c(min, max), low = '#D73027', high = '#1A9850', mid = '#e9e9e9', midpoint = 0) + ## this one was the best option
  #theme_void() +  '
  # theme(plot.title = element_text(size = 20, face = 'bold'),
  #       plot.background = element_blank(),
  #       panel.grid.major = element_blank(),
  #       axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
  #       legend.position = 'bottom',
  #       legend.key.width = unit(3, 'line')) +
  theme_bw() +
  #theme_ipsum_es() + 
  coord_sf() +
  ggtitle(label = spc, ) +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom',
        legend.key.width = unit(3, 'line')) +
  labs(x = 'Longitud', y = 'Latitude', fill = 'Slope') 


ggsave(plot = gslp, 
       filename = glue('./graphs/maps/slopes/{spc}_slpe13.png'), 
       units = 'in', width = 13, height = 10, dpi = 700)

message(crayon::green("Done: ", spc))

# Bar plot for changes ----------------------------------------------------
# making a bar plot -------------------------------------------------------

plot <- ggplot(data = canESM, aes(x = Year, y = pctChange)) +
  geom_col(position = 'dodge') +
  #scale_y_continuous(labels = scales::percent) +
  #facet_wrap(~  species, scale = 'free_y', ncol = 7) + 
  facet_wrap(~  species, ncol = 10, nrow = 8) + 
  theme_bw() +
  labs(title = 'Percent change per Year') +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(y =' % change') 

ggsave(plot = plot, filename = glue('./graphs/maps/ratio/test_{spc}.png'),
       units = 'in', width = 12, height = 9, dpi = 700)       

loli<- ggplot(data = change, aes(x = Year, y = pctChange, color = gc)) +
  geom_point(size = 4) +
  geom_segment(aes(x = Year, y = 0, xend = Year, yend= pctChange), 
               color = 'grey50', size = 1) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank())+
  xlab('') +
  ylab('% change') +
  facet_wrap(~ species, scale = 'free') 

ggsave(plot = loli, filename = glue('./graphs/maps/ratio/lolitest_{spc}.png'),
       units = 'in', width = 12, height = 9, dpi = 700) 


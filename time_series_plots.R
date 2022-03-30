# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(dplyr, ggplot2, glue,hrbrthemes, plotly,sf, stringr)
           
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
#set ggplot theme 
theme_set(theme_bw())

change <- qs::qread('./tables/yr_changeTable.qs')
totalTable <- qs::qread(file = glue('./tables/totalTable.qs'))
abundance<- qs::qread( './tables/totalAbundance.qs')

sum <- abundance |> 
  filter(year %in% c(2011, 2091)) |> 
  spread(year, value) |> 
  setNames(c('specie', 'gcm', 'y2011', 'y2091'))

traits <- read.csv('./inputs/birdSpecies_traits.csv')
traits2 <- traits %>% dplyr::select(Group,species,Family)
colnames(traits2) <- c('Group', 'specie', 'Family')


sppAll <- left_join(totalTable, traits2, by = 'species')
sppAllLong <- left_join(change, traits2, by = 'species')

sppGroup <- sppAllLong %>% group_by(gc, species, Group) %>% 
  mutate(rate_change = (Total - lag(Total, default = 0))/ lag(Total)) %>% 
  ungroup()

summaryGroup <- sppGroup %>%  group_by (gc, Year, Group) %>% 
               summarise(across(where(is.numeric))) %>%
               summarise(across(everything(), list(sum))) %>% 
  ungroup()
  


sppGroup %>% 
  filter(Group %in% c('Forest', 'Grasslands')) %>% 
  ggplot(aes(x = Year, y = Total, color = Group)) +
  geom_point() + 
  facet_wrap(~gc)+
  labs(x = 'Year', y = 'Abundance', 
       title = 'Predicted changes in total population size for Forest birds')
 
years <- c('2011', '2031', '2051', '2071','2091','2100') 

totalPlot<- summaryGroup %>% 
  ggplot(aes(x = Year, y = Total_1, group = Group, color = Group)) +
  geom_point(aes(colour = Group, shape = Group)) +
  geom_line(aes(colour = Group))+
  facet_wrap( ~ gc) +
  labs(x = 'Year', y = "Predicted Abundance") +
  scale_x_discrete(labels = years) +
  scale_y_continuous(labels = scales::comma)
  

  #scale_y_continuous(labels = scales::scientific)
ggsave(plot = totalPlot, filename = glue('./graphs/figs/yrChange/totalGroup.png'),
       units = 'in', width = 12, height = 9, dpi = 700)


data <- left_join(sum, traits2, by = 'specie')

# Making the scatterplot -------------------------------------------------

x <- 2011
y <- 2031
#Make comparisons between gcms
# gcm1 - gcm2 
# gcm1 - gcm3
# gcm2 - gcm3

# A simple scatterplot 
colnames(traits2)<- c('Group', 'specie', 'Family')
abundance <- left_join(abundance, traits2,by= 'specie' )
sum <- abundance |> 
  filter(year %in% c(2011, 2091)) |> 
  spread(year, value) |> 
  setNames(c('specie', 'gcm', 'Group', 'Family', 'y2011', 'y2091'))

data <- qs::qread(file = './tables/totalAbundance1191.qs')
data <-sum

# Functions ---------------------------------------------------------------
make_graph <- function(data){
  
  yr1 <- '2011'
  yr2 <- '2091'
  gcm <- unique(data$gcm)
  corrTable <- map(.x = 1:length(gcm), .f = function(gc){
    message(crayon::green('Loading files for', gcm[gc]))
    tble <- data |> filter(gcm == gcm[gc])
    corl <- tble |> 
      group_by(gcm) |> 
      summarise(corr = cor(y2011,y2091, method = 'pearson')) |> 
      ungroup() |> 
      mutate(corr = round(corr, 2))
    
    
    cat('Making the correlation graph\n')
    
    gsct <- tble %>%  ggplot(aes(x = y2011, y = y2091, color = Group)) + 
      geom_point(aes(color = Group, shape = Group), 
                 size = 2, alpha = 0.8) +
      #scale_color_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
      ggrepel::geom_text_repel(aes(label = specie), size = 5,
                      # min.segment.length = 0,
                      # seed = 42,
                      # box.padding = 0.5,
                      #max.overlaps = Inf,
                      # arrow = arrow(length = unit(0.010, "npc")),
                      # nudge_x = .15,
                      # nudge_y = .5,
                      color = 'Grey30'
      ) +
      # geom_label(label = tble$specie,
      #           nudge_x = 0.5, nudge_y = 0.5,
      #          check_overlap = T) +
      #scale_color_manual(values = c(CanESM2 = '#FF6A00', CCSM4 = '#C15CCB', INM.CM4 = '#00868B')) + 
      #geom_text(aes(x = 40000000, y = 20000000, label = glue('r = {corl[1,2]}')), col = '#BC679B') +
      #geom_text(aes(x = 40000000, y = 19000000, label = glue('r = {corl[2,2]}')), col = '#3E51E3') +
      #geom_smooth(method = 'lm', se = TRUE) +
      geom_abline(intercept = 0, slope = 1, colour = 'black') +
      geom_abline(intercept = 0, slope = 0.5, colour = 'blue', linetype = 'dashed') +
      geom_abline(intercept = 0, slope = 0.2, colour = 'red', linetype = 'dashed') +
      geom_abline(intercept = 0, slope = 2, colour = 'green', linetype = 'dashed') +
      ggtitle(label = gcm[gc]) +
      theme_bw() +
      scale_x_continuous(labels = scales::scientific) +
      scale_y_continuous(labels = scales::scientific)+
      theme(plot.title = element_text(size = 16, face = 'bold'),
            axis.title.x = element_text(size = 18, face = 'bold'),
            axis.title.y = element_text(size = 18, face = 'bold'),
            axis.text.y = element_text(angle = 90, vjust = 0.5,  hjust = 0.5, size = 16),
            axis.text.x = element_text(size = 16),
            aspect.ratio = 1,
            legend.position = 'bottom',
            legend.text = element_text(size= 12)) +
      labs(x = 2011, y = 2091, col = 'Group') 
      
    
    ggsave(plot = gsct, filename = glue('./graphs/figs/scatter/group_abund1191newslopes72_{gcm[gc]}.png'),  ## the notation is not scientific
           units = 'in', width = 12, height = 9, dpi = 700)
    
    return(gsct)
  })
}
corPlot <- make_graph(data = data)

# Join all into only one
gall <- ggpubr::ggarrange(corPlot, ncol = 1, nrow = 3)
gall <- gridExtra::grid.arrange(grobs = corPlot)
yr1yr2 <- make_graph(data = sum)

# Join all into only one
gall <- ggpubr::ggarrange(plotlist= yr1yr2, ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom')
design <- "#BB AA#"
gall <- wrap_plots(yr1yr2) & theme(legend.position = 'bottom')
gall<- gall + plot_layout(guides = 'collect')



ggsave(plot = gall, filename = './graphs/figs/scatter/corr_graph1131_slopnew.png', units = 'in', width = 9, height = 17, dpi = 700)

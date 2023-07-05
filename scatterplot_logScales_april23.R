#This script asses the comments made to the second draft of manuscript made on 
#april 2023. It only labels the bellringers, species that are increase double or that 
#decrease by half in abundance. 

# Load libraries  ---------------------------------------------------------
library(pacman)
p_load( dplyr,fs, ggplot2, glue,qs,sf,tidyverse)
g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]

# Load data ---------------------------------------------------------------
#abundance72<- qs::qread('./tables/totalAbundance1191.qs')
#abundancetotal<- qs::qread('./tables/totalTable.qs')
#change<- qs::qread('./tables/yr_changeTable.qs')

abundance<- qs::qread('./tables/totalAbundance.qs')

abundance <- abundance %>% filter(specie != 'BARS' & specie != 'NOFL', specie != 'PIWO')
#abundance <- abundance %>% filter(gcm != 'baseline')
#abundance <- abundance %>%  select(specie, year, gcm, total, log2Ratio)
traits <- read.csv('./inputs/birdSpecies_traits.csv')
traits2 <- traits %>% dplyr::select(Group,species,Family)
colnames(traits2) <- c('Group', 'specie', 'Family')
abundance <- left_join(abundance, traits2,by= 'specie' )

df <- abundance|> 
  filter(year %in% c(2011, 2031)) |> 
  spread(year, value) |> 
  setNames(c('specie', 'gcm', 'Group', 'Family', 'y2011', 'y2031'))
df<- df %>% select(specie, gcm, Group, Family, y2011, y2031) %>% 
  mutate(WL = ifelse(y2031 > 2 * y2011,  specie,
                     ifelse(y2031 < 0.5 * y2011, specie, '')))


data <- df
#add labels only for species that increase by double or decreased by half
data$WL[data$y2031 > 2 * data$y2011 & 
                    data$y2031 < 0.5 * data$y2011] <- data$specie
# Functions ---------------------------------------------------------------
make_graph <- function(data, yr1, yr2){
  

  gcm <- unique(data$gcm)
  corrTable <- map(.x = 1:length(gcm), .f = function(gc){
    message(crayon::green('Loading files for', gcm[gc]))
    tble <- data |> filter(gcm == gcm[gc])
   
    corl <- tble |> 
      group_by(gcm) |> 
      summarise(corr = cor(y2011,y2031, method = 'pearson')) |> 
      ungroup() |> 
      mutate(corr = round(corr, 2))
    
    y1 = tble$y2011
    y2 = tble$y2031
   
    
  cat('Making the correlation graph\n')
    
gsct <- tble %>%  ggplot(aes(x = y1, y = y2, color = Group)) + 
  geom_point(aes(color = Group, shape = Group), 
             size = 2.5, alpha = 0.8)+
  scale_x_continuous(limits = c(10000,45000000), trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(expr = .x))) +
  scale_y_continuous(limits = c(10000,45000000), trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(expr =.x))) +
  geom_abline(intercept = 1, slope = 1, colour = '#009E73', linetype = 'dashed') + #increase of a factor of 2
  geom_abline(intercept = log(1.5), slope = 1, colour = '#009E73', linetype = 'dotted') + #increase of a factor of 2
  geom_abline(intercept = -1, slope = 1, colour = '#661100', linetype = 'dashed') + # decrease of 2
  geom_abline(intercept = log(0.65), slope = 1, colour = '#AA4499', linetype = 'dotted') + # decrease of 2
  geom_abline(intercept = 0, slope = 1, colour = 'black') + # increase of 1.5
  ggtitle(label = gcm[gc]) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(size = 16, face = 'bold'),
        axis.title.y = element_text(size = 16, face = 'bold'),
        axis.text.y = element_text(vjust = 0.5,  hjust = 0.5, size = 12),
        axis.text.x = element_text(vjust = 0.5,  hjust = 0.5, size = 12),
        aspect.ratio = 1,
        legend.position = 'bottom',
        legend.text = element_text(size= 14)) +
  labs(x = expression(bold('log2(abundance)' [2011])),
       y = expression(bold('log2(abundance)' [2031]))) +
    ggrepel::geom_text_repel(#data = data[which(data$log2ratio >0.58), ],
    aes(label = WL), size = 4, 
    #fontface = 'bold',
    #family = 'Arial',
    #hjust = 0.05,
    # min.segment.length = 0,
    #force = 100,
    box.padding = unit(0.1, 'lines'),
    max.overlaps = 23,
    #max.overlaps = Inf, ## overlaps all labels 
    # arrow = arrow(length = unit(0.010, "npc")),
    # nudge_x = .15,
    # nudge_y = .5,
    color = 'Grey30'
  ) 
out <- glue('./graphs/figs/scatter') 
ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
  
ggsave(plot = gsct, 
       filename = glue('{out}/abun_{yr1}-{yr2}_log2_{gcm[gc]}.png'),  ## the notation is not scientific
       units = 'in', width = 12, height = 9, dpi = 700)

return(gsct)
  })
}
corPlot2 <- make_graph(data = data, yr1 = '2011', yr2 = '2031')

# Join all into only one
out <- glue('./graphs/figs/scatter') 
ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))

gall <-ggsave(glue('{out}/winnersLosers1131.png'), 
              ggpubr::ggarrange(plotlist = corPlot2,
                                heights = c(1,1), 
                                widths = c(1,1),
                                align = 'hv', ncol = 3,
                                labels = c('a)', "b)", 'c)'),
                                vjust = 3,
                                legend = 'bottom',
                                common.legend = T), 
              units = 'in', height = 7, width = 18, dpi = 700)


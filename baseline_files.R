# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './inputs/baseline'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

fls <- list.files(dirs, pattern = '.tif$', full.names = TRUE)
ras <- terra::rast(fls)
dfr <- lapply(ras, as.data.frame, xy = TRUE)
df <- reduce(dfr, full_join, by = c('x', 'y'))

qs::qsave(x = df2, file = glue('./outputs/baseline/baseline_all.qs'))
  
## Creating the baaseline table
table<- qs::qread(file = glue('./baseline/baseline_all.qs'))

base_total <- table %>% as_tibble() %>%  select(3:74)
colnames(base_total) <- spcs
base <- base_total %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
base_long <- pivot_longer(base_total,cols = 1:72, names_to = 'species', values_to = 'value')

baseline_total <- base_long %>% mutate(total = value * 6.25,
                                       year = '2010',
                                       gcm = 'baseline')
qs::qsave(x = baseline_total, file = glue('./outputs/baseline/baseline_total_all.qs'))

# reading tables 
abundance<- qs::qread( './tables/totalAbundance.qs')
colnames(abundance) <- c('specie', 'year', 'gcm', 'total')
abundance72 <- abundance %>% filter(specie != 'BARS' & specie != 'NOFL', specie != 'PIWO')
baseline<- qs::qread(file = './outputs/baseline/baseline_total_all.qs')
colnames(baseline) <- c('specie', 'value', 'total', 'year', 'gcm')
#ordering columns
baseline <- baseline %>% select(specie, year, gcm, total)
write.csv(baseline, file = './tables/baseline_total_all.csv')
baseline <- baseline %>% group_by(specie)
abun72_base <- full_join(abundance72, baseline)
abun72 <- abun72_base %>% group_by( gcm, specie) 
abun72 <- abun72_base %>% mutate(year = as.factor(year))
levels(abun$year) <- c('2010','2011','2031','2051', '2071', '2091', '2100')

#order table putting baseline at the begining of each species 
abun72 <- abun72 %>% arrange(specie,gcm, year, by_group = TRUE) %>%  ungroup()
#save the table
#write.csv(abun72, file = './tables/baseline_total_all.csv')

#abunAll <- read.csv('.tables/baseline_total_all.csv')
#abun72<- read.csv('./tables/baseline_total_72.csv')

traits <- read.csv('./inputs/birdSpecies_traits.csv')
traits2 <- traits %>% dplyr::select(Group,species,Family)
colnames(traits2) <- c('Group', 'specie', 'Family')


spp72_traits <- left_join(abun72, traits2, by = 'specie')
sppAll <- sppAll %>% mutate(year = as.factor(year))
levels(sppAll$year) <- c('2010','2011','2031','2051', '2071', '2091', '2100')



#THIS IS WORKKING !!!(https://thomasadventure.blog/posts/calculating-change-from-baseline-in-r/)
change<- spp72_traits%>%
  arrange(specie, gcm) %>%
  group_by(specie) %>%
  mutate(baseline = total[1L],
         change = total - baseline,
         rateChange = (total - baseline)/baseline,
         pctChg = rateChange *100)  %>%
  ungroup() %>%
  select(specie, gcm, year, total, change, rateChange,pctChg) 
write.csv(change, file = './tables/baseline_change_72.csv')

change <- read.csv('./tables/baseline_change_72.csv')
abundance 
sum <- spp72_traits |> 
  filter(year %in% c(2010, 2031)) |> 
  spread(year, total) |> 
  setNames(c('specie', 'gcm','group', 'family', 'y2010', 'y2031'))

# fill the empty NA for 2010 with the value of baseline 
data <- sum %>% group_by(specie) %>% 
  mutate (y2010 = first(y2010))
          
data2<- slice(data, -(1))

# Making the scatterplot -------------------------------------------------
#filter species 
x <- 2010
y <- 2031
#Make comparisons between gcms
# gcm1 - gcm2 
# gcm1 - gcm3
# gcm2 - gcm3

# A simple scatterplot 
colnames(traits2)<- c('Group', 'specie', 'Family')
abundance <- left_join(abundance, traits2,by= 'specie' )
sum <- change|> 
  filter(year %in% c(2010, 2031)) |> 
  spread(year, total) |> 
  setNames(c('specie', 'gcm', 'group', 'family', 'y2011', 'y2091'))

data <- qs::qread(file = './tables/totalAbundance1191.qs')
data <-sum

# Functions ---------------------------------------------------------------
make_graph <- function(data){
  
  yr1 <- '2010'
  yr2 <- '2031'
  gcm <- unique(data2$gcm)
  corrTable <- map(.x = 1:length(gcm), .f = function(gc){
    message(crayon::green('Loading files for', gcm[gc]))
    tble <- data |> filter(gcm == gcm[gc])
    corl <- tble |> 
      group_by(gcm) |> 
      summarise(corr = cor(y2010,y2031, method = 'pearson')) |> 
      ungroup() |> 
      mutate(corr = round(corr, 2))
    
    
    cat('Making the correlation graph\n')
    
    gsct <- tble %>%  ggplot(aes(x = y2010, y = y2031, color = group)) + 
      geom_point(aes(color = group, shape = group), 
                 size = 2, alpha = 0.8) +
      #scale_color_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
      ggrepel::geom_text_repel(aes(label = specie), size = 5,
                               # min.segment.length = 0,
                               # seed = 42,
                               # box.padding = 0.5,
                               # max.overlaps = Inf,
                               # arrow = arrow(length = unit(0.010, "npc")),
                               # nudge_x = .15,
                               # nudge_y = .5,
                               color = 'Grey30'
      ) +
       # geom_label(label = tble$specie,
       #           nudge_x = 0.5, nudge_y = 0.5,
       #          check_overlap = T) +
      scale_color_manual(values = c(CanESM2 = '#FF6A00', CCSM4 = '#C15CCB', INM.CM4 = '#00868B')) + 
      geom_text(aes(x = 40000000, y = 20000000, label = glue('r = {corl[1,2]}')), col = '#BC679B') +
      geom_text(aes(x = 40000000, y = 19000000, label = glue('r = {corl[2,2]}')), col = '#3E51E3') +
      geom_smooth(method = 'lm', se = TRUE) +
      #geom_abline(intercept = 0, slope = 1, colour = 'black') +
      #geom_abline(intercept = 0, slope = 0.5, colour = 'blue', linetype = 'dashed') +
      #geom_abline(intercept = 0, slope = 0.2, colour = 'red', linetype = 'dashed') +
      #geom_abline(intercept = 0, slope = 2, colour = 'green', linetype = 'dashed') +
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
      labs(x = 2010, y = 2031, col = 'group') 
    
    
    ggsave(plot = gsct, filename = glue('./graphs/figs/scatter/base/group72_abund1131_test_{gcm[gc]}.png'),  ## the notation is not scientific
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



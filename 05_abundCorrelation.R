##the aim of this script is to create the pairwise comparison between yr1 (eg.2011)
##and yr2 (eg. 2091) and gcm's. 
##it gets the sum of all pixels per species

g <- gc(reset = TRUE)
rm(list = ls())

# Load libraries
require(pacman)
p_load(raster, rgdal, rgeos, stringr, tidyverse, qs, fs, glue, ggrepel)
#options(scipen = 999) ## use only if you want to not use scientific notation

# Load data  --------------------------------------------------------------
#this internal function allow you to sums all pixels 

makeSum <- function(rasterStack){
   rasList = lapply(X = rasterStack, FUN = function(stack){
      sumRas <- lapply(stack, FUN = function(x){
      message(crayon::blue('Calculating sum for:', stack))
      ras <- cellStats(x, sum, na.rm = TRUE) 
     })
   return(sumRas)
  })
   return(rasList)
}

data <- makeSum(meanStack)




# Unlist and make data frame
data <- flatten(data)
data <- map(data, as.data.frame)
data <- map(data, rownames_to_column) ## pass the rownames to column names

data <- map(1:length(data), function(k){
  data[[k]] |> 
    setNames(c('name', 'sum'))
})
data <- bind_rows(data) |> as_tibble()

#multiply for 6.25 ha (250 * 250 pixel resolution) when working with densities
data <- data |> 
  mutate(total = sum * 6.25) |> 
  separate(col = name, into = c('sum', 'specie', 'year', 'gcm'), sep = '_')


sum <- data |> 
  dplyr::select(-sum)
qs::qsave(x = sum, file = './tables/totalAbundance72_spp.qs')
abundance <- qs::qread('./tables/totalAbundance.qs')

# Making the scatterplot -------------------------------------------------

x <- 2011
y <- 2031
#Make comparisons between gcms
# gcm1 - gcm2 
# gcm1 - gcm3
# gcm2 - gcm3

# A simple scatterplot 
sum <- abundance |> 
  filter(year %in% c(2011, 2031)) |> 
  spread(year, value) |> 
  setNames(c('specie', 'gcm', 'y2011', 'y2031'))

data <- qs::qread(file = './tables/totalAbundance1191.qs')


# Functions ---------------------------------------------------------------
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
      summarise(corr = cor(y2011,y2031, method = 'pearson')) |> 
      ungroup() |> 
      mutate(corr = round(corr, 2))
    
    
    cat('Making the correlation graph\n')
    
    gsct <- tble %>%  ggplot(aes(x = y2011, y = y2031, color = Group)) + 
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
      labs(x = 2011, y = 2031, col = 'Group') 
    
    
    ggsave(plot = gsct, filename = glue('./graphs/figs/scatter/group_abund1131newslopes72_{gcm[gc]}.png'),  ## the notation is not scientific
           units = 'in', width = 12, height = 9, dpi = 700)
    
    return(gsct)
  })
}

# Apply the function ------------------------------------------------------
corPlot <- make_graph(data = data)

# Join all into only one
gall <- ggpubr::ggarrange(corPlot, ncol = 1, nrow = 3)
gall <- gridExtra::grid.arrange(grobs = corPlot)


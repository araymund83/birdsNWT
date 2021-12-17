# Load libraries
require(pacman)
p_load(raster, rgdal, rgeos, stringr, tidyverse, qs, fs, glue, ggrepel)


# Load data  --------------------------------------------------------------
#this internal function allow you to sums all pixeles 

makeSum <- function(rasterStack){
   rasList = lapply(X = meanStack, FUN = function(stack){
      sumRas <- lapply(stack, FUN = function(x){
      message(crayon::blue('Calculating sum'))
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
    setNames(c('name', 'value'))
})
data <- bind_rows(data) |> as_tibble()

#multiply for 6.25 ha (250 * 250 pixel resolution)
data <- data |> 
  mutate(value = value * 6.25) |> 
  separate(col = name, into = c('mean', 'specie', 'year', 'gcm'), sep = '_')


data <- data |> 
  dplyr::select(-mean)
qs::qsave(x = data, file = './tables/totalAbundance.qs')
abundance <- qs::qread('./tables/totalAbundance.qs')

# Making the scatterplot -------------------------------------------------

# x <- 2011
# y <- 2091
#Make comparisons between gcms
# gcm1 - gcm2 
# gcm1 - gcm3
# gcm2 - gcm3

# A simple scatterplot 
data <- data |> 
  filter(year %in% c(2011, 2091)) |> 
  spread(year, value) |> 
  setNames(c('specie', 'gcm', 'y2011', 'y2091'))

data <- qs::qread(file = './tables/totalAbundance1191.qs')


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
      summarise(corr = cor(y2011, y2091, method = 'pearson')) |> 
      ungroup() |> 
      mutate(corr = round(corr, 2))
  
  
  cat('Making the correlation graph\n')
  
  gsct <- ggplot(data = tble, 
                 aes(x = y2011, y = y2091, col = gcm)) + 
    geom_point(aes(color = gcm, shape = gcm), 
               size = 1.5, alpha = 0.8) +
    scale_color_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
    # geom_text_repel(
    #   aes(label = specie),
    #   family = "Poppins",
    #   size = 3,
    #   min.segment.length = 0, 
    #   seed = 42, 
    #   box.padding = 0.5,
    #   max.overlaps = Inf,
    #   arrow = arrow(length = unit(0.010, "npc")),
    #   nudge_x = .15,
    #   nudge_y = .5,
    #   color = "grey50"
    # ) + 
    geom_label(label = tble$specie,
              nudge_x = 0.5, nudge_y = 0.5,
             check_overlap = T) +
    #scale_color_manual(values = c(CanESM2 = '#FF6A00', CCSM4 = '#C15CCB', INM.CM4 = '#00868B')) + 
    #geom_text(aes(x = 40000000, y = 20000000, label = glue('r = {corl[1,2]}')), col = '#BC679B') +
    #geom_text(aes(x = 40000000, y = 19000000, label = glue('r = {corl[2,2]}')), col = '#3E51E3') +
    #geom_smooth(method = 'lm', se = TRUE) +
    geom_abline() +
    ggtitle(label = gcm[gc]) +
    #theme_ipsum_es() + 
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = 'bold'),
          axis.text.y = element_text(angle = 90, vjust = 0.5,  hjust = 0.5),
          aspect.ratio = 1,
          legend.position = 'none') +
   
    labs(x = 2011, y = 2091, col = 'GCM')
  
  ggsave(plot = gsct, filename = glue('./graphs/figs/scatter/scatterPlot3_{gcm[gc]}.png'),  ## the notation is not scientific
         units = 'in', width = 12, height = 9, dpi = 700)
  
  return(gsct)
})
}

# Apply the function ------------------------------------------------------
gcms <- unique(data$gcm)
g1g2 <- make_graph(gcm1 = gcms[1], gcm2 = gcms[2])
g1g3 <- make_graph(gcm1 = gcms[1], gcm2 = gcms[3])
g2g3 <- make_graph(gcm1 = gcms[2], gcm2 = gcms[3])
corPlot <- make_graph(data = data)

# Join all into only one
gall <- ggpubr::ggarrange(g1g2, g2g3, g2g3, ncol = 1, nrow = 3)
ggsave(plot = gall, filename = './graphs/figs/corr_graph.png', units = 'in', width = 9, height = 17, dpi = 700)

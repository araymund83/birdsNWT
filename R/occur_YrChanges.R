
makeMean <- function(rasterStack){
  rasList = lapply(X = rasterStack, FUN = function(stack){
    meanRas <- lapply(stack, FUN = function(x){
      message(crayon::blue('Calculating mean for:', stack))
      ras <- cellStats(x, mean, na.rm = TRUE) 
    })
    return(meanRas)
  })
  return(rasList)
}

data <- makeMean(occStack)

# Unlist and make data frame
data <- flatten(data)
data <- map(data, as.data.frame)
data <- map(data, rownames_to_column) ## pass the rownames to column names

data <- map(1:length(data), function(k){
  data[[k]] |> 
    setNames(c('name', 'mean'))
})
data <- bind_rows(data) |> as_tibble()


change <- data %>% 
  group_by(gcm, specie) %>% 
  mutate(Total = mean * 6.25,
         Previous = lag(Total), 
         Next = lead(Total),
         Change <- (Total - Previous),
         pctChange = (Total - Previous)/Previous) %>% 
  ungroup()
#qs::qsave(change, file = glue('./tables/yr_changeTable.qs'))

change <- qs::qread('./tables/yr_changeTable.qs')
change <- change %>%  replace_na((list( pctChange = 0)))

make_loliPlot <- function(sp){
  #sp <- spcs[1]
  subd <- filter(change, specie == sp)
  years <- c('2011', '2031', '2051', '2071','2091','2100')
  
  message(crayon::green('Making lolipop plot for:', sp))  
  
  loliplot <- ggplot(data = subd, aes(x = year, y = pctChange))+ 
    geom_point(size = 8, aes(col = gcm, group = gcm)) + 
    geom_segment(aes(x = year, y = 0, xend = year, yend = pctChange, group = gcm, 
                     colour = gcm), size = 1) + 
    geom_hline(yintercept = 0, color = 'black', size = 0.5)+
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(.~gcm) +
    theme_bw() +
    ggtitle(label = sp) +
    theme(plot.title = element_text(size = 14, face = 'bold'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0.5),
          legend.position = 'none') +
    labs( y = '% change', x = 'Year') +
    scale_x_discrete(labels = years)
  
  ggsave(plot = loliplot, filename = glue('./graphs/figs/yrChange/occur/lolipopPlot_{sp}.png'),
         units = 'in', width = 12, height = 9, dpi = 700)
  return(loliplot)
}
# Apply the function ------------------------------------------------------
ggs <- map(.x = spcs, .f = make_loliPlot)

saveRDS(ggs, file = './ggs.rds')


gal <- ggarrange(plotlist = ggs, ncol = 7, nrow = 11)
ggsave(plot = gal, filename = './graphs/figs/yrChange/all_species_change2.png', 
       units = 'in', width = 30, height = 28, dpi = 700)

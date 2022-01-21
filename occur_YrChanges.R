# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
options(scipen = 999)



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
    setNames(c('name', 'value'))
})
data <- bind_rows(data) |> as_tibble()

data <- separate(data = data, col = name,
                         into = c('occu', 'specie', 'year', 'gcm'), sep = '_')

data <- data |> dplyr::select(-occu)

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
  subd <- data |> filter(specie == sp)
  years <- c('2011', '2031', '2051', '2071','2091','2100')
  
  message(crayon::green('Making lolipop plot for:', sp))  
  
  loliplot <- ggplot(data = subd, aes(x = year, y = value))+ 
    geom_point(size = 8, aes(col = gcm, group = gcm)) + 
    geom_segment(aes(x = year, y = 0, xend = year, yend = value, group = gcm, 
                     colour = gcm), size = 1) + 
    geom_hline(yintercept = 0, color = 'black', size = 0.5)+
   # scale_y_continuous(labels = scales::percent) +
    facet_wrap(.~gcm) +
    theme_bw() +
    ggtitle(label = sp) +
    theme(plot.title = element_text(size = 14, face = 'bold'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0.5),
          legend.position = 'none') +
    labs( y = 'Mean Probability of Occurrence') 
    
  
  ggsave(plot = loliplot, filename = glue('./graphs/figs/yrChange/occur/mean_occ_{sp}.png'),
         units = 'in', width = 12, height = 9, dpi = 700)
  return(loliplot)
}
# Apply the function ------------------------------------------------------
ggs <- map(.x = spcs, .f = make_loliPlot)

saveRDS(ggs, file = './ggs.rds')


gal <- ggarrange(plotlist = ggs, ncol = 7, nrow = 11)
ggsave(plot = gal, filename = './graphs/figs/yrChange/all_species_change2.png', 
       units = 'in', width = 30, height = 28, dpi = 700)


require('methods')
require('mgcv')
subd |> group_by(gcm)
p <- ggplot(a, aes(x = year, y = pctChange)) + 
  geom_point(size = 3, aes(col = gcm, group = gcm))
print(p)

a<- subd |> filter(gcm == 'CanESM2')

p + stat_smooth(method = 'gam', formula = y ~ s(x), size =1)
## R can automatically create these using the poly() function
poly <- p + geom_smooth(method = "lm", formula = pctChange ~ year, size = 1)

ggplot(a, aes(x = year, y = pctChange, colour = factor(gcm))) + geom_point() +
  stat_smooth(aes(group =1), method = "gam", formula = y ~ s(x, k = 3), size =1,
              se = FALSE, colour = 'violet')

p <- ggplot(subd, aes(x = year, y = pctChange, colour = factor(gcm))) +
  geom_point() +
  facet_grid(~gcm)
  stat_smooth( method = "lm", formula = y ~ poly(x,2), size =1,
              colour = 'violet')

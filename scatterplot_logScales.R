abundance<- qs::qread( './tables/totalAbundance.qs')
abundance <- abundance %>% filter(specie != 'BARS' & specie != 'NOFL', specie != 'PIWO')
#abundance <- abundance %>% filter(gcm != 'baseline')
#abundance <- abundance %>%  select(specie, year, gcm, total, log2Ratio)
traits <- read.csv('./inputs/birdSpecies_traits.csv')
traits2 <- traits %>% dplyr::select(Group,species,Family)
colnames(traits2) <- c('Group', 'specie', 'Family')
abundance <- left_join(abundance, traits2,by= 'specie' )

df <- abundance|> 
  filter(year %in% c(2011, 2091)) |> 
  spread(year, value) |> 
  setNames(c('specie', 'gcm', 'Group', 'Family', 'y2011', 'y2091'))
df<- df %>% select(specie, gcm, Group, Family, y2011, y2091)

data <- df
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
                 size = 1.5, alpha = 0.8) +
      #scale_color_manual(values = c( "#FF6A00","#C15CCB",  "#00868B")) +
      ggrepel::geom_text_repel(#data = data[which(data$log2ratio >0.58), ],
        aes(label = specie), size = 4,
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
      ) +
    
      scale_x_continuous(trans= log_trans(),
                         breaks = trans_breaks('log2', function(x) 2^x),
                         labels = trans_format('log2', math_format(2^.x))) +
      scale_y_continuous(trans =log_trans(),
                         breaks = trans_breaks('log2', function(x) 2^x),
                         labels = trans_format('log2', math_format(2^.x)))+
      # geom_abline(intercept = 0, slope = exp(.05), colour = 'green', linetype = 'dashed') + #decrease of half with log10
      # geom_abline(intercept = 0, slope = exp(-0.05), colour = 'red', linetype = 'dashed') + # increase of 1.5 with log10
      # geom_abline(intercept = 0, slope = 1, colour = 'black') + # increase of 1.5
      # 
      #geom_abline(intercept = 0, slope = 2^(.05), colour = 'green', linetype = 'dashed') + #decrease of half   for decrease-0.05
      geom_abline(intercept = 1, slope = 1, colour = '#009E73', linetype = 'dashed') + #increase of a factor of 2
      geom_abline(intercept = log(1.5), slope = 1, colour = '#009E73', linetype = 'dotted') + #increase of a factor of 2
      geom_abline(intercept = -1, slope = 1, colour = '#661100', linetype = 'dashed') + # decrease of 2
      geom_abline(intercept = log(0.65), slope = 1, colour = '#AA4499', linetype = 'dotted') + # decrease of 2
      geom_abline(intercept = 0, slope = 1, colour = 'black') + # increase of 1.5
      
      # geom_abline(intercept = 0, slope = 2, colour = 'green', linetype = 'dashed') + # increase of 2
      #geom_abline(intercept = 0, slope = 0.5, colour = 'red', linetype = 'dashed') +
      #geom_abline(intercept = 0, slope = 1, colour = 'black') +
      ggtitle(label = gcm[gc]) +
      theme_bw() + 
      # coord_trans(x = 'log2', y = 'log2', xlim = c(10000,50000000), ylim = c(10000,50000000)) +
      theme(plot.title = element_text(size = 16, face = 'bold'),
            axis.title.x = element_text(size = 18, face = 'bold'),
            axis.title.y = element_text(size = 18, face = 'bold'),
            axis.text.y = element_text( vjust = 0.5,  hjust = 0.5, size = 10),
            axis.text.x = element_text( vjust = 0.5,  hjust = 0.5, size = 10),
            aspect.ratio = 1,
            legend.position = 'bottom',
            legend.text = element_text(size= 12)) +
      labs(x = 2011, y = 2091, col = 'Group') 
    
    
    ggsave(plot = gsct, 
           filename = glue('./graphs/figs/scatter/abun2011_2091_2scalefixsep2022_log2_{gcm[gc]}.png'),  ## the notation is not scientific
           units = 'in', width = 12, height = 9, dpi = 700)
    
    return(gsct)
  })
}
corPlot <- make_graph(data = data)

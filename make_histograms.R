# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(fs, glue, ggplot2, tidyverse, fasterize, qs, dplyr)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Function to use ---------------------------------------------------------
make_histogram <- function(spc){
  #spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('Starting ', spc, '\n')
  cat('-------------------------------------------------------------\n')
  
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/tbl_allYears_{spc}.qs'))
  base <- qs::qread(file = glue('./outputs/{spc}/tbl_2010_base72_{spc}.qs'))
  base <- mutate(base, specie = spc)
  base_10 <- mutate(base, year = 2010, specie = spc)
  base_10 <- base_10 %>% select(y2010, year, specie, gc)
  tbl_11<- dplyr::select(tbl, '2011', gc, specie)
  tbl_11 <- mutate(tbl_11, year = 2011)
  tbl_11 <- tbl_11 %>%  select(`2011`, year, specie, gc)
  colnames(tbl_11) <- c('density', 'year', 'specie', 'gc')
  colnames(base_10) <- c('density', 'year', 'specie', 'gc')
  df <- rbind(base_10, tbl_11)
  
  cat('Making the graph', '\n')
  
  hist_plot <- ggplot(df, aes(x = density, fill = as.factor(year))) + 
    geom_histogram(colour ='black', alpha = 0.5, position = 'identity') +
    geom_vline(aes(xintercept = mean(density)), colour = 'gray', linetype = 'dashed') +
    facet_wrap(~ gc) + 
    scale_fill_manual(name = 'Year', values = c('#999999', '#E69F00'),
                                                labels = c('2010', '2011')) +
    ggtitle(label = spc) +
    theme_bw() 
  
  ogb <- glue('./graphs/figs/hist_baseline/{spc}_hist2.png')
  ggsave(plot = hist_plot, filename = ogb, units = 'in', width = 13, height = 6.8, dpi = 300)
  
  # par(mfrow = c(1,2))
  # hist(base$y2010, main = glue('{spc}_2010'), col = 'blue', 
  #      border = 'darkblue', xlab = 'Density', 
  #      xlim = c(min(base$y2010), max(base$y2010)))
  # hist(tbl_11$`2011`, main = glue('{spc}_2011'), col = 'blue',
  #      border = 'darkblue', xlab = 'Density',
  #      xlim = c(min(tbl_11$`2011`), max(tbl_11$`2011`)))
}
# Apply the function ------------------------------------------------------
map(.x = spcs[2:72], .f = make_histogram)


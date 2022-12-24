library(pacman)
p_load(tidyr, ggplot2, reshape2)

## Creating the baaseline table
table<- qs::qread(file = glue('./baseline/baseline_all.qs'))

base_total <- table %>% as_tibble() %>%  select(3:74)
colnames(base_total) <- spcs

d <- melt(base_total)

foo <- function(x){
  require(ggplot2)
  ggplot(d, aes (x = value)) + geom_histogram() +  facet_wrap(~variable)
}
histograms <- lapply(d, foo)

hist<-ggplot(d, aes(x = value)) +
  geom_histogram() + facet_wrap(~variable, scales = 'free_x')

ggsave(plot = hist, filename = glue('./graphs/figs/hist_baseline/hist_2010.png'),
        units = 'in', width = 12, height = 9, dpi = 300)


# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

rasterToTable <- function(sp){
  #sp <- spcs[1]
  cat('Start', sp, '\n')
  
  dir <- grep(sp, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- grep('mean', fls, value = TRUE)
  gcm <- c('CanESM2','CCSM4','INM-CM4')
  yrs <- c('2011', '2031', '2051', '2071','2091', '2100')

  cat('Raster to table\n')
  dfm <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    fl <- grep(gcm[k], fls, value = TRUE)
    tr <- terra::rast(fl)
    df <- terra::as.data.frame(tr)
    names(df) <- yrs
    df <- cbind(df)
    df <- as_tibble(df)
    df <- mutate(df, gc = gcm[k])
    df <- mutate(df, specie = sp)
    return(df)
  })
rsl <- bind_rows(dfm)  
qs::qsave(x = rsl, file = glue('./outputs/{sp}/tbl_allYears_{sp}.qs'))
cat('Finish!\n')
return(rsl)
}
### Raster to table ---------------------------------------------------------
dfrm <- map(.x = spcs[61:72], .f = rasterToTable)
    
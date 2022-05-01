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
table<- qs::qread(file = glue('./outputs/baseline/baseline_all.qs'))

base_total <- df2 %>% as_tibble() %>%  select(3:74)
colnames(base_total) <- spcs
base <- base_total %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
base_long <- pivot_longer(base,cols = 1:72, names_to = 'species', values_to = 'value')

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


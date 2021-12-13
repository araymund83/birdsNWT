# Load libraries --------------------------------------------
library(pacman)
pacman::p_load(colorspace, dplyr, FD, glue, hrbrthemes, gtools, qs,
               raster, rgdal, rgeos, readxl, reproducible, sf,
               stringr,tidyverse, gtools, purrr)

rm(list = ls())

# Load data ---------------------------------------------------------------
CanESM2 <- qs::qread('./tables/qs/comm/commAllSpp_CanESM2.qs')
CanESM2_2011 <- dplyr::select(CanESM2, starts_with('2011'))
CanESM2_2011 <- mutate(CanESM2_2011, pixelID = 1:nrow(CanESM2_2011))
CanESM2_2011 <- CanESM2_2011 %>% remove_rownames() %>% column_to_rownames(var = 'pixelID')
colnames(CanESM2_2011) <- gsub('2011_', '', colnames(CanESM2_2011))

## 2091
CanESM2_2091 <- dplyr::select(CanESM2, starts_with('2091'))
CanESM2_2091 <- mutate(CanESM2_2091, pixelID = 1:nrow(CanESM2_2091))
CanESM2_2091 <- CanESM2_2091 %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'pixelID')
colnames(CanESM2_2091) <- gsub('2091_', '', colnames(CanESM2_2091)) #retira el sufijo del a;o 



## I did this in the BC and then download the qs file
CanESM2Long <- CanESM2_2011 %>% pivot_longer(!pixelID, names_to = 'species', values_to = 'value')
qs::qsave(CanESM2Long, file= './tables/qs/comm/CanESM2Long.qs')

CanESM2Long <- qs::qread('./tables/qs/comm/CanESM2Long.qs')

CanESM2Long <- mutate(CanESM2Long, specie = str_sub(species, 6, nchar(species)))

CanESM2Long <- dplyr::select(CanESM2Long, pixelID, specie, value)

traits <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1SZvBeiCHs-zaofXXQiMGtA3ZlVupvDHF/view?usp=sharing',
                                   targetFile = 'birdSpecies_traits.csv',
                                   destinationPath = paths$inputPath,
                                   fun = data.table::fread)
# ## read the table that sumarizes trait category 
# traits_cat <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1SZvBeiCHs-zaofXXQiMGtA3ZlVupvDHF/view?usp=sharing',
#                                        targetFile = 'traits_cat.csv',
#                                        destinationPath = paths$inputPath,
#                                        fun = data.table::fread)
traits <- as_tibble(traits)
#get rid of the first 5 columns
traits <- traits %>% dplyr::select(Sp_Code:Nesting_Habitat3)
traits <- rename_with(traits, tolower)
traits <- rename(traits, species = sp_code)

#make a subset of the traits columns 
traits <- traits %>% dplyr::select(c('species', 'migration','feeding_class_breed1', 
                              'forage_breed1', 'nesting_habitat1'))
colnames(traits) <- c('species', 'migration', 'feeding1', 'forage1', 'nesting1')
## add species as rownames
traits <- traits %>% remove_rownames() %>% column_to_rownames(var = 'species')

traits <- rownames_to_column(traits)
colnames(traits)[1] <- 'specie'

joinTbl <- left_join(CanESM2Long, traits, by = 'specie')

##transform the traits colums to a factor
traits <- traits %>%  purrr::modify_at(c('migration', 'feeding1', 
                                         'forage1', 'nesting1'), factor)

## change each factor level to a numeric one
traits$migration <- as.numeric(traits$migration)
traits$feeding1 <- as.numeric(traits$feeding1)
traits$forage1 <- as.numeric(traits$forage1)
traits$nesting1 <- as.numeric(traits$nesting1)

##Save the object it took about 2.5 days to run
res1 <- adespatial::TBI(CanESM2_2011, CanESM2_2091, method = '%diff', nperm = 99, 
                        BCD = TRUE, test.t.perm = TRUE)

qs::qsave(res1, file ='./tables/qs/comm/TBI_perm99.qs')
s.names <- paste('pix', 1:6988670, sep = '')

a <- plot(res1, type = 'BC', s.names = NULL, pch.loss = 2, pch.gain = 21, cex.names = 1,
     col.bg = 'cadetblue2', main = 'B-C plot for 2011 & 2091')
## tpaired.krandtest allows you to see which species are contributing to the gains 
ttest <- adespatial::tpaired.krandtest(CanESM2_2011, CanESM2_2091, nperm = 99, list.all = FALSE)
qs::qsave(ttest, file ='./tables/qs/comm/ttest_perm99.qs')


bir




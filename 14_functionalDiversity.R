# Load libraries --------------------------------------------
library(pacman)
pacman::p_load(colorspace, FD, glue, hrbrthemes, gtools, qs,
               raster, rgdal, rgeos, readxl, reproducible, sf,
               stringr,tidyverse, gtools, purrr)

rm(list = ls())

# Load data ---------------------------------------------------------------
CanESM2 <- qs::qread('./tables/qs/comm/commAllSpp_CanESM2.qs')
CanESM2_2011 <- select(CanESM2, starts_with('2011'))
CanESM2_2011 <- mutate(CanESM2_2011, pixelID = 1:nrow(CanESM2_2011))
CanESM2_2011 <- select(CanESM2_2011, -c(76))

CanESM2Long <- CanESM2_2011 %>% pivot_longer(!pixelID, names_to = 'species', values_to = 'value')
qs::qsave(CanESM2Long, file= './tables/qs/comm/CanESM2Long.qs')

traits <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1WfESGnGs1aGdobjS38Rv-fa9wynln3LO/view?usp=sharing',
                                   targetFile = 'birdSpecies_traits.csv',
                                   destinationPath = paths$inputPath,
                                   fun = data.table::fread)

traits <- as_tibble(traits)
#get rid of the first 5 columns
traits <- traits %>% select(Sp_Code:Nesting_Habitat3)
traits <- rename_with(traits, tolower)
traits <- rename(traits, species = sp_code)
#make a subset of the traits columns 
traits <- traits %>% select(c('species', 'migration','feeding_class_breed1', 
                              'forage_breed1', 'nesting_habitat1'))
##transform the traits colums to a factor
traits <- traits %>%  purrr::modify_at(c('migration', 'feeding_class_breed1', 
                                         'forage_breed1', 'nesting_habitat1'), factor)


trait_comm <- CanESM2Long %>% full_join(traits, by = 'species')
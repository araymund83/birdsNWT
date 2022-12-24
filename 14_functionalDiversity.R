# Load libraries --------------------------------------------
library(pacman)
pacman::p_load(colorspace, dplyr, FD, glue,gawdis, hrbrthemes, gtools, mFD, qs,
               terra, sf, stringr,tidyverse,vegan)
rm(list = ls())

# Load data ---------------------------------------------------------------
CanESM2 <- qs::qread('./tables/qs/comm/commAllSpp_CanESM2.qs')
CanESM2_2011 <- dplyr::select(CanESM2, starts_with('2011'))
CanESM2_2011 <- CanESM2_2011 %>%  mutate(pixelID = row_number())
CanESM2_2011 <- CanESM2_2011 %>%   mutate(pixelID = paste0('S_', pixelID))
colnames(CanESM2_2011) <- gsub('2011_', '', colnames(CanESM2_2011))
CanESM2_2011 <- subset(CanESM2_2011, select= -c(BARS,NOFL,PIWO))
CanESM2_2011 <- as_tibble(CanESM2_2011)
CanESM2_2011 <- CanESM2_2011 %>% remove_rownames()
CanESM2_2011<-  column_to_rownames(CanESM2_2011, var = 'pixelID')
test <- as.matrix(CanESM2_2011)
row.names(test) <- CanESM2_2011$pixelID


## 2091
CanESM2_2091 <- dplyr::select(CanESM2, starts_with('2091'))
CanESM2_2091 <- mutate(CanESM2_2091, pixelID = 1:nrow(CanESM2_2091))
CanESM2_2091 <- CanESM2_2091 %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'pixelID')
colnames(CanESM2_2091) <- gsub('2091_', '', colnames(CanESM2_2091)) #retira el sufijo del a;o 
CanESM2_2091 <- subset(CanESM2_2091, select= -c(BARS,NOFL,PIWO))



## I did this in the BC and then download the qs file
CanESM2Long <- CanESM2_2011 %>% pivot_longer(!pixelID, names_to = 'species', values_to = 'value')
qs::qsave(CanESM2Long, file= './tables/qs/comm/CanESM2Long.qs')

CanESM2Long <- qs::qread('./tables/qs/comm/CanESM2Long.qs')

CanESM2Long <- mutate(CanESM2Long, specie = str_sub(species, 6, nchar(species)))

CanESM2Long <- dplyr::select(CanESM2Long, pixelID, specie, value)

traits <- read.csv('./inputs/birdSpecies_traits2.csv')
traits_type <- read.csv('./inputs/traits_type.csv')

traits2 <- traits %>% dplyr::select(Group,species,Migration, Feeding_Class_Breed1, Habitat2, Forage_Breed1,
                       Beak.Length_Culmen, Tarsus.Length, Wing.Length, Tail.Length, Mass)
colnames(traits2) <- c('group', 'species', 'migration','feeding', 'habitat', 'forage',
                       'beak_L', 'tarsus', 'wing', 'tail', 'mass')

traits2 <- as_tibble(traits2)
## add species as rownames
traits2 <- traits2 %>% remove_rownames() %>% column_to_rownames(var = 'species')
##transform the discrete traits columns to a factor
traits2 <- traits2 %>%  purrr::modify_at(c('group', 'migration', 'feeding', 
                                           'habitat', 'forage'), factor)

#get rid of the first 5 columns
traits_num <- traits2 %>% dplyr::select(beak_L:mass)
traits_nom <- traits2 %>% dplyr::select(group:forage)
traits_type_nom <- traits_type %>% 
  dplyr::filter(trait_name %in% c('group', 'migration','feeding', 'habitat',
                                  'forage'))
                                                                  


#traits2 <- rownames_to_column(traits2)
#colnames(traits2)[1] <- 'specie'

joinTbl <- left_join(CanESM2Long, traits, by = 'specie')


#Standarizing trait data 
traits_num_sc <- scale(traits.num)

##Save the object it took about 2.5 days to run
res1 <- adespatial::TBI(CanESM2_2011, CanESM2_2091, method = '%diff', nperm = 99, 
                        BCD = TRUE, test.t.perm = TRUE)

qs::qsave(res1, file ='./tables/qs/comm/TBI_perm99.qs')
TBI_perm99 <- qs::qread('./tables/qs/comm/TBI_perm99.qs')
s.names <- paste('pix', 1:6988670, sep = '')

a <- plot(TBI_perm99, type = 'BC', s.names = NULL, pch.loss = 2, pch.gain = 21, cex.names = 1,
     col.bg = 'cadetblue2', main = 'B-C plot for 2011 & 2091')
## tpaired.krandtest allows you to see which species are contributing to the gains 
ttest <- adespatial::tpaired.krandtest(CanESM2_2011, CanESM2_2091, nperm = 99, list.all = FALSE)
qs::qsave(ttest, file ='./tables/qs/comm/ttest_perm99.qs')

#summarize traits 
bird_traits_summary <- mFD::sp.tr.summary(tr_cat = traits_type,
                                       sp_tr = traits2)
bird_traits_distance <- gawdis(traits2, w.type = 'optimized', opti.maxiter = 100)
#Gower distance ranges from , 0.01 to 0.9860 species that share common traits 
#sum up distance matrix 
summary(as.matrix(bird_traits_distance))

cors.gaw <- attr(bird_traits_distance, 'correls')
attr(bird_traits_distance, 'weights')

fspaces_birds <- mFD::quality.fspaces(sp_dist = bird_traits_distance, maxdim_pcoa = 10,
                                      deviation_weighting = c('absolute', 'squared'),
                                      fdist_scaling = c(TRUE,FALSE),
                                      fdendro = 'average')
#display the table gathering quality metrics
quality_metrics <- round(fspaces_birds$quality_fspaces, 3)
#retrive the functional space associated with minimal quality metric 
apply(fspaces_birds$quality_fspaces, 2, which.min)

library("magrittr")

qp_fspaces_plot <- fspaces_birds$"quality_fspaces" %>%
  tibble::as_tibble(rownames = "Funct.space") %>%
  tidyr::pivot_longer(cols =! Funct.space, names_to = "quality_metric", values_to = "Quality") %>%
  ggplot2::ggplot(ggplot2::aes(x = Funct.space, y = Quality, 
                               color = quality_metric, shape = quality_metric)) +
  ggplot2::geom_point() 


quality_plot <- mFD::quality.fspaces.plot(fspaces_quality = fspaces_birds,
                                          quality_metric = 'mad',
                                          fspaces_plot = c('tree_average', 'pcoa_2d',
                                                           'pcoa_3d', 'pcoa_4d',
                                                           'pcoa_5d'),
                                          gradient_deviation = c(neg = 'darkblue',
                                                                 nul = 'grey80',
                                                                 pos = 'darkred'),
                                          gradient_deviation_quality = c(low = 'yellow',
                                                                         high = 'red'),
                                          x_lab = 'Trait-based distance')
#retrieve species coordinates in the 4D spaces 
sp_faxes_corr_birds11 <- fspaces_birds$"details_fspaces"$"sp_pc_coord"

birds11_tr_faces <- mFD::traits.faxes.cor(sp_tr = traits2,
                                          sp_faxes_coord = sp_faxes_corr_birds11[, c('PC1', 'PC2', 'PC3', 'PC4')],
                                          plot = TRUE)
# Print traits with significant effect:
birds11_tr_faces$"tr_faxes_stat"[which(birds11_tr_faces$"tr_faxes_stat"$"p.value" < 0.05), ]


#Compute dendogram 
bird11_dendo<- fspaces_birds$"details_fspaces"$"dendro" %>%
  as.dendrogram() %>%
  dendextend::plot_horiz.dendrogram(side = TRUE)
#compute alpha FD indices 
alpha_fd_indices_birds11 <- mFD::alpha.fd.multidim(sp_faxes_coord = sp_faxes_corr_birds11[, c('PC1', 'PC2', 'PC3', 'PC4', 'PC5')],
                                                   asb_sp_w = test,
                                                   ind_vect =  c('fdis','fmpd', 'fnnd','feve', 'fric','fdiv', 'fori', 'fspe', 'fide'),
                                                   scaling = TRUE,
                                                   check_input = TRUE,
                                                   details_returned = TRUE)

alpha_fd_birds11<- FD::dbFD(x = traits2, a = test, w.abun = TRUE, stand.x = TRUE,
     asym.bin = NULL,
     corr = "sqrt",
     calc.FRic = TRUE, m = 3, stand.FRic = TRUE,
     scale.RaoQ = FALSE, calc.FGR = TRUE, clust.type = "ward",
     calc.CWM = TRUE,
     CWM.type = c("dom", "all"), calc.FDiv = TRUE, dist.bin = 2, 
     print.pco = FALSE, messages = TRUE)

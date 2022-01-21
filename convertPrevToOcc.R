# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, hrbrthemes, colorspace)

rm(list = ls())


# Load data ---------------------------------------------------------------
prev<- read.csv('./inputs/prevalanceSpp.csv')  # this file was provided by 
## Diana Stralberg

prev<- prev |> rowwise()|> 
  mutate(pOccMean = 1- dpois(x = 0, lambda = meandens),
         pOccMed = 1 -dpois(x = 0, lambda = meddens))
write.csv(prev, './inputs/prevOcc.csv', row.names = FALSE)

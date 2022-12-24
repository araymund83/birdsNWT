# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(terra, stringr, glue, sf, tidyverse, fs, fst)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

rasterLReg <- function(sp){
 sp <- spcs[3]
  cat('Start', sp, '\n')
  
  dir <- grep(sp, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- grep('mean', fls, value = TRUE)
  gcm <- c('CanESM2','CCSM4','INM-CM4')
  yrs <- c('2010', '2011')
  
  cat('Regression\n')
  dfm <- map(.x = 1:length(gcm), .f = function(k){
    cat(gcm[k], '\n')
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- grep('2011', fl, value = TRUE)
    tr <- terra::rast(fl)
    base <- grep('2010', fls, value = TRUE)
    bas <- terra ::rast(base)
    stk <- c(bas, tr)
    names(stk) <- c('y2010', 'y2011')
    m <- lm(y2010~ y2011, data = as.data.frame(stk))
  # #t -statistics 
  #   tstats <- coef(m) / sqrt(diag(vcov(m)))
  #   #pvalue
  #   2 * pt(abs(tstats), df = df.residual(m), lower.tail = FALSE)
    dfCoeff <- as.data.frame(coef(summary(m)))
    dfResi <- as.data.frame(resid(m))
    do <- glue('./outputs/{sp}/lmBase11')
    ifelse(!dir.exists(do), dir_create(do), print('Directory already exists'))
    write.csv(dfCoeff, glue('{do}/{sp}_{gcm[k]}_coefs.csv'))
    write.csv(dfResi,glue('{do}/{sp}_{gcm[k]}_residuals.csv'))
    cat('Finish!\n')
  return(list(dfCoeff,dfResi))
  })
 names(dfm) <- gcm
 coef_gcm <- lapply(dfm, '[[', 1)
 coef_gcm <-coef_gcm %>% reduce(full_join)
 coef_gcm %>% as_tibble(.)
 coef_gcm %>% mutate(species = sp)
 write.csv(coef_gcm, glue('./outputs/{sp}/lmBase11/{sp}_allgcm_coefs.csv'))
}
### Raster to table ---------------------------------------------------------
dfrm <- map(.x = spcs[4:72], .f = rasterLReg)

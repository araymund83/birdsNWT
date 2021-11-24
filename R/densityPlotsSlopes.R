

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rcartocolor, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, hrbrthemes, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)


g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

loadSlopeRas<- function(spc){
    
    # Proof
    #spc <- spcs[1] # Run and comment (after)
    message(crayon::green("Loading slopes for: ", spc))
    dir <- grep(spc, dirs, value = TRUE)
    fls <- fs::dir_ls(dir, regexp = '.tif$')
    fls <- grep('slpe', fls, value = TRUE)
    gcm <- str_sub(basename(fls), start = 6, end = nchar(basename(fls)) - 4)
    gcm <- unique(gcm)
    
    slopes <- list()
    for (i in 1:length(fls)){
      slopes[[i]] <- raster(fls[i])
    }
    slopesStk <- stack(slopes)
  return(slopesStk)
  
}

slopes <- map(spcs, loadSlopeRas)
names(slopes) <- spcs 


# Convert rasters to data.frames ------------------------------------------
slopeDF <- lapply(X = slopes, FUN = function(x){
  eachSpDF <- as.data.frame(x)
  eachSpDF <- na.omit(eachSpDF)
  return(eachSpDF)
})

#qs::qsave(slopeDF, './tables/slopesDF.qs')
slopesDF <- qs::qread(file = './tables/slopesDF.qs')

# change column names -----------------------------------------------------
colnames <- c('CanESM2', 'CCSM4', 'INM.CM4')
slopeDF <-lapply(slopesDF, setNames, colnames)

# reshape data frames  ----------------------------------------------------
slopeLongDF <-lapply(X = slopeDF, FUN = function(x){
 longDF <- x %>%  gather(gcm, value)
 return(longDF)
})

# Make density plots with rasterVis ---------------------------------------
plot<- lapply(X = slopes, FUN = function(x){
  eachSpPlot<- densityplot(x, par.settings = rasterTheme())
  return(eachSpPlot)
})

## plots with ggplot using facet Wrap
densityPlots <- lapply(X = slopeLongDF, FUN = function(x){
  plotSp <- ggplot(data = x, aes(x= value, group = gcm, fill = gcm)) + 
            geom_density(adjust = 1.5, alpha = .4) + 
            theme_bw() +
            theme(legend.position = 'none',
                  panel.grid.major = element_blank(),
                  panel.spacing.x = unit(5, 'mm'),
                  axis.text.x = element_text(size = 8)) +
            facet_wrap(. ~ gcm, ncol = 3, nrow = 1) +
            ggtitle(label = spcs)
  return(plotSp)
})

densityPlots <- lapply(X = seq_along(slopeLongDF), FUN = function(x){
  plotSp <- ggplot(slopeLongDF[[x]], aes(x= value, group = gcm, fill = gcm)) + 
    geom_density(adjust = 1.5, alpha = .4) + 
    theme_bw() +
    theme(legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.spacing.x = unit(5, 'mm'),
          axis.text.x = element_text(size = 7)) +
    facet_wrap(. ~ gcm, ncol = 3, nrow = 1) +
    ggtitle(names(slopeLongDF)[x])
  return(plotSp)
})


names(densityPlots) <- spcs 

# Save plots --------------------------------------------------------------
 lapply(X = names(densityPlots[74:75]), FUN = function(x){
    ggsave(plot = densityPlots[[x]], filename = glue('./graphs/figs/slopes/slope_{x}.png', 
           units = 'in', width = 15, height = 11, dpi = 700))
  })

 densityPlots2 <- lapply(X = seq_along(slopeLongDF), FUN = function(x){
   plotSp <- ggplot(data = slopeLongDF[[x]], aes(x= value, group = gcm, fill = gcm)) + 
     geom_density(adjust = 1.5, alpha = .4) + 
     theme_bw() +
     theme(legend.position = 'right',
           #panel.grid.major = element_blank(),
           #panel.spacing.x = unit(5, 'mm'),
           axis.text.x = element_text(size = 7)) +
     ggtitle(names(slopeLongDF)[x]) + 
     labs(fill = "")
   return(plotSp)
 })
 names(densityPlots2) <- spcs 
 
 lapply(X = names(densityPlots2[71:75]), FUN = function(x){
   ggsave(plot = densityPlots2[[x]], filename = glue('./graphs/figs/slopes/slope2_{x}.png', 
          units = 'in', width = 15, height = 11, dpi = 700))
 })
 



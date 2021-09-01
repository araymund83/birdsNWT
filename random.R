##stack the list of files per bird species
#predStack <- lapply(predictions2,stack)
##name each element of the list with the species name
#names(predStack) <- birdList

##get mean values per species/run/year
years <- c(2011, 2031, 2051, 2071, 2091, 2100)
meanValuesTime <- lapply(predStack, FUN = function(predstack, Years = years){ 
   yearStack <- lapply(Years, FUN = function(year, birds = predstack){
   #browser()
    oneRep <- grep(pattern = paste0("*", year), x = names(birds))
    #subset the raster per each of the spp and run 
    runStack <- birds[[oneRep]]
    mean_ras <- raster::calc(runStack, fun = mean, na.rm = TRUE)
    return(mean_ras)
  })
 # browser()
  names(yearStack) <- years
  return(yearStack)
}
)

##stack all years per species
meanYearRun <- lapply(meanValuesTime, stack)

### change Rasterlayers names 
changeName <-lapply(names(meanYearRun), function(bird){
  name <- paste0("mean", climateScenario, bird)
  names(meanYearRun)[] <- gsub(pattern ="X", 
                               replacement = "mean_CanESM2_",bird, names(meanYearRun)[])
})

#TODO: HOW TO CHANGE THE NAMES IN A FUNCTION?? 
names(meanYearRun$ALFL) <- gsub(pattern ="X", replacement = paste0("mean","CanESM2_ALF"), names(meanYearRun$ALFL))
names(meanYearRun$AMCR) <- gsub(pattern ="X", replacement = "mean_CanESM2_AMCR", names(meanYearRun$AMCR))
names(meanYearRun$AMRE) <- gsub(pattern ="X", replacement = "mean_CanESM2_AMRE", names(meanYearRun$AMRE))


##write rasterStacks into the outputs path 
saveRasters <- lapply(meanYearRun, function(x) {
  writeRaster(x,  filename = file.path(getwd(),"outputs","mean"), 
              names(x), 
              bylayer = TRUE, format = "GTiff")
})

plotmeanRasters <- lapply(meanYearRun, function(x){
  meanPlotsTime <-plot(x)[]
})

# ##WRITE A SINGLE RASTERLAYERS
# writeRaster(meanYearRun$ALFL, file.path(getwd(),"outputs",climateScenario), 
#             names(meanYearRun$ALFL), 
#             bylayer = TRUE, format = "GTiff")

##TODO: CREATE DATA TABLE WITH THE MEAN VALUES PER YEAR PER SPECIES ????
##create DF of all species
meanDF <- lapply(meanYearRun, as.data.frame)

##conver to data.table
dt <- lapply(meanDF, as.data.table)


##create a column ID (pixelID) for each of the bird species
dt <- lapply(dt, function(x)
  cbind(x, pixelID = 1:nrow(x)))

## 
dtLong <- melt (data = dt,
                id.vars ="pixelID",
                variable.name = "Year",
                value.name = "Mean_density")



pixelID <- 1:ncell(meanYearRun[[1]])





allPredictions <- list.files(pathData, pattern = glob2rx( "CanESM2_*.tif$"), full.names = TRUE)


densityDT <- lapply(meanValuesTime[[]], function(i) as.data.frame(i, xy=TRUE, na.rm=TRUE))
densityDT <- lapply(meanValuesTime, function(i) as.data.frame(i))

valsdensity <- data.table(pixel.ID = 1 :ncell(meanYearRun[[1]]), Years = getValues())

library(data.table)
dflong <- melt(df, id.vars = "Year")


## transform to long format 

dtLong = melt(dt, measure.vars = names(meanDF),
             variable.name = "year", value.name = "density")

d<- dtLong[, lapply(.SD, mean, na.omit = TRUE), by = "year"]

nan <- sum(is.nan(dtLong))
### make the plot 
library(ggplot2)

runStack2 <- lapply(meanRun,stack)
plotStack<- lapply (out, stack)
plots <- lapply(plotStack, FUN = function(n){
  plot(plotStack[])
  }
  )







r_diff <- meanValuesTime$ALFL$X2100- meanValuesTime$ALFL$X2011
names(r_diff) <- "Difference"
plot(r_diff)

hist(r_diff)


diffALFL <- meanYearRun$ALFL[[6]] - meanYearRun$ALFL[[1]]
hist(diffALFL,
     col = "springgreen4",
     main = "Histogram of density model in the NWT",
     ylab = "Number of pixels")
mean_val <- cellStats(diffALFL,"mean")
std_val <- cellStats(diffALFL,"sd")

r_std <- (diffALFL - mean_val)/std_val # standardized image

threshold_val <- c(1.96,1.64)
plot(r_std)


##TODO: is this most efficient than the code above?? 
diff <- calc(meanYearRun, FUN =  function(x){x[[6]] - x[[1]]}) 

diff <- overlay(meanYearRun$ALFL[[6]], meanYearRun$ALFL[[1]], fun=function(a,b) return(a==b))
plot(diff,
     col=c('#FFE4E1','#228B22'),
     legend=FALSE,
     axes=FALSE)
legend("left", legend=c("Agree", "Disagree"),
       col=c("#228B22", "#FFE4E1"), pch = 15, cex=0.8)



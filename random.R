##stack the list of files per bird species
#predStack <- lapply(predictions2,stack)
##name each element of the list with the species name
#names(predStack) <- birdList

##get mean values per species/run/year
years <- c(2011, 2031, 2051, 2071, 2091, 2100)
meanRun <- lapply(predStack, FUN = function(predstack, Years = years){ 
   yearStack <- lapply(Years, FUN = function(year, birds = predstack){
   # browser()
    oneRep <- grep(pattern = paste0("*", year), x = names(birds))
    #subset the raster
    runStack <- birds[[oneRep]]
    mean_ras <- raster::calc(runStack, fun = mean, na.rm = TRUE)
    return(mean_ras)
  })
 # browser()
  names(yearStack) <- years
  return(yearStack)
}
)


names(meanRun) <- birdList

meanYearRun <- lapply(meanRun, stack)
#rename each layer within the stack with each simulation year 
names(meanYearRun$ALFL) <- years
names(meanYearRun$AMCR) <- years
#save rasters 
writeRaster(meanYearRun$ALFL, filename = names(meanYearRun$ALFL), bylayer = TRUE, format = "GTiff")


data_df <- na.omit(as.data.frame(r_NDVI_s)) #drop NA
df <- lapply(meanYearRun, as.data.frame)

library(data.table)
dflong <- melt(df$ALFL, id.vars = "Year")


dtALFL<-as.data.table(df$ALFL)
## transform to long format 

dtLong = melt(dtALFL, measure.vars = c("X2011", "X2031", "X2051", "X2071","X2091","X2100"),
             variable.name = "year", value.name = "density")

d<- dtLong[, lapply(.SD, mean, na.omit = TRUE), by = "year"]

nan <- sum(is.nan(dtLong))
### make the plot 
library(ggplot2)
plotALFL<- ggplot(dtLong,aes(x=year,y=density)) + geom_line()

runStack2 <- lapply(meanRun,stack)
plotStack<- lapply (out, stack)
plots <- lapply(plotStack, FUN = function(n){
  plot(plotStack[])
  }
  )

a <- retrieveRasters(dataFolder = pathData, 
                     years = c(2011, 2100),
                     patternsToRetrieveRasters = "CanESM2",
                     species = birdList)
                     #species = paste(birdList, collapse = "|"))


raster_stats <- lapply(predStack, FUN = function)



allFiles <- grepMulti(x = list.files(path = dataFolder[[eachScenario]][[bmod]][[run]],
                                     full.names = TRUE),
                      patterns = patternsToRetrieveRasters)
filesPath <- grepMulti(x = allFiles, patterns = c(sp, paste(years, collapse = "|")))
rastersTS <- raster::stack(lapply(X = years, FUN = function(eachTS){
  rasPath <- grepMulti(x = filesPath, patterns = eachTS)
  if (length(rasPath) == 0)
    stop("At least one of the rasters doesn't seem to exist for the year sequence provided. Please check your data")
  ras <- raster::raster(rasPath)
  names(ras) <- paste(eachScenario, bmod, run, eachTS, sep = "_")
  return(ras)
})

### This function allow me to read the rasters 

dir.files <- list.dirs(path =pathData, recursive = FALSE)
names(dir.files) <- basename(dir.files)
dir.files
data.files <- lapply(dir.files, list.files)

raster_stack <- lapply(predictions, function(ras){
  rasters <- stack(list.files(pathData, pattern = climateScenario,
                              full.names = TRUE, recursive = FALSE))
})

stkNames <- unlist(lapply(X= 1:length(predStack@layers), FUN = function(layers){
  layer <- predStack@layers[[layers]]@data@names
  returr(layers)
}))


r_diff <- meanYearRun$ALFL$X2100- meanYearRun$ALFL$X2011
names(r_diff) <- "Difference"
plot(r_diff)

hist(r_diff)


diff <- overlay(meanYearRun[[6]], meanYearRun[[1]])

allStack <- flatten(meanStack)
allStack<- stack(allStack)
names(allStack) <- gsub('mean_', '', names(allStack))

speciesDT <- rasterToPoints(allStack)

speciesDT <-na.omit(data.table(pixelID = 1:ncell(allStack[[1]]), vals = getValues(allStack[[1:500]])))

library(rasterVis)
densityplot(meanStack[[1]])



stackToDT <-lapply(meanStack, FUN = function(sp){
  browser()
  spList <- meanStack[[i]]
  spList <- flatten(meanStack)
  spStack <- stack(spList)
  message(crayon::blue(paste0('Making table for',  spList)))
  stkVals<- data.table::data.table(raster::getValues(spStack))
  stkVals$pixelID <- 1:ncell(a)

  
})
  
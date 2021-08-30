makeBirdDT <- function(birdRasterStack, landscapeClassesRasterStack) {
  reproducible::Require("raster")
  browser()
  meanBirdRasters <- names(birdRasterStack) %>%
    str_detect('mean') %>%
    keep(birdRasterStack, .)
  namesMeanBirdRasters <- names(meanBirdRasters)
  
  birdDatasets <- lapply(X = meanBirdRasters, FUN = function(birdRasterLayer) {
    
    landBirdRasterStack <- raster::addLayer(birdRasterLayer, landscapeClassesRasterStack)
    
    
    ## take the values from the rasters and input them to a data table called cellValues
    cellValues <- data.table(getValues(landBirdRasterStack))
    cellValues <- setnames(cellValues, c( "birdDensity", "landForClass", "age", "forestedStatus"))
    cellValues <- unite(cellValues, uniqueClasses, c(forestedStatus, landForClass), remove = FALSE)
    
    #get rid of any rows with NA values
    cellValues <- na.omit(cellValues)
    
    ## make sure landForClass and forestedStatus are categorical rather than numerical
    cellValues$landForClass <- as.factor(cellValues$landForClass)
    cellValues$forestedStatus <- as.factor(cellValues$forestedStatus)
    cellValues$uniqueClasses <- as.factor(cellValues$uniqueClasses)
    
    return(cellValues)
  })
  
  names(birdDatasets) <- namesMeanBirdRasters
  return(birdDatasets)
}
means <- meanValuesTime(ras = predStack$ALFL, run = 1, initialTime = 2011)

loadMeanRas<- function(birdList, 
                       pathData,
                       climateScenario = NULL,
                       year = NULL){
  meanPath <- checkPath(file.path(pathData), create = TRUE)
  
  browser()
  message(crayon::green(paste0("Looking for files in ", meanPath)))
  
  
  
  listDirs <- list.dirs(meanPath, recursive = FALSE)
  allMeans <- lapply(X = birdList, FUN = function(bird){
    ##list all files within the meanPath that match the pattern
    meanAvailable <- usefulFuns::grepMulti(x = list.files(listDirs, full.names = TRUE),
                                           patterns = c(bird, climateScenario))
    browser()
    if(length(meanAvailable) == 0)
      stop(paste0("Predictions for" , bird,
                  "are not available. Please verify species"))
    
    message(crayon::green("Loading the following file(s) for", bird))
    message(crayon::magenta(paste0(" "), paste0(meanAvailable, sep = "\n")))
    allRas <- lapply(meanAvailable, raster)
    return(allRas)
  }
  )
  stkRas <- lapply(allMeans, stack)
  names(stkRas) <- birdList
  return(stkRas)
}
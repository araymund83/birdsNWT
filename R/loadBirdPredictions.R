loadBirdPredictions<- function(birdList, 
                               pathData,
                               climateScenario = NULL,
                               year = NULL){
   predictionsPath <- checkPath(file.path(pathData), create = TRUE)

   message(crayon::green(paste0("Looking for files in ", predictionsPath)))
   
   # fls <- dir_ls(path = './outputs', regexp = '.tif$')
   # files <- fs::dir_ls(predictionsPath, regexp = '.tif$')
   # files <- grep('mean_', fls, value = TRUE
                 
   
   listDirs <- list.dirs(predictionsPath, recursive = FALSE)
   allPredictions <- lapply(X = birdList, FUN = function(bird){
   ##list all files within the predictionsPath that match the pattern
      predAvailable <- usefulFuns::grepMulti(x = list.files(listDirs, full.names = TRUE),
                                             patterns = c(bird, climateScenario))
      if(length(predAvailable) == 0)
         stop(paste0("Predictions for" , bird,
                     "are not available. Please verify species"))
      
      message(crayon::green("Loading the following file(s) for", bird))
      message(crayon::magenta(paste0(" "), paste0(predAvailable, sep = "\n")))
   allRas <- lapply(predAvailable, raster)
      return(allRas)
   }
   )
   stkRas <- lapply(allPredictions, stack)
   names(stkRas) <- birdList
   return(stkRas)
}




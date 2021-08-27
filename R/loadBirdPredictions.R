loadBirdPredictions<- function(birdList, 
                               pathData,
                               climateScenario = NULL,
                               year = NULL){
<<<<<<< HEAD
   predictionsPath <- checkPath(file.path(pathData), create = TRUE)
   message(crayon::green(paste0("Looking for files in ", predictionsPath)))
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


# list_dirs <- list.dirs(pathData, recursive = F)
# for(i in 1:length(list_dirs)){
#    predAvailable <- list.files(path =list_dirs, pattern = climateScenario, 
#                                full.names = TRUE, recursive = T, include.dirs = T)
#   # stackList[[i]] <- stack(predAvailable)
# }
# predAvailable <- usefulFuns::grepMulti(x = list.files(data, full.names = TRUE),
#                                        patterns = pattern)
# 
# names(list_dirs) <- basename(list_dirs)
# 
# raster.list <- lapply(list_dirs, function(dir) {
#    stack(list.files(dir, pattern = "cool", full.names = T, recursive = F))
# })
=======
  predictionsPath <- checkPath(file.path(pathData), create = TRUE)
  message(crayon::green(paste0("Looking for files in ", predictionsPath)))
  allPredictions <- lapply(X = birdList, FUN = function(bird){
    ##list all files within the predictionsPath that match the pattern
    predAvailable <- usefulFuns::grepMulti(x = list.files(predictionsPath, full.names = TRUE),
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
>>>>>>> ba9c7a0552441bcd077524cd2e4d5497cb7d1612

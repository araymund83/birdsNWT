downloadBirdPredictions <- function(folderUrl,
                                     birdsList,
                                     yearAnalysis,
                                     replicateNum = NULL,
                                     climateScenario = NULL,
                                     dataPath, 
                                     returnPath = FALSE){
  
  filesToDownload <- Cache(googledrive::drive_ls, path = as_id(folderUrl), ## it only needs the last bit of the https address. 
                           team_drive = as_id(folderUrl))
  
  predictionsForBirdList <- filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"), 
                                                  x = filesToDownload$name)]
  if (length(predictionsForBirdList) == 0){
    message(crayon::red(paste0("No prediction available for ", birdsList, 
                               "for GCM :", climateScenario)))
    return(NA)
  }
  browser()
  downloadedPredictions <- lapply(X = predictionsForBirdList, FUN = function(rasterFile){
    if (!file.exists(file.path(dataPath, rasterFile))){
      googledrive::drive_download(file = as_id(filesToDownload[filesToDownload$name %in% rasterFile, ]$id), #modelFile,
                                  path = file.path(dataPath, rasterFile), 
                                  overwrite = TRUE)
    }
    if(returnPath){
      return(file.path(dataPath, rasterFile))
    }else {
      return(get(load(file.path(dataPath, rasterFile))))
    }
    
  })
  names(downloadedPredictions) <- birdsList
  
  return(downloadedPredictions)
}

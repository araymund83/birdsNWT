downloadBirdPredictions <- function(folderUrl,
                                     birdsList,
                                     yearAnalysis,
                                     replicateNum = NULL,
                                     climateScenario = NULL,
                                     dataPath, 
                                     returnPath = FALSE){
  
  filesToDownload <- Cache(googledrive::drive_ls, path = as_id(folderUrl), ## it only needs the last bit of the https address. 
                           team_drive = as_id(folderUrl))
  
  modelsForBirdList <- filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"), 
                                                  x = filesToDownload$name)]
  if (length(modelsForBirdList) == 0){
    message(crayon::red(paste0("No prediction available for ", birdsList, 
                               " for models V", climateScenario)))
    return(NA)
  }
  #browser()
  downloadedModels <- lapply(X = modelsForBirdList, FUN = function(modelFile){
    if (!file.exists(file.path(dataPath, modelFile))){
      googledrive::drive_download(file = as_id(filesToDownload[filesToDownload$name %in% modelFile, ]$id), #modelFile,
                                  path = file.path(dataPath, modelFile), overwrite = TRUE)
    }
    if(returnPath){
      return(file.path(dataPath, modelFile))
    }else {
      return(get(load(file.path(dataPath, modelFile))))
    }
    
  })
  names(downloadedModels) <- birdsList
  
  return(downloadedModels)
}
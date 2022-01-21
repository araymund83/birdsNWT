loadOccRas<- function(species, 
                      pathData,
                      pattern, 
                      gcm= NULL,
                      years = NULL){
  
  pathData <- reproducible::checkPath(file.path(pathData), create = TRUE)

  rasAvailable <- lapply(X = species, FUN = function(sp){
    message(crayon::green('Loading files for', sp))
    subdir<-glue(pathData,'/',sp,'/occur')
    allFiles <- list.files(path = subdir, pattern = pattern, 
                           full.names = TRUE)
    gcm <- str_sub(basename(allFiles), start = 16, end = nchar(basename(allFiles)) - 4)
    gcm <- unique(gcm)
    
    gcmAvailable <- map(.x = 1:length(gcm), .f = function(gc){
      message(crayon::green('Creating time series for', gcm[gc]))
      files <- grep(gcm[gc], allFiles, value = TRUE)
      gcmFiles <- lapply(files, raster::raster)
      gcmStack <- raster::stack(gcmFiles)
      
    })
    names(gcmAvailable) <- gcm
    return(gcmAvailable)
    return(rasAvailable)
  })
}


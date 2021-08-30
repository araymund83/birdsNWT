loadPredRas <- function(birdsList,
                        folderUrl,
                        rastersPath,
                        ClimateScenerio = NULL,
                        YEAR= NULL) {
  reproducible::Require("raster")
  ## check that there is a folder at the given rastersPath. if not, create it.
  rastersPath <- checkPath(file.path(rastersPath), create = TRUE)
  ## download the rasters on the birdList. Return List of downloaded files.
  downloadedRasters <- downloadPredRas(folderUrl = folderUrl,
                                       birdsList = birdsList,
                                       rastersPath = rastersPath)
  
  
  
  
  
  ## lapply applys the custom function to each raster in turn
  
  
  postprocessedRasters <- lapply(X = downloadedRasters, FUN = function(RasterLayer) {
    ## the function postProcesses the layer, cropping and masking it to a given study area and rasterToMatch, and saving it to a given destination path
    proRaster <- postProcess(RasterLayer,
                             studyArea = studyArea,
                             rasterToMatch = rasterToMatch,
                             destinationPath = rastersPath)
    ## each layer is returned into the object proRaster
    return(proRaster)
  })
  ## Finally the whole function returns the birdRasterStacks
  return(postprocessedRasters)
}
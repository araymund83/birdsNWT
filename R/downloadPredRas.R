downloadPredRas <- function(folderUrl, 
                            birdsList,
                            climateScenario,
                            rastersPath) {
  ## drive_ls function is used to list all the files it finds using the folder url with the given pattern
  filesToDownload <-
    filesToDownload <- Cache(googledrive::drive_ls, path = as_id(folderUrl), ## it only needs the last bit of the https address. 
                             team_drive = as_id(folderUrl))
  #note: has to be changed if filenaming system changes
  ## grepl function searches for all items in the filesToDownload that are on birdList & stores their names in rastersforBirdList
  rastersForBirdList <-
    filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"),
                               x = filesToDownload$name)]
  
  ## for each item in turn from rastersForBirdlist the following function is applied:
  downloadedRasters <-
    lapply( X = rastersForBirdList, FUN = function(rasterFile) {
        ## if the item in rastersForBirdList is not already present at rastersPath, googledrive package downloads it
        if (!file.exists(file.path(rastersPath, rasterFile))) {
          googledrive::drive_download(
            file = googledrive::as_id(filesToDownload[filesToDownload$name %in% rasterFile,]$id),
            path = file.path(rastersPath, rasterFile),
            overwrite = TRUE
          )
        }
        
        ## otherwise, if it is already present and downloaded, just get the name of the item
        return(raster(file.path(rastersPath, rasterFile), verbose = TRUE))
      }
    )
  
return(downloadedRasters)
}

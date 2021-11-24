speciesTable <- function(list, 
                         species, 
                         gcm){
  
  message(crayon::green ('Creating data table for:', sp))
  dtEachSp <- data.table(pixelID = 1:ncell(sp), getValues(sp))
  dtEachSp <- na.omit(dtEachSp)
  
}

unlist(allStack)

dtSp <- lapply(X = allStack, FUN = function(sp){
  dtEachSp <- data.table(pixelID = 1:ncell(sp), getValues(sp))
  dtEachSp <- na.omit(dtEachSp)
  return(dtEachSp)
})
 
allSpDT <- rbindlist(dtSp, fill = T, use.names = T)
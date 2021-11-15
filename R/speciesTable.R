speciesTable <- function(stack){
  message(crayon::green ('Creating data frame for:',  ))
    
    allStack <- flatten(meanStack)
    allStack <- stack(allStack)
    names(allStack) <- gsub('mean_', '', names(allStack))
   
    allTble <- rasterToPoints(allStack, spatial = FALSE)
    allTble <- as_tibble(allTble)
    
    return(allTble)
}
 
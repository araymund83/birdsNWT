speciesTable <- function(stack,
                         species = NULL){
  message(crayon::green ('Creating data frame for:',  ))
    
    allStack <- flatten(stack)
    allStack <- stack(allStack)
    names(allStack) <- gsub('mean_', '', names(allStack))
   
    allTble <- rasterToPoints(allStack, spatial = FALSE)
    allTble <- as_tibble(allTble)
    
    return(allTble)
}
 
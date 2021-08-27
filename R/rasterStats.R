
  
  years <- c(2011, 2031, 2051, 2071, 2091, 2100)
  out <- lapply(predStack, FUN = function(predstack, Years = years){ 
    #browser()
    yearStack <- lapply(Years, FUN = function(year, birds = predstack){
      # browser()
      oneRep <- grep(pattern = paste0("*", year), x = names(birds))
      #subset the raster
      runStack <- birds[[oneRep]]
      mean_ras <- raster::calc(runStack, fun = mean, na.rm = TRUE)
      return(mean_ras)
    })
    return(yearStack)
  })
  
 
  
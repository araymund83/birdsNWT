## The aim of this function is to convert rasters into data.frames. It has an 
## optional argument 'aggregate' which can be specified to downscale the raster 
## before converting to a data.frame.
## The code in the function extracts the x and y coordinates from the raster,
## extracts the data values from the rasater, and then combines them into a data
## frame that contains the coordinates and the data.
## output : rasterDF  
## 
## source: 
## https://bookdown.org/mcwimberly/gdswr-book/raster-data-continuous-variables.html

rasterDF <- function(x, aggregate = 1){
  resampleFactor <- aggregate
  inputRaster <- x
  inCols <- ncol(inputRaster)
  inRows <- nrow(inputRaster)
  # compute numbers of columns and rows in the new raster for mapping
  resampledRaster <- raster (ncols = (inCols / resampleFactor),
                            nrow = (inRows / resampleFactor))
  # match to the extent of the original raster
  y <- resample(inputRaster, resampledRaster, method = 'ngb')
  
  # extract cell cordinates into a data frame
  coords <- xyFromCell (y, seq_len(ncell(y)))
  
  # extract layer names
  dat <- stack(as.data.frame(getValues(y)))
  
  # add names - 'value' for data, 'variable' to indicate different raster layers 
  #in a stack
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  return(dat)
  
  }
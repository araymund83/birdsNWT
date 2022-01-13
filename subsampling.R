#polyRas <- raster('./inputs/ecrgPixelID.tif') ## change directory

#get the pixel indexes per polygon into a dataTable
polyRasDT <- data.table::data.table(getValues(polyRas))
polyRasDT$pixelID <- 1:nrow(polyRasDT)
#rename columns
colnames(polyRasDT) <- c('ecrg','pixelID')

#extract a proportional sample per ecoregion polygon
psample <- function(df, ecrg){
  smpDT2 <- df|> group_by(ecrg) |> 
    sample_frac(20000/nrow(df))
  return(smpDT2) 
}

## replicate the function 10000 

r <- replicate(10000, {psample(polyRasDT, polyRasDT$ecrg)})

## use the cell index to extract the same cells in the rasterStack 


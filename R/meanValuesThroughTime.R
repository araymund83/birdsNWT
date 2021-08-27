' Calculates the mean value of rasters through time
#'
#' @param ras RasterStack. Time series used to calculate the mean value through time
#' @param climatScenario character. Which climate scenario are you using
#'            Needs to match the rasters. Default is NULL (i.e. the whole raster is only one area)
#' @param initialTime numeric. Format of the first year of analysis.
#' @return table with average, SD and CI95%
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom stats median
#' @include substrBoth.R
#'
#' @rdname meanValuesThroughTime
meanValuesTime <- function(ras, run, initialTime){
  if(is(ras, "RasterStack")){
    fullTable <- lapply(names(ras), FUN = function(year){
      browser()
      eachRasToCalc <- ras[[year]]
      if(is(eachRasToCalc, list)){
        meanAndUnc <- lapply(eachRasToCalc, function(eachRas){
          average <- median(eachRas[], na.rm = TRUE)
          rasType <- ifelse(grepl(names(eachRas), pattern = "Uncertain"), "SD", "AVERAGE")
          yr <- as.numeric(usefulFuns::substrBoth(strng = names(eachRas), howManyCharacters = nchar(initialTime)))
          dt <- data.table::data.table(average = average, year = yr, run = run)
          return(dt)
        })
 }
})
  }
}

means <- meanValuesTime(ras = predStack, run = 1, initialTime = 2011)

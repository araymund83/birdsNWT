' Calculates the mean value of rasters through time
#'
#' @param ras RasterStack. Time series used to calculate the mean value through time
#' @param climateScenario character. Which climate scenario are you using (CanESM2,CCSM4 or INM-CM4)
#'             Default is NULL 
#' @param initialTime numeric. Format of the first year of analysis.
#' @return table with average, SD and CI95%
#'
#' @author  modified from Tati Micheletti
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom stats median
#' @include substrBoth.R
#'
#' @rdname meanValuesThroughTime
meanValuesTime <- function(ras, climateScenario, initialTime){
  if(is(ras, "RasterStack")){
    fullTable <- lapply(names(ras), FUN = function(year){
      browser()
      eachRasToCalc <- ras[[year]]
      if(is(eachRasToCalc, list)){
        meanAndUnc <- lapply(eachRasToCalc, function(eachRas){
          average <- mean(eachRas[], na.rm = TRUE)
          rasType <- ifelse(grepl(names(eachRas), pattern = "Uncertain"), "SD", "AVERAGE")
          yr <- as.numeric(usefulFuns::substrBoth(strng = names(eachRas), howManyCharacters = nchar(initialTime)))
          dt <- data.table::data.table(average = average, year = yr, run = run)
          return(dt)
        })
 }
})
  }
}

means <- meanValuesTime(ras = meanYearRun[], climateScenario = "Can_ESM2", initialTime = 2011)

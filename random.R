##stack the list of files per bird species
#predStack <- lapply(predictions2,stack)
##name each element of the list with the species name
#names(predStack) <- birdList

##get mean values per species/run/year
years <- c(2011, 2031, 2051, 2071, 2091, 2100)
meanValuesTime2 <- lapply(predStack, FUN = function(predstack, Years = years){ 
   yearStack <- lapply(Years, FUN = function(year, birds = predstack){
   #browser()
    oneRep <- grep(pattern = paste0("*", year), x = names(birds))
    #subset the raster per each of the spp and run 
    runStack <- birds[[oneRep]]
    mean_ras <- raster::calc(runStack, fun = mean, na.rm = TRUE)
    #browser()
    #sd_ras <- raster::calc(runStack, fun = sd)
    #return(list(mean = mean_ras, sd = sd_ras))
    return(mean_ras)
  })
 # browser()
  names(yearStack) <- years
  return(yearStack)
}
)

###Error in h(simpleError(msg, call)) : 
###error in evaluating the argument 'x' in selecting a method for function 'raster': list has no "x" 

##stack all years per species
meanYearRun <- lapply(X = meanValuesTime,  FUN = function(ras){
  meanSdStack <- raster(stack(ras)[])
  return(meanSdStack)
})


 meanYearRun2 <- lapply(meanValuesTime2, stack)

 #### make a data table per species 
dtSp <- lapply(X = meanYearRun2, FUN = function(sp){
  browser()
  dtEachSp <- data.table(pixelID = 1:ncell(sp), getValues(sp))
  dtEachSp <- na.omit(dtEachSp)
  return(dtEachSp)
})


### fit a linear regression model to calculate the trend in each of the pixels
  
dtSP <- lapply(X = meanYearRun2, FUN = function(sp){
  arrayStack <- raster::as.array(x = sp)
  times <- c(2011, 2041, 2061)
  slopeValues <- apply(X = arrayStack, MARGIN = c(1, 2), FUN = function(x){
    slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, times), y = x) # Original formula was way slower: lm(x ~ times, data = dfX, na.action = na.omit)
    coef <- slpCoef$coefficients[2]
    pVal <- 2*pt(abs(slpCoef$coefficients/slpCoef$stderr), slpCoef$df.residual, lower.tail=FALSE)[2]
    return(list(coef = coef, pVal = pVal))
  }
  )
})
  slopeCoefficientVals <- matrix(unlist(lapply(slopeValues, [[, 1)),
                                 nrow = nrow(arrayStack),
                                 ncol = ncol(arrayStack),
                                 byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
  slopeSignificancyVals <- matrix(unlist(lapply(slopeValues, [[, 2)),
                                  nrow = nrow(arrayStack),
                                  ncol = ncol(arrayStack),
                                  byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
  slopeCoeff <- sp[[1]] %>%
    raster::setValues(slopeCoefficientVals)
  names(slopeCoeff) <- "slopeCoeff"
  
  slopeSignificancy <- sp[[1]] %>%
    raster::setValues(slopeSignificancyVals)
  names(slopeSignificancy) <- "slopeSignificancy"
  return(list(slopeCoefficient = slopeCoeff, slopeSignificancy = slopeSignificancy))
  })






### change Rasterlayers names 
changeName <-lapply(names(meanYearRun), function(bird){
  name <- paste0("mean", climateScenario, bird)
  names(meanYearRun)[] <- gsub(pattern ="X", 
                               replacement = "mean_CanESM2_",bird, names(meanYearRun)[])
})

#TODO: HOW TO CHANGE THE NAMES IN A FUNCTION?? 
names(meanYearRun$ALFL) <- gsub(pattern ="X", replacement = paste0("mean","CanESM2_ALF"), names(meanYearRun$ALFL))
names(meanYearRun$AMCR) <- gsub(pattern ="X", replacement = "mean_CanESM2_AMCR", names(meanYearRun$AMCR))
names(meanYearRun$AMRE) <- gsub(pattern ="X", replacement = "mean_CanESM2_AMRE", names(meanYearRun$AMRE))


##write rasterStacks into the outputs path 
saveRasters <- lapply(meanYearRun, function(x) {
  writeRaster(x,  filename = file.path(getwd(),"outputs","mean"), 
              names(x), 
              bylayer = TRUE, format = "GTiff")
})

plotmeanRasters <- lapply(meanYearRun2, function(x){
  meanPlotsTime <-plot(x)[]
})

# ##WRITE A SINGLE RASTERLAYERS
# writeRaster(meanYearRun$ALFL, file.path(getwd(),"outputs",climateScenario), 
#             names(meanYearRun$ALFL), 
#             bylayer = TRUE, format = "GTiff")

##TODO: CREATE DATA TABLE WITH THE MEAN VALUES PER YEAR PER SPECIES ????
##create DF of all species
meanDF <- lapply(meanYearRun, as.data.frame)

##conver to data.table
dt <- lapply(meanDF, as.data.table)
dt <- lapply(meanYearRun)


##create a column ID (pixelID) for each of the bird species
dt <- lapply(dt, function(x)
  cbind(x, pixelID = 1:nrow(x)))

##  HOW TO APPLY TO ALL THE DT in my list ????
dtLong <- melt (data = dt,
                id.vars ="pixelID",
                variable.name = "Year",
                value.name = "Mean_density")











### make the plot 
library(ggplot2)

runStack2 <- lapply(meanRun,stack)
plotStack<- lapply (out, stack)
plots <- lapply(plotStack, FUN = function(n){
  plot(plotStack[])
  }
  )







r_diff <- meanValuesTime$ALFL$X2100- meanValuesTime$ALFL$X2011
names(r_diff) <- "Difference"
plot(r_diff)

hist(r_diff)


diffALFL <- meanYearRun$ALFL[[6]] - meanYearRun$ALFL[[1]]
hist(diffALFL,
     col = "springgreen4",
     main = "Histogram of density model in the NWT",
     ylab = "Number of pixels")
mean_val <- cellStats(diffALFL,"mean")
std_val <- cellStats(diffALFL,"sd")

r_std <- (diffALFL - mean_val)/std_val # standardized image

threshold_val <- c(1.96,1.64)
plot(r_std)


##TODO: is this most efficient than the code above?? 
diff <- calc(meanYearRun, FUN =  function(x){x[[6]] - x[[1]]}) 

diff <- overlay(meanYearRun$ALFL[[6]], meanYearRun$ALFL[[1]], fun=function(a,b) return(a==b))
plot(diff,
     col=c('#FFE4E1','#228B22'),
     legend=FALSE,
     axes=FALSE)
legend("left", legend=c("Agree", "Disagree"),
       col=c("#228B22", "#FFE4E1"), pch = 15, cex=0.8)



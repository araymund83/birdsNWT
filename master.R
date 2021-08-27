birdList <- c("ALFL", "AMCR", "AMRE")
climateScenario <- "CanESM2_"
year <- paste(c(2011,2100), collapse = "|")
pathData <- file.path(getwd(), "inputs/predictions")



birdPredictions <- downloadBirdPredictions(folderUrl= "1O34zQIem_RUxxCDOZMGEUPIkUtCWsS_c",
                                           birdsList = paste(birdList, collapse = "|"),
                                           yearAnalysis = paste(c(2011, 2100), collapse = "|"),
                                           #climateScenario = "CanESM2",
                                           dataPath = paths$inputPath,
                                           returnPath = TRUE)

predStack <- loadBirdPredictions(birdList = birdList,
                                    pathData = pathData,
                                    climateScenario = "CanESM2_",
                                    year = year)



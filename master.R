birdList <- c("ALFL", "AMCR", "AMRE")

library(pacman)
pacman::p_load(raster, rgdal, rgeos, readxl, stringr, sf, tidyverse, terra, foreach, fs)


#birdList <- c("AMRO","ATSP","BAWW")

##complete list of species from google drive
# birdList <- c("ALFL", "AMCR", "AMRE","AMRO","ATSP","BAWW", "BBWA", "BBWO", "BCCH",
#              "BHCO", "BHVI","BLPW", "BOCH", "BRBL", "BRCR", "BTNW", "CAWA", "CCSP",
#              "CHSP", "CORA", "COYE", "DEJU", "EAKI", "FOSP", "GRAJ", "HAFL", "HETH",
#              "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", "NOWA", "OCWA", "OVEN",
#              "PAWA", "PHVI", "PISI", "PIWO", "PUFI", "RBGR", "RBNU", "RCKI", "REVI",
#              "RUBL", "RUGR", "RWBL", "SAVS", "SOSP", "SWSP", "SWTH", "TEWA" ,"TRES",
#              "VATH", "WAVI", "WCSP", "WETA", "WEWP", "WIWA", "WIWR", "WTSP", "WWCR",
#              "YBFL", "YBSA", "YEWA", "YRWA")

climateScenario <- "CanESM2_" #There are other two options for GCM:  CCSM4 and INM-CM4
year <- paste(c(2011,2100), collapse = "|")
pathData <- file.path(getwd(), "inputs/predictions")



birdPredictions <- downloadBirdPredictions(folderUrl= "1O34zQIem_RUxxCDOZMGEUPIkUtCWsS_c",
                                           birdsList = paste(birdList, collapse = "|"),
                                           yearAnalysis = paste(c(2011, 2100), collapse = "|"),
                                           climateScenario = "CanESM2",
                                           dataPath = pathData
                                           )
# birdPredictions <- downloadPredRas(folderUrl= "1O34zQIem_RUxxCDOZMGEUPIkUtCWsS_c",
#                                            birdsList = paste(birdList, collapse = "|"),
#                                            #yearAnalysis = paste(c(2011, 2100), collapse = "|"),
#                                            #climateScenario = "CanESM2",
#                                            rastersPath =pathData
#                                            )

predStack <- loadBirdPredictions(birdList = birdList,
                                    pathData = pathData,
                                    climateScenario = "CanESM2_",
                                    year = year)


# Apply the function -----------------------------------------------------
map(spcs, make_average_reps) #Esto aplicara la funcion para todas las especies              




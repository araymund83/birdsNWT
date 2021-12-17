species <- c('BLPW')

library(pacman)
pacman::p_load(raster, rgdal, rgeos, reproducible, readxl, stringr, sf, 
               tidyverse, terra, foreach, fs, data.table)


species <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP")

#complete list of species from google drive
species <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "ATTW", "BARS", "BAWW",
              "BBWA", "BBWO", "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL",
              "BRCR", "BTNW", "CAWA", "CCSP", "CHSP", "CONW", "CORA", "COYE",
              "DEJU", "EAKI", "EVGR", "FOSP", "GCKI", "GCTH", "GRAJ", "HAFL",
              "HETH", "HOLA", "LCSP", "LEFL", "LEYE", "LISP", "MAWA", "NOFL",
              "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PHVI", "PISI", "PIWO",
              "PUFI", "RBGR", "RBNU", "RCKI", "REVI", "RUBL", "RUGR", "RWBL",
              "SAVS", "SOSP", "SWSP", "SWTH", "TEWA" ,"TRES", "VATH", "WAVI",
              "WCSP", "WETA", "WEWP", "WIWA", "WIWR", "WTSP", "WWCR", "YBFL",
              "YBSA", "YEWA", "YRWA")

gcm<- paste(c('CanESM2', 'CCSM4', 'INM-CM4'), collapse = '|')  #There are other two options for GCM: 'CanESM2', 'CCSM4' and 'INM-CM4'
years <- paste(c(2011, 2031,2051,2071,2091, 2100), collapse = "|")

pathData <- './inputs/predictions'
#residents
residents <- c('ATTW', 'BBWO', 'BCCH', 'BOCH', 'CORA', 'GRAJ', 'PIWO', 'RBNU',
               'RUGR')
#short distant migrants
sdm <- c('AMCR', 'AMRO', 'ATSP', 'BHCO', 'BHVI', 'BRBL', 'BRCR', 'CHSP', 'DEJU',
         'FOSP', 'GCKI', 'HETH', 'HOLA', 'LCSP', 'LEYE', 'LISP','NOFL', 'OCWA',
         'PAWA', 'PUFI', 'RCKI', 'RUBL', 'RWBL' ,'SAVS', 'SOSP', 'SWSP', 'TRES',
         'VATH', 'WCSP', 'WIWR', 'WTSP', 'YBSA', 'YRWA')
#neotropical migrants
nm <- c('ALFL', 'AMRE', 'BARS', 'BAWW', 'BBWA', 'BLPW', 'BTNW', 'CAWA', 'CCSP', 
        'CONW', 'COYE', 'EAKI' ,'GCTH', 'HAFL', 'LEFL', 'MAWA', 'NOWA' , 'OSFL',
        'OVEN', 'PHVI', 'RBGR', 'REVI','SWTH', 'TEWA', 'WAVI', 'WETA', 'WEWP',
        'WIWA', 'YBFL', 'YEWA')
#nomadic
nomadic <- c('EVGR', 'PISI', 'WWCR')


birdPredictions <- downloadBirdPredictions(folderUrl= "1O34zQIem_RUxxCDOZMGEUPIkUtCWsS_c",
                                           birdsList = paste(species, collapse = "|"),
                                           yearAnalysis = paste(c(2011, 2100), collapse = "|"),
                                           #climateScenario = "CanESM2",
                                           dataPath = pathData
                                           
                                           )
birdPredictions <- downloadPredRas(folderUrl= "1O34zQIem_RUxxCDOZMGEUPIkUtCWsS_c",
                                           birdsList = paste(species, collapse = "|"),
                                           #yearAnalysis = paste(c(2011, 2100), collapse = "|"),
                                           #climateScenario = "CanESM2",
                                          rastersPath =pathData
                                            )

birdPred <- loadBirdPredictions(birdList = species,
                                pathData = pathData
                                )
meanStack <- loadMeanRas(species = species,
                         pathData = pathData,
                         pattern = 'mean')

names(meanStack) <- species

flatten(meanStack)                             



# Apply the function -----------------------------------------------------
map(spcs, make_average_reps) #Esto aplicara la funcion para todas las especies   
map(.x = spcs, .f = see_changes)




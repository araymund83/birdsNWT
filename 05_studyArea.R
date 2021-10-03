allowedStudyAreas <- c("AB", "BC", "MB", "NT", "NU", "SK", "YT") ## prov/terr x BCR intersections


provs <- c("British Columbia", "Alberta", "Saskatchewan", "Manitoba")
terrs <- c("Yukon", "Northwest Territories", "Nunavut")
WB <- c(provs, terrs)

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

bcrzip <- "https://www.birdscanada.org/download/gislab/bcr_terrestrial_shape.zip"

bcrshp <- Cache(prepInputs,
                url = bcrzip,
                destinationPath = paths$inputPath,
                targetCRS = targetCRS,
                useCache =TRUE,
                fun = "sf::st_read")

if (packageVersion("reproducible") >= "1.2.5") {
  fn1 <- function(x) {
    x <- readRDS(x)
    x <- st_as_sf(x)
    st_transform(x, targetCRS)
  }
} else {
  fn1 <- "readRDS"
}

canProvs <- Cache(prepInputs,
                  "GADM",
                  fun = fn1,
                  dlFun = "raster::getData",
                  country = "CAN", level = 1, path = paths$inputPath,
                  #targetCRS = targetCRS, ## TODO: fails on Windows
                  targetFile = "gadm36_CAN_1_sp.rds",
                  cacheRepo = paths$cachePath,
                  destinationPath = paths$inputPath
)

if (packageVersion("reproducible") < "1.2.5") {
  canProvs <- st_as_sf(canProvs) %>%
    st_transform(., targetCRS)
}

#################################################################################
## BCR for Western Boreal
#################################################################################

bcrWB <- bcrshp[bcrshp$BCR %in% c(4, 6:8), ]
provsWB <- canProvs[canProvs$NAME_1 %in% WB, ]

WBstudyArea <- Cache(postProcess,
                     provsWB,
                     studyArea = bcrWB,
                     useSAcrs = TRUE,
                     cacheRepo = paths$cachePath,
                     filename2 = NULL) %>%
  as_Spatial(.)

#################################################################################
## BCR subdivision
#################################################################################
AB <- c("Alberta")
BC <- c("British Columbia")
MB <- c("Manitoba")
SK <- c("Saskatchewan")
NT <- c("Northwest Territories")
NU <- c("Nunavut")
YK <- c("Yukon")
##BCR in WB
bcr4 <- bcrshp[bcrshp$BCR %in% c(4), ]
bcr6 <- bcrshp[bcrshp$BCR %in% c(6), ]
bcr7 <- bcrshp[bcrshp$BCR %in% c(7), ]
bcr8 <- bcrshp[bcrshp$BCR %in% c(8), ]

##provinces and territories in WB
AB <- canProvs[canProvs$NAME_1 %in% AB, ]
BC <- canProvs[canProvs$NAME_1 %in% BC, ]
MB <- canProvs[canProvs$NAME_1 %in% MB,]
SK <- canProvs[canProvs$NAME_1 %in% SK,]
NT <- canProvs[canProvs$NAME_1 %in% NT, ]
NU <- canProvs[canProvs$NAME_1 %in% NU, ]
YK <- canProvs[canProvs$NAME_1 %in% YK, ]

bcr6NT <- st_intersection(bcr6, NT)

ecodist<- 

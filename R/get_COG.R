rast <- raster('./outputs/ALFL/mean_ALFL_2011_CanESM2.tif')
rast2 <- raster('./outputs/ALFL/mean_ALFL_2091_CanESM2.tif')
rast<- trim(rast)

source('./R')

ras_asc2 <- asc.from.raster(rast2)

coG2<- COGravity(ras_asc2)

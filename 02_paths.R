#--------------------------------------------
## Set paths for each part of the simulation
#--------------------------------------------

## scratch directory for raster operations (see 01-init.R)
message("The 'raster' package is using ", scratchDirRas, " as scratch directory.")

## studyAreas
paths1 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path("cache"),
  modulePath = "modules",
  inputPath = file.path("inputs"),
  outputPath = file.path("outputs")
)
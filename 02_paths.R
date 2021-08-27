#--------------------------------------------
<<<<<<< HEAD
## Set paths
#--------------------------------------------
paths <- list(
  cachePath = checkPath("cache", create = TRUE),
  modulePath = file.path("modules"),
  inputPath = file.path("inputs"),
  outputPath = file.path("outputs")
)

=======
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
>>>>>>> ba9c7a0552441bcd077524cd2e4d5497cb7d1612

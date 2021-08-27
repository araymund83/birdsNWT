<<<<<<< HEAD
###############################################################################
=======
################################################################################
>>>>>>> ba9c7a0552441bcd077524cd2e4d5497cb7d1612
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()
message("Your current temporary directory is ", tempdir())

## user- and machine-specific settings
cacheDB <- "sqlite" ## default cache backend
machine <- Sys.info()[["nodename"]]
user <- Sys.info()[["user"]]
if (user == "araymundo") {
<<<<<<< HEAD
  scratchDirRas <- reproducible::checkPath(file.path("~/scratch/birdsNWT"), create = TRUE)
=======
  scratchDirRas <- reproducible::checkPath(file.path("~/scratch/WB_LandCoverClass"), create = TRUE)
>>>>>>> ba9c7a0552441bcd077524cd2e4d5497cb7d1612
  if (grepl(pattern = "spades", x = machine)) {
    system(paste0("chmod -R 777 ", scratchDirRas), wait = TRUE) ## TODO: why? also, too open
  }
  userEmail <- "araymund83@gmail.com"
<<<<<<< HEAD
}


## scratch directory for raster operations (see 01-init.R)
message("The 'raster' package is using ", scratchDirRas, " as scratch directory.")

birdList = c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", "BCCH",
             "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", "CAWA", "CHSP",
             "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", "CAJA", "HETH", "HOLA",
             "LSCP", "LEFL","LISP", "MAWA", "NOFL", "NOWA", "OCWA", "OSFL", "OVEN",
             "PAWA", "PISI", "PIWO", "PUFI", "RBGR", "RBNU", "RCKI", "REVI", "RUGR",
             "RWBL", "SAVS", "SOSP", "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP",
             "WETA", "WEWP", "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA",
             "YRWA")
             
=======
}
>>>>>>> ba9c7a0552441bcd077524cd2e4d5497cb7d1612

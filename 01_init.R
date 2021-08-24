################################################################################
## Initialization parameters and settings
################################################################################

.starttime <- Sys.time()
message("Your current temporary directory is ", tempdir())

## user- and machine-specific settings
cacheDB <- "sqlite" ## default cache backend
machine <- Sys.info()[["nodename"]]
user <- Sys.info()[["user"]]
if (user == "araymundo") {
  scratchDirRas <- reproducible::checkPath(file.path("~/scratch/WB_LandCoverClass"), create = TRUE)
  if (grepl(pattern = "spades", x = machine)) {
    system(paste0("chmod -R 777 ", scratchDirRas), wait = TRUE) ## TODO: why? also, too open
  }
  userEmail <- "araymund83@gmail.com"
}
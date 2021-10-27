library(parallel)
ncores <- detectCores(all.test = FALSE, logical = TRUE)
cl <- makeCluster(16)
registerDoSNOW(cl)

length_run <- length(rst)



cat('To calculate the slopes\n')
slpeK <- lapply(rst, function(k){
  cat('Start\n')
  slp <- raster.kendall(x = k, p.value = TRUE)
  cat('Done\n')
  
  return(slpeK)
})
slopeKendall<- foreach(i = 1 :length(rst), packages = c('spatialEco', 'raster'), 
                        .verbose = TRUE) %dopar%{
                          slpeK(rst)
                          
                        }

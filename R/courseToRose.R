courseToRose <- function(course, arc = 45, nameVec = rose_labs2){ # course are numbers between 0 and 360
  i <- course %/% arc
  i <- i  + ifelse(course %% arc > arc/2, 1,0) 
  i  <- i + 1
  return(nameVec[i])
}
  

COGravity <- function(x, y = NULL, z = NULL, wt = NULL ){
  #check if raster from sp or raster package and convert if necessary
  if (any(class(x) %in% 'RasterLayer')) x = asc.from.raster(x)
  
  #check if x is vector or matrix
  if (is.vector(x)) { #if the data is a vector...do calculations
    if (is.null(wt)) {	#if no weighting supplied, calculate means & standard deviations
      out = c(COGx=mean(x,na.rm=TRUE),COGx.sd=sd(x,na.rm=TRUE))
      if (!is.null(y)) out = c(out,COGy=mean(y,na.rm=TRUE),COGy.sd=sd(y,na.rm=TRUE))
      if (!is.null(z)) out = c(out,COGz=mean(z,na.rm=TRUE),COGz.sd=sd(z,na.rm=TRUE))
    } else { #if weighting supplied, calculate weighted means and variances to get COG
      out = c(COGx=wt.mean(x,wt),COGx.sd=wt.sd(x,wt))
      if (!is.null(y)) out = c(out,COGy=wt.mean(y,wt),COGy.sd=wt.sd(y,wt))
      if (!is.null(z)) out = c(out,COGz=wt.mean(z,wt),COGz.sd=wt.sd(z,wt))
    }
  }
  else if (any(class(x) == 'asc')) { #if x is of class 'asc'
    if (is.null(wt)) { #if wt is null then assume that values in x are the weights
      pos = as.data.frame(which(is.finite(x),arr.ind=TRUE))
      pos$x = getXYcoords(x)$x[pos$row]
      pos$y = getXYcoords(x)$y[pos$col]
      pos$wt = x[cbind(pos$row,pos$col)]
      out = c(COGx=wt.mean(pos$x,pos$wt),COGx.sd=wt.sd(pos$x,pos$wt),COGy=wt.mean(pos$y,pos$wt),COGy.sd=wt.sd(pos$y,pos$wt))
    }
  }
    # return the output	
    return(out)
}

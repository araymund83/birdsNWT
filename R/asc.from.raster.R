## functions taken from: https://github.com/cran/SDMTools/blob/master/R/asc.from.raster.R

asc.from.raster = function(x) {
  if (!any(class(x) %in% 'RasterLayer')) stop('x must be of class raster or RasterLayer')
  cellsize = (x@extent@ymax-x@extent@ymin)/x@nrows
  yll = x@extent@ymin + 0.5 * cellsize
  xll = x@extent@xmin + 0.5 * cellsize
  tmat = t(matrix(getValues(x),nrow=x@nrows,ncol=x@ncols,byrow=T)[x@nrows:1,])
  tmat[which(tmat==x@file@nodatavalue)] = NA
  return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}


#' @rdname asc.from.raster
#' @export
as.asc = function(x, xll=1, yll=1, cellsize=1,type=c("numeric", "factor"),lev=levels(factor(x))) {
  #check inputs
  type=match.arg(type)
  if (!inherits(x, "matrix")) stop("x should be a matrix")
  # creates the attributes
  mode(x) = "numeric"; attr(x, "xll") = xll; attr(x, "yll") = yll
  attr(x, "cellsize")=cellsize; attr(x, "type") = type
  if (type=="factor") attr(x, "levels") = lev
  class(x) = "asc"
  #return the object
  return(x)
}

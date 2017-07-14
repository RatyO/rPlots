
#' plotTriangleMatrix plots two matrices z1 and z2 of equal size 
#' as a matrix plot such that the corresponding matrix elements 
#' are show in the same picture cell by diving the cell to 
#' two triangles.
#'
#' @param z1 
#' @param z2 
#' @param keep.rect If \code(true), individual elements in the 
#' figure are kept rectangular. If \code(false), the whole figure 
#' area is kept rectangular.
#' @param colRamp User-specified colorRamp function. By default, 
#' \code(grey()) is used. 
#' @param xlab Labels in x-direction
#' @param ylab Labels in y-direction
#' @param main Main title
#' @param xOffset An offset value for x labels. By default, 
#' value 0.03 is used.
#' @param yOffset An offset value for y labels. By default, 
#' value 0.03 is used.
#' @param cex.main Character expasion value for the main title.
#' @param cex.x Character expasion value for x labels.
#' @param cex.y Character expasion value for y labels .
#' @param ... Not implemented yet.
#'
#' @return
#' @export
#' @importFrom fields colorbar.plot
#'
#' @examples
plotTriangleMatrix <- function(z1,z2,keep.rect=T,colRamp=NULL,xlab=NULL,ylab=NULL,main=NULL,
                           xOffset=0.03,yOffset=0.03,cex.main=1,cex.x=1,cex.y=1,...){
  
# Generate the polygon corner coordinates
  nx <- ncol(z1)
  ny <- nrow(z1)
  print(nx)
  print(ncol(z2))
  stopifnot(nx==ncol(z2), ny==nrow(z2))
  
  xMax <- yMax <- 1
  xMin <- yMin <- 0
  
  #Keep the larger dimension length as 1
  if(keep.rect){
    if(nx>ny) yMax <- ny/nx
    if(ny>nx) xMax <- nx/ny 
  }
  
  rectXTopLeft <- rectXBottomLeft <- seq(0,xMax-xMax/nx,xMax/nx)
  rectXTopRight <- rectXBottomRight <- seq(0+xMax/nx,xMax,xMax/nx)
  
  rectYTopLeft <- rectYTopRight <-  seq(0+yMax/ny,yMax,yMax/ny)
  rectYBottomLeft <- rectYBottomRight <- seq(0,yMax-yMax/ny,yMax/ny)
  
  #Create rectangular polygons
  
  xVals <- cbind(rectXTopLeft,rectXTopRight,rectXBottomRight,rectXBottomLeft)
  yVals <- cbind(rectYTopLeft,rectYTopRight,rectYBottomRight,rectYBottomLeft)
  
  rectangles <- list()
  
  n <- 1
  for(i in 1:ny){
    for(j in 1:nx){
      rectangles[[n]] <- list(xx=xVals[j,],yy=yVals[i,])
      n <- n + 1
    }
  }
  
  #Create upper triangles, column major order
  
  xVals <- cbind(rectXTopLeft,rectXTopRight,rectXBottomLeft)
  yVals <- cbind(rectYTopLeft,rectYTopRight,rectYBottomLeft)
  
  upperTriangle <- list()
  
  n <- 1
  for(i in 1:nx){
    for(j in 1:ny){
      upperTriangle[[n]] <- list(xx=xVals[i,],yy=yVals[j,])
      n <- n + 1
    }
  }
  
  #Create lower triangles, column major order
  
  xVals <- cbind(rectXTopRight,rectXBottomRight,rectXBottomLeft)
  yVals <- cbind(rectYTopRight,rectYBottomRight,rectYBottomLeft)
  
  lowerTriangle <- list()
  
  n <- 1
  for(i in 1:nx){
    for(j in 1:ny){
      lowerTriangle[[n]] <- list(xx=xVals[i,],yy=yVals[j,])
      n <- n + 1
    }
  }
  
  #plot the matrix
    
  colMin <- min(c(z1,z2))
  colMax <- max(c(z1,z2))
  
  colMin <- colMin - 0.1*(colMax-colMin)
  colMax <- colMax + 0.1*(colMax-colMin)
  
  z1Scaled <- (z1-colMin)/(colMax-colMin)
  z2Scaled <- (z2-colMin)/(colMax-colMin)
    
  plot.new()
  for(i in 1:length(upperTriangle)){
    if(!is.null(colRamp)){
      polygon(lowerTriangle[[i]]$xx,lowerTriangle[[i]]$yy,col=rgb(colRamp(z1Scaled[i]),maxColorValue = 255))
      polygon(upperTriangle[[i]]$xx,upperTriangle[[i]]$yy,col=rgb(colRamp(z2Scaled[i]),maxColorValue = 255))
    }else{
      polygon(lowerTriangle[[i]]$xx,lowerTriangle[[i]]$yy,col=grey(z1Scaled[i]))
      polygon(upperTriangle[[i]]$xx,upperTriangle[[i]]$yy,col=grey(z2Scaled[i]))
    }
  }
  lapply(rectangles,function(x){polygon(x$xx,x$yy,lwd=2)})
  
  #Put labels to the figure
  par(xpd=TRUE)
  
  xLabCoords <- list(x = (rectXBottomLeft+rectXBottomRight)/2 , y = rep(rectYBottomLeft[1],length(rectXBottomLeft))-xOffset)
  text(xLabCoords$x,xLabCoords$y,xlab,cex=cex.x,pos=1)
  
  yLabCoords <- list(x = rep(rectXTopLeft[1],length(rectYTopLeft))-yOffset , y = (rectYTopLeft+rectYBottomLeft)/2)
  text(yLabCoords$x,yLabCoords$y,ylab,cex=cex.y,pos=2)
  
  #Main text 
  mainCoords <- list(x=xMax/2 ,y=yMax)
  text(mainCoords$x,mainCoords$y,main,pos=3,cex=cex.main)
}
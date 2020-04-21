drawField<-function(comp, larg){
  #png(file = "campo.png", bg = "transparent")
  
  par(mfrow=c(1,1))
  xx <- c(0, 0, comp, comp)  
  yy <- c(0, larg, larg,0) 
  plot   (xx, yy, type = "n", xlab = "x", ylab = "y")
  polygon(xx, yy, border = "black", col=NULL)
  # polygon(c(0,0,comp/4,comp/4), c(0,larg,larg,0), border = "black", col="skyblue")
  # polygon(c(comp/4,comp/4, comp/2,comp/2), c(0,larg,larg,0), border = "black", col="slateblue3")  
  # polygon(c(comp/2,comp/2, comp*3/4,comp*3/4), c(0,larg,larg,0), border = "black", col="tomato")  
  # polygon(c(comp*3/4,comp*3/4, comp, comp), c(0,larg,larg,0), border = "black", col="tomato3")  
  
  radius=9.15
  draw.circle(comp/2,larg/2,radius,nv=1000,border=NULL,col=NA,lty=1,lwd=1)
  draw.circle(comp/2,larg/2,0.1,nv=1000,border=NULL,col=NA,lty=1,lwd=1)
  segments(comp/2, 0, comp/2, larg, col= 'black')
  
  
  
  segments(0, larg/2 -20.16, 16.5, larg/2 -20.16, col= 'black')
  segments(0, larg/2 +20.16, 16.5, larg/2 +20.16, col= 'black')
  segments(16.5, larg/2 -20.16, 16.5, larg/2 +20.16, col= 'black')
  
  segments(comp-16.5, larg/2-20.16, comp, larg/2-20.16, col= 'black')
  segments(comp-16.5, larg/2+20.16, comp, larg/2+20.16, col= 'black')
  segments(comp-16.5, larg/2-20.16, comp-16.5, larg/2+20.16, col= 'black')
  
  
  segments(0, (larg/2)-9.16, 5.5, (larg/2)-9.16, col= 'black')
  segments(0, (larg/2)+9.16, 5.5, (larg/2)+9.16, col= 'black')
  segments(5.5, (larg/2)-9.16, 5.5, (larg/2)+9.16, col= 'black')
  
  segments(comp-5.5, (larg/2)-9.16, comp, (larg/2)-9.16, col= 'black')
  segments(comp-5.5, (larg/2)+9.16, comp, (larg/2)+9.16, col= 'black')
  segments(comp-5.5, (larg/2)-9.16, comp-5.5, (larg/2)+9.16, col= 'black')
  
  draw.circle(11,larg/2,0.05,nv=1000,border=NULL,col=NA,lty=1,lwd=1)
  draw.circle(comp-11,larg/2,0.05,nv=1000,border=NULL,col=NA,lty=1,lwd=1)
  
  draw.arc(11, larg/2, 9.16, deg1=307,deg2=414, col = "black")
  draw.arc(comp-11, larg/2, 9.16, deg1=127,deg2=234, col = "black")
  
  #dev.off()
}

drawGame <- function(g){
  drawField(108, 72)
  par(new=TRUE)
  if (length(V(g)) > 0){
    plot.igraph(g, rescale=FALSE,
                xlim=range(0,108), ylim=range(0,72), vertex.shape="square", vertex.label = NA)
  }
}

getImage <- function(image, imageFilter, color){
  if (color == 'green') {
    col  = rgb(green = (0:32)/32, red=0, blue=0)
  }
  else col  = rgb(red = (0:32)/32, green=0, blue=0)
  
  
  i <- applyFilter(image, imageFilter)
  
  image(t(apply(i,2,rev)), col = col)
  
}

applyFilter <- function(image, imageFilter){
  kernel <- matrix(1, nrow = 5, ncol = 31)
  if (imageFilter == 'None'){
    i <- as.matrix(image)
  } else
    if (imageFilter == 'Edge Detection'){
      i <- edge_detection(as.matrix(image), method='Scharr')
    } else
      if (imageFilter == 'Opening'){
        i <- mmand::opening(as.matrix(image),kernel)
      } else
        if (imageFilter == 'Closing'){
          i <- mmand::closing(as.matrix(image),kernel)
        } else
          if (imageFilter == 'Erode'){
            i <- mmand::erode(as.matrix(image),kernel)
          } else
            if (imageFilter == 'Dilate'){
              i <- mmand::dilate(as.matrix(image),kernel)
            } else
              if (imageFilter == 'Mean filter'){
                i <- mmand::meanFilter(as.matrix(image),kernel)
              }
  i
}

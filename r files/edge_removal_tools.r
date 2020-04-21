
removeEdges <- function(graph, distance=3){
  library(igraph)
  distancePoints<-function(x1,y1,x2,y2)
  {
    x<-x1-x2
    x<-x*x
    y<-y1-y2
    y<-y*y
    
    
    sqrt(x+y)
  }
  
  
  distancePointSegment <- function(px, py, x1, y1, x2, y2) {
    ## px,py is the point to test.
    ## x1,y1,x2,y2 is the line to check distance.
    ##
    ## Returns distance from the line, or if the intersecting point on the line nearest
    ## the point tested is outside the endpoints of the line, the distance to the
    ## nearest endpoint.
    ##
    ## Returns 9999 on 0 denominator conditions.
    lineMagnitude <- function(x1, y1, x2, y2) sqrt((x2-x1)^2+(y2-y1)^2)
    ans <- NULL
    ix <- iy <- 0   # intersecting point
    lineMag <- lineMagnitude(x1, y1, x2, y2)
    if(is.null(lineMag) | length(lineMag) == 0)
      return(99999)
    
    if( lineMag < 0.00000001) {
      warning("short segment")
      return(999999)
    }
    u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
    u <- u / (lineMag * lineMag)
    if(is.na(u))
    {
      warning("u is na")
      
      warning(px)
      warning(py)
      warning(x1)
      warning(x2)
      warning(y1)
      warning(y2)
      
      return(999999)
    }
    
    if((u < 0.00001) || (u > 1)) {
      ## closest point does not fall within the line segment, take the shorter distance
      ## to an endpoint
      ix <- lineMagnitude(px, py, x1, y1)
      iy <- lineMagnitude(px, py, x2, y2)
      if(ix > iy)  ans <- iy
      else ans <- ix
    } else {
      ## Intersecting point is on the line, use the formula
      ix <- x1 + u * (x2 - x1)
      iy <- y1 + u * (y2 - y1)
      ans <- lineMagnitude(px, py, ix, iy)
    }
    ans
  }
  
  edges <- get.edgelist(graph)
  t1 <- V(graph)[V(graph)$color == 'green']
  t1_positions <- list(t1$name, t1$x, t1$y)
  t2 <- V(graph)[V(graph)$color == 'red']
  t2_positions <- list(t2$name, t2$x, t2$y)
  
  if (length(edges) > 0){
    for (y in seq(1, length(edges)/2)){
      n1 <- edges[y,1]
      n2 <- edges[y,2]
      if (as.numeric(n1) < 12){
        n1 <- (t1[t1$name == n1])
        n2 <- (t1[t1$name == n2])
        
        for (i in seq(1, length(t2))){
          if (distancePointSegment(t2[[i]]$x, t2[[i]]$y, n1$x, n1$y, n2$x, n2$y) < distance){
            try(graph <- delete_edges(graph, paste(n1$name, "|", n2$name , sep='')), silent=TRUE)
            #print(paste(n1$name, "|", n2$name , sep=''))
          }
        }
      }else{
        n1 <- (t2[t2$name == n1])
        n2 <- (t2[t2$name == n2])
        for (i in seq(1, length(t1))){
          if (distancePointSegment(t1[[i]]$x, t1[[i]]$y, n1$x, n1$y, n2$x, n2$y) < distance){
            #print(paste(n1$name, "|", n2$name , sep=''))
            try(graph <- delete_edges(graph, paste(n1$name, "|", n2$name , sep='')), silent=TRUE)
          }
        }
      }
    }
  }
  graph
}

rotLeft <- function(p) {
  return(matrix(c(0,1,-1,0),2,2)%*%p)
}

rotShape <- function(s) {
  t(apply(s, 1, function(x) c(rotLeft(c(x[1],x[2])),rotLeft(c(x[3],x[4])))))
}


addOffs <- function(m, x, y) {
  m[,1] <- m[,1]+x
  m[,3] <- m[,3]+x
  m[,2] <- m[,2]+y
  m[,4] <- m[,4]+y
  m
}

drawTile <- function(sol) {
  apply(sol,1,function(x) lines(c(x[1],x[3]),c(x[2],x[4])))
  Sys.sleep(1)
}

solveL <- function(n, x, ox, oy) {
	shape <- data.matrix(read.csv("~/Documents/code/RKurs/L.txt",header=F))
  if (n==1) {
    if (identical(x,c(2,1))) {
      drawTile(addOffs(shape+2,ox,oy))
      return(shape+2)
    } else if (identical(x,c(2,2))) {
      drawTile(addOffs(rotShape(shape)+2,ox,oy))
      return(rotShape(shape)+2)
    } else if (identical(x,c(1,2))) {
      drawTile(addOffs(rotShape(rotShape(shape))+2,ox,oy))
      return(rotShape(rotShape(shape))+2)
    } else {
      drawTile(addOffs(rotShape(rotShape(rotShape(shape)))+2,ox,oy))
      return(rotShape(rotShape(rotShape(shape)))+2)
    }
  }

  if (x[1]<=2^(n-1)) {
    if (x[2]<=2^(n-1)) {
      tl <- solveL(n-1,x,ox,oy+2^(n-1))
      tr <- solveL(n-1,c(2^(n-1),1),ox+2^(n-1),oy+2^(n-1))
      bl <- solveL(n-1,c(1,2^(n-1)),ox,oy)
      br <- solveL(n-1,c(1,1),ox+2^(n-1),oy)
      cor <- rotShape(rotShape(rotShape(shape)))+2
    } else {
      tr <- solveL(n-1,c(x[1], x[2]-2^(n-1)),ox+2^(n-1),oy+2^(n-1))
      tl <- solveL(n-1,c(2^(n-1),2^(n-1)),ox,oy+2^(n-1))
      bl <- solveL(n-1,c(1,2^(n-1)),ox,oy)
      br <- solveL(n-1,c(1,1),ox+2^(n-1),oy)
      cor <- rotShape(rotShape(shape))+2
    }
  } else {
    if (x[2]<=2^(n-1)) {
      bl <- solveL(n-1,c(x[1]-2^(n-1),x[2]),ox,oy)
      tl <- solveL(n-1,c(2^(n-1),2^(n-1)),ox,oy+2^(n-1))
      tr <- solveL(n-1,c(2^(n-1),1),ox+2^(n-1),oy+2^(n-1))
      br <- solveL(n-1,c(1,1),ox+2^(n-1),oy)
      cor <- shape+2
    } else {
      br <- solveL(n-1,x-2^(n-1),ox+2^(n-1),oy)
      tl <- solveL(n-1,c(2^(n-1),2^(n-1)),ox,oy+2^(n-1))
      tr <- solveL(n-1,c(2^(n-1),1),ox+2^(n-1),oy+2^(n-1))
      bl <- solveL(n-1,c(1,2^(n-1)),ox,oy)
      cor <- rotShape(shape)+2
    }
  }
  drawTile(addOffs(cor,ox+2^(n-1)-1,oy+2^(n-1)-1))
  return(rbind(bl,
               addOffs(tl ,0,2^(n-1)),
               addOffs(tr,2^(n-1),2^(n-1)),
               addOffs(br,2^(n-1),0),
               addOffs(cor,2^(n-1)-1,2^(n-1)-1)))
}

drawSol <- function(n, x) {
  X11("",10,10)
  plot(NULL, NULL, xlim=c(0,2^n+2), ylim=c(0,2^n+2), type="n")
  rect(x[2], 2^n+1-x[1],x[2]+1,2^n+2-x[1],col="blue")
  sol<-solveL(n,x,0,0)
}

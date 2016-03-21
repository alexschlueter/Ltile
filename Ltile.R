rotLeft <- function(p) {
  return(matrix(c(0,1,-1,0),2,2)%*%p)
}

rotShape <- function(s) {
  t(apply(s, 1, function(x) c(rotLeft(c(x[1],x[2])),rotLeft(c(x[3],x[4])))))
}

addX <- function(m,b) {
  m[,1] <- m[,1]+b
  m[,3] <- m[,3]+b
  m
}

addY <- function(m,b) {
  m[,2] <- m[,2]+b
  m[,4] <- m[,4]+b
  m
}

solveL <- function(n, x) {
  if (n==1) {
    shape <- data.matrix(read.csv("~/Documents/code/RKurs/L.txt",header=F))
    if (identical(x,c(2,1))) {
      return(shape+2)
    } else if (identical(x,c(2,2))) {
      return(rotShape(shape)+2)
    } else if (identical(x,c(1,2))) {
      return(rotShape(rotShape(shape))+2)
    } else {
      return(rotShape(rotShape(rotShape(shape)))+2)
    }
  }

  if (x[1]<=2^(n-1)) {
    if (x[2]<=2^(n-1)) {
      tl <- solveL(n-1,x)
      tr <- solveL(n-1,c(2^(n-1),1))
      bl <- solveL(n-1,c(1,2^(n-1)))
      br <- solveL(n-1,c(1,1))
      cor <- rotShape(rotShape(rotShape(shape)))+2
    } else {
      tl <- solveL(n-1,c(2^(n-1),2^(n-1)))
      tr <- solveL(n-1,c(x[1], x[2]-2^(n-1)))
      bl <- solveL(n-1,c(1,2^(n-1)))
      br <- solveL(n-1,c(1,1))
      cor <- rotShape(rotShape(shape))+2
    }
  } else {
    if (x[2]<=2^(n-1)) {
      tl <- solveL(n-1,c(2^(n-1),2^(n-1)))
      tr <- solveL(n-1,c(2^(n-1),1))
      bl <- solveL(n-1,c(x[1]-2^(n-1),x[2]))
      br <- solveL(n-1,c(1,1))
      cor <- shape+2
    } else {
      tl <- solveL(n-1,c(2^(n-1),2^(n-1)))
      tr <- solveL(n-1,c(2^(n-1),1))
      bl <- solveL(n-1,c(1,2^(n-1)))
      br <- solveL(n-1,x-2^(n-1))
      cor <- rotShape(shape)+2
    }
  }
  return(rbind(bl,
               addY(tl ,2^(n-1)),
               addY(addX(tr,2^(n-1)),2^(n-1)),
               addX(br,2^(n-1)),
               addY(addX(cor,2^(n-1)-1),2^(n-1)-1)))
}

drawSol <- function(n, x) {
  plot(NULL, NULL, xlim=c(0,2^n+2), ylim=c(0,2^n+2), type="n")
  sol<-solveL(n,x)
  apply(sol,1,function(x) lines(c(x[1],x[3]),c(x[2],x[4])))
  rect(x[2], 2^n+1-x[1],x[2]+1,2^n+2-x[1],col="blue")
}
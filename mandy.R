library(dplyr)
library(spatstat)

makegrid <- function(n){
  w<- unit.square()
  g <- gridcentres(w,n,n)
  g[[1]] <- g[[1]] -2
  g[[2]] <- g[[2]] -1
  g[[1]] <- g[[1]] /2
  g[[2]] <- g[[2]] /2
  
  g
}



iterate <- function(L, i, o){
  distance <- function(g)(g$x^2 + g$y^2)
  mandelbrot <- function(g)(list(x = g$x*g$x - g$y*g$y +o$x, y=2*g$x*g$y+o$y))
  
  L[[i]]$M <- distance(L[[i]])
  L[[i+1]] <- mandelbrot(L[[i]])
  L[[i+1]]$M <- distance(L[[i+1]])
  
  L
}

#############################
n<- 1001
iter= 60
g <- makegrid(n)
L <- list(g)

for(i in 1:iter){
  L <- iterate(L,i,g)
}



results <- matrix(rep(0,n*n),nrow=n)
for(i in iter:1){
  results[L[[i]]$M >= 4] <- i/iter
}


image(1-results, col=matlab.like(iter+1))



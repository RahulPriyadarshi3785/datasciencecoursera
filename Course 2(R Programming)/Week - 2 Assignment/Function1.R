add2 <- function(x, y){
  x + y
}

above30 <- function(x){
  use <- x > 30
  x[use]
}

above <- function(x, n = 10){
  use <- x > n
  x[use]
}

columnmean <- function(x){
  nc <- ncol(x)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(x[,i])
  }
  means
}

columnmean <- function(x, removeNA = TRUE){
  nc <- ncol(x)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}
makecachematrix <- function(x = matrix()) { 
  # assumed matrix is always non-singular.
  inv <-  NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inv <<- inverseMatrix
  getInverse <- function() inv
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheInverse <- function(x, ...) {
  inv <- x$getInverse
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
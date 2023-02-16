## makeCachemMatrix() and cacheSolve() together provide a matrix object and a way to cache it's 
## inverse

## Create a special "matrix" object, X,  that can cache its inverse. Includes setters and getters for
## both X and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setmatrixInverse <- function(matrixInverse) xInverse <<- matrixInverse
  getmatrixInverse <- function() xInverse
  list(set = set, 
       get = get,
       setmatrixInverse = setmatrixInverse,
       getmatrixInverse = getmatrixInverse)
}

## returns the inverse of a matrix. First checks for cached inverse, then calculates and caches inverse 
## if there is not already a cached version of the inverse.
cacheSolve <- function(x, ...) {
  xInverse <- x$getmatrixInverse()
  if(!is.null(xInverse)) {
    message("getting matrix inverse")
    return(xInverse)
  }
  data <- x$get()
  xInverse <- solve(data,...)
  x$setmatrixInverse(xInverse)
  xInverse
}

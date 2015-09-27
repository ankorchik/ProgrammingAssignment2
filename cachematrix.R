## R functions, which are able to cache potentially time-consuming computations
## (inverse of a matrix). 

## makeCacheMatrix is a function that stores a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set is a function that changes the matrix stored in the main function.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get is a function that returns the matrix 
  get <- function() x
  ## setInverse and getInverse don't calculate the inverse matrix, 
  ## they store the value of the input in a variable m into the main function 
  ## makeCacheMatrix (setInverse) and return it (getInverse).
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  ## verifing the value m, stored previously with getInverse, 
  ## exists and is not NULL
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## calculating the inverse of the matrix
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

## Functions to compute and cache inverse of a matrix 
## To demonstrate the lexical scoping

## Create a special matrix 'object' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Compute the inverse of the matrix created by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message ("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
}

## How to test:
## > m = matrix(sample(9),3,3)
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)
## Run this again to get the cached value:
## > cacheSolve(cm)
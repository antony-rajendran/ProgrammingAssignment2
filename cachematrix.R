## Functions to compute and cache inverse of a matrix 
## To demonstrate the lexical scoping

## Create a special matrix 'object' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse matrix
  inverse <- NULL
  
  ## Function to initialize the matrices ( <<- is used to change value of x for the whole environment )
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Return the input
  get <- function() x
  
  ## Store inverse matrix
  setInverse <- function(inv) inverse <<- inv
  
  ## Return inverse matrix
  getInverse <- function() inverse
  
  ## Return a list of functions available to the makeCacheMatrix object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the matrix created by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        
  ## Check if a inverse already exist
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message ("getting cached data")
    ## Return cached inverse matrix & exit function
    return (inverse)
  }
  
  ## If inverse doesn't exist, compute new inverse
  ## Retrieve input matrix
  data <- x$get()
  
  ## compute matrix inverse using solve()
  inverse <- solve(data,...)
  
  ## Store new inverse in the cached matrix object
  x$setInverse(inverse)
  
  ## Return the computed inverse of the matrix 
  inverse
}

## How to test:
## > m = matrix(sample(9),3,3)
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)
## Run this again to get the cached value:
## > cacheSolve(cm)
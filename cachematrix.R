## In this assignment we are creating two functions. Overall our goal is to
## to 'cache' functions so that we do not need to recreate them later. In this
## example, we will be caching a process to determine the inverse of a matrix.

## This function creates a matrix that will 'cache' the inverse of itself. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function actually finds the inverse of the matrix given in the prior
## function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse    ## Return a matrix that is the inverse of 'x'
  
}

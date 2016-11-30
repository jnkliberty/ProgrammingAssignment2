## copied from Assignment description... 
##The first function, makeVector creates a special "vector", which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the matrix inverse
##get the value of the matrix inverse

## TL;DR - The first function is meant to create a matrix that is able to cahce its own inverse.
## It was built using the example in the initial prompt as inspiration.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve (x)
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Purpose of this function taken from assignment prompt
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.          


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("we're fetching your data now.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

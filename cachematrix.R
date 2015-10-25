## Project: Cached Inverse Matrix
## Author: rpatal
## Date: 20151025
## Descripction: The following functions allow to cache the inverse of a matrix to reduce the computing cost.

## This function create a special object of matrix that allows to set and get the data and its inverse.
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y)
  {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of a matrix but only if it is not in cache. Otherwise it returns the cached value.
cacheSolve <- function(x, ...) {
  ## Get inverse matrix 
  i <- x$getInverse()
  
  ## If the inverse matrix has been calculated, return it. Otherwise calculate and save it on the x matrix. 
  if (!is.null(i))
  {
    message("Getting cached data")
    return(i)
  }
  original <- x$get()
  i <- solve(original)
  x$setInverse(i)
  i
}

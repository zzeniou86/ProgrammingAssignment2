## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. Below are two 
## functions that are used to create a special object that stores a matrix and 
##cache's its inverse


## This function creates a "special matrix", which is a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of a matrix created by makeCacheMatrix function. 
## It checks if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and it skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
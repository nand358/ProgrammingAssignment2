## 1. Function makeCacheMatrix returns a special matrix which is a list of 4 functions 
##    	These functions act as getter and setter for the input matrix and its inverse 
## 
## 2. Function cacheSolve takes in the specialCacheMatrix and returns the inverse of the matrix. 
## 	This function assumes that the input matrix is invertible
## 	If the inverse is computed for the first time it does not print the message "Getting cached data". 
## 	If the inverse is being returned from the cached value, then "Getting cached data" string is printed and the 
## 	inverse of x is returned.

## Function makeCacheMatrix creates a list of the following functions
##    1. set function - Sets a new matrix and also resets the cached Inverse to NULL
##    2. get function - Returns the matrix x
##    3. setInverse function - Sets the inverse after the first computation of the inverse
##    4. getInverse function - Returns the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL

  set <- function(y) {
    x <<- y
    xInverse <<- NULL 
  }
  
  get <- function() x
  
  setInverse <- function(inverse) xInverse <<- inverse
  
  getInverse <- function() xInverse
  
  # Returns a list of functions - set, get, setInverse, getInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function cacheSolve takes in the specialCacheMatrix x and 
## returns the inverse from the cache is already calculated else calculates the inverse 
## and caches the inverse and returns the inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("Getting cached data")
    return(xInv)
  }
  
  data <- x$get()
  xInv <- solve(data)
  x$setInverse(xInv)
  xInv
}


## Creating 2 functions that (1) create a special matrix and cache its inverse and
## (2) a funciton that can pull the cached inverse

## These function creates a special matrix and calculates its inverse
## The set function sets the value of the matrix
## The get function gets the value of the matrix
## The setinverse function calculates the inverse of the matrix
## The getinverse function retreives the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will calculate the inverse of the matrix but will first check to see if the
## inverse as already been calculated and if it has, skip the calculation and pull it from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


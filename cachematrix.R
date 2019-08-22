## The following two functions caches the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <-function(y) {
    x <<- y 
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) m <<-inverse
  getinverse <- function() m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix.
##It first checks to verify if an inverse has already been calculated, in which case it would retrieve 
##the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <-x$get() 
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



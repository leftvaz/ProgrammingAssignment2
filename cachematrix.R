## These two functions will calculate the inverse of a matrix or 
## retrieve it from cache if it has been computed.


## This function can set the value if a matrix, retrieve it, calculate 
## the inverse matrix and retrieve it. We assume the original matrix is 
## reversible

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


## This function calculates the inverse of a matrix produced by the 
## above function. If the inverse of the specified matrix has already
## been calculated, it is retrieved from the cache.

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



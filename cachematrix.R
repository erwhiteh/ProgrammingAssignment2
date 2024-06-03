## The idea of these functions is to cache values from the inverse of a matrix to shorten calculation times

## This is designed to create a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    getmatrix <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }



## This funtion looks to see if the results have already been completed and gathers them from the cache or if not returned the calculation.

cacheSolve <- function(x, ...) {
            m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

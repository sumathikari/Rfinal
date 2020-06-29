# Rfinal
## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of solve
# 4. get the value of solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
       set <- function(y) {
             x <<- y
             m <<- NULL
         }
       get <- function() x
       setsolve <- function(solve) m <<- solve
       getsolve <- function() m
       list(set = set, get = get,
                       setsolve = setsolve,
                       getsolve = getsolve)
}

## CacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above using solve(). If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

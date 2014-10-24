## Put comments here that give an overall description of what your
## functions do
#
## makeCacheMatrix: This function creates a special "matrix" object that can 
##                  cache its inverse.
#
## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. If the inverse has already 
##             been calculated (and the matrix has not changed), then the 
##             cachesolve should retrieve the inverse from the cache.
#
## Take the makeVector & cachemean codes of the assignment as a key reference
## Created by: YuChou Chen (Joe Chen)
## =============================================================================
## Usage of example:
## c=rbind(c(1, -1/4), c(-1/4, 1))
## m <- makeCacheMatrix(c)
## cacheSolve(m)
## cacheSolve(m) # will show "getting cached data" of the matrix
## =============================================================================
## Write a short comment describing this function
#
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      Im <- NULL   # Im means Inverse of matrix
      set <- function(y) {
            x <<- y
            Im <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) Im <<- solve   # change mean to solve
      getInverse <- function() Im
      list(set = set, 
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## =============================================================================
## Write a short comment describing this function
#
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      Im <- x$getInverse() # Im means Inverse of matrix
      if(!is.null(Im)) {
            message("getting cached data")
            return(Im)
      }
      data <- x$get()
      Im <- solve(data, ...)
      x$setInverse(Im)
      Im
}

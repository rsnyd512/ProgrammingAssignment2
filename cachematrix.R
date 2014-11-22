## Coursera rprog-009, Nov 2014

## Solves for inverse matrix; caches result; speeds further calculations by
## recalling cahced data (if available) to avoid excess computation, i.e. during
## loops. 


## makeCacheMatrix creates a matrix object with the following subroutines
## (function calls): set, get, setmatrix, getmatrix
## Example:  m <- makeCacheMatrix()
##           m$set(matrix(1:6, nrow=2, ncol=3))

makeCacheMatrix <- function(x = matrix()) {
      v <- NULL
      set <- function(y) {
            x <<- y
            v <<- NULL
      }
      get <- function() x
      setmatrix <- function(s) v <<- s
      getmatrix <- function() v
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}


## cacheSolve solves for inverse of matrix. First attempts to look for cached 
## data; if not available, solves using solve() and caches result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      v <- x$getmatrix()
      if(!is.null(v)) {
            message("retrieving cached matrix data")
            return(v)
      }
      data <- x$get()
      v <- solve(data)
      x$setmatrix(v)
      v
}

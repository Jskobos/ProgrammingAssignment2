## Set of functions for calculating the inverse of an invertible 
## square matrix and caching the result.

## Takes an invertible square matrix as its argument and builds a
## wrapper around it to handle inverse caching and retrieval.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    getInverse <- function() inverse
    setInverse <- function(i) inverse <<- i
    list(get=get,set=set,
         getInverse=getInverse,
         setInverse=setInverse)
}


## Take a makeCacheMatrix list object as an argument. If the matrix's
## inverse has already been calculated and cached, returns the cache.
## Otherwise calculates and caches the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    this_inverse <- x$getInverse()
    if (!is.null(this_inverse)) {
        message("Using cached inverse.")
        return(this_inverse)
    }
    new_inverse <- solve(x$get())
    x$setInverse(new_inverse)
    new_inverse
}

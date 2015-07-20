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


## Takes a makeCacheMatrix list object as an argument. If the matrix's
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

testAssignment2 <- function() {
    ## Set sample matrices
    t1 <- matrix(c(2,3,2,2),nrow=2,ncol=2)
    t2 <- matrix(c(1,0,5,2,1,6,3,4,0),ncol=3,nrow=3)
    
    ## Create cacheable matrix
    test_cache <- makeCacheMatrix(t1)
    stopifnot(test_cache$get() == t1)
    
    ## Test set() function
    test_cache$set(t2)
    stopifnot(test_cache$get() == t2)
    
    ## Does cacheSolve() return the correct result?
    solved <- cacheSolve(test_cache)
    stopifnot(solved == solve(t2))
    
    ## Retry the call to see if the cache is used.
    solved_again <- cacheSolve(test_cache)
    stopifnot(solved_again == solve(t2))
    
    ## Now try with t1
    test_cache$set(t1)
    solved <- cacheSolve(test_cache)
    stopifnot(solved == solve(t1))
    
    ## Once again, hopefully using the cache:
    solved_again <- cacheSolve(test_cache)
    stopifnot(solved_again == solve(t1))
    
    message("All tests passed. Check console to ensure caching was used.")
}

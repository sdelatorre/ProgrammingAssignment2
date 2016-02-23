## The following utility functions provide users with the ability to 
## efficiently calculate and cache the inverse of a standard R matrix.

## Creates a matrix object wrapper with the ability to store the 
## cached inverse of the wrapped matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of a matrix object returned by the makeCacheMatrix
## function. This function will use a cached version of the inverse if
## one is found in the matrix object; otherwise, the inverse will be 
## calculated, cached, and returned to the caller. 

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

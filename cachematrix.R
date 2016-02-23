## Put comments here that give an overall description of what your
## functions do

## Creates an enhanced matrix object with the ability to store the 
## cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the matrix passed in by the user. A cached
## version will be used if found, otherwise the inverse will be calculated,
## cached, and returned to the calller. 

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

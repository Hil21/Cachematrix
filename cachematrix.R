## This script contains functions for caching the inverse of a matrix.
# Caching is used to optimize performance by avoiding redundant calculations
# of the matrix inverse, which is computationally expensive.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }
    




## ## This function computes the inverse of the "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed,
# it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # If the cached inverse is not available, calculate it, cache it, and return it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

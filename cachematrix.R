## The makeCacheMatrix and cacheSolve functions below cache the inverse of the matrix. As a result,
## the inverse of the matrix does not need to be computed repeatedly if the value is already cached. 

## The makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<-inverse
getInverse <- function() inv
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object that is returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

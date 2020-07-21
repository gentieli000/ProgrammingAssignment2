## Caching the Inverse of a Matrix Assignment:Caching the inverse of
## a matrix can save time, so that it can be looked up rather than
## recomputed.

## This function creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        e <- NULL
        set <- function(y) {
                x <<- y
                e <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) e <<- inverse
        getInverse <- function() e
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the "matrix" object, unless
## it has already been calculated in which case it is retrieved from
## the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        e <- x$getInverse()
        if(!is.null(e)) {
                message("getting cached data")
                return(e)
        }
        mat <- x$get()
        e <- mean(mat, ...)
        x$setInverse(e)
        e
}

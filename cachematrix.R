## These functions are for caching and calculating the invert of a matrix.


## The first function "makeCacheMatrix" caches a matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               ## m is NULL if inverse is not calculated
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setinverse <- function(solve) {         ## inverse is m after calculating
                m <<- solve
        } 
        getinverse <- function() {s}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second function "cachesolve" computes the inverse of a matrix. 
## If the inverse has already been calculated by makeCacheMatrix, it can retrieve the inverse from the cache. 
cachemean <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {                       ## retrieve the inverse from the cache if the inverse 
                message("getting cached data")  ## has already been calculated by makeCacheMatrix
                return(m)
        }
        data <- x$get()                 ## Calculate the inverse of the matrix if m is null
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

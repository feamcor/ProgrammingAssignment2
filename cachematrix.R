## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
##
## The functions below cache the inverse of a matrix.

# AUTHOR: Fabio Correa
# EMAIL : feamcor@gmail.com
# COURSE: R Programming (rprog-032)
# TASK  : Programming Assignment 2 - Caching the Inverse of a Matrix

## Create special matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL
    set <- function(x.new) {
        x <<- x.new
        x.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) x.inverse <<- inv
    getinverse <- function() x.inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by
## 'makeCacheMatrix', unless it exists in cache, where it
## is retrieved from.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x.inverse <- x$getinverse()
    if(!is.null(x.inverse)) {
        message("Getting cached data")
        return(x.inverse)
    }
    data <- x$get()
    x.inverse <- solve(data, ...)
    x$setinverse(x.inverse)
    x.inverse
}

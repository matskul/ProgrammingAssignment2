## Creates a list of functions that are able
## to cache the inverse of the specified matrix to use
## it later rather than repeatedly compute it at runtime

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list (set = set, get = get,
          setsolve = setsolve, getsolve = getsolve)
}


## Computes the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case
## it retrieves it from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if (!is.null(s)) {
        # Using cached result if available
        message("getting cached data")
        return(s)
    }
    # Otherwise calculate the inverse
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

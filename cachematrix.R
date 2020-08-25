## These functions, makeCacheMatrix and cacheSolve, are used to process the
## inverse matrix functions in a more efficient way. Rather than recalculating
## the inverse each time, it stores the value for a given matrix's inverse,
## and if it is called upon again it can simply be given from cache.

## makeCacheMatrix takes a matrix as input and caches 4 functions as a list.
## get and set allow you to input and retrieve a matrix, getinv and setinv are
## used to input and receive their inverses, if used with cacheSolve below.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve takes the input in the form of a makeCacheMatrix list. It sets m
## to be the inverse from makeCacheMatrix. If this is null, then the inverse
## is calculated. If it is not null then the value for inverse already exists
## within the cache and is retrieved, rather than recalculated.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

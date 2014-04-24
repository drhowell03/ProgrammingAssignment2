## makeCacheMatrix creates a list containing a function to:
## set value of the matrix 
## get value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## cacheSolve calculates the inverse of makeCacheMatrix
## it first checks to see if the inverse has already been calculated 
## if so, it gets the inverse from the cache and skips the computation
## otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache, via the setSolve function.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}

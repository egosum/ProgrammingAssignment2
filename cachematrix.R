## Calculating the inverse of a matrix could be an
## expensive computation, so caching it would allow
## an optimization on future calculations of it.
## This script contains two functions which can be
## used for calculating and caching an inverse of
## a matrix.

## This function creates an object which will cache
## the inverse of a matrix, thanks to several functions.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverted <- function(inverted) m <<- inverted
        
        getinverted <- function() m
        
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)
}


## This function calculates the inverse of a matrix or returns
## its cached value for it, depending on whether this inverse
## has been calculated before or not.

cacheSolve <- function(x, ...) {
                
        m <- x$getinverted()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverted(m)
        
        m
}

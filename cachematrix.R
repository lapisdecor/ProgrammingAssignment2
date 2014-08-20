## These functions cache the inverse of a matrix so that it
## can be looked up in the cache rather than recomputed,
## which is less time-consuming, specially in a long matrix

## makeCacheMatrix creates a special "matrix" which is
## really a list containing a function to set the value
## of the matrix, get the value of the matrix, set the
## inverse matrix, get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve returns the inverse of a "matrix" by first
## checking the cache to see if the inverse matrix has
## already been computed in which case returns the cached
## inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

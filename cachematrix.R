## Put comments here that give an overall description of what your
## functions do
# The first function makeCacheMatrix is used to create the special matrix 
# object. That object is special because it can "store" a cached copy of
# its inverse matrix. However, in order create and use the cached inverse
# matrix it must be generated with the second function cacheSolve.

## Write a short comment describing this function
# This function creates the special matrix with a cached copy of
# its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# This function takes as argument a special matrix object created
# with makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

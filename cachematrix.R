## cachematrix.R provides functions which allow for the efficient 
## computation of the inverse of a matrix.  The implementation relies
## on a specific matrix created via makeCacheMatrix() combined with
## use of cacheSolve() to compute the inverse.  Subsequent calls to 
## cacheSolve() will return the cached inverse of a matrix as long as the 
## matrix has not changed.


# This function creates a special "matrix" object with the ability to cache
# the value of its inverse.  The cache is cleared whenever the matrix is 
# changed.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    
    getsolve <- function() s
    
    list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# This function computes the inverse of a matrix created via the makeMatrix
# function.  If the specified matrix already contains a cached value, then 
# that value is used instead of recomputing the inverse.
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    
    if (!is.null(s)) {
        print('returning cached solve value')
        return(s)
    }
    
    s <- solve(x$get(), ...)
    x$setsolve(s)
    s
}

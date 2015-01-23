## It is a programming assignment 2

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    xSolve <- NULL # initialize the cached value of inverted matrix as NULL
    get <- function() x # return x
    set <- function(x){ # set x
        x <<- x 
        xSolve <<- NULL # if the matrix is changed, its inveted value changes too
    }
    getSolve <- function() xSolve # get the cached value of the inversion
    setSolve <- function(xSolve) xSolve <<-xSolve # # get the cached value of the inversion
    # return the list of available "methods"
    list(
        get = get,
        set = set,
        setSolve = setSolve,
        getSolve = getSolve
    )
}


## This function computes the inverse of the special "matrix"returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    xs <- x$getSolve() # get the cached value of inverted `x`
    if (is.null(xs)) # cache it if it has been NULL
    {
        xs <- solve(x$get(), ...)
        x$setSolve(xs)
        message("caching data")
    }
    else message("getting cached data")
    xs ## Return a matrix that is the inverse of 'x'
}

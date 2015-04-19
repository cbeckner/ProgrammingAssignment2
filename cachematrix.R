##The functions below allow to specify an invertable matrix,
##Store its inverse and cache the inversion.

## Creates a special "matrix" object that can cache its inverse.
## This does not currently work with singular or degenerate matrices
makeCacheMatrix <- function(x = matrix()) {
    #Initialize the inverse matrix variable
    imat <- NULL

    #Sets a new value for the matrix and clear the cached version
    set <- function(y) {
        x <<- y
        imat <<- NULL
    }

    #Gets the current underlying matrix
    get <- function() x

    #Sets the inverse of the matrix.
    #If no value is provided for inverse, the inverse will be calculated
    set.inverse <- function(inverse = NULL, ...) {
        if(is.null(inverse)) {
            imat <<- solve(x, ...)
        } else {
            imat <<- inverse
        }
    }

    #Gets the current stored inverse
    get.inverse <- function() imat
    list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## Return a matrix that is the inverse of 'x'
## This will return a cached version of the inverse if one exists.
## If no cached version is available, the inverse will be calculated
cacheSolve <- function(x = makeCacheMatrix, ...) {
    imat <- x$get.inverse()
    #check to see if there is a cached versoin available
    if(!is.null(imat)) {
        message("Returning cached data")
        return(imat)
    }

    #cached data was not available.
    #compute the inverse of the matrix
    data <- x$get()
    imat <- solve(data, ...)
    x$set.inverse(imat)

    #return computed inverse
    imat
}

## These functions accomplish the caching of a matrix inversion, which can be
## a costly operation. Using a previously computed inverse of a matrix
## instead of computing it again saves computation time.
##
## makeCacheMatrix can be used to obtain a special object of a matrix.
## cacheSolve uses a matrix object created by makeCachematrix to compute
## the inverse of the matrix and store the result in the object.
## If the inversion of the same matrix has been done before,
## the result will be pulled out of the cache.

## create a matrix object with the following functions:
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse of the matrix
## * get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse of the matrix with NULL
    inv <- NULL
    
    # set stored matrix to new matrix y and reinitialize inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get stored matrix
    get <- function() x
    
    # set inverse of matrix
    setinv <- function(inv) inv <<- inv
    
    # get inverse of matrix
    getinv <- function() inv
    
    # set list of operations
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## compute the inverse of a matrix object created by makeCacheMatrix
## if the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # get inverse of matrix object
    inv <- x$getinv()
    
    # if cached version of inverse is available, return that
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # get the underlying matrix
    data <- x$get()
    
    # get the inverse of the matrix
    inv <- solve(data, ...)
    
    # store the inversed matrix in the cache
    x$setinv(inv)
    
    # return the calculated inverse matrix
    inv
}
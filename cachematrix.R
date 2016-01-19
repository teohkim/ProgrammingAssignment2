## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## makeCacheMatrix function and cachSolve function can be used to cache the 
## computed inverse matrix.

## Creates a special "matrix" object that can cache its inverse.
## It is actuallly a list containing functions to
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the inverse (setinv)
## get the value of the inverse (getinv)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function () x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse matrix of the special matrix created with makeCacheMatrix.
## It first check to see if the inverse matrix has already been calculated. If 
## so, it get ths inverse matrix from the cache and skips the compuation. 
## Otherwise, it caculcates the inverse matrix of the data using solve function
## and set the value of the inverse matrix in the cache via the setinv function.
##
## In this assignment, it is assumed that the matrix supplied is always
## invertible.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

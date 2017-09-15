
## This function creates a special matrix object
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}




## This function computes the inverse of the special
## matrix returned by the makeCacheMatrix function.  If
## the inverse has already been calculated, then this
## function will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
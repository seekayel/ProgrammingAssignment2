## CacheMatrix is a package that enables you to cache calculations on a matrix
## which could be expensive to compute. Currently "solve" aka calculating the
## inverse is the only supported function.
## Functions:
##   makeCacheMatrix - creates the object that hold the matrix and inverse.
##   cacheSolve - access function to return cached value or calculate it.


## makeCacheMatrix: Takes an matrix (optional) and stores it internally. Get and
## set functions are available for the matrix. getinverse & setinverse serve as
## access functions to the cached value of solve(X). Modifications of x will
## reset the cached inverse value.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse<- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setinverse = setInverse,
         getinverse = getInverse)
}


## cacheSolve - takes a cacheMatrix, additional arguements are passed directly
## into the solve function if it is called. This function either returns the
## cached value inside of x or computes it, sets it and returns it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

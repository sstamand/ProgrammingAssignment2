## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                    x <<- y
                    inv <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) inv <<- inv
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has alreday been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the 
## cache

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
                message("getting chached data")
                return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
}


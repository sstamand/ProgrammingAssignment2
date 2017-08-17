## The function caches the value of the inverse of a matrix by creating
## a special "matrix" that takes the form of a list that contains a function
## to set the value of a matrix, get the value of a matrix, set the inverse
## value, and get the inverse value. To retrieve the inverse of a matrix, the
## function first checks to see if the inverse of the matrix is already in the
## cache, otherwise it calculates the inverse of the matrix.

## makeCacheMatrix creates a special "matrix" object in the form of a list
## that contains a function to set the value of a matrix, get the value of 
## a matrix, set the inverse value, and get the inverse value.

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


## cacheSolve function checks to see if the the inverse of the matrix is in the
## cache. If the inverse is not in the cache or the matrix has changed, 
## the function calculates the inverse using the solve function.

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


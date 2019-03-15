## Caching the Inverse of a Matrix
## As described in the instructions for Programming Assignment 2, "matrix inversion is
## usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly." These two functions cache the inverse
## of a matrix to allow for more efficient computation.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" function returned by the
## makeCacheMatrix function above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Put comments here that give an overall description of what your
## functions do

## implementation of a cacheable inverse function for square matrices

## makeCacheMatrix
## x [input] a square matrix 
## returns: a list of functions to operate on the input matrix and/or its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## x [input] a list of operator functions that operate on a matrix
## returns: the inverse of the matrix. The inverse is cached. 
## Subsequent calls return the cached value

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

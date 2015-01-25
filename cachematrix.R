## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverseMatrix) matrixInverse <<- inverseMatrix
    getInverse <- function() matrixInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function gets the inverse of the special matrix created by the function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    matrixInverse <- x$getInverse()
    
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    
    matrixInverse
}

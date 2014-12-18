## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Example: makeCacheMatrix(matrix(1:16, nrows=4))
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    #setMatrix used to change the matrix
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    #getMatrix to get the current matrix
    getMatrix <- function() x
    #set the inverse for the current matrix
    setInverse <- function(inv) inverse <<- inv
    #get the inverse for the current matrix
    getInverse <- function() inverse
    #list of methods needed
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    # check if the result is already in cache if yes return the cached result
    if(!is.null(inverse)) {
        message("This is a cached data")
        return(inverse)
    }
    matrix <- x$getMatrix()
    # compute inverse
    inverse <- solve(matrix)
    # cache the computed result
    x$setInverse(inverse)
    inverse
}

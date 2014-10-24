## Put comments here that give an overall description of what your
## functions do

## A function that creates a cache-able matrix, returns an object that has set, get, getInverse and setInverse properties 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## A function that returns the inverse of a "makeCacheMatrix" object. Only computes if it hasnt been computed before 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
            message("getting cached inverse")
            return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setInverse(i)
        i
}

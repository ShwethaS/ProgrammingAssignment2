## Caching the Inverse of a Matrix
## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its Inverse

## This function creates a special "matrix" objecct that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(invMatrix) inv <<- invMatrix
        getInverse <- function() inv
        list(set=set, get = get, 
             setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If inverse is already available for same matrix, 
## then this should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting Cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv 
}

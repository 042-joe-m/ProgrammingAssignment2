## ProgrammingAssignment2 (27 September 2015)
##
## This file contains two functions that work together to cache the inverse of a matrix.  
## The first function (makeCacheMatrix) gets and stores a matrix, and the second function
## (cacheSolve) calculates and caches the inverse of a matrix.  Because matrix inversion
## is usually a costly computation, the second function attempts to gain some benefit by 
## caching the inverse of a matrix rather than computing it repeatedly. 
##

## Function makeCacheMatix: creates a special "matrix" object that can cache its inverse.
## This function contains four functions: set, get,  setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve: computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has
## not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Retrieves a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## If the inverse was already calculated, then the cached value is used
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If the inverse has not been calculated, then the matrix is retrieved
        ## and the inverse is calculated using solve()
        data <- x$get()
        i <- solve(data)
        ## The inverse is then cached (set) for possible use in the future
        x$setinverse(i)
        i
}

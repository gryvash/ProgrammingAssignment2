## Assignment of week3 from R programming at Coursera
## For the matrix x and object which contains the matrix
## itself and methods to get and set its inverse is created

## This function creates aforementioned object
## "get" method returns the stored matrix
## "set" method puts the new matrix to the object and resets
## inverse matrix
## "setInverse" method is used by cacheSolve function to set
## the inverse of stored matrix
## "getInverse" method returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix())
{
        cacheInverse <- NULL
        set <- function(y) {
                x <<- y
                cacheInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(InversedMatrix) cacheInverse <<- InversedMatrix
        getInverse <- function() cacheInverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the given matrix
## or takes is from cache if it is already exists.
## If matrix is not invertable the warning message is thrown

cacheSolve <- function(x, ...) {
        cacheInverse <- x$getInverse()
        if(!is.null(cacheInverse))
        {
                message("getting cached data")
                return(cacheInverse)
        }
        mMatrix <- x$get()
        if(det(mMatrix) == 0)
        {
                message("Inverse matrix does not exist")
        }
        else
        {
                cacheInverse <- solve(mMatrix, ...)
                x$setInverse(cacheInverse)
                cacheInverse
        }
}

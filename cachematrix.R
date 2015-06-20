## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # initialize the cache value to null
        cache <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of the inverse 
        setInverse <- function(inverse) cache <<- inverse
        
        # get the value of the inverse
        getInverse <- function() cache
        
        # returns the 'special matrix' containing all of the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # get the inverse of the matrix stored in cache
        cache <- x$getInverse()
        
        # return inverted matrix from cache if it exists
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        
        # else create the matrix in working environment
        data <- x$get()
        cache <- solve(data, ...)
        x$setInverse(cache)
        return (cache)
}

## Enables the caching of inverse matrix operations.
## Example:
## x <- matrix(c(-8,-3,24,2),ncol=2,nrow=2)
## y <- makeCacheMatrix(x)
## cacheSolve(y)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## makeCacheMatrix takes the given matrix and stores it in cache
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve returns the inverse of the given matrix by checking if the inverse has been cached.
## if so, it returns the inverse that was stored in cache.
## otherwise, it computes the inverse and stores it in cache, then returns the computed result.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}

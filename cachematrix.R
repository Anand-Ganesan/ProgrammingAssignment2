## The following functions facilitate creating and retrieving an invertible matrix and caching its inverse

## makeCacheMatrix enables setting and retrieving a matrix, setting and retrieving the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of a matrix. It first checks if the inverse is cached.
## gets the result from the cache if already cached, else computes the inverse and sets the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

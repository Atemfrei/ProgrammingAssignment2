## This pair of functions calculate and cache the inverse of a matrix
## to avoid repeated computations. 

## makeCacheMatrix() sets up storate and functions for caching computed inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # placeholder for storage of the inverse in cache 
        i <- NULL
        
        # set a new matrix value and clear the cache for inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # return the matrix
        get <- function() x
        
        # store the inverse in the cache
        setsolve <- function(inverse) i <<- inverse
        
        # return the inverse from cache
        getsolve <- function() i
        
        # create a list for handling the cache
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve() checks for a cached inverse matrix and returns it if available.
## Otherwise it solves the matrix and stores it into the cache

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        
        # Check if the inverse is allready chached. If so, return the inverse with an additional message
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # If the cache is empty, create the inverse and store it in the cache
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

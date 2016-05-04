## These functions creates an environment for a matrix and four functions
## that allow storing the inverse of the matrix for future use.

## In this function the environment is created and a vector of functions as a 
## setup for the inversion calculation and caching.

makeCacheMatrix <- function(x = matrix()) {
        
        # we start with no inverse defined
        inverse <- NULL
        
        # set will set the value of x to the desired value
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # get returns current value of x
        get <- function() x
        
        # setinverse sets the value of inverse to the given value
        setinverse <- function(i) inverse <<- i
        
        # getinverse returns current value of inverse
        getinverse <- function() inverse
        
        # make and return list of functions as they are build above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if a cached inverse is present. If so it returns it.
## If not it calculates the inverse and caches it in the environment 
## created by the function makeCacheMatrix 

cacheSolve <- function(x, ...) {
        
        ## Checks the environment created with makeCacheMatrix for an
        ## inverse. If it is there, it returns it with a message
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## This code is executed when no inverse was found. It will 
        ## calculate the inverse and cache it in the environment
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

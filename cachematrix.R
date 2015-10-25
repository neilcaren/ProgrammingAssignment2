## These functions work together to create and cache the inverse of a matrix.  
## THis is handy when you have a large matrix and need its inveresre repeatlly.  
## Instead of calculating the inverse each time its needed, these functiopns together manage 
## a cached version located in the global environment and quickly return a cached version.


## This function essentially returns a list of functions designed to work with the cacheSolv function  
## the function creates get, set, getinverse and setinverse functions to be used by cachedSolve 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## THis function returns a matrix that is the inverse of 'x'
## 'x' needs to be the object returned by makeCacheMatrix function
## using the function hooks that are part of 'x', the function determines if the 
## inverse needs to be calculated or simply returned from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

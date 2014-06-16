## These functions are used to calculate the
##inverse of a matrix in a efficient manner
## using cache of already calculated values


## This function is used to cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    #This function is used to set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #function to return the matrix
    get <- function() x
    
    #function to set the inverse
    setinverse <- function(inverse) i <<- inverse
    
    #function to get the inverse
    getinverse <- function() i
    
    #Create a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is used to return the inverse of a matrix
## either by looking it up if it is already calculated or 
## by calculating it

cacheSolve <- function(x, ...) {
    
    ## Returns a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    
    ##Returns the cached inverse if available
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ##Calculate the inverse, set it in the cache
    ##and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

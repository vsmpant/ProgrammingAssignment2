## Matrix inversion is usually a costly computation and there 
##  may be some benefit to caching the inverse of a matrix rather 
##  than compute it repeatedly (there are also alternatives to 
##  matrix inversion that we will not discuss here). Your assignment 
##  is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix:
## Creates a special "matrix" object that can cache its inverse.
## creates a list containing a function to
##  -  get and set the value of the matrix
##  -  get and set the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    # initialize the inverse value
    inv <- NULL
    
    # set the value of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of matrix
    get <- function() x
    
    # set the value of inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # get the valuf of inverse of the matrix
    getinverse <- function() inv
    
    # return a list of all the above functions
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special 
##  "matrix" returned by makeCacheMatrix above. If the inverse has 
##  already been calculated (and the matrix has not changed), then 
##  the cachesolve should retrieve the inverse from the cache.

## USAGE:
##  cacheSolve(m) - where m is 'makeCacheMatrix' object
##  
##   - calculates on the first run (no cache on first run)
##   - returns from the cache for subsequent runs

## For example:
##  x = rbind(c(1,2), c(3,4))
##  m = makeCacheMatrix(x)
##  m$get()
##  cacheSolve(m)  # outputs inverse
##  cacheSolve(m)  # outputs inverse from cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # obtain from cache
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
            message("obtaining the cached data..")
            return(inv)
        }
        
        # cache is empty on first run
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

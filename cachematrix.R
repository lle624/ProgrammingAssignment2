## This is the 2nd programming assignment for Coursera R Programming. The objective is to write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # initializing cim to NULL to store Cached Inverse Matrix
    cim <- NULL
    
    # setting the value of the matrix
    set <- function(y) {
        x <<- y
        cim <<- NULL
    }
    
    # getting the value of the matrix
    get <- function() x
    
    # inverting the matrix and storing in cache
    setInverse <- function(inverse) cim <<- inverse
    
    # getting the inverted matrix from cache
    getInverse <- function() cim
    
    # returning the above functions to the working environment
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

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
    ## Return a Cached Inverse Matrix (cim) that is the inverse of 'x'
    cim <- x$getInverse()
    
    # If the inverse is already calculated, return it
    if (!is.null(cim)) {
        message("Retrieving inverse from cache")
        return(cim)
    }
    
    # If the inverse is not yet calculated, calculate it
    data <- x$get()
    cim <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(cim)
    
    # Return it
    cim
}

##Example output:
# > x<-matrix(rnorm(9), nrow=3)
# > cx<-makeCacheMatrix(x)
# > cx$get()
# [,1]       [,2]        [,3]
# [1,] -1.6921979  0.8477106 -0.02534846
# [2,] -0.2902412  1.1355206 -2.32625720
# [3,] -1.7088447 -0.0977927  0.24889753
# > cacheSolve(cx)
# [,1]        [,2]       [,3]
# [1,] 0.01676997 -0.06341944 -0.5910257
# [2,] 1.23102981 -0.14127748 -1.1950422
# [3,] 0.59881271 -0.49092446 -0.5095976
# > cacheSolve(cx)
# Retrieving inverse from cache
# [,1]        [,2]       [,3]
# [1,] 0.01676997 -0.06341944 -0.5910257
# [2,] 1.23102981 -0.14127748 -1.1950422
# [3,] 0.59881271 -0.49092446 -0.5095976

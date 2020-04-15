## this file contains two functions, one creates a matrix and cache it's
## information, and the other calculates the inverse of a matrix

## this function creates a matrix, offers functions to set the value of 
## the matrix, get the value of the matrix, set the inverse matrix 
## get the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL 
    set <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) mInverse <<- Inverse
    getInverse <- function() mInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## caheSolve() checks if an inverse matrix is already cached. if so, 
## it returns it's value immediately. if not,it calculates the inverse matrix
## and then returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mInverse <- x$getInverse()
    if(!is.null(mInverse)){
        message("getting cached inverse matrix")
        return(mInverse)
    }
    data <- x$get()
    mInverse <- solve(data, ...)
    x$setInverse(mInverse)
    mInverse
}

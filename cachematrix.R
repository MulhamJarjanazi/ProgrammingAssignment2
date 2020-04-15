## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

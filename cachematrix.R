## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and has functions to cache and return the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This will check to see if there is a stored Inverse matrix, it not, then it will take the transpose of the matrix
## and assign the Inverse to m

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}

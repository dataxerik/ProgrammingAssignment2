## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and has functions to cache and return the transpose of that matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setTranspose <- function(t) m <<- t
    getTranspose <- function() m
    list(set = set, get = get,
         setTranspose = setTranspose,
         getTranspose = getTranspose)
}


## This will check to see if there is a stored transpose matrix, it not, then it will take the transpose of the matrix
## and assign the transpose to m

cacheSolve <- function(x, ...) {
    m <- x$getTranspose()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- t(data)
    x$setTranspose(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

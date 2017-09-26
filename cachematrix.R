## Put comments here that give an overall description of what your
## functions do

## Here a function is constructed with which the inverse of a given matrix is 
## stored  

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) { 
            x <<- y
            mat_inv <<- NULL
        }
    get <- function() x
    setinv <- function(solve) mat_inv <<- solve(x)
    getinv <- function() mat_inv
    list(set = set, 
         get = get, 
         setinv = setinv,
         getinv = getinv)
}


## This function solves the matrix, whilst first checking whether an inverse 
## stored in the cache can be used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$getinv
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
    data <- x$get()
    mat_inv <- solve(data,...)
    x$setinv 
    return(mat_inv)
}

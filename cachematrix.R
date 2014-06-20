## PROGRAMMING ASSIGNMENT #2, DUE 22 JUNE 2014
## This pair of functions cache the inverse of a matrix (useful for large matrices). 


## Create a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## Create four functions that comprise the "list" describing input x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    ## Compile these four functions into a list and return
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute and return the inverse of x (via the "matrix" returned by makeCacheMatrix).

cacheSolve <- function(x, ...) {
    
    ## Retrieve inverse if it exists
    i <- x$getinv()                        
    if(!is.null(i)) {
        message("getting cached data")
        return(i)                           
    }
    
    ## Calculate and return inverse of matrix x
    mat <- x$get() 
    i <- solve(mat, ...)
    x$setinv(i)
    i

}
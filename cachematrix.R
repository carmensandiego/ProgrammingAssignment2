## MakeCacheMatrix and cacheSolve are used to efficiently return
## the inverse of a matrix and cache the output without recalculating in 
## the global environment

## MakeCacheMatrix returns a list of pointers to the global environment
## which contains functions that grant access to the environment in which
## the functions were defined demonstrating lexical scoping

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve returns the inverse of a matrix using the functions 
## contained within the list of pointers returned by makeCacheMatrix
## it checks to see if the inverse was already calculated and if so,
## it will return the cached inverse, rather than recalculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
                message("getting inverse of matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

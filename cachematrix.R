## Matrix inversion can be a costly computation so we will try and help that
## by caching the inverse of a matrix instead of repeating the computation

## This function creates a special "matrix" obj that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(mean) inv <<- mean
                getinv <- function() inv
                list(set = set, get = get,
                     setinv = setinv, getinv = getinv)
        
}


## This fn computes the inverse of the matrix above, if it has already been
## calculated, with no changes having been made, then the inverse will be
## retrieved from the cache

cacheSolve <- function(x, ...) {
                inv <- x$getinv()
                if(!is.null(inv)){
                        message("Getting cached data")
                        return(inv)
                }
                mtx <- x$get()
                inv <- solve(mtx, ...)
                x$setinv(inv)
                inv
}
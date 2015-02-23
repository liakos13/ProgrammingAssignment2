## Put comments here that give an overall description of what your
## functions do
##
## These 2 functions are able to cache the inverse of a matrix
##
## The makeCacheMatrix functioncreates a special matrix than caches its inverese

## Write a short comment describing this function
## The makeCacheMatrix function creates a special matrix than caches its inverese

 makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
 }


## Write a short comment describing this function
## The cacheSolve functions calculates the inverse of the special matrix we created
## using the makeCacheMatrix function previously

 cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
 }

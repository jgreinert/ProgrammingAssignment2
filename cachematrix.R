### Programming Assignment 2.  Jeff Greinert.
### Matrix inversion can be a costly computation.  These routines cache the
### inverse of a matrix rather than compute it repeatedly, and return it if
### available.  Otherwise, the inverse is computed, returned, and cached for
### subsequent use.

### Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


### Compute the inverse of the special "matrix" returned by makeCacheMatrix
### above. If the inverse has already been calculated (and the matrix has not
### changed), then retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}

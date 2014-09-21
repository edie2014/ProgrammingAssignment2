# Two functions that are used to create a special object
# that stores a matrix and cache its mean

# A function that creates a special "matrix", which is
# really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the invverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


# A function that calculates the inverse of the special
# "matrix" created with makeCacheMatrix. If the inverse
# has already been calculated, it gets it from the cache
# and skips the computations. Otherwise, it calculates
# the inverse of the data and sets the value of the inverse
# in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

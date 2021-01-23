## These functions are used to create a data structure that holds a matrix and
## its inverse so that the inverse does not need to be calculated every time it's
## needed.

## Creates a special matrix that can store its own inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Gets data from cache matrix created with makeCacheMatrix() and either retrieves
## its cached inverse if it exists, or calculates the inverse and caches it.
## An inverse can only be found for square matrices with a non-zero determinant,
## so an if statement checks for those attributes.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        if (nrow(data) == ncol(data) && det(data) != 0) {
                i <- solve(data, ...)
                x$setinverse(i)
                i
        }
        else {
                print("Cached matrix must be square and have a non-zero determinant.")
        }
}
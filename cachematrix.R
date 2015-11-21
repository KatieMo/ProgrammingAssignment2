## These functions calculate the inverse of a matrix and store that inverse
## in a cashe for future use.

## Creates a matrix object and stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## This function inverses the matrix "x". If the inverse has already
## been calculated it is retrieved from the cashe.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get()
        if(!is.null(m)) {
                message("getting cashed data")
                return(m)
        }
        data <- x$get()
        m <- solve(x, ...)
        x$setinverse(m)
        m
    
}

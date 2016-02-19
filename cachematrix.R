## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function takes a matrix argument and return a list with
## get and set matrix and get and set inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y  
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## It is to check if inverse has been ready. If so, get inverse from cache
## otherwise calculate inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        ## check if inversion is ready and return the data if so.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## otherwise, calculate inversion
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

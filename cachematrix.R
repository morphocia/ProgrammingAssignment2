## makeCacheMatrix:  
## This function creates a special "matrix" object that can cache its inverse.
## It is a list of 3 functions
## 1. get the value of the matrix
## 2. get the value of the inverse of the matrix
## 3. set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
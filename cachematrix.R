# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(m = matrix()) {
    minv <- NULL
    set <- function(n) {
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of the given "matrix"
## given matrix has to be created using makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
## It is assumed matrix is always invertible 
cacheSolve <- function(m, ...) {
    minv <- m$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- m$get()
    minv <- solve(data)
    m$setinverse(minv)
    minv
}
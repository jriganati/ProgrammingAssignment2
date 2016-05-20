# The following functions calculate and cache the inverse of a matrix.
# It is assumed that the matrix is invertible.
#    makeCacheMatrix: This function creates a special "matrix" object that 
#         can cache its inverse.
#    cacheSolve: This function computes the inverse of the special "matrix" 
#         created by makeCacheMatrix.  If the inverse has already been calculated, 
#         then the inverse is retrieved from the cache.

## makeCacheMatrix
## This function creates a special "matrix", which is actually a list containing
## the following functions:
##   set            set the value of the matrix
##   get            get the value of the matrix
##   setinverse     set (cache) the inverse of the matrix
##   getinverse     get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## cacheSolve
## This function calculates and returns the inverse of a special matrix created with
## the makeCacheMatrix() function.
## Before calculating the inverse, it uses the matrix's getinverse() function
## to see if the inverse was already calculated and cached.
##   If the inverse was cached (not NULL), it returns that cached value.
##   If the inverse was not cached (NULL), it calculates the inverse, caches the value
##        with the matrix's setinverse() function, and returns the calculated inverse.
cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached inverse")
          return(i)
     }
     
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}

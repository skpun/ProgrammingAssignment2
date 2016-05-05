# Matrix inversion is usually a costly computation.
# This is an example of caching the inverse of a matrix rather than computing it repeatedly. 
# The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set and get the value of the matrix
## 2. set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}


## Returns the inverse of the matrix.
## Checks if the inverse has already been computed. 
## If yes, get the result and skips the computation. 
## If no, compute the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {    
	i <- x$getinverse()
    if(!is.null(i)) {
        message("Retrieving matrix from cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving matrix from cache in the second run
## > cacheSolve(m)
## Retrieving matrix from cache
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
## As matrix inversion is the expensive calculation, it is wise to cache the
## inverse of the matrix if it is to be accessed repeatadely

## This function creates a special matrix object that can cache its inverse.
## It has 4 nested functions to get the matrix and it's inverse and also
## to set the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       get <- function() x
       set <- function(y) {
       	x <<- y
       	inv <<- NULL
       }
       getinv <- function() inv
       setinv <- function(inverse) inv <<- inverse
       list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## The following function calculates the inverse of the matrix. It first checks if
## the inverse is akready available in the cache. If yes, it gets the inverse 
## from the cache or it will calculate the inverse and return it. It also
## saves the inverse in the cache for future

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
        	print('getting inverse from cache')
        	return(inv)
        }	
        else {
        	data <- x$get()
        	inv = solve(data)
        	x$setinv(inv)
        	inv
        }	
}

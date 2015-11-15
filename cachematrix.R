## this function takes a matrix object as a parameter and returns a list of functions
## this function is used internally by the next function to implement caching of inverse matrix operation (in case matrix does not change)
## you are supposed to use these functions like this:
## assume x is a matrix object (which is invertable)
## mat <- makeCacheMatrix(x)
## cacheSolve(mat)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this function calculates the inverse of a matrix and stores it in an object
## next time this function is called with the same matrix, it does not recalculate the inveres. instead, it just returns the values from its 'cache'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if( !is.null(i) ) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

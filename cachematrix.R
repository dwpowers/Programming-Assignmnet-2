##These functions calculate the inverse matrix and stores it so that 
##it can be returned instead of having to do another calculation.

##This function creates a creates a special "matrix", which is really a 
##list containing a function to set and getthe value of the matrix and 
##its inverse.

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
##This function calculates the inverse of the matrix created in the previous
##function, if it hasn't already been calculated. If it has previously been
##calculated then it gets the inverse, but if not then it calculates the 
##inverse.

cacheSolve <- function(x, ...) { 
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
	  m <- solve(data, ...)
        x$setinverse(m)
        m
}
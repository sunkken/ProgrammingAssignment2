## Functions to create a matrix object with cachable inverse

## Creates "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	
	get <- function() x
	setinv <- function(inv) xinv <<- inv
	getinv <- function() xinv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes inverse if not cached, otherwise retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

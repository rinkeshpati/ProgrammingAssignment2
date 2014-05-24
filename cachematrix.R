## The following pair of functions enable us to cache the inverse of a given 
## matrix and save us from recomputing potentially time consuming inverse calculation 



## The first function, makeCacheMatrix creates the list containing functions to 
## set and get the values of the matrix and matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function checks the cache if the inverse of the matrix is already
## calculated. If so, it gets the value from the cache, otherwise computes
## the inverse of the matrix

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


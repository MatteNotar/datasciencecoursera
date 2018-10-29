
## makeCacheMatrix instantiates a new special matrix
## this matrix creates in its environment reference points to original matrix and its inverse
## the reference point to inverse is used as cache
## additionally, it creates 4 methods to get and set the matrix values and the inverse value
## returns the functions as part of a list

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setCache <- function(solve) inv <<- solve 
	getCache <- function() inv
	list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## cacheSolve tries first to retrieve the value of the inverse from cache
## if not present in cache, will calcualte inverse with solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getCache()
	if(!is.null(inv)) {
		message("getting the inverse matrix")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setCache(inv)
	inv
}

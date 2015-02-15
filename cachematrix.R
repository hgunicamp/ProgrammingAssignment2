## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	Set     <- function(y) {
		x       <<- y
		inverse <<- NULL
	}
	Get        <- function()    x
	SetInverse <- function(inv) inverse <<- inv
	GetInverse <- function()    inverse
	list("Get"        = Get,
	     "Set"        = Set,
	     "GetInverse" = GetInverse,
	     "SetInverse" = SetInverse )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	inverse <- x$GetInverse()
	if (!is.null(inverse)) {
		message("From Cache")
		return(inverse)
	}
	data <- x$Get()
	inverse <- solve(data)
	x$SetInverse(inverse)
	inverse
}

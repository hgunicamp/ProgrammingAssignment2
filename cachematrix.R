## makeCacheMatrix(x): function
## - Arg: 
##     x: matrix()
## - Use:
##     aMatrix <- makeCacheMatri(x)
## - Description:
##     Returns a list of functions to manipulate a square matrix internally
##     stored, and its inverse, created using the "x" argument.
##     The functions are:
##     - Get(): Returns the internally stored square matrix. 
##         - Use:
##             y <- aMatrix$Get()
##     - Set(x): Stores internally the matrix passed through
##             argument "x". It invalidates the value of the
##             inverse previously stored.
##         - Args:
##             x: matrix()
##         - Use:
##             aMatrix$Set(x)
##     - GetInverse(): Returns the value of the inverse matrix stored
##                   internally. If it was not calculated yet, resturns NULL.
##         - Use:
##             y <- aMatrix$GetInverse()
##     - SetInverse(x): Stores internally the matrix passed through
##                    argument "x" as the inverse of the previous stored
##                    matrix.
##         - Args:
##             x: A square matrix
##         - Use:
##             aMatrix$SetInverse(x)

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

## cacheSolve(x,...): function
## - Arg: 
##     x: makeCacheMatrix()
## - Use:
##     y <- cacheSolve(x,...)
## - Description:
##     Returns an internally stored inverse matrix stored into a list created
##     through makeCacheMatrix function. If the inverse was not calculated yet,
##     calculates its inverse, updates the list and returns the inverse
##     calculed.


cacheSolve <- function(x, ...) {
	inverse <- x$GetInverse()
	if (!is.null(inverse)) {
		# From Cache
		return(inverse)
	}
	data <- x$Get()
	inverse <- solve(data,...)
	x$SetInverse(inverse)
	inverse
}

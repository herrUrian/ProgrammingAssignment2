## The following two functions are used to create a special object that stores a matrix and caches its inverse.


## This function creates a list object that sets/gets the value of the matrix and sets/gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
	    x <<- y
	    inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function calculates the inverse of the special "matrix" created with the above function. If it has already been calculated (and cached), it skips the computation and returns the cached value. Otherwise calculates the inverse and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
			  message ("getting cached data")
			  return(inv)
	}
	mat <- x$get()
	inverse <- solve(mat, ...)
	x$setinv(inverse)
	inverse
}

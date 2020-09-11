
## The goal of these functions is to demonstrate scoping rules in R
## and to utilize the special assignment operator "<<-" to help cache
## values that can be picked up by another function if necessary


## This function creates an object storing a given matrix and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
	# initialize the inverse variable, starting as NULL
	inv <- NULL
	# function to set the matrix of the 'makeCacheMatrix' object
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	# function to retrieve the matrix of the 'makeCacheMatrix' object
	get_matrix <- function() x
	# function to set the inverse of the 'makeCacheMatrix' object
	set_inverse <- function(inverse) inv <<- inverse
	# function to retrieve the inverse of the 'makeCacheMatrix' object
	get_inverse <-function() inv
	# create the 'makeCacheMatrix' object as a list of functions
	list(set=set, get_matrix=get_matrix, set_inverse=set_inverse, get_inverse = get_inverse)
}

## This function computes the inverse of the object returned by makeCacheMatrix, unless
## it has already been calculated and cached.
cacheSolve <- function(x, ...) {
	# check to see if inverse is cached in the 'makeCacheMatrix' object and
	# return the inverse if it is
	inv <- x$get_inverse()
	if(!is.null(inv)) {
		message("retrieving cached inverse")
		return(inv)
	}
	# if inverse is not cached, get the matrix from the 'makeCachedMatrix' object
	mat <- x$get_matrix()
	# calculate the inverse, assuming matrix is invertible
	inv <- solve(mat)
	# store the inverse in the cache of the 'makeCachedMatrix' object
	x$set_inverse(inv)
	# return the inverse
	inv
}

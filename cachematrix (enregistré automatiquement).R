## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## @x: a square invertible matrix
        ## return: a list containing functions to
        ##        1. set the matrix
        ##        2. get the matrix
        ##        3. set the inverse
        ##        4. get the inverse
        ##  this list is used as the input to cacheSolve()
        ## `<<-` assign a value to an object in an environment 
         # different from the current environment. 


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv )
}


## Write a short comment describing this function
    ## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to keCacheMatrix()
    ## if the inverse has already been calculated
	## get it from the cache and skips the computation
	## otherwise, calculates the inverse 
	## sets the value of the inverse in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        m.data <- x$get()
        inv <- solve(m.data, ...)
        x$setinv(inv)
        inv
}

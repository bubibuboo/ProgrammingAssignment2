## The following pair of functions can cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# m will store the cached inverse matrix
	m <- NULL
	
	# Set the value of the matrix
	set <- function (y) {
		x <<- y
		m <<- NULL
	}

	# Get the value of the matrix 
	get <- function () x 

	# Set the value of the inverse
	setinverse <- function(inverse) m <<- inverse
	
	# Get the value the inverse
	getinverse <- function() m

	# Return the matrix 
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()

	# If the inverse has been calculated already, return it.
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	# If the inverse has not yet calculated, calculate it.
	data <- x$get()
	m <- solve(data, ...)

	# Cache the inverse
	x$setinverse(m)

	# Return it
	m
}

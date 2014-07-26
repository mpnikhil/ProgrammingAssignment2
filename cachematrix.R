## makeCacheMatrix returns a list of 
## functions to set, get, set inverse and get inverse of a matrix
## cacheSolve computes the inverse of the matrix from the list 
## and caches it and returns the cached value on subsequent calls


## Returns a list of functions to get and set a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function (inverse) m <<- inverse
	getInverse <- function() m
	list (set = set, get = get, 
	setInverse = setInverse,
	getInverse = getInverse)

}


## Compute the inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) {
        	message("getting cached inverse")
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

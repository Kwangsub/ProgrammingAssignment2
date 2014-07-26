## Assignment: Caching the Inverse of a Matrix
## It reduces computation cost for the inverse of a matrix using cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It contains functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not cahnged,
## it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

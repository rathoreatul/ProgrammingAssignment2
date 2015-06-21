## Comments here gives an overall description of what these functions do
## There are two functions below, they create matrix objects and computes and caches its inverse.  
## Since it may be costly event to compute each time it runs e.g. in a loop, the function caches and reuses it each time it runs in future. 
## This improves performance dramatically.

## Short comment describing this function
## This function creates list of matrix objects locally (using <<- assignment) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i_x <- NULL
	set <- function(y) {
		x <<- y
		i_x <<- NULL 
	}
	get <- function() x
	setinverse <- function(inverse) i_x <<- inverse 
	getinverse <- function() i_x 
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Short comment describing this function
## This function cacheSolve computes inverse of matrix returned by previous function (makeCacheMatirx). 
## It first checks if it is already in cache(from makeCacheMatrix function ran previously) then it gets it from cache 
## else it will calculate it fresh and will return results back.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i_x <- x$getinverse()
	if(!is.null(i_x)) {
		message("getting cached inversed matrix")
		return(i_x)
	}
	data <- x$get()
	i_x <- solve(data)
	x$setinverse(i_x)
	i_x
}


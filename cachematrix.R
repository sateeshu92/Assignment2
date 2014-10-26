## This function creates a special "matrix" object that can cache its inverse.
cachematrix <- function(x = matrix()) {
	m <- NULL  ## Initialize local variable
	## Function to cache matrix passed from the command line
	set <- function(y) {
		cache_x <<- y ## Put the initial matrix from the command line into cache as cache_x
		cache_m <<- NULL ## Initialize caache_m to NULL so we can tell when cacheSolve has run at least once.
	}

	get <- function() cache_x 
	
	set_cache_m <- function(local_m) 
		cache_m <<- local_m 

	get_cache_m <- function() cache_m 	
	list(set = set, get = get,
	set_cache_m = set_cache_m,
	get_cache_m = get_cache_m)

}



## This function computes the inverse of the special "matrix" returned 
## by cachematrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	local_m<- x$get_cache_m() ## Get value from Cache

	if(!is.null(local_m)) { ## Check to see if m is NULL.
		message("getting cached data") ## If m is not NULL, return the value of m 
		return(local_m)
	} 

	startingmatrix <- x$get() 
	endingmatrix <- solve(startingmatrix) 
	x$set_cache_m(endingmatrix) 
	endingmatrix ## Return matrix
}
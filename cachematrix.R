## Function makeCacheMatrix will create a matrix 
## 	that can cache its inverse
## If the inverse has already been calculated and 
## 	the matrix has not changed then Function cacheSolve 
## 	will retrieve the inverse from the cache


## The following function makeCacheMatrix will:
## 	1 - set the value of the matrix
## 	2 - get the value of the matrix
## 	3 - set the value of the inverse
## 	4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inverse <<- solve
	getinv <- function() inverse
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following fucntion cacheSolve calculate the inverse
## 	of the matrix created by the above function
## It first checks to see if the inverse has been calculated
## If so, it will get the inverse from the cache and 
##	skips the computation
## Otherwise, it calculates the inverse of the matrix and 
##	sets the inverse in the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinv()
	if(!is.null(inverse)) {
		message("getting cached inverse")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data,...)
	x$setinv(inverse)
	inverse
}

## These functions do exactly what the example functions did : if ##cacheSolve is called on a object that is cached, it will retrieve ##the cached data instead of recalculating the inverse.

##Given a matrix, this function makes a special list that contains functions to set, get, setinv and getinv.

makeCacheMatrix <- function(x= matrix()){
	inv <- NULL
	set <- function (y){
		x<<- y
		inv <<- NULL
	}
	
	get <- function() x
	setinv <- function (inverse) inv <<- inverse
	getinv <- function () inv
	list( set = set, get = get, setinv = setinv, getinv= getinv)
}

##This function checks to see if the inverse has already been cached, and if so retrieves the cached value. If not, it calculates the inverse of the matrix. 

cacheSolve <- function(x, ...){
	## Return a matrix that is the inverse of 'x'
	inv = x$getinv()
	if (!is.null(inv)){
		message ("getting cached data")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	return (inv)
}

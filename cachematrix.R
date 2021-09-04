## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL #initilizing inverse to null
	set <- function(y) {
		x<<-y
		inv <<- NULL
		
	}
	get <- function() {x} # function to get matrix
	set Inverse <- function(inverse){inv <<- inverse}
	get Inverse <- function () {inv} #function to obtain inverse of matrix
	list(set = set, get = get,setInverse = setInverse, getInverse = getInverse )
	
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) { #gets cache data
	inv <- x$getInverse()
	if(!is.null(inv)){ #checking whether inverse is null
		message("getting cached data")
		return(inv) # returns inverse value
	}
	mat <- x$get()
	inv <- solve(mat, ...) # calculates inverse value
	x$setInverse(inv)
	inv #returns a matrix that is the inverse of 'x'
 }

### This file contains two functions 
# makeCacheMatrix 
# cacheSolve 

# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly. These functions are used to cache the inverse of a matrix. 

# makeCacheMatrix creates a special "matrix" object 
# that can cache its inverse. 

# cacheSolve computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. 
### This file contains two functions 
# makeCacheMatrix 
# cacheSolve 

# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly. These functions are used to cache the inverse of a matrix. 

# makeCacheMatrix creates a special "matrix" object 
# that can cache its inverse. 

# cacheSolve computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve retrieves the inverse from the cache. 


## makeCacheMatrix

# makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {	
	inv <- NULL
	set <- function(y) {
					x <<- y
					inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set,  
		get = get,
		setinv = setinv,
		getinv = getinv)
	}

## end of function makeCacheMatrix


## function cacheSolve 

# This function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse and 
# sets the value in the cache via setinv function.

# This function assumes that the matrix is always invertible. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

## end of function cacheSolve

### end of file
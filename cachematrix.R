# cache the inverse of a matrix: take a matrix as input: 
# 1) create a special matrix that can cache its inverse 
# 2) compute the inverse of the matrix or retrieve it from cache if already calculated 



# makeCacheMatrix: creates a special matrix which is a list containing a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of the inverse 
# 4. get the value of the inverse

makeCacheMatrix <- function(x=matrix()){
		inverse <-NULL
		set <- function(y){
			x<<-y
			inverse<<-NULL
		}
		get <-function() x
		setsolve <-function(solve) inverse <<- solve
		getsolve <-function() inverse
		list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# cacheSolve: calculating the inverse of the special matrix created with makeCacheMatrix. If already calculated, it gets the inverse from cache and skips calculation. 

# this function assumes that the matrix is always invertible

cacheSolve <- function(x=matrix(), ...) {
		inverse <- x$getsolve()
		if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
         	}
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}


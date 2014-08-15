## makeCacheMatrix(x) creates a list of functions that 
## stores the matrix x, and caches its inverse matrix 
## when an appropriate function is called
##
## x is a inversable matrix
## 
## the returned list contians following functions:
## set the value of the matrix
## get the value of the matrix
## setinv caches the inverse matrix
## getinv get the value of the inverse matrix if cached, 
## otherwise a NULL 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_matrix) inv <<- inv_matrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve(x, ...) attempts to retrive and return the 
## inverse matrix the cachable matrix x. If cached, it returns
## the inverse matrix and print the message "getting cached 
## data", otherwise solve for the inverse matrix and then
## caches and return it

## x is the cachable matrix produced from makeCacheMatrix(matrix)
## ... are the configuration of solve(matrix, ...)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_matrix <- x$getinv()
	if(!is.null(inv_matrix)){
		message("getting cached data")
		return(inv_matrix)
	}
	matrix <- x$get()
	inv_matrix <- solve(matrix)
	x$setinv(inv_matrix)
	inv_matrix
}
## makeCacheMatrix is a function which gives a list containing
## the functions to: 

## 1. set a matrix ($
## 2. get the matrix
## 3. set the invert of the matrix
## 4. get the invert of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL	
        set <- function(y, ncol, nrow) {
                x <<- matrix(y, ncol, nrow)
                i <<- NULL
        }
        get <- function() x
        setInvert <- function(Inv, ncol=1, nrow=1) {
                i <<- matrix(Inv, ncol, nrow)
                }
        getInvert <- function() i
        list(set = set, get = get, 
                setInvert = setInvert, 
                getInvert = getInvert)
}


## cacheSolve is a function that return a matrix that is the inverse 
## of matrix x. It first checks if the inverse matrix is already cached, 
## and if it exists the computation is skipped and the cached inverted
## matrix is returned.

        cacheSolve <- function(x, ...) {
        i <- x$getInvert()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }	
        data <- x$get()	
        i <- solve(data)
        x$setInvert(i, ncol(i), nrow(i))
        i	
}
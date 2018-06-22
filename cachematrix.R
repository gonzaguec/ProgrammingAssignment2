## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## In order to do this function, I took the original function given
## in the README.md file and work on it with inverse, set_inv and 
## get_inv. 

makeCacheMatrix <- function (x = matrix()) { ##the difference here is that x is a matrix rather than a numeric vector
        m <- NULL 
        set <- function (y) { ## this is the inverse function, which takes 'y' that in this context is 'x'
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) m <<- inverse  ## this line generates the inverse of the matrix
        get_inv <- function() m  ## this line get inverse of the matrix
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv) ## this line list functions
}


## Write a short comment describing this function
## As with the function listed above, I took the README.md example, and worked it. 
## define functions from the makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$get_inv()  ## this line use the get_inv function already defined
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ## this is in the case the cache is not empty
        } else { ## and if the cache is empty, it resolves the operation
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(m)
        m
        }
}

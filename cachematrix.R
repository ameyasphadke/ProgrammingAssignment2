## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns special matrix, which overrides existing matrix behavior
## The new matrix has setter/getters for matrix and for inverse of the matrix - via set/get
## solve methods
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        matrix(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

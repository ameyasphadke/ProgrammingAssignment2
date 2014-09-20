## This function returns list of funtions, which "overrides" existing matrix behavior
## The new matrix has setter/getters for matrix and for inverse of the matrix - via set/get
## solve methods
makeCacheMatrix <- function(x = matrix(numeric())) {
        m <- NULL
        ## sets new matrix and nullifies its inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## gets
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

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

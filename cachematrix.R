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
        ## returns the original matrix i.e. matrix x
        get <- function() x
        
        ## "Wraper" method for solve. Returns inverse if available in cache or calls 
        ## solve method to compute inverse
        setsolve <- function(solve) m <<- solve
        
        ##  Returns inverse of the matrix i.e. matrix m
        getsolve <- function() m
        ## Finally rwturn list of methods
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}

## This function calls required methos to retrieve inverse of the matrix 
## from cache - if available or calls solve and sets the result in cache for future use
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

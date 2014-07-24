## Purpose: Functions to implement caching the inverse of a matrix.
## Usage:
##   1) The 'makeCacheMatrix()' should be run before 'cacheSolve()'
## Reference: https://class.coursera.org/rprog-005/human_grading/view/courses/972576/assessments/3/submissions

## Name: makeCacheMatrix
## Purpose: Create a 'special' matrix in the environment of the call &
##   functions for accessing & modifying that matrix.
## Inputs:  matrix
## Outputs: matrix x in calling enviornment
##          variable m in calling environment
## Usage:
##   1) x$get():       return the matrix 'x'
##   2) x$set():       set the value for the matrix 'x'
##   3) x$getMatrix(): return the matrix 'x'
##   4) x$setMatrix(): set the value for the matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

## Name: cacheSolve
## Purpose: If the inverse of the matrix 'x' has not been calculated, then calculate it, cache it, and return it.
##   If the inverse of 'x' _has_ been calculated, then return the cached value.
## Inputs:
##   1) Matrix created using 'makeCacheMatrix'
##   2) Parameters to pass to base::solve() function.  Use ?solve to see these parameters.
## Outputs: Inverse of input matrix.
## Usage:
##   1) cacheSolve(x): returns inverse of matrix x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

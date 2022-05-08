## The following is a pair of functions that cache and compute the
## inverse of a matrix.

#' example usage
#' > source("cachematrix.R")
#' > x <- matrix(c(1, 2, 0, -2, 5, 9, 0, 3, -3), 3, 3)
#' > x
#'      [,1] [,2] [,3]
#' [1,]    1   -2    0
#' [2,]    2    5    3
#' [3,]    0    9   -3
#' > solve(x)
#'            [,1]       [,2]        [,3]
#' [1,]  0.7777778 0.11111111  0.11111111
#' [2,] -0.1111111 0.05555556  0.05555556
#' [3,] -0.3333333 0.16666667 -0.16666667
#' > xm <- makeCacheMatrix(x)
#' > cacheSolve(xm)
#'            [,1]       [,2]        [,3]
#' [1,]  0.7777778 0.11111111  0.11111111
#' [2,] -0.1111111 0.05555556  0.05555556
#' [3,] -0.3333333 0.16666667 -0.16666667
#' > cacheSolve(xm)
#' getting cached data
#'            [,1]       [,2]        [,3]
#' [1,]  0.7777778 0.11111111  0.11111111
#' [2,] -0.1111111 0.05555556  0.05555556
#' [3,] -0.3333333 0.16666667 -0.16666667

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

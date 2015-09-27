## I am trying to optimize compute and time intensive operation like Inverse of a Matrix
## This will also be my solution for Programming Assinment 2

## The makeCacheMatrix is a list of functions that will store 4 functions
## When invoked for the first time, the inverse matrix value (m) is set to NULL
## since, we have not comouted anything. 
## The set function is the first funtion and will update the value of the global
## matrix since, a new matrix has been sent as an input. The inverse of this new
## matrix has not been computed and hence, m is set to NULL.
## The next function is get which basically returns the value of x matrix.
## The setinvmatix is the third functon which caches the value of inverse matrix.
## The getinvmatri
## x is like the get finction and returns the inverse matrix that
## is stored.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve is a function which will create the inverse of the matrix. 
## However, since I am optimizing this function, it will only compute the
## Inverse Matrix function when it has not seen the object earlier.
## Very cool! :)

x <- makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return (m)
        }
        data <- x$get ()
        m <- solve(data)
        x$setsolve(m)
        m
}
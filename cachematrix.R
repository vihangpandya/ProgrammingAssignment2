## Trying to reduce the time for calculating the inverse of a matrix. Thought is that it is
## better to save the object and the inverse after the computing the inverse of a matrix. 
## When the same matrix is provided as an input the inverse of the matrix is not calculated
## but provided from the saved copy. The saved copy is referred to as the ""cached copy."

## Basically there are 2 functions in this R script. makeCacheMatrix & cacheSolve.
## makeCacheMatrix is a list of 4 functions.
## get returns the values in new matrix
## set caches the matrix if it has not been cached earlier
## set also assigns NULL to "inv" which is checked to confirm if the inverse of the matrix
## exists and is not NULL.
## getinvmatrix is like get and returns the inverse matrix
## setinvmatrix is like set and stores the inv matrix in the cache.

# I have tested this script using the test scritp provided b Shamik Mitra and the output
# is as shown below:

#> test()
# Enter duration parameter. The higher the number, the longer it takes. Duration: 2000
# Creating a random matrix and its inverse to test the program ...
# Done. 
# 
# 
# Calling the makeCacheMatrix function ...
# Done. 
# 
# 
# Calling the cacheSolve function for the first time ...
# Done. 
# 
# 
# Calling the cacheSolve function for the second time ...
# getting cached data
# Done. 
# 
# 
# The function has returned a valid matrix inverse
# The first run took   12.399  seconds
# The second run took  0.000  seconds

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinvmatrix <- function(solve) inv <<- solve
        getinvmatrix <- function() inv
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}

## cacheSolve is a function which will create the inverse of the matrix. 
## However, since I am optimizing this function, it will only compute the
## Inverse Matrix function when it has not seen the object earlier.
## It does this by checking to see if the valus for "inv" is NULL
## Very cool! :)

x <- makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinvmatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        data <- x$get ()
        inv <- solve(data)
        x$setinvmatrix(inv)
        inv
}
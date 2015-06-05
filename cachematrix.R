## cachematrix.R: Contains two function that work together to speed up processing by caching the
##                the inverse of the matrix (rather than recalculating it every time).

## Notes: 
##       -As per instructions, these functions assume the matrix is ALWAYS invertible
##       -Written for Coursera Data Science R Programming Class: Assignment 2.
##       -Last updated: 06.04.15



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
    
    makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
            # Sets value of x (even outside of function) to y and inv to NULL
            x <<- y
            inv <<- NULL
        }
        get = function() {x}
            #get = a function that will output x
        setinverse = function(inverse) inv <<- inverse
            #setinverse = a function that will set the inverse value in the cache
        getinverse = function() inv
            #getinverse = a function that will output the inverse value
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
            #Sets up the cache matrix
    }

    

## cacheSolve: This function computes the inverse of the special "matrix returned by makeCacheMatrix
##             above. If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.

    cacheSolve <- function(x, ...) {
        inv = x$getinv()
            #inv is saved as the output of getinv factor in x
        if(!is.null(inv)) {
            #if inv isn't null, then just retrieves cached value & exits functions instead of recalculating
            message("getting cached data")
            return(inv)
        }
        data = x$get()
        inv = solve(data, ...)
        x$setinverse(inv)
        #If inv is null (not cached), pulls the matrix and calculates the inverse, saving it to back to x
        inv
    }








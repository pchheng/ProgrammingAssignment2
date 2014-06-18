#####################################
## R-Programming, Assignment # 2
#####################################

# This assignment # 2 requires writing 2 functions for caching the inverse:

## makeCacheMatrix: This function will create a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {         ## define a function, makeCacheMatrix, that creats a matrix. It contains a list of 4 founctions below
        m <- NULL                                   ## 
        set <- function(y) {                        ## define a function, set, that returns the value of y
                x <<- y
                m <<- NULL
        }
        get <- function() x                         ## define a function, get, that returns the value of x
        setmatrix <- function(solve) m <<- solve    ## define a function, setmatrix, that set the value of inverse matrix
        getmatrix <- function() m                   ## define a function, getmatrix, that returns the value of inverse matrix
        list(set = set, get = get,                  ## create a list with variable names 'set', 'get', 'setmatrix', and 'getmatrix' that are equal to the functions defined above    
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve: will compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve  <- function(x=matrix(), ...) {           ## define a function, cacheSolve, that computes, caches and returns inverse matrix
        m <- x$getmatrix()                           ## Returns original inverse matrix,
        if(!is.null(m)) {                            ## if it exists then return it
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()                            ## use x's get function to retrieve value of inverse matrix
        m <- solve(matrix, ...)                      ## if not exist, computes inverse of the matrix
        x$setmatrix(m)                               ## and set the value of inverse matrix via setmatrix 
        m                                             
}

## Note that the above function was based on example provided in the assignment 2.




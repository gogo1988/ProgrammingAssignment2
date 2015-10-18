## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

## The "makeCacheMatrix" function  create a special "Matrix", which is really a list 
## containinga function to 1) set the value of the Matrix, 2) get the value of the 
## matrix, 3)set the inverse of the matrix 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  # x is the input matrix that needs to be caluculated for the inverse
        i <- NULL
        set <- function(y) {  
                x <<- y        # set a matrix as the input
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse    # arbitrarily set the inverse of a matrix to the variable 'i'
        getInverse <- function() i
        list(set = set, get = get,   # construct a list of four functions for the object
             setInverse = setInverse,
             getInverse = getInverse)
}


## "cacheSolve" function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {    #'x' is the object produced by 'makeCacheMatrix' function
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached the calculated inverse")
                return(i)  # 'i' already contains a inversed matrix set by 'setInverse' function
        }
        data <- x$get()
        i <- solve(data, ...)  # calculate the inverse for the matrix contained in the object produced by 'makeCacheMatrix'
        x$setInverse(i)
        i     # 'i' stores the inverse of the matrix
}

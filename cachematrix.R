## Functions that cache the inverse of a matrix

## Makes a matrix object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Inverse property
        i <- NULL
        
        ## Set matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }

        ## Get the matrix
        get <- function() {
                ## Return the matrix
                m
        }

        ## Set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }

        ## Get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute for the inverse of the matrix from makeCacheMatrix
## If the inverse has already been computed, cacheSolve returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()

        ## Just return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get matrix
        data <- x$get()
        
        ## Solve for inverse via matrix multiplication
        m <- solve(data) %*% data
        
        ## Set inverse
        x$setInverse(m)
        
        ## Return matrix
        m
}

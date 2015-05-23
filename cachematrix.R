## The functions in this file are designed to make matrix inversion
## calculations more computationally efficient by caching the matrix inverse
## along with the matrix itself.  Unless the value of the matrix changes,
## the inverse is simply returned as a value instead of running the
## computationally intensive "solve" function.

## The makeCacheMatrix function takes a matrix x and creates a special
## "matrix" that contains the matrix itself, as well as being able to
## cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {

        # When instantiating the CacheMatrix, set the inverse to NULL.
        inverse <- NULL
        
        # Define the set function to set the value of x.
        set <- function( matrixValue )
        {
                # Setting a new matrix should also clear any cached inverse.
                x <<- matrixValue
                inverse <<- NULL
        }
        
        # Define the get function to return the value of x.
        get <- function ()
        {
              return(x)
        }
        
        # Define the setInverse function to compute the inverse of x.
        setInverse <- function(newInverse)
        {
              inverse <<- newInverse
        }
        
        # Define the getInverse function to return the inverse of x.
        getInverse <- function() 
        {
              return(inverse)
        }
        
        # A list of the four subfunctions within makeCacheMatrix.
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
  
}


## The cacheSolve function takes a matrix x and returns a matrix that is the
## inverse of x.  It is more potentially more computationally efficient than
## the standard solve function, since it only calls the solve function if
## the inverse of the matrix is not currently known.  Otherwise, it returns
## the cached value of the inverse matrix.
cacheSolve <- function(x, ...) {
        
        # Retrieve the inverse using the getInverse function.
        inverse <- x$getInverse()

        # Check to see if the inverse is not defined.
        if(is.null(inverse))
        {
                # Copy the data to a new matrix and invert it.
                newMatrix <- x$get()
                newInverse <- solve(newMatrix)
                
                # Set and return the matrix inverse.
                x$setInverse(newInverse)
                return(newInverse)
        }
        
        # The inverse was previously cached, so return the cached value.
        message("Returning cached inverse...")
        return(inverse)
        
}

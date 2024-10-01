## creates a special "matrix" object that caches its inverse. returns a list containing functions to get, set, and get inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse to NULL
    
    # Function to set the matrix value
    set <- function(y) {
        x <<- y    # Assign y to x in the parent environment
        inv <<- NULL # Reset inv to NULL, since the matrix has changed
    }
    
    # Function to get the matrix value
    get <- function() x
    
    # Function to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## computes the inverse of the matrix output of makeCacheMatrix. if it already exists, it retrieves it and if it doesn't, inverse is calculated and cached.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Retrieve the cached inverse, if it exists
    
    # Check if the inverse is already cached
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)  # Return the cached inverse
    }
    
    # If the inverse is not cached, calculate it
    mat <- x$get()       # Get the matrix data
    inv <- solve(mat, ...)  # Compute the inverse using solve()
    
    x$setInverse(inv)    # Cache the inverse
    inv  # Return the inverse
}

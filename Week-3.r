# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    set <- function(y) {
        x <<- y  # Set the value of the matrix
        inv <<- NULL  # Reset the cached inverse
    }
    get <- function() x  # Get the value of the matrix
    setInverse <- function(inverse) inv <<- inverse  # Set the cached inverse
    getInverse <- function() inv  # Get the cached inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Get the cached inverse
    if (!is.null(inv)) {  # Check if the inverse is already calculated
        message("getting cached data")
        return(inv)  # Return the cached inverse
    }
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the computed inverse
    inv  # Return the inverse
}

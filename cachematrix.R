# This function will create a list allowing to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inverted_matrix <- NULL
    # Setting our matrix in the cache
    set <- function(y) {
        x <<- y
        inverted_matrix <<- NULL
    }
    get <- function() x
    # Setting the Inverse of the matrix in the cache 
    # Later it can be retreived from there
    setInverse <- function(inverse) inverted_matrix <<- inverse
    getInverse <- function() inverted_matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This Function will get the Inverse a matrix x from the cache if it was calculated before
## In case the inverse was never calculated, the inverse will be calculated and returned
cacheSolve <- function(x, ...) {
    # set the inverted_matrix variable to the cached value of the inverse of the matrix
    inverted_matrix <- x$getInverse()
    # Verify if the inerse of that matrix was ever calculated
    # If it was, we'll get in that if block and return the inverse from the cache
    if(!is.null(inverted_matrix)) {
        message("Getting the inverse of the matrix from the cached data")
        # The line below will be the last line executed in our function
        return(inverted_matrix)
    }
    # In case the matrix inverse was never calculated, first get the matrix
    message("The inverse of that matrix was never calculated, I will do that now and cache it!")
    data <- x$get()
    # Calculate the inverse of the matrix
    # Note that we assume the matrix supplied is invertible. We'll not check anything! 
    inverted_matrix <- solve(data, ...)
    # Set the inverse of our matrix to be cached
    # We will not calculate it anymore on a second funtion call!
    x$setInverse(inverted_matrix)
    inverted_matrix
}

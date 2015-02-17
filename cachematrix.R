## For this assignment, assume that the matrix supplied is always invertible.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # The cache starts out NULL
  cache <- NULL
  
  # Set the matrix
  set <- function(newMatrix) {
    x <<- newMatrix
    # Flush the cache because we have a new value
    cache <<- NULL
  }
  
  # Return the matrix
  get <- function() x
  
  # Set the inverse of the matrix to the cache. This code doesn't create the inverse
  setInverse <- function(inverse) cache <<- inverse
  
  # Get the inverse from the cach
  getInverse <- function() cache
  
  # List out the methods in the function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    # Return the inverse if it's already cached and exit the function
    return(m)
  }
  # No inverse? Get the original matrix
  mat <- x$get()
  # Get the inverse of the matrix
  m <- solve(mat)
  # Store the inverse
  x$setInverse(m)
  # Return the inverse
  m
}

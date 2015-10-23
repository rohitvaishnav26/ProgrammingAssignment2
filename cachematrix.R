## The following two functions accepts and input matrix, calculates the inverse
## and then caches the value so that it need not be re-calculated for the same
## matrix.
## 1. makeCacheMatrix
## 2. cacheSolve


## makeCacheMatrix - This function creates a matrix (invertible) .
## It also acts as a cache/storage for the inverse of the matrix so that 
## it can be used when required, hence preventing recalculation.

makeCacheMatrix <- function(x = matrix()) {
  val1 <- NULL
  # Matrix Setter function
  set <- function(y = matrix()) {
    # Check to reset the inverse only if the matrix is different/new
    if((dim(x) != dim(y)) || (x != y)){
      # Set x to the new matrix that is made available in the parent environment
      x <<- y
      # Reset the inverse for the new matrix (parent environment)
      val1 <<- NULL
    }
  }
  # Matrix getter function
  get <- function() x
  # Matrix inverse setter function - Called from cacheSolve
  setinv <- function(inv) val1 <<- inv
  #Matrix inverse getter function
  getinv <- function() val1
  # Create a list for setter and getter functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the matrix only if it is not 
## already existing in the cache. Once it is calculated, the inverse is cached.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  # Check to see if inverse already exists
  if(!is.null(m)) {
    message("Retrieving cached data")
    return(m)
  }
  # Get the matrix
  data <- x$get()
  # Compute the Inverse of the matrix
  m <- solve(data, ...)
  # Set the Inverse
  x$setinv(m)
  # Return the inverse
  m
}

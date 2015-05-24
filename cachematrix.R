# Instead of solving matrix inversion over and over from scratch 
# it's more efficient to cache the inverse in memory which is what 
# the makeCacheMatrix() and cacheSolve() functions will perform


# makeCacheMatrix() will set & get the value of the matrix and
# then set & get the value of the inverse of the matrix
# It will return the list of the functions

makeCacheMatrix <- function(x = matrix()) {
  
  # Init invrs & y
  invrs <- NULL
  y <- NULL
  
  # Set the matrix
  setMatrix <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  
  # Get matrix
  getMatrix <- function() x
  
  # Set the inverse of the matrix
  setInvMatrix <- function(inverse) invrs <<- inverse
  
  # Get the inverse of the matrix
  getInvMatrix <- function() invrs
  
  # Return the list of matrix & inverse matrix functions
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}


## Gets the inverse matrix and if not already calculated returns the inverted matrix
## If it has already been derived then it will return the cached matrix.
cacheSolve <- function(x, ...) {
  
  # Get inverse matrix
  invrs <- x$getInvMatrix()
  
  # If matrix was already calculated then return cached matrix
  if(!is.null(invrs)) {
    message("retrieving cached matrix.")
    return(invrs)
  }
  
  # Get matrix
  matrix <- x$getMatrix()
  
  # Init the inverse
  invrs <- solve(matrix, ...)
  
  # Set the inverse matrix
  x$setInvMatrix(invrs)
  
  # Return inverse
  invrs
}

# These functions will allow a matrix to be stored, as well as its inverse.  The benefit to this is that a
# matrix will only have to have its inverse calculated once.  Once the inverse is calculated, it is stored
# in the object that also holds the matrix.  This will save on computation time if the inverse for the matrix
# is needed many times throughout the execution of whatever program is utilizing these functions.


# The makeCacheMatrix function has a matrix as an input and creates an object that contains information about 
# the matrix, and functions to get or set the matrix, as well as getting or setting the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  # Initially sets the inverse to NULL
  inv <- NULL
  
  # set function allows you to set the matrix
  set <- function(newMat) {
    x <<- newMat
    inv <<- NULL
  }
  
  # get function returns the matrix
  get <- function() x
  
  # setInverse function sets the inverse of the matrix into the variable "inv"
  setInverse <- function(inverse) inv <<- inverse
  
  # getInverse function returns the inverse of the matrix
  getInverse <- function() inv
  
  # list function shows the available functions you can call within the object
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The cacheInverse function has a special matrix object as an input (created from makeCacheMatrix function).  
# It outputs the inverse of the matrix.  However, the interesting part about the function is how it returns 
# the inverse.  When the inverse is computed for the first time, it is stored into the special matrix object
# as well.  On subsequent "calculations" of the inverse, the function merely grabs the already calculated
# inverse from the special matrix object.  It will only ever calculate the matrix (using the solve() function)
# once.
cacheSolve <- function(x, ...) {
  # Stores the value of the inverse from the special matrix object.
  inv <- x$getInverse()
  
  # Checks the inverse that was just stored in the variable "inv."  If it is not null, that means the inverse
  # has been calculated before, and it will just be returned as the inverse.
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  
  # The inverse did not exist in the special matrix object.  The function will now calculate the inverse of
  # the matrix using the solve() function, set the inverse into the special matrix object (so it does not need
  # to be calculated next time), and returns the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

#
#makeCacheMatrix - this function will take in a matrix and store for 
#                  calculating the inverse.  The inverse will be cached
#                  after the initial setting.
#
makeCacheMatrix <- function(x = matrix()) {
  data <- NULL
  
  # Store the raw matrix
  set <- function(theMatrix) {
    #message("makeCacheMatrix.set")
    x <<- theMatrix
    data <<- NULL
  }
  
  # Return the raw matrix
  get <- function() {
    #message("makeCacheMatrix.get")
    x
  }
  
  # Calculate the inverse of the matrix
  setInverse <- function() {
    #message("makeCacheMatrix.setInverse")
    data <<- solve(x)
  }
  
  # Return the inverse of the matrix.  If not cached, cache it
  # for future calls
  getInverse <- function() {
    #message("makeCacheMatrix.getInverse")
    if (is.null(data)) {
      setInverse()
    }
    data
  }
  
  # Store the function operations
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#
# This function will return the inverse of a matrix using
# the created object from makeCacheMatrix
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  value <- x$getInverse()
  
  ## if inverse matrix has not been calculated, calculate and store in cache
  if (is.null(value)) {
    #message("cacheSolve SetValue")
    x$setInverse()
    myval <- x$getInverse()
    #message("the value is ")
    return(value)
  }
  
  ## Return the value
  value
}

mv <- makeCacheMatrix()
mv$set(matrix(1:4,2))
mv$get()
#mv$getInverse()
cacheSolve(mv)

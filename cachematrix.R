## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  data <- NULL
  set <- function(theMatrix) {
    message("makeCacheMatrix.set")
    x <<- theMatrix
    data <<- NULL
  }
  get <- function() {
    message("makeCacheMatrix.get")
    x
  }
  setInverse <- function() {
    message("makeCacheMatrix.setInverse")
    data <<- solve(x)
  }
  getInverse <- function() {
    message("makeCacheMatrix.getInverse")
    if (is.null(data)) {
      setInverse()
    }
    data
  }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  value <- x$getInverse()
  if (is.null(value)) {
    message("cacheSolve SetValue")
    x$setInverse()
    myval <- x$getInverse()
    message("the value is ")
    return(value)
  }
  value
}

mv <- makeCacheMatrix()
mv$set(matrix(1:4,2))
mv$get()
mv$getInverse()
cacheSolve(mv)

## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setI <- function(inverse) I <<- inverse
  getI <- function() I
  list(set = set,
       get = get,
       setI = setI,
       getI = getI)
}


## Claculate the inverse of makeCacheMatrix. If the inverse was previously 
## calculated this function gets the inverse from the cahce.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getI()
  if (!is.null(I)){
    message("getting data from cache")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setI(I)
  I
}

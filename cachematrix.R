
## This function creates a special "matrix" object that can cache its inverse
## It contains a set of function    
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse



makeCacheMatrix <- function(x = matrix()) {
  rev <- NULL
  set <- function(y) {
    x <<- y
    rev <<- NULL
  }
  get <- function() x
  setrev <- function(reverse) rev <<- reverse
  getrev <- function() rev
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}


## The following function calculates the inverse of the special "Marix" created with 
## makeCacheMatrix() function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
## cache via the setrev() function.

cacheSolve <- function(x, ...) {

  
  reverse <- x$getrev()
  if(!is.null(reverse)) {
    message("getting cached data")
    return(reverse)
  }
  data <- x$get()
  reverse <- solve(data)
  x$setrev(reverse)
  reverse
}

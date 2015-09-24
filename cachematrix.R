## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
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

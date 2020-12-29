## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setrev <- function(solve) r <<- solve
  getrev <- function() r
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  r <- x$getrev()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setrev(r)
  r
  
}

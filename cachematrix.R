## Coursera R Programming course Assignment 2
## practice for using cached data and making functions that returns functions
## The idea is to make calculations for a matrix that are cached in a special 
## Environment. This way, when we create a big Matrix we can cache its Reverse 
## so that it isn't necessary to repeat the calculation each time we need it, so
## saving that calculation time, that could be relatively long when working with
## many big objects.

## First, we create "makeCacheMatrix()" function, that gets a Matrix as input, and
## creates a list that holds both the the functions needed to set & get the 
## cached Matrix Value and its Reverse
## Returned functions,
##  function "x$get()" returns the value of the Matrix taken from cache 
##  function "x$set(x)" sets the value of the Matrix and stores it into special
##   variable 'x', and at the same time invalidates the cached Reverse value, 
##   stored in "r" special variable  
##  function "x$setrev()" gets a calculation over the Matrix 
##   (the Reverse in this example, but could be any other) and stores
##   it into special variable 'r'
##   the calculation will be of type matrix, in this example 
##   but actually it could of any type
##  function "x$getrev()" returns the value of the Matrix Reverse from cache "r"
##   variable

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


## "cacheSolve(x,...)" function is used to actually store the Reverse of "x" 
##  related Matrix 
##  it uses x$getrev() function to get the current Matrix Reverse value if it 
##  exists.If it doesn't, it performs the calculation and calls the x$setrev(r)
##  function to set the cached value equal to the calculated value
## We are not actually checking the reversibility of the Matrix. We assume it is
##  reversible. If it isn't, this function will return an error

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

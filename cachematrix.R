## cachematrix.R   MNLOUIE   JULY 21 2014  
## Two functions allow for input matrix to be inversed using the 'solve' function
## and saves the inversed matrix to the cache so that command doesn't need to be 
## excuted over - instead the previously computed value is just returned. 

## Function to provide, access, and store matrix 'x' and inverse of x after 
## cacheSolve is used.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x  #  function acts on input x, same as function() {x}
  setinverse <- function(solve) m <<- solve  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function takes a matrix that was defined using makeCacheMatrix and finds 
## the inverse - writes as x$setinverse. This inverse can later be accessed with x$getinverse After 
## function has been used once to find the inverse, cacheSolves no longer computes
## the matrix value but instead pulls information from matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
      message(" ... getting cached data ... ")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

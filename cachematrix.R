## cachematrix.R script consists of two functions makeCacheMatrix() function and
## cacheSolve() function, that return the inverse of a matrix. makeCacheMatrix() 
## function stores the matrix and its inverse in variables. cacheSolve() computes
## the inverse of the matrix for the first time, and stores it in a variable. It
## outputs the inverse of the matrix from either the cached(stored) value if 
## available, or the computed value.

## makeCacheMatrix() stores the matrix (set()) and its inverse (setinvers()). It
## defines functions to obtain the matrix(get()) and its inverse(getinverse()).

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() function obtains the inverse of the matrix through getinverse()
## described in the makeCacheMatrix() function, if it has been computed and stored
## previously. If the value is NULL (not available), then it performs solve()
## on the matrix to determine the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  else {
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
  }
  s
}

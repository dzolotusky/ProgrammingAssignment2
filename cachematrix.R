## Solution to Coursera's R Programming Week 2 Assignmend
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
## Caching a matrix's inverse

## Creates a list of functions to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the given matrix.
## On the first call, it calculates the inverse
## On subsequent calls, it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(n) {
    x <<- n
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) x_inverse <<- solve
  getinverse <- function() x_inverse
  list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setinverse(x_inverse)
  x_inverse
}
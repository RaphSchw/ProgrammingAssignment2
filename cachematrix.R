## The 2 functions (makeCacheMatrix and cacheSolve) enable the calculation and
## storage of a matrix and its inverse in the cache and avoid recalculation if
## they are present in the cache

## Write a short comment describing this function
## The function :
## 1. Creates the x_inverse variable
## 2. Stores it and the matrix x in its environment
## 3. Defines functions for:
##    a) getting the value of the matrix from its environment
##    b) calculating the inverse of the matrix and assigning it to the x_inverse 
##       variable
##    c) getting the value of the inversed matrix 
##    d) creating the output list containing all previous elements

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


## The function :
## 1. Evaluates whether the x_inverse of the makeCacheMatrix function is in the 
##    its environment
## 2a: if the x_inverse value is in the cache this value is returned
## 2b: if x_inverse is not in the function environment of makeCacheMatrix
##     it is calculated and using the setinverse function from makeCacheMatrix,
##     which stores it in its function environment 

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
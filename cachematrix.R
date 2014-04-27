## The makeCacheMatrix function creates a special "matrix" object that cache its inverse
## The cacheSolve function takes the special "matrix" returned by 1st function and computes its inverse

## This function takes a matrix m as a parameter and creates a special object for matrix m and caches its inverse.


makeCacheMatrix <- function(m = matrix()) {
  inverse_matrix <- NULL
  set_matrix <<- function(y){
    m <<- y
    inverse_matrix <- NULL
  }
  get_matrix <- function() m
  set_inverse <- function(solve) inverse_matrix <<- solve
  get_inverse <- function() inverse_matrix
  list(set_matrix=set_matrix, get_matrix=get_matrix, set_inverse=set_inverse, get_inverse=get_inverse)
}


## This function computes the inverse of matrix m with the get_inverse function and storing it in the inverse_matrix
## variable, then checks if the inverse has already calculated, in order to get the inverse from cache.
## Otherwise it calculates the inverse of the matrix and sets the inverse of the matrix in inverse_matrix variable.

cacheSolve <- function(m) {
  inverse_matrix <- m$get_inverse()
  if(!is.null(inverse_matrix)){
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- m$get_matrix()
  inverse_matrix <- solve(data)
  m$set_inverse(inverse_matrix)
  inverse_matrix
}

## cachematrix.R
## copyright 2015 Jani Okker
## Solution for programming assignment 2 for R Programming course
## in Coursera by John Hopkins University
##
## Implementation includes way to store and read a matrix and 
## its inverse as well as method to calculate the inverse

## makeCacheMatrix is a function that actually returns a list
## of functions
## set_matrix to set a matrix
## get_matrix to get the matrix
## set_inv to set the inverse of the matrix
## get_inv to get the inverse of the matrix
## Please note that there is no check at all that matrix set
## with set_inv is actually a inverse of the stored matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set_matrix <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get_matrix <- function()
  {
    x
  }
  set_inv <- function(a_inv)
  {
    inv <<- a_inv
  }
  get_inv <- function()
  {
    inv
  }
  list(set_matrix = set_matrix, get_matrix = get_matrix, 
       set_inv = set_inv, get_inv = get_inv)
}

## function cacheSolve(x, ...): Returns a matrix that is the inverse of 'x'
## Get cached calculation of inverse, if it is not available then calculate
## it and store it to "cache". Lastly return the inverse. 
## In assignment it was told that it is OK to assume that given matrix is 
## always invertible, so no checks implemented
cacheSolve <- function(x, ...) 
{
  inv <- x$get_inv()
  if(is.null(inv))
  {
    matrx <- x$get_matrix()
    inv <- solve(matrx)
    x$set_inv(inv)
  }
  inv
}


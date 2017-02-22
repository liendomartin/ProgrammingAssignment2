## Put comments here that give an overall description of what your
## This code tend to solve the assignment 2 of the R programming course (JHU)
## The assignment require to code two R functions that calculates & cache the
##    inverse of a matrix.
## Overall, scoping rules, inverse matrix(R function Solve), among others are 
##    evaluated in this week of the course. 
rm(list=ls())
## Write a short comment describing this function
## MakeCacheMatrix is a set of functions that are build to create a matrix
##    that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      mat_inv <- NULL
      set <- function (y) {
            x <<- y
            mat_inv <<- NULL
      }
      get <- function() x
      setInv <- function(solvematrix) mat_inv <<- solvematrix
      getInv <- function() mat_inv
      list( set = set, get = get, setInv = setInv, 
            getInv = getInv) 
}
## Write a short comment describing this function
## This function computes the inverse of the matrix calculate by makeCacheMatrix
cacheSolve <- function(x, ...) {
      mat_inv <- x$getInv()
      if(!is.null(mat_inv)){
            message("getting cached data")
            return(mat_inv)
      }
      data <- x$get()
      mat_inv <- solve(data, ...)
      x$setInv(mat_inv)
      mat_inv
        ## Return a matrix that is the inverse of 'x'
}
##Test the functions
#m1 <-matrix(c(6, 2, 8, 4), nrow = 2, ncol= 2)
#mymatrix_test <- makeCacheMatrix(m1)
#mymatrix_test$get()
#cacheSolve(mymatrix_test)


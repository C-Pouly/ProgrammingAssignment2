## Put comments here that give an overall description of what your
## functions do

#Below are two functions that are used :
#  1. to create a special object that stores a matrix with it's associated funtions, 
#  2. calculate it's inverse and cache's it for further use. 




## Write a short comment describing this function

#The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to :
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse of the matrix
#4.get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  invmat<- NULL
  
  setmat <- function(y) {
    x <<- y
    invmat<<- NULL
  }
  
  getmat <- function() x
  
  setinvmat <- function (mat) invmat <<- mat
  
  getinvmat <- function() invmat
  
  list(setmat = setmat, getmat = getmat,
       setinvmat = setinvmat,
       getinvmat = getinvmat)

}


## Write a short comment describing this function

#The following function, cacheSolve, calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinvmat function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat<- x$getinvmat()
  
  if(!is.null(invmat)) {
    message("Getting cached inverse matrix")
    return(invmat)
  }
  
  data <- x$getmat()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  invmat
}



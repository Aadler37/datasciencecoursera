## Per the instructions, this function creates a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the inverse
##get the value of the inverse
## This function also uses the double operator to assign the values to a different environment
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) matinv <<- inverse
  getinv <- function() matinv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Per the instructions, this function will calcuate the inverse of the matrix. 
## First it will check to see if that value has already been computed. If no, it will calculate and return that value.
## If yes, it will just return the value without calculating again.

cacheSolve <- function(x, ...) {
      matinv <- x$getinv()
    if(!is.null(matinv)) {
      message("getting cached data.")
      return(matinv)
    }
    data <- x$get()
    matinv <- solve(data)
    x$setinv(matinv)
    return(matinv)
}
## Tested the functionality of the code using the simple matrix and inverse code (m1 and n1) provided
## on the coursera forum. Received the expected values. 
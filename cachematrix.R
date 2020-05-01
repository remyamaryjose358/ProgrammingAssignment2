## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Inputs a matrix as an argument 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # Get the value of the matrix 
  setInverse <- function(inverse) inv <<- inverse # Set the value of the invertible matrix
  getInverse <- function() inv # Get the value of the invertible matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       # To check if the inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")   
    return(invMatrix)                             #Return the invertible matrix
  }else{
    MatrixData <- x$getMatrix()                     #get the original Matrix Data 
    invMatrix <- solve(MatrixData, ...)             #Use solve function to inverse the matrix
    x$setInverse(invMatrix)                         #Set the invertible matrix 
    return(invMatrix)                               #Return the invertible matrix  
  }
}

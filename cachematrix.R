## The purpose of this code is to create functions that can save a matrix in cache for future use
## as well as a function to calculate the inverse of a matrix, or to retrieve it from memory if already calculated

## makeCacheMatrix is a function that returns a list of functions to create a matrix, retrieve it, or create and retrieve its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  mat_obj <- function() x
  setinvr <- function(inverse) inv <<- inverse
  getinvr <- function() inv
  list(mat_obj = mat_obj,
       setinvr = setinvr,
       getinvr = getinvr)
}


## cacheSolve is a function that either calculates the inverse of a given matrix, or to retrieve the inverse if it already has been calculated

cacheSolve <- function(x, ...) {
  inver <- x$getinvr()
  if(!is.null(inver)){
    print("Already obtained inverse, retrieving...")
    return(inver)
  }
  matr <- x$mat_obj()
  solution <- solve(matr)
  x$setinvr(solution)
  solution
}

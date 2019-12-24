## Put comments here that give an overall description of what your
## functions do 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function

## makeCacheMatrix is a function which creates a special "matrix" object 
##that can achieve its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize 
  i <- NULL
  
  ## set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    m
  }
  
  ## set the inverse
  setInversemat <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse
  getInversemat <- function() {
    i
  }
  
  ## Return a list of the functions
  list(set = set, get = get,
       setInversemat = setInversemat,
       getInversemat = getInversemat)

}


## Write a short comment describing this function

## cacheSolve function produces the inverse of the special "matrix" 
## returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInversemat()
  
  ## return the inverse if already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix 
  data <- x$get()
  
  ## inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse 
  x$setInversemat(m)
  
  ## Return matrix
  m
}

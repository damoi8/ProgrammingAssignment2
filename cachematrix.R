## Put comments here that give an overall description of what your
## functions do
##set = set the value of the matrix
##get = get the value of the matrix
##setinverse = set the value of the inverse
##getinverse = get the value of the inverse

## makeCacheMatrix creates a "matrix" object , which is a list 
## containing a function to set and get the values of the matrix 
## and set and get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of makeCacheMatrix. 
## If the matrix is the same and the inverse has been calculated, 
## then cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

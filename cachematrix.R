## makeCacheMatrix creates the special "matrix" 
## cacheSolve will return the inverse of the matrix from the cache
## if it already exists or it will calculate afresh and display the results
## and adds to the cahce so that it need not be calculated again

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  ## set the matrix
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  ## get the matrix
  get <- function() x
  setinverse<- function(inverse) inverse_x <<-inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix from the cache if exists or 
##caculate it and display and add to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  ##check for cache
  if (!is.null(inverse_x)) {
    message("getting cached value of the inverse of matrix x")
    return(inverse_x)
  } else {
    inverse_x <- solve(x$get())
    x$setinverse(inverse_x)
    return(inverse_x)
  }
}

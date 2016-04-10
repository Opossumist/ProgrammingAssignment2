## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(input){
    x <<- input
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_input) inverse <<- inverse_input
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("You cached it! Getting it now...")
    return(i)
  }
  i <- solve(x$get())
  x$setinverse(i)
  i
}

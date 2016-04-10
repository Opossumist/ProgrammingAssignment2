## There are two functions included.  The first creates an object that can store a matrix,
## and have that matrix recalled and changed.  The first function also will store a matrix
## given to it that should be the inverse of the original matrix. This can also be recalled.
## The second function will take the matrix from the first function and retrieve the inverse
## matrix if it is stored. If not, it will calculate it and store.

## Object to store the matrix and its inverse, once calculated.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(input) {
    if (!is.matrix(input)) {
      return("must be a matrix")
    }
    x <<- input
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_input) inverse <<- inverse_input
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieves the inverse matrix if it is found. If not, it will calculate it, store it,
## and return it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("You cached it! Getting it now...")
    return(i)
  }
  i <- solve(x$get())
  x$setinverse(i)
  i
}

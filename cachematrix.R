## There are two functions included.  The first creates an object that can store a matrix,
## and have that matrix recalled and changed.  The first function also will store a matrix
## given to it that should be the inverse of the original matrix. This can also be recalled.
## The second function will take the matrix from the first function and retrieve the inverse
## matrix if it is stored. If not, it will calculate it and store.

## Object to store the matrix and its inverse, once calculated.

makeCacheMatrix <- function(x = matrix()) {
  # there is initially no inverse matrix
  inverse <- NULL
  set <- function(input) {
    # allows the user to input a new matrix, checks that it is a matrix, records there is no inverse stored
    if (!is.matrix(input)) {
      return("must be a matrix")
    }
    x <<- input
    inverse <<- NULL
  }
  get <- function() x    # returns matrix
  setinverse <- function(inverse_input) inverse <<- inverse_input    # stores inverse matrix
  getinverse <- function() inverse    # returns inverse matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieves the inverse matrix if it is found. If not, it will calculate it, store it,
## and return it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()    # finds what is stored in inverse in inputted object
  if(!is.null(i)){
    # checks to see if a matrix is stored, and if so, returns it
    message("You cached it! Getting it now...")
    return(i)
  }
  # this only runs if no inverse matrix is stored in the object
  i <- solve(x$get())    # creates the inverse matrix from the matrix stored in the object
  x$setinverse(i)    # stores the inverse matrix in the object
  i    # returns inverse matrix
}

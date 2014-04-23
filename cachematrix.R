

# makeCacheMatrix function has information on one or two matrices and a list 
# that holds functions to getting and setting values to those matrices. 

makeCacheMatrix <- function(x = matrix()) {
  
  # Inverse matrix variable is set to NULL by default, this matrix refers to 
  # the inverse matrix only if there is one.  
  
  inverseMatrix <- NULL
  
  # A function to (re)set current matrix 
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  # A function to return current matrix.
  
  get <- function() x
  
  # A function that computes a inverse matrix and assigns it into inverseMatrix
  # variable
  
  setinverse <- function(z = makeCacheMatrix) inverseMatrix <<- solve(z)
  
  ## Returns a cached inverse matrix
  
  getinverse <- function() inverseMatrix
  
  ## A list that holds functions presented above.
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## A function to test makeCacheMatrix function

cacheSolve <- function(x =makeCacheMatrix(), ...) {
  
  
  inverse <- x$getinverse()
  
  # If a cached inverse matrix is found: it is returned and cacheSolve() stops
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # If there is no cached inverse matrix then it will be created. 
  
  data <- x$get()
  inverse <- x$setinverse(data)
  x$getinverse()
}

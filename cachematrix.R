# The technique below uses lexical scope in order to allow caching objects in another 
# environment. This is useful because prevents unecessary computation.

# ......................................................................................

# The function below returns a list of functions that allows caching both a matrix x and 
# its inverse i, with methods for both storage and retrieval.

# The matrices "x" and "i' can be stored through the functions set() and setinverse() 
# The matrices "x" and "i' can be recovered through the functions get() and getinverse() 

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) i <<- inv
  
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The function below evaluates the inverse through the solve() function and returns it.
# If the evaluation has already been calculated previously and it is stored then
# the function will recover the cached inverse and warn the user about it.

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
  
}

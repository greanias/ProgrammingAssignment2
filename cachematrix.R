## Working in conjunction, these two functions return the inverse of a matrix.  They
## first check to determine whether his inverse has been calculated, and if so return
## that cached value.  If not, they calculate the value, cache it, then return it.


## This function takes a square, invertable matrix and creates a list of four functions.  These
## will be called by the cacheSolve function to validate whether a solution exists prior to
## executing the solve command.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## When cacheSolve is passed the matrix created makeCacheMatrix it will validate whether the
## cached value already exists.  This would be stored in "m".  If it exists, this value will
## be returned.  If not, it will perform the solve() function on the matrix, cache the value,
## and return the answer.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

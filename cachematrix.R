## This function creates a special "matrix" object that can cache its inverse 
## return a list of the following functions
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {     
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinverse = function(inverse) i <<- inverse 
  getinverse = function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## inverse of the matrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i = x$getinverse()
  
  # if inverse already exists/calculated
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  # or else return the inverse 
  data = x$get()
  i = solve(data, ...)
  
  x$setinverse(i)
  
  return(i)
}

## These functions perform the creation of a matrix, the
## inverse of that matrix, and preserve the inverse
## calculation as a cache to increase performance

## This function creates a special "matrix" object that
## can cache its inverse.
##
## x is assumed to always be a square invertible matrix
##
## this function returns a list of functions:
##   set the matrix
##   get the matrix
##   set the inverse
##   get the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## intialize the matrix inverse
  mi <- NULL
  
  ## define the set function, look to set values for
  ## x and m in parent environments
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  
  ## define the get funtion for the matrix
  get <- function() x

  ## define the setinv function, look to set the inverse
  ## variable m in parent environments
  setinv <- function(inv) mi <<- inv

  ## define the get funtion for the matrix inverse
  getinv <- function() mi
  
  ## return the list of functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve
## the inverse from the cache.
## 
## Uses makeCacheMatrix() function to compute the inverse
##
## x is assumed to be a square invertible matrix
## remaining parameters are for the solve() function
cacheSolve <- function(x, ...) {
  inverse = x$getinv()
  
  # has the inverse been calculated before?
  if (!is.null(inverse)){
    # nab the cached inverse to avoid recalculating
    message("getting cached data")
    return(inverse) #return so we don't calculate again
  }
  # otherwise, calculate the inverse and...
  mat.data = x$get()
  inverse = solve(mat.data, ...)
  
  # ...then cache the value of the inverse
  x$setinv(inverse)
  
  return(inverse)
}

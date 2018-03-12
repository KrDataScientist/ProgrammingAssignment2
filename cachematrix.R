## Writing this function made me revise the chapter on Matrices!
## 

## This function caches the inverse of a matrix input.

makeCacheMatrix <- function(mymatrix = matrix()) {
  
  inv = NULL
  set = function(inputvalue) {
    mymatrix <<- inputvalue
    inv <<- NULL
  }
  get = function() mymatrix
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function below is used to set or get/retrieve the cached values of the inverse of a matrix input.

cacheSolve <- function(x, ...) {
  makeCacheMatrix()
  
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("get the cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
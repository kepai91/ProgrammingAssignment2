## Caching the Inverse of a Matrix

## This function creates a special matrix, that is a list containing a funcation to:
## set & get the value of matrix
## setinverse & getinverse of the inverse of the matrix.

## The assumption being the matrix supplied is always invertible and does not contain any NA.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- matrix(, nrow(x), ncol(x))
  set <- function(y){
    x <<- y
    inverse <<- matrix(, nrow(x), ncol(x))
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!anyNA(inverse)) {
    message("getting inverse of matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

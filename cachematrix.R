## These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## 1. set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## 2. get the value of the matrix
  get <- function() x
  ## 3. set the value of the inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  ## 4. get the value of the inverse of the matrix
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## 1. check to see if the inverse has already been calculated; if so, 
  ## get the inverse from the cache and skip the computation
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## 2. calculate the inverse of the matrix x and set the value of the matrix
  ## in the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}

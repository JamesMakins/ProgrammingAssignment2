## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(i = matrix()) {
  inv <- NULL
  set <- function(h) {
    i <<- h
    inv <<- NULL
  }
  get <- function() i
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(i, ...) {
  ## Return inverse of 'i'
  inv <- i$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- i$get()
  inv <- solve(mat, ...)
  i$setInverse(inv)
  inv
}

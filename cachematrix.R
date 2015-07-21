## These functions store the results of computationally expensive matrix inversion
##   within a caching function along with the matrix itself 
##  -- Assumes the matrix provided is square and invertible.

## Stores a given matrix and space for its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} 
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Given a cacheMatrix function, return the inverse of its
## stored matrix either from cache or, if no cached value, by solving
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(is.null(inv)) {  
    ##Solve for inverse
    message("solving for inverse")
    inv <- solve(x$get())
    x$setInverse(inv)   
  } 
  else { 
    ##Return inverse from cache
    message("getting cached data")
  }
  return(inv)
}

## The two functions below provide a "wrapper" for a matrix, so that
## its inverse is not computed repeatedly everytime it is needed

## makeCacheMatrix creates a special matrix whose inverse is cached
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setinv <- function(inverse) inv <<- inverse
 getinv <- function() inv
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}

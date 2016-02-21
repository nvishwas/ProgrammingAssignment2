## makeCacheMatrix creates a matrix and cache its inverse.
## It also provides the functions get/set to read/set the values
## to the matrix created and getinv and setinv to read/set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of matrix created using makeCacheMatrix
## If the cached inverse is not set (NULL) then solve() function is used to
## compute the inverse and also save it to the cache
## if the inverse already exists then the cached ivnerse is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

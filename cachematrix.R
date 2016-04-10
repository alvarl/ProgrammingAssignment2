## makeCacheMatrix and cacheSolve support cacheable inversions of matrices

## makeCacheMatrix constructs a wrapper around a matrix that supports caching
## the inverted matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves a matrix with a cacheable inverse
## In case the inverted matrix is already cached, the result is returned from cache
## If cache is empty, the matrix is inverted, result stored in cache and returned

cacheSolve <- function(x) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  result <- solve(data)
  x$setinverse(result)
  result
}

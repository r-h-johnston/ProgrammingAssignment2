## "Caching the Inverse of a Matrix"

## This function creates a matirx that can cache (store) the 
## inverse of the original matrix, say x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The input of this function is the matrix produced by
## makeCacheMatrix(x), the inverse is then computed or if the 
## inverse has already been calculated this info will be 
## retrieved from the cache

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data,...)
  x$setinverse(n)
  n
}
## Creating two functions. 
##The first 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse. 
##The second 'cacheSolve'computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Creating 'makeCacheMatrix' function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) m <<- inversematrix
  getinversematrix <- function() m
  list(set=set, get=get, setinversematrix=setinversematrix, getinversematrix=getinversematrix)
}


## Creating 'cacheSolve' function

cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinversematrix(m)
  m
}


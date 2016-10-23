## creates a variable to cache a matrix and its inverse
makeCacheMatrix <- function(x = list()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- x
  setinv <- function(q1){
    m <<- q1
    }
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## caches/returns the inverse of a submitted matrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data1 <- x$get
  m <- solve(data1, ...)
  x$setinv(m)
  m
}

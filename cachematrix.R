# cache some variate
makeCacheMatrix <- function(x=numeric()) {
  inv <- NULL
  # set data
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get data
  get <- function() x
  # set solved data
  setsolve <- function(solve) inv <<- solve
  # get solved data
  getsolve <- function() inv
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

# compute the inverse of an invertible matrix
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if (!is.null(inv)) {
    message("getting cached data ...")
    return(inv)
  }
  # if no cached, then set solved data
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

# make an invertible matrix (the job's requirement, see the last sentence.)
x <- matrix(rnorm(25),5,5)
d <- makeCacheMatrix(x)
# first time, no cached
cacheSolve(d)
# second time, cached
cacheSolve(d)
# third time, cached
cacheSolve(d)

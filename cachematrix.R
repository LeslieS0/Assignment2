makeCacheMatrix <- function(x) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInver <- function(solve) inv <<- solve
  getInver <- function() inv
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInver()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInver(inv)
  inv
}
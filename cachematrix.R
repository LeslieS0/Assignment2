#Create special list containing four functions
#Ensure matrix is invertible
makeCacheMatrix <- function(x=matrix()) {
  if(dim(x)[1] == dim(x)[2]){
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
  else{
    message("Matrix is not invertible. Check dimensions")
    NULL
  }
}

#Solve or get inverse matrix
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
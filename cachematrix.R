## This is code for caching the inverse
## of an input matrix

## This function returns list of four functions given an inout matrix

makeCacheMatrix <- function(x = matrix()) {
  theInv <- NULL
  set <- function(y) {
    x <<- y
    theInv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) theInv <<- inverse
  getinv <- function() theInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function accepts the list of functions returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  theInv <- x$getinv()
  if(!is.null(theInv)) {
    message("getting cached data")
    return(theInv)
  }
  thematrix <- x$get()
  theInv <- solve(thematrix, ...)
  x$setinv(theInv)
  theInv
}
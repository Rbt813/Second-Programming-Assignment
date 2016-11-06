## The functions below estimate the inverse of (an invertible) matrix and cache the result
## When called upon, the functions will retrieve the value of the inverse matrix

## The function 'makeCacheMatrix' creates a "matrix object" that stores de inverse of the given matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) matinv <<- solve
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The 'cacheSolve' function does two things:
## * Looks for the inverse matrix in the cache; if found it will retrieve the inverse matrix
## * If the function cannot find the inverse, it will calculate it and print it
        
  cacheSolve <- function(x, ...) {
    matinv <- x$getinv()
    if(!is.null(matinv)) {
      message("getting cached data")
      return(matinv)
    }
    reloaded <- x$get()
    matinv <- solve(reloaded, ...)
    x$setinv(matinv)
    matinv
}

## makeCacheMatrix will return a list of four functions: set, get, setsolve, getsolve.
## At first, user should set a matrix by using the function set. For example:
## matrixx <- matrix (c(1,0,5,2,1,6,3,4,0), 3, 3)
## mcm <- makeCacheMatrix()
## mcm$set(matrixx)

## Then if we call the function get, we will retrive the matrix that was set before.
## If we don't compute the inversed matrix, when we call mcm$getsolve(), NULL will be
## returned. 

## Note that we can set an wrong inversed matrix by using mcm$setsolve(). In this case,
## when we use cacheSolve later, the wrong inversed matrix will be returned.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The functin cacheSolve returns an inversed matrix that was set in makeCacheMatrix before
## If there is no inversed matrix was stored in the makeCacheMatrix, cacheSolve
## will compute and return an inversed matrix, as well as store it in the makeCacheMatrix
## where we can retrive it later by using: makeCacheMatrix$getsolve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve (data, ...)
  x$setsolve(m)
  m
}

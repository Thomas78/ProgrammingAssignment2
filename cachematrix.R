## Solution to Assignment 2. This solution tweaks the example code for the assignment by replacing
## mean with the solve function in cachesolve

## makeCacheMatrix will do 4 things
##    get the matrix
##    set the matrix
##    Set the inverse of the matrix
##    get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve will
##    check for a cached inverse of the matrix and get it if so
##    find the inverse of the matrix if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

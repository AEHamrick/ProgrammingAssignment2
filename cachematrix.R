## Write a short comment describing this function
## This function accepts a matrix and performs a series of steps to firstly
## set up the X and M objects in the parent environment and then create
## some setters and getters in the form of a list of functions

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve first tries to retrieve the cache object m; if it is null 
## it instead retrieves the original input matrix, freshly computes the 
##inverse, and sets it with $setInverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
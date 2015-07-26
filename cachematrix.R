## Functions to get the Inverse of a Matrix, using a cache to store the results, so it´s not re-calculated again.

## Creates a special "vector", which is really a list containing functions to: 
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(new_value) {
    x <<- new_value
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve_value) m <<- solve_value
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Returns the inverse of matrix (uses a cache to avoid re-calculations).
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  message("getting cached data")
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

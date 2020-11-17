## Caching the inverse of a matrix

## creates a matrix with additional setter/getter functions 
## for caching the solved inverse matrix.
## Parameters:
##   x: invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  print(class(x))
  # basic error checking to make sure we at least have a square matrix
  stopifnot("Parameter is not a matrix" = is.matrix(x))
  stopifnot("Parameter is not a square matrix" = nrow(x) == ncol(x))
  
  # set cached value to NULL
  m <- NULL
  set <- function(y) { ## setter
    x <<- y
    m <<- NULL
  }
  get <- function() x ## getter
  
  ## setter for inverse, overrides solve to run within the environment 
  ## that this matrix was defined in rather than the calling environment.
  set_inverse <- function(solve) m <<- solve 
  
  ## getter for inverse
  get_inverse <- function() m 
  
  ## example private function, not revealed in the returned list below
  private_fn <- function() return("foobar")
  
  ## revealing pattern returns list of functions, 
  ## allows private functions to be revealed as public functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Function to solve the inverse matrix from a cache matrix. Returns the cached
## value if set, otherwise calls solve in the scoped environment the matrix was
## defined in. Matrics must be created with the makeCacheMatrix function.
## Parameters:
##   x: cache matrix, a square invertable matrix.
##   ...: additional parameters to pass to the "solve" function.
cacheSolve <- function(x, ...) {
  # basic error checking to make sure we at least have a matrix created with makeCacheMatrix
  stopifnot("Paramater can not be NULL or NA" = (!is.na(x) && !is.null(x)))
  stopifnot("Parameter was not created with makeCacheMatrix" = ("get_inverse" %in% names(x)))
  
  # Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  # Solve within a tryCatch block in case solve throws an error. 
  # On success this will return m.
  tryCatch({ 
      m <- solve(data, ...)
      x$set_inverse(m) ## set solved inverse to cache
      m
      },
    error = function(c) paste("Solve returned an error - ", c)
    )
}

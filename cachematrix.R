## This function builds a set of functions and returns
## them within a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(a_matrix) {
    x <<- a_matrix
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m<<- inv 
  getinv <- function()m
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## This function checks if the inverse of the matrix
## has already been computed and saved in the cache
## (list of the other function)
## If it has been saved it returns it immediately, if
## it hasn't it computes it and sets the values in 
## the list from the other function

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
        
}

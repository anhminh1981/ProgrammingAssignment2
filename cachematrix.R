
## This function returns a list with the original matrix, 
## its cached inverse (initialized as NULL), as well as 
## getter and setter functions for them. If the matrix is
## changed, the cache is reset to NULL.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) { 
    inv <<- inverse 
  }
  getInv <- function()  {inv }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## takes a return value of the makeCacheMatrix function
## as an argument, and returns the inverse of the original
## matrix. If it has already been calculated and the matrix
## unchanged since, it returns the cached data.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

## Functions work together to calculate the inverse of a matrix and cache it for future use if the same result is required,
##  otherwise it resets inverse result

## makeCacheMatrix caches the matrix if already calculated and resets if new x is passed.


makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
  
}


## Function checks if matrix has already been inversed, if so, returns cached value, otherwise it calculates inverses and caches it.

cacheSolve <- function(x, ...) {
        ## Return inverse of 'x'
  
  inv = x$getinv()
  
  # if the inverse calculated
  if (!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse 
  x$setinv(inv)
  
  return(inv)
  
}

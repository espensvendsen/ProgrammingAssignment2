# The two functions shown here help in caching the
# inverse of a matrix. 
#
# DEMO USE
# > m <- matrix(1:4, nrow = 2, ncol = 2)
# > cm <- makeCacheMatrix(m)
# > cacheSolve(cm)
#calculating and caching data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(cm)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#

# Function that cache the inverse of matrix by means of lexical scoping. 
# Returns a list of local functions (getters/setters) that can access the
# cached value.
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  get <- function() m
  set <- function(x) {
    # "<<-" creates global variable and assigns the value of the right-hand side
    x <<- y 
    inv <<- NULL          
  }
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  list(get = get, set = set,
       getinverse = getinverse,
       setinverse = setinverse)
}

# Function that calculates the inverse of a matrix that is created by function
# makeCacheMatrix(m). First calculation is cached for subsequent retrievals. 
cacheSolve <- function(m, ...) {
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating and caching data")
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}

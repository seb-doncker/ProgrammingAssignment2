
##--------------------------------------------------------------------
## makeCacheMatrix
## 
## This function wrap a matrix with the cache of its related 
## inverted matrix.
## This function expose 4 functions to get/set the matrix and to
## get/set its inverted matrix
##
## parameters:
##    mat : matrix
##--------------------------------------------------------------------
makeCacheMatrix <- function(mat = matrix()) {
  # the cached inverted matrix
  invertedMat <- NULL
  
  # get/set for original matrix
  set <- function(newMat) {
    mat <<- newMat
    invertedMat <<- NULL
  }
  get <- function() {
    mat
  }
  
  # get/set for inverted matrix
  setInverted <- function(inv) {
    invertedMat <<- inv
  }
  getInverted <- function() {
    invertedMat
  }
  
  # return a list wrapping all 4 functions.
  list(
    set = set, 
    get = get,
    setInverted = setInverted,
    getInverted = getInverted
  )
}


##--------------------------------------------------------------------
## cacheSolve
## 
## This function calculate the Inverted Matrix of x and use
## the cached result if it exists.
## If not, it compute the inverted matrix then cache it into 
## the argument x
##
## parameters:
##    x: should be a wrapped matrix created with the 
##       function makeCacheMatrix
##--------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  inv <- x$getInverted()
  if(!is.null(inv)) {
    # dont bother the inverted matrix has already been cached.
    message("getting cached data")
  }
  else {
    # compute the inverted matrix and cache it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverted(inv)
  }
  
  # return the inverted matrix
  inv
}

##--------------------------------------------------------------------
## testCacheMatrix
## 
## This function tests the 2 previous functions
##--------------------------------------------------------------------
testCacheMatrix <- function() {
  m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
  cm <- makeCacheMatrix(m)
  print("Matrix: ")
  print(cm$get())
  print("Inverted Matrix (1st attempt): ")
  print(cacheSolve(cm))
  print("Inverted Matrix (2nd attempt): ")
  print(cacheSolve(cm))
}


##Functions that create a Matrix Object and cache the inverse, and calculate the inverse if it has not already been calculated

## This function creates a Matrix Object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


#Testing
#m <- matrix(c(16,9,25,36),2,2)
#m1 <- solve(m)
# orig <- makeCacheMatrix(m)
# invm<- cacheSolve(m1)
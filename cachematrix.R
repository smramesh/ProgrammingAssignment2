## makeCacheMatrix will take a matrix object and cache it. The reason that  
## the matrix needs to be cached is because if the matrix is huge, it would
## be extremely time consuming to do calculations on it as we would continually
## be creating new identical matrices. This also wastes memory
## Since the matrix remains unchanged, it is efficient to simply cache it. 
## cacheSolve will calculate the inverse of the matrix. It is one such calculation
## that would be time consuming if we did not cache the matrix in the 
## makeCacheMatrix function.

## makeCacheMatrix has the get and set method in order to cache the matrix we
## pass. The get methods allows other functions to access the cached matrix,
## while the set method allows other functions to create a cached matrix. The
## setsolve and getsolve methods allow other functions to cache and access 
## the inverse matrix. The 'solve' is the built in R function that calculates
## the inverse matrix. Other functions use the methods of makeCacheMatrix to 
## perform calculations on the cached matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function(solve) m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function starts by getting the inverse matrix. If the inverse matrix is 
## not in the cached data, this function uses setsolve to actually cache the data.
## If the matrix is in the cached data already, it will print "getting cached 
# data" and then return the inverse matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
}

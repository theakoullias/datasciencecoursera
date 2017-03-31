## Function 1 takes a square matrix, computes the inverse, and caches the result.
## Function 2 returns the inverse of a square matrix from the cache; otherwise it computes it on the spot, stores it, and returns it. 

## FUNCTION 1
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 														##sets value of the matrix
  get <- function() x 										##gets value of the matrix
  setinv <- function(solve) inv <<- solve 					##sets value of the inverse
  getinv <- function() inv 									##gets value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## FUNCTION 2
## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 														##returns inverse from cache if already calculated
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
} 									##calculates inverse and stores it in the cache if it hasn't been calculated
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Allows the caching of matrix inversions in order to not have to recalculate
# previously inverted matrixes

# 1. Get the value of the matrix
# 2. Set the value of the matrix
# 3. Get the value of the inverted matrix
# 4. Set the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinvert <- function(inverse) invert <<- inverse
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## Write a short comment describing this function
## Checks to see if the matrix inversion has already been computered and
# if so, returns the cached value (along with displaying a message that
# a cached value has been used). If there is no cached value, the inversion
# is calculated and stored in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data,...)
  x$setinvert(invert)
  invert
}

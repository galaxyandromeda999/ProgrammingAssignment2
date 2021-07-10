## Put comments here that give an overall description of what your
## functions do

# Make a pair of function in order to cache the inverse of a matrix

## Write a short comment describing this function

# First Function : makeCacheMatrix
# To creates a special "matrix" object that can cache its inverse
# x is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  # Initializing invers as NULL 
  inver <- NULL
  
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  get <- function() x  # Make Function to get matrix
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# Second Function : cacheSolve 
# To computes the inverse of special "matrix" returned by makeCacheMatrix function.
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache
# A function to gets cached data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  # To check wheteher inverse is NULL
  if(!is.null(inver)) {
    message("Getting Cached Matrix Inverse Data")
    return(inver)   # To return inverse value
  }
  data <- x$get()
  inver <- solve(data, ...)  # To calculate inverse value
  x$setinverse(inver)
  inver  # To return a matrix which is the inverse of x
}

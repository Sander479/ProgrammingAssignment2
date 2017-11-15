## Put comments here that give an overall description of what your
## functions do
## the function CacheSolve checks if the inverse of the matrix is already in the cache, and if so it uses the cache.
## if not, it calculates the inverse
## the combination of the 2 functions makes sure that it uses the cached version if it exists and if not it adds the newly calculated
## inverse into the cache.

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to
## be able to get or set the matrix and get and set the inverse of the matrix into/from the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## The following function calculates the inverse of the special matrix created with the above function
## if the inverse is not in the cache already, or else it calculates the inverse and puts it into the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

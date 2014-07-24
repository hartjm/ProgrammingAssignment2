## Solution to the CacheMatrix problem whereby we want to calculate the inverse
## of a matrix and also cache the value as the calculation can be time intensive
## we solve this problem my implmementing a wrapper around matrix 
## (like creating a facade class that wraps the matrix in an oo language )
## and then we use this new makeCacheMatrix instead of matrix() with another
## functions that unstands the makeCacheMatrix signature (list of 4 objects)

## This function creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                   # initalise i which will hold the inverse value
  set <- function(y) {        # define a function set() which sets the matrix and
    x <<- y                   # (re-)initalises the inverse value to NULL 
    i <<- NULL
  }
  get <- function() x         # getter to retrive matrix
  setinverse <- function(inverse) i <<- inverse  # setter to store matrix inverse
  getinverse <- function() i  # getter to retrieve matrix inverse
  list(set = set, get = get,  # return the 4 cachematrix functions as a list 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of a matrix returned from makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()               # attempt to retrived inverse value
  if(!is.null(i)) {                 # was inverse value found?
    message("getting cached data")  # yes, so retrive from cache
    return(i)                       # have what we need to leave now
  }
  data <- x$get()                   # no inverse found so must now get matrix...
  i <- solve(data, ...) # ...so that we can calculate the inverse of it
  x$setinverse(i)       # done so set the value so other callers can get later on
  i ## and return the matrix that is the inverse of 'x'
}

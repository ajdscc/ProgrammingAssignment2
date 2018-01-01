## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse, 
## which is really a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
##
## and the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inverse of the special "matrix" object 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of makeCacheMatrix object
  
  if (!is.list(x)) {
    message("cacheSolve(x,...): 'x' must be a special 'list_obj' returned by MakeCacheMatrix(x = matrix())")
    return(invisible(NULL))
  }
  
  if (length(x$get()) ==1) {
    message("Matrix not set. Store the matrix using 'list_obj'$set(x = matrix()); 'list_obj' is the list returned by MakeCacheMatrix(x = matrix())")
    return(invisible(NULL))
  }
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## calculate the inverse
  x$setsolve(m)  ## store in cache
  m                ## return the inverse
  
}

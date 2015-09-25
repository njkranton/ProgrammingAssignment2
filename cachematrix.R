## This file contains 2 functions for the Programming Assigment 2 for the
## R-Programming (032) course.
## Because matrix inversion is computationally intensive, these functions 
## cache the inverse of a matrix so that it does not need to be computed 
## repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse. This is actually a list of functions (set, get, 
## setinverse, getinverse) - the object created with this function will 
## have these "sub-functions".

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## initialize inverse as NULL
  set <- function(y = matrix()) {  ## sub-function for setting the matrix
    x <<- y
    inv <<- NULL  ## set inverse to NULL in the calling environment
  }
  get <- function() x  ## sub-function to return the value of x
  setinverse <- function(inverse) inv <<- inverse  ## sub-function to set the inverse
  getinverse <- function() inv  ## sub-function to return the inverse
  list(set = set, get = get,  ## list of functions - this will store the functions
                              ## and enable the object created with the main function
                              ## to have the sub-functions defined above
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  ## get the inverse currently stored for matrix x, if any
  if(!is.null(inv)) {    ## if inverse exists (i.e. has been calculated)
    message("getting cached data")   ## Tell that cached value will be used
    return(inv)   ## return the cached value
  }   ## if no inverse has yet been calculated for x
  data <- x$get()  ## get the content of x ("the original matrix")
  inv <- solve(data, ...)  ## use solve function to get its inverse
  x$setinverse(inv)  ## set the result as the inverse in x
  inv   ## return the newly computed inverse
}

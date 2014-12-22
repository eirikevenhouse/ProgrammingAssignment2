## Put comments here that give an overall description of what your
## functions do

# First function create a "superfunction" which will be 
# a collection of functions designed to read in a matrix and cache its inverse.

## Write a short comment describing this function
# This first function defines the "read in matrix" function as well as the
# functions for storing and retrieveing the inverse if it exists outside
# the function (i.e. if it has been cached)
makeCacheMatrix <- function(x=matrix()) {
      matinverse <-NULL
      set <- function(y){ 
             x <<- y
             matinverse <<- NULL
      }
      get <- function() x
      setinv <- function(solve) matinverse <<- solve
      getinv  <- function() matinverse
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
# Function returns the inverse for the special matrix object
# created by makeCacheMatrix, either by retrieving the cached
# inverse or by actually computing it if it hasn't already been
# cached 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinverse <- x$getinv()
        if(!is.null(matinverse)){
             message("Getting the cached inverse")
             return(matinverse)
        }
        data <- x$get()
        matinverse <- solve(data, ...)
        x$setinv(matinverse)
        matinverse
}

# Test the functions above
# Create an invertible matrix
y <- matrix(c(2,2,3,2),ncol=2)
cacheSolve(makeCacheMatrix(y))


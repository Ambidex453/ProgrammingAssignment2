## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Here I am writing a function to calculate Inverse of a square matrix
# (we will assume) that the matrix given as input is invertible.
# 
# The first function The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)


}


## Write a short comment describing this function

# The following function calculates the inverse of the matrix
# created by above function
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the
# inverse in the cache via the setInv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

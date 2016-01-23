## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix function creates a variable which holds a matrix and its inverse
# cacheSolve computes the inverse of the matrix if it is not computed before else it returns the computed inverse as it is.


#-------------
# example usage
#tmpMat <- matrix(c(7,2,1,0,3,-1,-3,4,-2),3,3,byrow = TRUE)
#tmpMat
#     [,1] [,2] [,3]
#[1,]    7    2    1
#[2,]    0    3   -1
#[3,]   -3    4   -2

#x <- makeCacheMatrix(tmpMat)
#x$get() # returns the matrix
#     [,1] [,2] [,3]
#[1,]    7    2    1
#[2,]    0    3   -1
#[3,]   -3    4   -2

#x$getInverse()
#NULL

#cacheSolve(x)
#     [,1] [,2] [,3]
#[1,]   -2    8   -5
#[2,]    3  -11    7
#[3,]    9  -34   21

#cacheSolve(x) #using the cached inverse
#getting Inverse of matrix
#    [,1] [,2] [,3]
#[1,]   -2    8   -5
#[2,]    3  -11    7
#[3,]    9  -34   21

#x$getInverse()
#    [,1] [,2] [,3]
#[1,]   -2    8   -5
#[2,]    3  -11    7
#[3,]    9  -34   21

#-------------

## Write a short comment describing this function
# this function creates a variable which holds a matrix and its inverse. First the inverse is null. 
# This function has some attributes as below:
# inv -> keeping the inverse of the matrix
# getInverse -> function that returns the cached inverse of the matrix (if the cacheSolve didn't run it will return NULL)
# setInverse -> function that set the inv value (cached inverse) 
# get -> returns the matrix
# set -> set the matrix data
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getInverse <- function() inv
  setInverse <- function(solve) inv <<- solve
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve function takes the makeCacheMatrix variable 
# if the variable have the inverse of the matrix data it returns immidietly
# else it calculates the inverse, then store it and returns it. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting Inverse of matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. The 
# following two are used in tandem to either calculate the inverse
# the first time around we pass a matrix object 
# OR
# return a cached value of the inverse for the matrix object if we repeatedly
# request the inverse of the same matrix object in the current session

# makeCacheMatrix is a function that returns a list of objects 
# for the current matrix object we are going to evaluate
# This list of objects is then made available to the cacheSolve function
# The cacheSolve function does the actual computation of returning the inverse
# of the matrix object we want to compute 
# In the makeCacheMatrix function we do the following:-
# 1. set -> set the value of the matrix object we pass to the function argument
# 2. get -> get the value of the matrix object we pass to the function argument
# 3. setinverse -> the value of inverse of the matrix object we pass to the function argument
# 4. getinverse -> get the value of inverse of the matrix object we pass to the function argument
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  get <- function(){x}  
  setinverse <- function(inverse) {inv <<- inverse} 
  getinverse <- function() {inv}
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
  
} 


# The cacheMatrix function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the 
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function. 

# This function assumes that the matrix is always invertible. 
cacheSolve <- function(x, ...) { 
  # Call the getinverse() object (which is a function) from the list of objects
  # created by the makeCacheMatrix function from the Global environment
  # variable 'inv' and assign it to Local environment variable 'inv'
  # If its the first call to the function the value of 'inv' will be NULL
  # If NOT the value of 'inv' will be the cached value from the first run
  inv <- x$getinverse() 
  if(!is.null(inv)) { 
    message("Cached inverse value available - displaying cached value") 
    return(inv) 
  } 
  # Get the matrix object from the list of objects created by the makeCacheMatrix function
  data <- x$get() 
  inv <- solve(data) 
  # Write the value of the inverse assigning it to Global environment 
  # variable 'inv' through the call to the getinverse() object (which is a 
  # function) from the list of objects created by the makeCacheMatrix function
  
  x$setinverse(inv) 
  inv 
} 

##TESTS:

##> nn <- matrix(c(2:5), nrow = 2, ncol = 2, byrow = TRUE)
##> nn
##[,1] [,2]
##[1,]    2    3
##[2,]    4    5
##> deep<-makeCacheMatrix(nn)
##> deep$getinverse()
##NULL # As cached value not assigned yet - this happens in the cacheSolve call
##> cacheSolve(deep)
##[,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0
##> cacheSolve(deep)
##Cached inverse value available - displaying cached value
##[,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0
##> deep$getinverse() # As cached value assigned and available
##[,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0
##> deep$get()
##[,1] [,2]
##[1,]    2    3
##[2,]    4    5
 

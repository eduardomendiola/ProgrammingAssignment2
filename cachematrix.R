########################################################################################
##This function creates a makeCacheMatrix object with the following available functions:
########################################################################################
# set: initialize matrix, inverse matrix and control flag
# get: get matrix contents
# setflag: set value of cache status flag
# getflag: get value of cache status flag
# setinverse: set value of inverse matrix
# getinverse: get value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  #cache status flag
  has_been_cached_flag <- FALSE
  m <- NULL
  
  #initialize matrix and reset inverse matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
    has_been_cached_flag <<- FALSE
  }
  #get matrix
  get <- function() x
  
  #get/set cache status flag
  getflag <- function() has_been_cached_flag
  setflag <- function(f) has_been_cached_flag <<- f
  
  #get/set cached inverse matrix
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  #functions available to makeCacheMatrix
  list(set = set, get = get,
       getflag = getflag,
       setflag = setflag,
       setinverse = setinverse,
       getinverse = getinverse)
}

########################################################################################
#This function will retrieve the inverse matrix from the cache if it has been 
#already processed. Otherwise, it will invoke the solve() function and set the 
#has_been_cached flag.
########################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(x$getflag()) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  x$setflag(TRUE)
  m
}  


########################################################################################
# 
# Examples:
#   
# > source("ProgrammingAssignment2/cachematrix.R")
# > z<-makeCacheMatrix()
# > z$set(matrix(c(3,5,7,9),2,2))
# > z$getflag()
# [1] FALSE
# > z$getinverse()
# NULL
# > cacheSolve(z)
# [,1]   [,2]
# [1,] -1.125  0.875
# [2,]  0.625 -0.375
# > cacheSolve(z)
# getting cached data
# [,1]   [,2]
# [1,] -1.125  0.875
# [2,]  0.625 -0.375
# > z$getflag()
# [1] TRUE
# > z$getinverse()
# [,1]   [,2]
# [1,] -1.125  0.875
# [2,]  0.625 -0.375
# > z$get()
# [,1] [,2]
# [1,]    3    7
# [2,]    5    9
# > solve(z$getinverse())
# [,1] [,2]
# [1,]    3    7
# [2,]    5    9
# > 
#   
#   ...
# #re-initialize matrix
# ...
# 
# > z$set(matrix(c(9,11,13,21),2,2))
# > z$getflag()
# [1] FALSE
# > cacheSolve(z)
# [,1]       [,2]
# [1,]  0.4565217 -0.2826087
# [2,] -0.2391304  0.1956522
# > cacheSolve(z)
# getting cached data
# [,1]       [,2]
# [1,]  0.4565217 -0.2826087
# [2,] -0.2391304  0.1956522
# > z$get()
# [,1] [,2]
# [1,]    9   13
# [2,]   11   21
# > solve(z$getinverse())
# [,1] [,2]
# [1,]    9   13
# [2,]   11   21
# > 
#   
#
########################################################################################

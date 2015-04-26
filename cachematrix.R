## Put comments here that give an overall description of what your
## functions do
###################################################################
# There are two functions in this file:
# 1. makeCacheMatrix (x = matrix())
#        ## This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve (x, ...)
#        ## Return a matrix that is the inverse of 'x'
###################################################################

  
## Write a short comment describing this function
######################################################################################
# This function creates a special "matrix" object that can cache its inverse.        #
######################################################################################
# Input: x - a square invertible matrix                                              #
# Output: myList, which is list(set, get, iMatrix.set, iMatrix.get)                  #
#        Where:                                                                      #
#              set sets x in the parent environment;                                 #
#              get gets x from the parent environment;                               #
#              iMatrix.set sets the inverse of x from the parent environment;        #
#              iMatrix.get gets the inverse of x from the parent environment.        #
######################################################################################      

makeCacheMatrix <- function(x = matrix()) {
  ########################
  # Initiating variables #
  ########################
  iMatrix <- NULL
  
  ###########################
  # Get and Set methods     #
  # For x            #
  # *AT PARENT ENVIRONMENT* #
  ###########################
  set <- function(tempI) {
    x <<- tempI
    iMatrix <<- NULL                 # Must initialize iMatrix here too
  }
  get <- function() {
    x
  }
  
  #########################################
  # Get and Set methods                   #
  # For iMatrix (the inverse of x) #
  # *AT PARENT ENVIRONMENT*               #
  #########################################
  iMatrix.set <- function(tempI) {
    iMatrix <<- tempI
  }
  iMatrix.get <- function() {
    iMatrix
  }
  
  ######################################
  # Return list with functions/methods #
  ######################################
  myList <- list(set=set, get=get, iMatrix.set=iMatrix.set, iMatrix.get=iMatrix.get)
  return(myList)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ###########################################################################################
  # This function computes the inverse of the special "matrix" returned by makeCacheMatrix. #
  # If the inverse has already been calculated (and the matrix has not changed), then the   #
  # cachesolve should retrieve the inverse from the cache.                                  #
  ###########################################################################################
  # Input: x, as the output of makeCacheMatrix()                                            #
  # Output: the inverted matrix                                                             #
  ###########################################################################################
  message("BEGIN of cacheSolve(x, ...)")
  iMatrix <- x$iMatrix.get()
  
  # if the inverse has not been calculated (not in cache), calculate it and put it in cache...
  if (is.null(iMatrix)) {
    message("        Inverted matrix not found in cache.")
    message("        Calculating...")
    iMatrix <- solve(x$get(), ...) # calculating
    message("        Saving it in cache...")
    x$iMatrix.set(iMatrix)         # saving on cache
  } else {
    message("        Inverted matrix found in cache.")
  }
  
  # getting from cache to return the inverted Matrix...
  message("        Retrieving contents...")
  message("END of cacheSolve(x, ...).")
  return(iMatrix)
}

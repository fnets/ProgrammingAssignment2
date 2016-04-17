## OVERALL DESCRIPTION:

## These are two methods that will store an inputted
## matrix into the cache and calculate it's inverse.
## This is done so that when calculating the inverses
## of a large vector or set of matrices, we only have
## to perform each calculation once, and save
## it for later.


##DESCRIPTION OF makeCacheMatrix:

## This function stores the method's inputted 
## matrix in the cache for use later.  It returns
## a list of the available getter and setter
## functions for the input matrix and its 
## inverse (from the cache).



## Var i is the variable where the matrix will be 
##    stored (ie: the cache).
## setMat stores the input matrix
## getMat retrieves the input matrix
## setInv sets the inverse matrix into the cache.
## getInv returns the inverse matrix

## This is copied from Roger Peng's supplied code 
## for the assignment.  I have explained the lines
## that I think are difficult to read.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #makes sure cache is clear
  
  setMat <- function(y) {
    x <<- y  ##inputs the matrix.
    i <<- NULL  ##So, again the cache is set to empty
  }
  
  getMat <- function() x  
            ##prints the input matrix
  
  setInv <- function(inv) i <<- inv  
            ##stores the inverse matrix in the cache
  getInv <- function() i
            ##prints the inverse matrix
  list(setMat = setMat, getMat = getMat,
    setInv = setInv,
    getInv = getInv
  )
}

##DESCRIPTION OF cacheSolve:

## This function solves for the inverse of
## an inputted matrix.  It uses the function above
## to store and retrieve both the input matrix and
## the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invMat <- x$getInv()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$getMat()
  invMat <- solve(data)
  x$setInv(invMat)
  
  invMat
}

## Thanks for reading! :)
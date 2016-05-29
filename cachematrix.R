## These functions work together to invert a square matrix.
## The inverted matrix may be needed many times, but inverting matrices
## takes a long time. A parent function called makeCacheMatrix is used to
## store the various commands for inverting the matrix or retrieving what
## has already been inverted.

## These functions assume the matrix is square (same # of rows and columns)
## and invertible so that the solve function can be used. 
## However, the matrix passed to cacheSolve to get its inversion may change 
## or have a different # of rows and columns. If this happens, the
## inversion needs to be recalculated.

## Order of Commands used to test these functions. Messages are writtn
## to indicate the result.
## listA<-makeCacheMatrix()
## a <- matrix(c(1,2,3,4), 2, 2)
## b <- cacheSolve(a)
##    repeat with the same matrix to see that cache is used
## b <- cacheSolve(a)
##    repeat with a new matrix of the same size
## c <- matrix(c(5,6,7,8), 2, 2)
## d <- cacheSolve(c)
##    repeat with the same matrix of a larger size
## e <- matrix(c(5))
## f <- cacheSolve(e)

## makeCacheMatrix: This parent function creates a special "matrix" object that
## controls caching.
## This object defines the functions that will be passed into cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  #invMatrix is the inverted matrix. savMatrix is the matrix used to invert
  #initialize to null to use as a flag later
  invMatrix<-NULL
  savMatrix<-NULL
  
  # set will change the matrix being used and clear the inverted matrix
  set<-function(y){
    
    # savMatrix saves the matrix to compare when called again to see if anything has
    # changed
    savMatrix<<-y
    
    # if the matrix changes, then we need to re-initialize the inverted matrix
    # so that it will be calculated again
    invMatrix<<-NULL
  }
  
  # The get function returns the matrix that was used for the cached inverse

  get<-function() savMatrix
  
  # The setmatrix function will calculate the inverse and store it in invMatrix when called
  setmatrix<-function(solveData) invMatrix<<-solveData
  
  # The getmatrix function will retrieve the cached inverse when called
  getmatrix<-function() invMatrix
  
  # this final command stores the four actions above into a list along with a
  # name that the cachSolve function can use. For example:
  # listA <- makeCacheMatrix(a) will create a list from which these 
  # can be used:
  #    listA$set  will change the matrix and initialize cache
  #    listA$get  will return the last matrix that was inverted
  #    listA$setmatrix   will calculate and store the inverted matrix
  #    listA$getmatrix   will return the inverted matrix
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.If the inverse has already been
## calculated, then cacheSolve retrieves
## the inverse from the cache. This will avoid having to perform
## the calculation again. If the matrix changes, then the inverted matrix
## is cleared out to show that we'll need to invert again.
## e.g. cacheSolve(a)

cacheSolve <- function(x, ...) {
 
  # check to see if the matrix has changed (checks to make sure same size)
  compMatrix <- listA$get()
  
  if(is.null(compMatrix) || dim(compMatrix)!=dim(x) || !identical(compMatrix,x))
    {
    message("First run or matrix has changed")
    listA$set(x)
    message("Inverting Matrix")
    invMatrix <- solve(x)
    listA$setmatrix(invMatrix)
    
    return(invMatrix)
  }
  
  # same matrix so we can return our cached inversion     
    message("return the cached inversion")
    invMatrix <- listA$getmatrix()
  
    invMatrix
  }

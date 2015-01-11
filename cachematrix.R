## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #makeCacheMatrix is a function that makes four other functions:set,get,setinvers and getinverse
  
  invOfMatrixValue <- NULL  #Variable "invOfMatrixValue" is placeholder for the value of the inverse of a Matrix and gets default value NULL
  set <-function(y){ #I set the value of matrix x through a function with argument y
        x<<- y  #y is saved to x in the enclosing environment 
        invOfMatrixValue<<- NULL    #the variable "invOfMatrixValue" gets default NULL and is saved in the enclosing environment
  }
  
  get <- function() x
  setinverse <- function(inverse) invOfMatrixValue<<- inverse
  getinverse <- function() invOfMatrixValue
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(objectWithFunctions, ...) {
        ## Return a matrix that is the inverse of 'x'
  invOfMatrixValue <- objectWithFunctions$getinverse()  #calls getinverse function of objectWithFunctions and passes result to invOfMatrixValue
  if (!is.null(invOfMatrixValue)){         # Checks if value of invOfMatrixValue (from line above) is not NULL and if that is the case, it returns the value
      message("getting cached data")
      return(invOfMatrixValue)
  }
  data <- objectWithFunctions$get()          #if value of getinverse function is NULL, the get-function runs and returns matrix x to data
  invOfMatrixValue <- solve(data,...)        #data is passed to solve() and the result(inverse of matrix x) is saved in invOfMatrixValue
  objectWithFunctions$setinverse(invOfMatrixValue)  #invOfMatrixValue is passed to setinverse() function which saves the calculated value for invOfMatrixValue in the enclosing environment
  invOfMatrixValue             #the calculated inverse of matrix x is returned
  
}

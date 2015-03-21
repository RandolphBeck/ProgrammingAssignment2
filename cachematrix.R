## ***************************************************************************************
## ***************************************************************************************
## The functions makeCacheMatrix and cacheSolve work together through a common
## data object to store the inverse of a square matrix X so that it need not be 
## recalculated when the underlying matrix has not changed.
##
##
## ***************************************************************************************
## makeCacheMatrix creates and returns a list of 5 functions which can be used to store
## and retrieve a square numeric matrix and its inverse. The formal argument x passed to 
## makeCacheMatrix
## is the initial matrix to be inverted. The API is as follows:
##  let Z<- makeCacheMatrix(x)
##  Z$set(x)    cache the matrix x and set the validinverse flag to FALSE
##  Z$get()     return the cached matrix from storage
##  Z$setinv(k) cache the calculated inverse k and set the validinverse flag to TRUE
##  Z$getinv()  return the cached inverse
##  Z$validinverse  return TRUE if a valid inverse is cached, otherwise FALSE
##
makeCacheMatrix <- function(x = matrix()) {
## 
## create a zero-filled matrix of the right size to hold the inverse of x.
  inv <- matrix(0, nrow = nrow(x), ncol = nrow(x))
## create a logical variable to indicate if the inverse has been calculated
  invcalc <- FALSE
  
## define a set function to store the data in the original matrix x and reset the flag
  set <- function(y = matrix()) {
    x <<- y  ## store the matrix passed to set in the storage location x
            ## this allows the user to update the value of the original matrix and note 
            ## that any previously calculated inverse is not valid
    invcalc <<- FALSE  ##reset the flag indicating inverse has not been calculated
  }

## define a get function to retrieve the last stored value of x
  get <- function() x
  
## define a set function to store a newly calculated inverse 
## use the superassignment operator to store the inverse in the matrix
## in the environment that called setinv
  setinv<- function(xinv) {
    inv <<- xinv
    invcalc <<- TRUE
  }

## define a get function to retrieve the inverse stored by the setinv function
  getinv<- function() inv

## define a get function to check the status of the inverse: returns TRUE if inverse is available
  validinverse <- function() invcalc

## create a list containing the functions defined here and return it
  list(set = set, get = get, setinv = setinv, getinv = getinv, validinverse = validinverse)
}

## ***************************************************************************************
## cacheSolve(x) takes a 5 element list of functions returned by makeCacheMatrix as its
## argument and returns the inverse of the original cached matrix.  If valid inverse has
## been previously cached, it returns the inverse from cache, else it solves for the
## inverse and caches it for future use.  The results may be confirmed by the following test
## Z$get() %*% Z$getinv() = Identity  where Identity is the n by n diagonal matrix of 1s
## and Z is the list returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of the previously cached matrix in x$get()
  if (x$validinverse()){
    return(x$getinv())
  }
  else
  {
    a <- x$get()  ## retrieve the value of the original matrix
    I <- diag(1, nrow(a), nrow(a))  ## create the identity matrix for the original matrix
    b <- solve(a, I)  ## solve for the inverse
    c <- x$setinv(b)  ## cache the new inverse for future use and set validinverse to true
    b ## return the inverse matrix
  }
}

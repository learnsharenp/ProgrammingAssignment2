## makeCacheMatrix function returns special list to 
## store Matrix and it's Inverse Matrix 
## cacheSolve use special list of makeCacheMatrix and return cache inverse
## matrix is avaible else calculate the inverse and cache it

## makeCacheMatrix function create the list of function that
## set the matrix
## get the matrix
## set the Inverse matrix
## get the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
  setMat <- function(y) {
    normMat <<- y
    invM <<- NULL
  }
  getMat <- function() normMat
  setInvM <- function(invMat) invM <<- invMat
  getInvM <- function() invM
  list(setMat = setMat, getMat = getMat,
       setInvM = setInvM,
       getInvM = getInvM)
}


## cacheSolve function use the special list created by makeCacheMatrix
## which return the inverse matrix if it is already calculated, else create
## inverse matrix and cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInvM()
  if(!is.null(InvMat)) {
    message("getting cached Invert Matrix")
    return(InvMat)
  }
  norMat <- x$getMat()
  InvMat <- solve(norMat)
  x$setInvM(InvMat)
  InvMat
}

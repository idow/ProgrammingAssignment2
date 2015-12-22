## Put comments here that give an overall description of what your
## functions do

## This is a function that receive a matrix as argument and caches a matrix into a
## Lexical Scoping (= global) variable mat. The cached mat equalls NULL until it 
## is assigned with a matrix using the setInv function ## any matrix can be assigned
## into mat, for this excersice, and inverse matrix will be 
## to mat by the cacheSolve function below

makeCacheMatrix <- function(x = matrix()) {

  mat <<- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  getInv <- function() mat
  ## set the inversed matrix from CacheSolve into global variable mat
  setInv <- function(invMat) mat <<- invMat
  
  list(set = set, getInv = getInv, 
       setInv = setInv, getInv = getInv)
}


## This is a function that check if an inverse matrix was calculated and assignd into the 
## Lexical Scoping (= global) ##  variable mat. if it was - it will return the cached 
## matrix. if not - the function will calculate the inverse matrix and cache it into mat
## using the function makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInv()
  ##check if inverse matrix was already calculated, if it was then it returns the cached matrix
  if(!is.null(mat)) {                   
    message("getting cached data")
    return(mat)
  }
  ## calculates the inverse matrix and set it into the global variable mat using makeChacheMatrix
  
  tempMat <- x$getInv()
  invMat <- solve(tempMat)
  x$setInv(invMat)
  ## returns the Lexical Scoping (= global) variable mat after 
  ## setting it to be the invered matrix
  mat
  
}

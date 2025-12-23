## functions calculates an inverse matrix and cache it for later use

## makeCacheMatrix function caches an inverse matrix until the new one is introduced

makeCacheMatrix <- function(x=matrix()){
  inverseMatrix <- NULL
  
  setMatrix  <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function()x
  setInverse <- function(i){
    inverseMatrix <<- i
  }
  
  getInverse <- function() inverseMatrix
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}

## Calculates an inverse matrix, if not present in cache already

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$getMatrix()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

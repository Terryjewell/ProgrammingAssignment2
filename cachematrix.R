## This function will computing the inverse of a square matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
  MatrixInv <- NULL ## set inversion matrix value to null
  
  set <- function(y)
  {
    x <<- y
    MatrixInv <<- NULL
  }
  get <- function() x
  setInverse <- function(i) MatrixInv <<- solve(x)
  getInverse <- function() MatrixInv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...)
{
  MatrixInv <- x$getInverse()
  if(!is.null(MatrixInv))##check for blank matrix
    {
      message("getting matrix from cash instead of re-inverting")
      return(MatrixInv) 
    }
  
  MatrixInv <- solve(x$get())
  x$setInverse(MatrixInv)
  MatrixInv
  
}


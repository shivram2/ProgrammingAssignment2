## makeCacheMatrix creates a matrix that can store (cache) its inverse
## cacheSolve finds the inverse of the matrix from makeCacheMatrix


## makeCacheMatrix takes in a parameter: a matrix 'x', which is assumed to be invertible
##It sets up a way to cache the inverse of the matrix 'x'
makeCacheMatrix <- function(x = matrix()) 
{
  inverseM <- NULL
  set <- function(y) 
  {
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseM <<- solve
  getInverse <- function() inverseM
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve finds the inverse of the matrix from makeCacheMatrix.
##It will take an instance of that function as an argument
cacheSolve <- function(x, ...) 
{
  inverseM <- x$getInverse()
  if(!is.null(inverseM)) 
  {
    message("getting cached data")
    return(inverseM)
  }
  data <- x$get()
  inverseM <- solve(data)
  x$setInverse(inverseM)
  inverseM
}

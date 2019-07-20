
# 'This method takes an invertible matrix
# '@param x a matrix
# '@return list

makeCacheMatrix <- function(x = matrix())
{
  inverse <- NULL
  # set function is used when a new caching is required
  # this method will reset the inverse that it caches
  setMatrix <- function(y)
  {
    x <- y
    inverse <- NULL
  }
  # this function simply returns the matrix
  
  getMatrix <- function() x
  
  # methods to set and get inverse of a matrix
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = setMatrix, get = getMatrix,
       setinverse = setInverse,
       getinverse = getInverse)
}


# 'This method inverts a matrix. It uses cached value if it already exists
# '@param x a cached matrix list as returned by function makeCacheMatrix()
# '@return inverse of matrix

cacheSolve <- function(x,...)
{
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse of matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

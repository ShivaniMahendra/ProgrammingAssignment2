## The following function will cache the inverse of the matrix.

##The below function will cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix())
  {
    cache <- NULL    
    setMatrix <- function(newValue) 
      {
        x <<- newValue  
        cache <<- NULL
      }
    
    
    getMatrix <- function() 
      {
        x
      }
    
    cacheInverse <- function(solve) 
      {
        cache <<- solve
      }
    
    getInverse <- function() 
      {
        cache
      }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         cacheInverse = cacheInverse, getInverse = getInverse)
    
  }


## The following function will compute the inverse of the matrix returned 
## by the above function. If the inverse is already made then this function will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
    inverse <- x$getInverse()
    if(!is.null(inverse)) 
      {
        message("getting cached data")
        return(inverse)
      }
    
    data <- x$getMatrix()
    inverse <- solve(data)
    x$cacheInverse(inverse)
    
    inverse
  }

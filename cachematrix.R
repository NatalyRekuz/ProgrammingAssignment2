## A pair of functions that cache the inverse of a matrix

## Function "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
              
      ## The function "set" changes matrix stored in the main function
      set <- function(y) {   
      x <<- y
      s <<- NULL
      }
          
      get <- function() x
      setsolve <- function(solve) s <<- solve  ## just stores the value
      getsolve <- function() s   ## just stores the value
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The function "cacheSolve" computes the inverse of the special "matrix" 
## returned by the "makeCacheMatrix" 

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
              
      ## If the inverse has already been calculated 
      ## (and the matrix has not changed) 
      ## then the inverse should been retrieved from the cache    
      if(!is.null(s)) {
          message("getting cached data")
          return(s)
      }
          
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
      ## Return a matrix that is the inverse of 'x'
}
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are used to cache the inverse of a matrix.


## The function "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse

## The function "set" changes matrix stored in the main function. The new 
## inverse needs to be recalculated through the function "cacheSolve"

## The function "get" returns the matrix x stored in "makeCacheMatrix"   
## The functions "setsolve" and "getsolve" stores the value
## The function list() stores the 4 functions in "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {   
    x <<- y
    s <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) s <<- solve  
    getsolve <- function() s  
    
    list(set = set, get = get, 
        setsolve = setsolve,
        getsolve = getsolve)
}


## The function "cacheSolve" returns the inverse of the matrix 
## If the inverse has already been calculated (and the matrix has not changed)
## it gets the result and skips the computation. If not, it computes the 
## inverse and sets the value in the cache via "setsolve" function

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
   
      if(!is.null(s)) {
          message("getting cached data")
          return(s)
      }

      data <- x$get()        ## gets the matrix stored with "makeCacheMatrix"
      
      s <- solve(data, ...)  
      
      x$setsolve(s)          ## stores inverse in the object generated  
                             ## assigned with "makeCacheMatrix"
      s
      ## Return a matrix that is the inverse of 'x'
}
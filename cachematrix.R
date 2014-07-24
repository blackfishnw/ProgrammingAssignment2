## Overview: Compute and cache a matrix, and its inverted form
## 

## function makeCacheMatrix creates a cache of a matrix
makeCacheMatrix <- function(x = matrix()) {
  
    mi <- NULL  
    
    # function to Set a copy of the matrix
    set <- function(y) {
      x <<- y
      mi <<- NULL
    }
    
    get <- function() x  #get the matrix
    setimatrix <- function(solve) mi <<- solve
    getimatrix <- function() mi
    
    # return a list containing functions (methods of cache object so to speak)
    list(set = set, get = get, setimatrix = setimatrix, getimatrix = getimatrix)
}


## cacheSolve computes the inverse of the matrix of 'x', if it
##  is not in the cache (assumes it has not changed)

cacheSolve <- function(x, ...) {
    
    m <- x$getimatrix() #get a copy of the inverted matrix from cache
    
    # test if the cache returned anything, and has not changed
    if(!is.null(m) {
      message("getting cached data")
      
      return(m) #return the cached inverted matrix
    }
    
    #at this point cache is empty, so compute inversion and store away for future use
    data <- x$get()
    m <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
    x$setimatrix(m)
    
    m  #return inverted matrix
}

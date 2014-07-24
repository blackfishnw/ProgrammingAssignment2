## Put comments here that give an overall description of what your
## functions do



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


## cacheSolve computes the inverse of the matrix of 'x', if it:
##  is not in the cache, and has not changed. 

cacheSolve <- function(x, ...) {
    
    m <- x$getimatrix()
    
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
    x$setimatrix(m)
    
    m  #return
}

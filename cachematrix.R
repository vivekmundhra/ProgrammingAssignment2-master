
## makeCacheMatrix creates a special matrix creates a special "matrix" object that can cache its inverse
## makeCacheMatrix returns list of functions which do the following operations
## get - get the martix passed in with makeCacheMatrix
## set - set a new matrix 
## getinverse - get the inverse of the matrix
## setinverse - set the inverse of the matrix to be cached
## Arguments:
##    x     a matrix, which has an inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  ##get - return the matrix  
  get <- function() {x}
  
  ##set - update the matrix, make sure that the 
  set <- function(m) {

    ##ONLY if the matrix is null or is not equal to the present
    ##reset the inverse
    if(is.null(m) ){
      x <<- m 
      inv <<- NULL
    } else {
      v <- which(m != x)
      if(length(v) != 0) {
        x <<- m 
        inv <<- NULL
      }
    }
  }
  
  ##getinverse the cached inverse for the matrix
  getinverse <- function() { 
    inv
  }

  ##setinverse the inverse of the matrix    
  setinverse <- function(i) { 
    inv <<- i
  }
    
  
  #return the list of functions  
  list (get = get, 
        set = set, 
        getinverse = getinverse, 
        setinverse = setinverse)
}
  
    
  ## cacheSolve encapsulates the 'solve' function
  ## and return a matrix that is the inverse of 'x'
  ## if the inverse already exists, the cached inverse is returned
  ##
  ## Arguments:
  ##    x     makeCachedMatrix
  ##    ...	  further arguments passed to solve
  cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    message("solving and setting cache")
    m <- solve(data, ...) #missing 'b', 'a' is an identity matrix
    x$setinverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m    
  }
  
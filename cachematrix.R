## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## To initialize the stored inverse value
  inv <- NULL
  
  ## To set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## To get the value of the matrix
  get <- function() x
  
  ## To set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## To get the inverse of the matrix
  getinverse <- function() inv
  
  ## Return a list of all the above functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Check if inverse is already cached
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  ## If not cached, we get the matrix into data
  data <- x$get()
  
  ## and calculate the inverse
  inv <- solve(data)
  
  ## set and retrieve the inverse
  x$setinverse(inv)
  inv
}

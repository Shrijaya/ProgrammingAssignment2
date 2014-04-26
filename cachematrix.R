## This function creates a special "matrix" object that can cache its inverse as per the Assignment.

## makeCacheMatrix will store the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {


# initialize the stored inverse value to NULL
  inv <- NULL
  
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv_) inv <<- inv_
  # to get the inverse
  getinv <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  

}


## ## cacheSolve function will calculates the inverse of the 
## "matrix" created with the makeCacheMatrix function. Before that  it will 
## checks  if the inverse has already been calculated. 
## If syes, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
# check if the inverse is already cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    
    return(inv)
  }
  # not cached, so we get the matrix into data
  data <- x$get()
  # and compute the inverse
  inv <- solve(data, ...)
  # then cache the inverse
  x$setinv(inv)
  # and return it as well
  inv
        
}

## a pair of functions that cache the inverse of a matrix because matrix 
## inversion is usually a costly computation.

## This function creates a special "matrix" object that can cache its 
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  #x is the source matrix defined in function call
  inv <- NULL #the inverse matrix, set to null since it is not calculated yet
  
  set <- function(y) {
    #set the value of the matrix in cache, clear (any) existing inverse
    #this function does not calculate the inverse matrix.
    x <<- y
    inv <<- NULL #clear stored inverse matrix since it won't match our new input
  }
  
  get <- function() x
  #return the value of the cached matrix
  #does not calculate or return the value of the inverse matrix
  
  setinverse <- function(inverse) inv <<- inverse
  #set the value of the inverse matrix in cache
  #does not set or clear the cached matrix
  
  getinverse <- function() inv
  #return the value of the cached inverse matrix
  #does not calculate or return the value of the cached matrix.
  
  #list of functions returned when makeCacheMatrix invoked
  #this must be at the bottom of the makeCacheMatrix function

  list(set = set, #names set(), defined above
       get = get, #names get(), defined above
       setinverse = setinverse, #names setinverse(), defined above
       getinverse = getinverse) #names getinverse(), defined above
  #Naming the list elements allows use of the $ form of the extract operator
  #to access these functions
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.
## REQUIRES an input argument of type makeCacheMatrix() as it searches 
## for an inverse matrix "inv" lexically scoped through it
cacheSolve <- function(x, ...) {
  inv <- x$getinverse() #retrieve inverse matrix from cache
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) #function returns the stored inverse matrix
  }
  
  #If we get here, then we need to calculate the inverse matrix
  data <- x$get() #retrieve the matrix from cache
  inv <- solve(data, ...) #calculate the inverse matrix
  x$setinverse(inv) #save the inverse matrix to cache
  inv # Return a matrix that is the inverse of 'x'
}

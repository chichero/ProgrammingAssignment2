## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = diag(2)) {
      # makeCacheMatrix(x) creates a matrix-like object that can strore the matrix itself
      # and its inverse. The inverse is not calculated by default. A call to cacheSolve()
      # is necessary. 
      # Arguments:
      # x - matrix (default, identity matrix 2x2)
      
      s <- NULL # s is the inverse of x - is set to NULL but calculated later in cacheSolve()
      
      # set() function updates the matrix part of the object
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      
      # get() function returns the matrix part of the object
      get <- function() x
      
      # setinv() function updates the inverse-matrix part of the object
      setinv <- function(inv) s <<- inv
      
      # getinv() function returns the inverse-matrix part of the object
      getinv <- function() s
      
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
      # cacheSolve(x) function calculates the inverse of the matrix object x.
      # If the inverse has been calculated before (and stored in x), it retrieves
      # the inverse from x
      
      s <- x$getinv() # check if calculated before
      
      # if alredy calculated, retrieve from x
      if(!is.null(s)) {
            message("getting cached data")
      }
      # if not, calculate and store
      else{
            data <- x$get() 
            s <- solve(data)
            x$setinv(s)
      }
      s
}
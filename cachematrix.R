# These two following functions does matrix inversion by making a matrix cache 
# then retrieving the inverse from the cache or computing it if it doesn't exist


# makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# This is done by setting the value of the matrix, then getting the 
# value of the inverse. Afterwards, the value of the inverse is set. Lastly, 
# makeCacheMatrix gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()){
      #set the value of the matrix
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      #get the value of the matrix
      get <- function() x
      #set the value of the inverse
      setinv <- function(solve) m <<- solve
      #get the value of the inverse
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


# cacheSolve function below computes the inverse of the matrix data. It checks 
# if it has already been calculated. If so, the calculated inverse is returned
# Otherwise, cacheSolve calculates the inverse of the data and sets the value of 
# the inverse in the cache via the solve function.

cacheSolve <- function(x, ...){
      m <- x$getinv
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      dt <- x$get()
      m <- solve(dt, ...)
      x$setinv(m)
      m
}

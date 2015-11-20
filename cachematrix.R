## This R function allows you to take the inverse of a matrix and cache 
## the value so that when it is needed again, it can be looked up
## rather than recomputed. 

## This function makes a special "matrix" which allows one to set the 
## value of the matrix, get its value, set the matrix inverse, and get 
## the matrix inverse. 

makeCacheMatrix <- function(a = matrix()) {
      m <- NULL
      set <- function(y) {
            a <<-y
            m <<- NULL
      }
      get <- function() a # does not require input arguments
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the matrix inverse of the special "matrix" 
## created. It first checks to see if the matrix inverse has been 
## calculated. If it has been, it obtains the inverse from the cache. 
## If the matrix inverse has not been calculated, it calculates it. 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
        ## Return a matrix that is the inverse of 'x'
}

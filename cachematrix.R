## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This will be done using two functions, makeCacheMatrix, and cacheSolve.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##                  makeCacheMatrix actually creates a list containing a function to
##                  1. set the matrix
##                  2. get the matrix
##                  3. set the inverse of the matrix
##                  4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #"set" is a function to set x as y (in the global environment)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #"get" is a function to return the original matrix, x
  get <- function() x
  
  #"setinverse" is a function to solve a matrix, m
  setinverse <- function(solve) m <<- solve
  
  #"getinverse" is a function to retrieve the inverse of the matrix, m
  getinverse <- function() m
  
  #returns a list of the above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #attempt to retrieve a cached version of the inverse matrix of x (if no cache, m becomes NULL)
  m <- x$getinverse() 
  
  #if there is a cached version, then m is not null, and this will return m, the cached inverse matrix, and exit the function
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) #also exits the function
  }
  
  #if no cache was found, will solve the inverse of x, cache it, and return it
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}


######

## Example:
## matrix <- matrix(data= c(1,3,2,4), nrow = 2, ncol= 2)
#### [,1] [,2]
#### [1,] -2.0  1.0
#### [2,]  1.5 -0.5
## b <-makeCacheMatrix(matrix)
## cacheSolve(b) #running the command first time to cache the inverse matrix
## cacheSolve(b) #running a second time to make sure the inverse matrix was cached (should say "getting cached data")

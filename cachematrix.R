## In the following assignement I am going to write a pair of functions that
cache the inverse of a matrix.


## This function creates a special "matrix" object
   that can cache its inverse.The function is a list containing a function to:

1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse of the matrix
4.  get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get<- function () x
  setinverse <- function(solve) p <<-solve
  getinverse <- function () p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
   If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache. 
   If so, it gets the inverse from the cache and skips the computation. Otherwise it calculates the inverse of the  data and sets the value 
   of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  
  p <- x[["getinverse"()]]				##Partial matching with [[ instead of $ because we are dealing with a matrix.
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- x[["get"()]]
  p <- solve(data, ...)
  x[["setinverse"(p)]]
  p
}

## Return a matrix that is the inverse of 'x'
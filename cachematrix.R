# In the following assignement I am going to write a pair of functions that
  cache the inverse of a matrix.


# This function creates a special "matrix" object
  that can cache its inverse.


 myMatrix<-matrix(1:4,2,2)
 makeCacheMatrix <- function(myMatrix) {
  
    p <- solve(myMatrix)
    set <- function(y) {
	 myMatrix <<- y
	 p <<- solve(myMatrix)
    }
    get<- function () myMatrix
    setsolve <- function(solve) p <<-solve
    getsolve <- function () p
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
 }

# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
  If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache. 
  If so, it gets the inverse from the cache and skips the computation. Otherwise it calculates the inverse of the  data and sets the value 
  of the inverse in the cache via the setinverse function.

 cacheSolve <- function(myMatrix) {
  
   p <- myMatrix$getsolve()
   if(!is.null(p)) {
     message("getting cached data")
     return(p)
   }
   data <- myMatrix$get()
   p <- solve(data, ...)
   myMatrix$setsolve(p)
   p
 }

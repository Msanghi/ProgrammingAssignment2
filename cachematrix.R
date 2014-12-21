
## makeCacheMatrix set a matrix. 
## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setmatrix <- function(Solve) inv <<- Solve
  
  getmatrix <- function() inv
  
  
  list(set = set, get = get,setmatrix = setmatrix,getmatrix = getmatrix)
  

}


## The cacheSolve function calculates the inverse of the  "matrix"
## which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it returns the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setmatrix function.


cacheSolve <- function(x=matrix(), ...) {
  inv<-x$getmatrix()
  if(!is.null(inv)){
    message("getting cached data.... done")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix)
  x$setmatrix(inv)
  inv
}
        

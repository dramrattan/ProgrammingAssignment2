## Functions to create a special "matrix" object 
## and allows for the caching of the inverse of the matrix

## Function creates a special "matrix" object with get,set,getSolve,setSolve functions

makeCacheMatrix <- function(x = matrix()) {
  nr <- nrow(x)
  nc <- ncol(x)
  s <- NULL
  set <- function(y){
    x <<- y
    s <- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## function returns the inverse of the passed special "matrix"
## calculates the inverse or returns cached inverse if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  s <- x$getSolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  mtrx <- x$get()
  s <- solve(mtrx,...)
  x$setSolve(s)
  s  
}

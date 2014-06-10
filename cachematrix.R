## The following functions solve the inverse of a matrix by cacheSolve and
## cache the result in the function makeCacheMatrix. cacheSolve will retrieve
## inverse matrix if it has already been calculated from makeCacheMatrix.

## This function saves any matrix, any previous calculated inverse of the
## matrix and has functions for retrieving these or setting these 

makeCacheMatrix <- function(x = matrix()) {
  # invertedm will be the inverted matrix if calculated previously
  invertedm <- NULL
  
  set <- function(y) {
    x <<- y
    invertedm <<- NULL
  }
  get <- function() x
  setinvertedm <- function(solved) invertedm <<- solved
  getinvertedm <- function() invertedm
  list(set = set, get = get,
       setinvertedm = setinvertedm,
       getinvertedm = getinvertedm)
}


## This function either retrieves the inverse matrix if previously calculated or
## solves for the inverse and caches this in verse via cached matrix above

cacheSolve <- function(x, ...) {
  invertedm <- x$getinvertedm()
  if(!is.null(invertedm)) {
    message("getting cached data")
    return(invertedm)
  }
  data <- x$get()
  invertedm <- solve(data)
  x$setinvertedm(invertedm)
  invertedm
}

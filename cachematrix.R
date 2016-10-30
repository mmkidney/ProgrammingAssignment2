## Assignment for R Programming
## SECTION BELOW IS RELEVANT FOR ASSESSMENT

## makeCacheMatrix takes a matrixes as a paremeter and create a matrix object that can 
## cache its inverse
## base matrix data can get gotten using <var>$get()
## base matrix data can be changed by using <var>$set(matrix 
## inverse can get gotten by getinverse() but will only return a value other than null
## if cacheSolve has already cached the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- setinverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve generates the inverse matrix give a matrix created through makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    ##Inverse has already been cached so skip operation
    message("getting cached data")
    return(i)
  }
  ##i is null therefore calculate inverse for matrix x
  data <- x$get()
  i <- solve(data, ...)
  ##Set inverse i for matrix object x
  x$setinverse(i)
  i
}

##BELOW IS NOT RELEVANT FOR ASSESSMENT
##Sample Functions for caching the mean of a Vector
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
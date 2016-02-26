## These functions create a staging space to cache and invert a matrix, then sets the inverse matrix.

## As with the mean example, but with inverse matrices instead: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
      }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This solves the inverse matrix.  First, if the matrix has already been solved,
## it merely pulls that data with a message that it is "getting cached data"
## Otherwise, it goes through the full process.

cacheSolve <- <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

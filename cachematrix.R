## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(m) i <<- m
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)){
          message("getting cached data")
          return(i)
     }
     else{
          matrix <- x$get()
          inverse <- solve(matrix)
          x$setinverse(inverse)
          inverse
     }
        
}

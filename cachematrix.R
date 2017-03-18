## The formatting of both functions is based on the example "makeVector" and "cachemean" functions from the course example.
## The cached inverse is represented using the variable "i".
## makeCacheMatrix takes an R matrix object as a parameter, and allows it to be set to be cached using x$setinverse().
## cacheSolve checks for the existence of a cached inverse, and calculates it if it is NULL.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

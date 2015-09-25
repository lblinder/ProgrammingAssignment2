## The makeCacheMatrix Function is designed to work nested inside the 
## cacheSolve funtion.  Both functions are used together to solve
## for the inverse of a matrix if it has not previously been inverted.

## create a list to "cache" or store the inverse of matrices

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## check and only if needed solve for the inverse of a matrix
##        store the result if it didn't exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
          m <- x$getinv()
          if(!is.null(m)) {
               message("getting cached data")
               return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinv(m)
          m

}
## below is an example of using these functions

## x<- makeCacheMatrix( matrix(c(1,2,3,4), nrow =2, ncol=2))
## cacheSolve(x)

## Notes: 
## submitting cacheSolve(x) the first time, the function will return the inverse
## submitting it again will still return the inverse, but from the stored list
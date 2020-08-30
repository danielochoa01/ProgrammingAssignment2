## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function caches the value of the inverse matrix
## parameter x: a matrix
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
       setmean = setmean,
       getmean = getmean)  
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix function.
## parameter x: the return vale from makeCacheMatrix function
## parameter ...: additional parameters
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message('getting cached data')
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}

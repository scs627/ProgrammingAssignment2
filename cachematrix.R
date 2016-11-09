## To avoid repeatedly calculating the inverse of a matrix, create a special matrix object that can cache its inverse, then retrieve the inverse from the cache if it has already been calculated

## Make a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m_inv <<- inv
  getinv <- function() m_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Compute the inverse of cache matrix 
## or retrieve inverse from the cache if the inverse has already been calculated 

cacheSolve <- function(x, ...) {
    m_inv <- x$getinv()
    if(!is.null(m_inv)) {
      message("getting cached data")
      return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$setinv(m_inv)
    ## Return a matrix that is the inverse of 'x'
    m_inv
}

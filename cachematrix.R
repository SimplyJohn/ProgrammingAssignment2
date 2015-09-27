## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMat <- NULL
  set <- function(y) {
    x <<- y
    inverseMat <<- NULL
  }
  get <- function() x
  setInverseMat <- function(im) inverseMat <<- im
  getInverseMat <- function() inverseMat
  list(set = set, get = get,
       setInverseMat = setInverseMat,
       getInverseMat = getInverseMat)  
}


## This functions finds the inverse matrix of a "special" cached matrix.
## if the inverse was already calculated it retrieves it from the cache

cacheSolve <- function(x, ...) {
  im <- x$getInverseMat()
  data <- x$get()
  
  if(!is.null(im)) {
    message("getting inverse cached matrix")
    return(im)
  }
  
  im <- solve(data, ...)
  x$setInverseMat(im)
  im
}
